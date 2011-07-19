;;;;;; Strings-replace                                   -*- Scheme -*-
;;;;;; A substring replacer for multiple parallel replacements.

;;; Taylor Campbell wrote this code; he places it in the public domain.

;;; This code depends on SRFIs 8 & 23, as well as Taylor Campbell's
;;; string collector library.

;;; (STRINGS-REPLACE <input-string> <transformations> <char=?>)
;;;       -> string
;;;     TRANSFORMATIONS -> (list-of (cons <from> <to>))
;;;   Transform each FROM to TO in INPUT-STRING.  The transformation is
;;;   strictly parallel; once a transformation from a FROM to a TO has
;;;   taken place, the contents of TO in that position will never be
;;;   considered as if they were in the initial INPUT-STRING.  CHAR=?
;;;   is used to compare characters.
;;;
;;; There is only one major shortcoming (as far as I know): it's just
;;; strings -- no regexps, no output based on input patterns, et cetera
;;; --.

(define (strings-replace input replacements char=?)

  ;; States ought to be pooled.  They're allocated and deallocated very
  ;; frequently during the replacing process.  Perhaps pooling of them
  ;; could be parameterized?  Neil van Dyke did that with his library
  ;; for calculating Levenshtein distances.

  ;; The OVER field of states indicates the size of the output string
  ;; that will be overwritten.  This is necessary because some states
  ;; will overwrite the output string to something with a different
  ;; size, but other states might continue, so we need to keep track of
  ;; what has been overwritten.  If nothing was overwritten, the OVER
  ;; field is just #F.

  (define (make-state input output)
    ;; Start with an index of 1, because we already tested the first
    ;; character; if we didn't, we'd not call MAKE-STATE.
    (vector input 1 output #f))
  (define (state-input  state) (vector-ref state 0))
  (define (state-index  state) (vector-ref state 1))
  (define (state-output state) (vector-ref state 2))
  (define (state-over   state) (vector-ref state 3))

  (define (step-state state)
    (vector (state-input state)
            (+ (state-index state) 1)
            (state-output state)
            (cond ((state-over state) => (lambda (over) (+ over 1)))
                  (else #f))))

  (define (state-with-over state over)
    (vector (state-input state)
            (state-index state)
            (state-output state)
            over))

  (define (state-finished? state)
    (= (state-index state) (string-length (state-input state))))

  (define (apply-state! state out-sc)
    (recede-string-collector! out-sc
                              (or (state-over state)
                                  (string-length (state-input state))))
    (collect-string! out-sc (state-output state)))

  ;; Apply a state if possible, and return a list of states that might
  ;; still be applicable.
  (define (maybe-apply-states! states out-sc)
    (receive (finished unfinished)
             (collect-finished-states states '() '())
      (cond ((null? finished) states)
            ((null? (cdr finished))
             (let ((finished-state (car finished)))
               (apply-state! finished-state out-sc)
               (collect-states-with-over states finished-state
                 (string-length (state-output finished-state))
                 '())))
            (else
             (error "More than one transformation for one input"
                    (map (lambda (state)
                           (list (state-input  state)
                                 (state-output state)))
                         finished))))))

  (define (collect-finished-states states finished unfinished)
    (if (null? states)
        (values finished unfinished)
        (let ((state (car states)))
          (if (state-finished? state)
              (collect-finished-states (cdr states)
                                       (cons state finished)
                                       unfinished)
              (collect-finished-states (cdr states)
                                       finished
                                       (cons state unfinished))))))

  ;; Update all of the states' overs to reflect BASE's new output, but
  ;; throw out all of those that didn't start at the same index in the
  ;; input string as BASE.
  (define (collect-states-with-over states base over states-kept)
    (if (null? states)
        states-kept
        (collect-states-with-over (cdr states) base over
          (let ((state (car states)))
            (if (= (state-index state) (state-index base))
                (cons (state-with-over state over) states-kept)
                states-kept)))))

  (define (collect-live-states states in-char distance)
    (let* ((old-states (step-filtered-states states in-char '())))
      (collect-new-states replacements in-char distance old-states)))

  ;; Kill states that can't continue, and step forward those states
  ;; that aren't dead yet.
  (define (step-filtered-states states in-char tail)
    (if (null? states)
        tail
        (let ((state (car states)))
          (step-filtered-states (cdr states) in-char
            (if (and (< (state-index state)
                        (string-length (state-input state)))
                     (char=? (string-ref (state-input state)
                                         (state-index state))
                             in-char))
                (cons (step-state state) tail)
                tail)))))

  ;; Generate a list of the new potential states.
  (define (collect-new-states replacements in-char distance tail)
    (if (null? replacements)
        tail
        (let ((r (car replacements)))
          (collect-new-states (cdr replacements) in-char distance
            (if (and (char=? (string-ref (car r) 0) in-char)
                     ;; This mild optimization prevents starting states
                     ;; that would just fall off the end of the input
                     ;; string.
                     (<= (string-length (car r)) distance))
                (cons (make-state (car r) (cdr r)) tail)
                tail)))))

  ;; Main loop.
  (define (loop in-index out-sc states countdown)
    (if (zero? countdown)
        (string-collector->string out-sc)
        (let* ((in-char (string-ref input in-index))
               (live-states (collect-live-states states in-char
                                                 countdown)))
          (collect-char! out-sc in-char)
          (loop (+ in-index 1)
                out-sc
                (if (null? live-states)
                    '()                 ; Minor optimization here.
                    (maybe-apply-states! live-states out-sc))
                (- countdown 1)))))

  (loop 0 (make-string-collector) '() (string-length input)))
