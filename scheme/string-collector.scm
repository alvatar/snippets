;;;;;; String collectors                                 -*- Scheme -*-

;;; This code is written by Taylor Campbell, though influenced by Olin
;;; Shivers' own string collector utility; he places it in the public
;;; domain.

;;; This code depends on SRFIs 9, 13, and 23.

;;; The interface is fairly self explanatory.
;;;
;;; (MAKE-STRING-COLLECTOR)
;;; (CLEAR-STRING-COLLECTOR! <string-coll>)
;;; (COLLECT-STRING! <string-coll> <string>)
;;; (COLLECT-CHAR! <string-coll> <char>)
;;; (STRING-COLLECTOR->STRING <string-coll>)
;;; (STRING-COLLECTOR->CACHED-STRING <string-coll>)
;;; (RECEDE-STRING-COLLECTOR! <string-coll> <char-count>)
;;;
;;; This library differs from Olin's in that there is a variant of
;;; STRING-COLLECTOR->STRING that caches the result, and there is a
;;; facility for 'discollecting' strings, RECEDE-STRING-COLLECTOR!.

(define-record-type rtd/string-collector
  (really-make-string-collector chunks length chunk left)
  string-collector?
  (chunks string-collector-chunks set-string-collector-chunks!)
  (length string-collector-length set-string-collector-length!)
  (chunk  string-collector-chunk  set-string-collector-chunk!)
  (left   string-collector-left   set-string-collector-left!))

(define (make-string-collector)
  (really-make-string-collector '() 0 #f 0))

(define CHUNK-SIZE 128)                 ; Just a guess for a good size.

(define (clear-string-collector! sc)
  (set-string-collector-chunks! sc '())
  (set-string-collector-length! sc 0)
  (set-string-collector-chunk!  sc #f)
  (set-string-collector-left!   sc 0))

(define (collect-string! sc string)

  (define (push-chunk! chunk)
    (set-string-collector-chunks! sc
      (cons chunk (string-collector-chunks sc))))

  (define (copy-with-chunk! chunk chunk-left slen)
    (cond ((> chunk-left slen)          ; It fits in the current chunk.
           (string-copy! chunk (- (string-length chunk) chunk-left)
                         string 0 slen)
           (set-string-collector-left! sc (- chunk-left slen)))
          (else                         ; It won't fit.
           (string-copy! chunk (- (string-length chunk) chunk-left)
                         string 0 chunk-left)
           (push-chunk! chunk)
           (let ((string-left (- slen chunk-left)))
             (cond ((zero? string-left)
                    ;; It was completely copied into the current chunk.
                    (set-string-collector-chunk! sc #f))
                   ((>= string-left CHUNK-SIZE)
                    ;; A lot of characters left.  Push them as a single
                    ;; chunk.
                    (push-chunk!
                     (substring/shared string chunk-left slen))
                    (set-string-collector-chunk! sc #f))
                   (else
                    ;; It will fit into another chunk.
                    (let ((new-chunk (make-string CHUNK-SIZE)))
                      (string-copy! new-chunk 0
                                    string    chunk-left
                                    (+ chunk-left string-left))
                      (set-string-collector-chunk! sc new-chunk)
                      (set-string-collector-left! sc
                        (- CHUNK-SIZE string-left)))))))))

  (define (copy-without-chunk! slen)
    (if (>= slen CHUNK-SIZE)
        (push-chunk!                    ; Too many characters to bother
         (string-copy string))          ;   allocating a fresh chunk.
        (let ((new-chunk (make-string CHUNK-SIZE)))
          (set-string-collector-chunk! sc new-chunk)
          (string-copy! new-chunk 0 string 0 slen)
          (set-string-collector-left! sc (- CHUNK-SIZE slen)))))

  (define (update-length! slen)
    (set-string-collector-length! sc
      (+ slen (string-collector-length sc))))

  (let ((slen (string-length string)))
    (cond ((zero? slen))
          ((= slen 1)                   ;+++
           (collect-char! sc (string-ref string 0)))
          ((string-collector-chunk sc)
           => (lambda (chunk)
                (copy-with-chunk! chunk (string-collector-left sc)
                                  slen)
                (update-length! slen)))
          (else
           (copy-without-chunk! slen)
           (update-length! slen)))))

(define (collect-char! sc char)

  (define (push-chunk! chunk)
    (set-string-collector-chunks! sc
      (cons chunk (string-collector-chunks sc))))

  (cond ((string-collector-chunk sc)
         => (lambda (chunk)
              (let ((left (string-collector-left sc)))
                (string-set! chunk (- (string-length chunk) left) char)
                (cond ((= left 1)
                       (push-chunk! chunk)
                       (set-string-collector-chunk! sc #f))
                      (else
                       (set-string-collector-left! sc (- left 1)))))))
        (else
         (let ((new-chunk (make-string CHUNK-SIZE)))
           (string-set! new-chunk 0 char)
           (set-string-collector-left! sc (- CHUNK-SIZE 1))
           (set-string-collector-chunk! sc new-chunk))))
  (set-string-collector-length! sc (+ 1 (string-collector-length sc))))

(define (string-collector->string sc)
  (let* ((out-len (string-collector-length sc))
         (out-string (make-string out-len))
         (operating-chunk (string-collector-chunk sc))
         (operating-chunk-len (if operating-chunk
                                  (- (string-length operating-chunk)
                                     (string-collector-left sc))
                                  0)))
    (if operating-chunk
        (string-copy! out-string (- out-len operating-chunk-len)
                      operating-chunk 0 operating-chunk-len))
    (let loop ((chunks (string-collector-chunks sc))
               (cursor (- out-len operating-chunk-len)))
      (if (zero? cursor)
          out-string
          ;; There must be more chunks to copy.  Everything should be
          ;; well-aligned.
          (let* ((chunk (car chunks))
                 (chunk-len (string-length chunk))
                 (cursor* (- cursor chunk-len)))
            (string-copy! out-string cursor* chunk 0 chunk-len)
            (loop (cdr chunks) cursor*))))))

(define (string-collector->cached-string sc)
  (let ((out-string (string-collector->string sc)))
    (set-string-collector-chunk!  sc #f)
    (set-string-collector-chunks! sc (list out-string))
    (string-copy out-string)))

(define (recede-string-collector! sc count)

  (define (pop-chunks! count)
    (let loop ((chunks (string-collector-chunks sc))
               (count count))
      (let* ((chunk (car chunks))
             (chunk-len (string-length chunk)))
        (cond ((= chunk-len count)      ; Last one: it fits exactly.
               (set-string-collector-chunks! sc (cdr chunks)))
              ((> chunk-len count)      ; Last one: it doesn't fit.
               (set-string-collector-chunk!  sc chunk)
               (set-string-collector-left!   sc count)
               (set-string-collector-chunks! sc (cdr chunks)))
              (else
               (loop (cdr chunks) (- count chunk-len)))))))

  (let ((sclen (string-collector-length sc)))
    (cond ((= sclen count)
           (clear-string-collector! sc))
          ((< sclen count)
           (error "Can't recede string collector that far" sc count))
          ((string-collector-chunk sc)
           => (lambda (chunk)
                (let* ((left (string-collector-left sc))
                       (len (- (string-length chunk) left)))
                  (cond ((< count len)
                         (set-string-collector-left! sc
                           (+ left count)))
                        (else
                         (set-string-collector-chunk! sc #f)
                         (if (> count len)
                             ;; Only if they're inequal need we do it.
                             (pop-chunks! (- count len))))))))
          (else (pop-chunks! count)))

    (set-string-collector-length! sc (- sclen count))))
