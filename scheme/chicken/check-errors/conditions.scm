;;;; conditions.scm
;;;; Kon Lovett, Apr '09

;;;

(module conditions (;export
  make-exn-condition
  make-exn-condition+
  make-condition+
  condition-predicate*
  condition-property-accessor*
  (make-condition-predicate condition-predicate*)
  (make-condition-property-accessor condition-property-accessor*))

  (import scheme chicken (only srfi-1 alist-cons) #;srfi-12 type-checks)

  (require-library srfi-1 #;srfi-12 type-checks)

;;

(define (make-exn-condition loc msg args)
  (apply make-property-condition
         'exn
         (append (if loc `(location ,loc) '())
                 (if msg `(message ,msg) '())
                 (if (and args (not (null? args))) `(arguments ,args) '()))) )

;; cond:
;; <condition>  ->  <condition>
;; <symbol>     ->  (make-property-condition <symbol>)
;; <pair>       ->  (apply make-property-condition <pair>)
;;   (<symbol> [<symbol> <object>]...)

(define (expand-property-conditions cnds)
   (map (lambda (cnd)
          (cond ((condition? cnd)  cnd )
                ((symbol? cnd)     (make-property-condition cnd) )
                ((pair? cnd)       (apply make-property-condition cnd) ) ) )
        cnds) )

;;

(define (make-exn-condition+ loc msg args . cnds)
  (apply make-composite-condition
         (make-exn-condition loc msg args)
         (expand-property-conditions cnds)) )

;;

(define (make-condition+ . cnds)
  (apply make-composite-condition (expand-property-conditions cnds)) )

;;

(define condition-predicate*
  (let ((preds '()))
    (lambda (kind)
      (check-symbol 'condition-predicate* kind)
      (let ((cell (assq kind preds)))
        (if cell (cdr cell)
            (let ((pred (condition-predicate kind)))
              (set! preds (alist-cons kind pred preds))
              pred ) ) ) ) ) )

;;

(define condition-property-accessor*
  (let ((accrs '()))
    (lambda (kind prop #!optional dflt)
      (check-symbol 'condition-property-accessor* kind)
      (check-symbol 'condition-property-accessor* prop)
      (let ((cell (assoc (cons kind prop) accrs)))
        (if cell (cdr cell)
            (let ((accr (condition-property-accessor kind prop dflt)))
              (set! accrs (alist-cons (cons kind prop) accr accrs))
              accr ) ) ) ) ) )

;;

(define-syntax make-condition-predicate
  (syntax-rules ()
    ((_ kind0 ...) (lambda (obj) (and ((condition-predicate* 'kind0) obj) ...) ) ) ) )

;;

(define-syntax make-condition-property-accessor
  (syntax-rules ()
    ((_ kind prop) (make-condition-property-accessor kind prop #f) )
    ((_ kind prop dflt) (condition-property-accessor* 'kind 'prop dflt) ) ) )

) ;module conditions
