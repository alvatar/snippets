;;;; special-case.scm -- special casing optimizations
;;
;; This code is written by Alex Shinn and placed in the
;; Public Domain.  All warranties are disclaimed.

;; Premature optimization is the root of all evil!
;;
;; These macros will double your code size!  Use at your own risk!
;;
;; _But_ if you need speed enough that you're going to write special
;; cases anyway, using these macros will at least make that a little
;; cleaner and more manageable.

;; Examples:
;;
;; Defines the SRFI-1 `member' procedure with optional equality
;; argument, but duplicates the inner loop conditioned on eq,
;; special-casing one on the eq? case.
;;
;; (define (member key ls . o)
;;   (let ((eq (if (pair? o) (car o) equal?)))
;;     (with-special-cased-procedures ((eq eq?))
;;       (let lp ((ls ls))
;;         (cond
;;          ((null? ls)
;;           #f)
;;          ((eq key (car ls))
;;           ls)
;;          (else
;;           (lp (cdr ls))))))))
;;
;; Defines a for-each variation that optionally writes trace output
;; every 100 elements.  Two versions of the inner loop get created,
;; one with and one without the tracing call, so that there's no extra
;; overhead when not tracing.
;;
;; (define (for-each/trace proc ls . o)
;;   (with-special-cased-conditional (trace? (and (pair? o) (car o)))
;;     (let lp ((ls ls) (i 0))
;;       (if (and (trace?) (zero? (modulo i 100)))
;;           (print "iteration: " i))
;;       (cond
;;        ((pair? ls)
;;         (proc (car ls))
;;         (lp (cdr ls) (+ i 1)))))))

(module special-case
  (with-special-cased-procedures
   with-all-special-cased-procedures
   with-special-cased-conditional)

(import scheme)

(define-syntax with-all-special-cased-procedures
  (syntax-rules ()
    ((with-all-special-cased-procedures ()
       . body)
     (let () . body))
    ((with-all-special-cased-procedures ((param specific) . rest)
       . body)
     (cond
      ((eq? param specific)
       (letrec-syntax ((param
                        (syntax-rules ()
                          ((_ . args)
                           (specific . args)))))
         (with-all-special-cased-procedures rest . body)))
      (else
       (with-all-special-cased-procedures rest . body))))))

(define-syntax with-special-cased-procedures
  (syntax-rules ()
    ((with-special-cased-procedures ((param specific) ...)
       . body)
     (if (and (eq? param specific) ...)
         (letrec-syntax ((param
                          (syntax-rules ()
                            ((_ . args)
                             (specific . args))))
                         ...)
           (let ()
             . body))
         (let ()
           . body)))))

(define-syntax with-special-cased-conditional
  (syntax-rules ()
    ((with-special-cased-conditional (test expr)
       . body)
     (if expr
         (let-syntax ((test (syntax-rules () ((_) #t)))) . body)
         (let-syntax ((test (syntax-rules () ((_) #f)))) . body)))))

)
