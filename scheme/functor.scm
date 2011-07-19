; An implementation of David Rush's functor system, by Taylor Campbell, using
; simpler macrology and some differing macros.

; Create an anonymous functor.
(define-syntax functor
  (syntax-rules (=> <=)
    ((FUNCTOR (import ...) => (export ...) body1 body2 ...)
     (LAMBDA (K import ...)
       body1 body2 ...
       (K export ...)))
    ((FUNCTOR (export ...) <= (import ...) body1 body2 ...)
     (FUNCTOR (import ...) => (export ...) body1 body2 ...))))

; Convenience for defining functors.
(define-syntax define-functor
  (syntax-rules (=> <=)
    ((DEFINE-FUNCTOR (name import ...) => (export ...)
       body1 body2 ...)
     (DEFINE name (FUNCTOR (import ...) => (export ...)
                    body1 body2 ...)))
    ((DEFINE-FUNCTOR (export ...) <= (name import ...)
       body1 body2 ...)
     (DEFINE-FUNCTOR (name import ...) => (export ...)
       body1 body2 ...))))

; Evaluate code within a functor and given arguments.
(define-syntax within-functor
  (syntax-rules (=> <=)
    ((WITHIN-FUNCTOR (functor arg ...) => (import ...)
       body1 body2 ...)
     (functor (LAMBDA (import ...) body1 body2 ...)
              arg ...))
    ((WITHIN-FUNCTOR (import ...) <= (functor arg ...)
       body1 body2 ...)
     (WITHIN-FUNCTOR (functor arg ...) => (import ...)
       body1 body2 ...))))

; Instantiate a functor: define all of the names it exports at the top level.
(define-syntax instantiate-functor
  (syntax-rules (=> <=)
    ((INSTANTIATE-FUNCTOR (functor arg ...) => (import ...))
     (DEFINE-VALUES (import ...)
       (functor VALUES arg ...)))
    ((INSTANTIATE-FUNCTOR (import ...) <= (functor arg ...))
     (INSTANTIATE-FUNCTOR (functor arg ...) => (import ...)))))

; Nifty example of using functors.  NUMERIC-FACT computes factorials with their
; numeric definition, but PAIR-FACT takes a list of N elements and returns a
; list of N! elements, and the code for NUMERIC-FACT & PAIR-FACT is the same.
;
; (define-functor (fact-functor < two one * sub1) => (fact)
;   (define (fact n)
;     (if (< n two)
;         one
;         (* n (fact (sub1 n))))))
;
; (instantiate-functor (numeric-fact) <=
;   (fact-functor < 2 1 * (lambda (x) (- x 1))))
; (instantiate-functor (pair-fact) <=
;   (fact-functor (lambda (l r)         ; <
;                   (let loop ((l l) (r r))
;                     (cond ((null? l) (not (null? r)))
;                           ((null? r) #f)
;                           (else      (loop (cdr l) (cdr r))))))
;                 '(a a)                ; two
;                 '(a)                  ; one
;                 (lambda (x y)         ; *
;                   (let loop ((r '()) (y y))
;                     (if (null? y)
;                         r
;                         (loop (append x r) (cdr y)))))
;                 cdr))                 ; sub1
;
; Another example of the same idea.
;
; (define-functor (fib-functor <= zero one two + sub1) => (fib)
;   (define (fib n)
;     (cond ((<= n one) zero)
;           ((<= n two) one)
;           (else       (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))))))
;
; (instantiate-functor (numeric-fib) <=
;   (fib-functor <= 0 1 2 + (lambda (x) (- x 1))))
; (instantiate-functor (pair-fib) <=
;   (fib-functor (lambda (l r)          ; Our listic <= comparison operator.
;                  (let loop ((l l) (r r))
;                    (cond ((null? l) #t)
;                          ((null? r) #f)
;                          (else      (loop (cdr l) (cdr r))))))
;                '()                    ; zero
;                '(a)                   ; one
;                '(a a)                 ; two
;                append                 ; +
;                cdr))                  ; sub1
