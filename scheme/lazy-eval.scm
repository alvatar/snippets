; Lazy evaluation with shift/reset
;
; Lazy evaluation, aka call-by-need, is a call-by-name evaluation with the 
; memoization of the result. A lazy expression is not evaluated until 
; its result is needed. At that point, the expression is evaluated and 
; the result is memoized. Further evaluations of the lazy expression 
; merely retrieve the memoized result. Thus a lazy expression is
; evaluated at most once. 
; Lazy expressions may nest: a lazy expression may include
; other lazy expressions.
; We implement lazy evaluation using delimited continuations rather
; than mutation.
;
; If a lazy expression is to be treated first-class, it has to be, in a 
; call-by-value language such as Scheme, to be embedded in a thunk
; (or quoted in some other way).
;
; The only tricky issue is `identifying' expressions. This is
; indeed tricky in any kind of memoization (see the paper by Amit
; and Harper at POPL2003). In the present code, we use essentially
; gensym. Again, we use no mutation.

(load "shift-reset.scm")

; The following is a simple Scheme implementation of the following
; datatype:
; data H v exp k = HV v | HIDReq k | H expr-id exp k

; constructors
(define (hv a) a)
(define (hv-tag) (hv-tag))
(define (h-id-req k) (list hv-tag k))
(define (h expr-id exp k) (list hv-tag expr-id exp k))

; deconstructor
(define-syntax case-h
  (syntax-rules ()
    ((case-h e (a on-hv) (kid on-h-id) ((expr-id exp k) on-h))
     (let ((v e))
       (if (and (pair? v) (eq? hv-tag (car v)))
	 (if (null? (cddr v))
	   (let ((kid (cadr v))) on-h-id)
	   (apply (lambda (expr-id exp k) on-h) (cdr v)))
	 (let ((a v)) on-hv))))))

(define-syntax lazy-exp
  (syntax-rules ()
    ((lazy-exp exp)
      (let ((expr-id (shift f (h-id-req f))))
	(lambda () (shift f (h expr-id (lambda () exp) f)))))))

(define (top-level body-thunk)
  (car
    (let loop ((id-counter 1)
	       (memo-table '())
	       (r (reset (hv (body-thunk)))))
    (case-h r
      (v (list v id-counter memo-table)) ; got the result -- yield it and memo-t
      (kid (loop (+ 1 id-counter) memo-table (kid id-counter))) ; req for id
      ((expr-id exp k)
	(cond
	  ((assoc expr-id memo-table)	; computed already
	    => (lambda (a) (loop id-counter memo-table (k (cdr a)))))
	  (else				; compute and memoize
	                                ; exp may use lazy eval itself
	    (let* ((vr (loop id-counter memo-table (reset (hv (exp)))))
		   (v (car vr))
		   (id-counter1 (cadr vr))
		   (memo-table1 (caddr vr)))
	      (loop id-counter1 (cons (cons expr-id v) memo-table1)
		(k v))))))))))

; Tests

; First test: each of the labels "lazy exp 1" and "lazy exp 2"
; should be printed exactly once.
; Label "lazy exp 3" should not be printed at all
(display "First test of lazy eval") (newline)
(top-level (lambda ()
 (let ((le1 (lazy-exp (begin (display "lazy exp 1") (newline) 10)))
       (le2 (lazy-exp (begin (display "lazy exp 2") (newline) 20)))
       (le3 (lazy-exp (begin (display "lazy exp 3") (newline) 30)))
      )
   (display (+ (le1) (le2) (le1) (le1) (le2))))))
(newline)
;; lazy exp 2
;; lazy exp 1
;; 70


(display "Nested lazy eval. Each label must be printed once") (newline)
(top-level (lambda ()
 (let* ((le1 (lazy-exp (begin (display "lazy exp 1") (newline) 10)))
        (le2 (lazy-exp (begin (display "lazy exp 1-2") (newline)
			  (+ (le1) 10))))
        (le3 (lazy-exp (begin (display "lazy exp 1-3") (newline)
			  (+ (le1) (le2)))))
       )
   (display (+ (le2) (le1) (le3) (le3) (le2))))))
(newline)
; The order of the labels below may vary (since the order of evaluating
; the arguments is not defined in Scheme). It is most likely that
; the label "lazy exp 1" is not printed first.

;; lazy exp 1-2
;; lazy exp 1
;; lazy exp 1-3
;; 110


; Lazy data structures
;; Lazy Fibonacci stream

(define-syntax llcons			; cons, lazy in both arguments
  (syntax-rules ()
    ((llcons x y) (cons (lazy-exp x) (lazy-exp y)))))

(define (force ll)			; force the lazy expression if needed
  (if (procedure? ll) (ll) ll))

(define (lhead ll) (car (force ll)))
(define (ltail ll) (cdr (force ll)))

(define (lmap f . lls)
  (let loop ((lls lls) (args '()) (tts '()))
    (if (null? lls) 
      (llcons (apply f (reverse args)) (apply lmap f (reverse tts)))
      (let ((ll (force (car lls))))
	(if (null? ll) '()		; shorter list terminates
	  (loop (cdr lls) (cons (car ll) args) (cons (cdr ll) tts)))))))

(define (take n ll)
  (if (zero? n) '()
    (let ((e (force ll)))
      (if (null? e) '()
	(cons (force (car e)) (take (- n 1) (cdr e)))))))

(display "Tests of lazy map") (newline)
; `datum 1' and `datum 2' should be printed only once
(display
  (let ((datum 
	  (lambda (i) (display "datum ") (display i) (newline) i)))
  (top-level (lambda () (take 3 
		 (lmap force (lmap force 
		       (llcons (datum 1) (llcons (datum 2) '())))))))))
(newline) (newline)
; (1 2)

(top-level (lambda () (take 3 (lmap
  (lambda args (display (map force args)) (newline))
  (llcons 1 (llcons 2 (llcons 'none '())))
  (llcons 3 (llcons 4 '()))))))
; (1 3) (2 4)

; fib = 1:1:zipWith (+) fib (tail fib)
(display "Fibonacci test") (newline)
(top-level (lambda ()
 (define fib
  (let ((fib-datum 
	  (lambda (i) (display "fib datum ") (display i) (newline) i)))
  (llcons (fib-datum 1)
    (llcons (fib-datum 1)
      (lmap (lambda (x y) (fib-datum (+ (force x) (force y))))
		  fib (ltail fib))))))
 (display (take 10 fib))))
(newline)

