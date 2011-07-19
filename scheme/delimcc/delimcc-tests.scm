; Test suite for multip-prompt delimited control delimcc.mli
; This is the Scheme version of the OCaml test suite testd0.ml

(load "delimcc.scm")

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (cout "Testing " title nl)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (error 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(define-syntax assert
  (syntax-rules ()
    ((_ e) 
      (if (not e)
	(error 'assertion
	  "Assertion failed: ~a~%"
	  'e)))))

(define nl (string #\newline))

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

; less-efficient abort, used for testing
(define (abort-too p v)
  (take-subcont p sk v))

(test-check 'test1
  (let ((p (new-prompt)))
    (assert (not (prompt-set? p)))
    (push-prompt p (assert (prompt-set? p)) 1))
  1)


(test-check 'test2
  (let ((p (new-prompt)))
    (+ (push-prompt p (push-prompt p 5))
      4))
  9)

(test-check 'test3
  (let ((p (new-prompt)))
    (+ (push-prompt p (+ (abortP p 5) 6))
      4))
  9)

(test-check 'test3-less-efficient
  (let ((p (new-prompt)))
    (+ (push-prompt p (+ (abort-too p 5) 6))
      4))
  9)

(test-check 'test3-1
  (let ((p (new-prompt)))
    (+ (push-prompt p (push-prompt p (+ (abortP p 5) 6)))
      4))
  9)

(test-check 'test3-2
  (let ((p (new-prompt)))
    (let ((v (push-prompt p
	       (let* ((v1 (push-prompt p (+ (abortP p 5) 6)))
		      (v1 (abortP p 7)))
		 (+ v1 10)))))
      (+ v 20)))
  27)

(test-check 'test3-2-less-efficient
  (let ((p (new-prompt)))
    (let ((v (push-prompt p
	       (let* ((v1 (push-prompt p (+ (abort-too p 5) 6)))
		      (v1 (abort-too p 7)))
		 (+ v1 10)))))
      (+ v 20)))
  27)

'(test-check 'test3-3
  (let ((p (new-prompt)))
    (let ((v (push-prompt p
	       (let* ((v1 (push-prompt p (+ (abortP p 5) 6)))
		      (v1 (abortP p 7)))
		 (+ v1 10)))))
      (abortP p 9)
      (+ v 20)))
  'must-be-error)


(test-check 'test3-3-1
  (let ((p (new-prompt)))
    (let ((v (push-prompt p
	       (let* ((v1 (push-prompt p (+ (abortP p 5) 6)))
		      (v1 (abortP p 7)))
		 (+ v1 10)))))
      (prompt-set? p)))
  #f)

(test-check 'test4
  (let ((p (new-prompt)))
    (+ (push-prompt p 
	 (+ (take-subcont p sk (push-prompt p (push-subcont sk 5)))
	   10))
      20))
  35)

;The classical shift test 
;(display (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))))
; --> 117

(test-check 'test5
  (let ((p0 (new-prompt))
	(p1 (new-prompt)))
    (+ 10
      (push-prompt p0 (+ 2 (shift p0 k (+ 100 (k (k 3))))))))
  117)

(test-check 'test5-1
  (let ((p0 (new-prompt))
	(p1 (new-prompt)))
    (+ 10
      (push-prompt p0 (+ 2 (shift p0 k (+ 100 (k 3)))))))
  115)


(test-check 'test5-2
  (let ((p0 (new-prompt))
	(p1 (new-prompt)))
    (+ 10
      (push-prompt p0 (+ 2 (shift p0 k (+ 100 (k
						 (push-prompt p1
						   k (abortP p1 3)))))))))
  115)

(test-check 'test5-3
  (let ((p0 (new-prompt))
	(p1 (new-prompt)))
    (+ 10
      (push-prompt p0 (+ 2 ((shift p0 k (+ 100 (k (lambda ()
						 (push-prompt p1
						   (+ 9 (k (lambda ()
							     (abortP p1 3)))))))
					  )))))))
  115)

(test-check 'test5-4
  (let ((p0 (new-prompt))
	(p1 (new-prompt)))
    (+ 10
      (push-prompt p0 (+ 2 ((shift p0 k (+ 100 (k (lambda ()
						 (push-prompt p1
						   (+ 9 (k (lambda ()
							     (abortP p0 3)))))))
					  )))))))
  124)


(test-check 'test6
  (let ((p1 (new-prompt))
	(p2 (new-prompt))
	(push-twice (lambda (sk)
		      (push-subcont sk (push-subcont sk 3)))))
    (+ 10
      (push-prompt p1 (+ 1
			(push-prompt p2 (take-subcont p1 sk (push-twice sk))))
	)))
  15)


; The most difficult test

(test-check 'test7
  (let* ((p1 (new-prompt))
	 (p2 (new-prompt))
	 (p3 (new-prompt))
	 (push-twice
	    (lambda (sk)
	      (push-subcont sk (push-subcont sk 
		(take-subcont p2 sk2
		  (push-subcont sk2
		    (push-subcont sk2 3))))))))
    (+ 100
      (push-prompt p1
	(+ 1
	  (push-prompt p2
	    (+ 10
	      (push-prompt p3 (take-subcont p1 sk (push-twice sk)))))))))
  135)


(test-check 'test7-1
  (let* ((p1 (new-prompt))
	 (p2 (new-prompt))
	 (p3 (new-prompt))
	 (push-twice (lambda (f) 
		       (f (lambda ()
			    (f (lambda () (shift p2 f2 (f2 (f2 3))))))))))
    (+ 100
      (push-prompt p1
	(+ 1
	  (push-prompt p2
	    (+ 10
	      ((push-prompt p3 (shift p1 sk (push-twice sk))))))))))
  135)

(test-check 'test7-2
  (let* ((p1 (new-prompt))
	 (p2 (new-prompt))
	 (p3 (new-prompt))
	 (push-twice (lambda (f) 
		       (f (lambda ()
			    (f (lambda () (shift0 p2 f2 (f2 (f2 3))))))))))
    (+ 100
      (push-prompt p1
	(+ 1
	  (push-prompt p2
	    (+ 10
	      ((push-prompt p3 (shift0 p1 sk (push-twice sk))))))))))
  135)


(test-check 'test-ls
  (let ((p (new-prompt)))
    (push-prompt p
      (let ((x (shift p f (cons 'a (f '())))))
	(shift p g x))))
  '(a))

(test-check 'test-lc
  (let ((p (new-prompt)))
    (push-prompt p
      (let ((x (control p f (cons 'a (f '())))))
	(control p g x))))
  '())

(test-check 'test-lc-1
  (let ((p (new-prompt)))
    (push-prompt p
      (let ((x (control p f (cons 'a (f '())))))
	(control p g (g x)))))
  '(a))

(test-check 'test-lc-1-2
  (let ((p (new-prompt)))
    (push-prompt p
      (take-subcont p sk (push-prompt p (push-subcont sk 1)))
      (take-subcont p sk (push-subcont sk 2))))
  2)

; traversing puzzle by Olivier Danvy
(define (traverse op lst)
  (let ((p (new-prompt)))
    (letrec ((visit
	       (lambda (l)
		 (if (null? l) '()
		   (visit (op p (lambda (f) (cons (car l) (f (cdr l))))))))))
      (push-prompt p (visit lst)))))

(test-check 'traverse-shift
  (traverse (lambda (p f) (shift p sk (f sk))) '(1 2 3 4 5))
  '(1 2 3 4 5))

(test-check 'traverse-control
  (traverse (lambda (p f) (control p sk (f sk))) '(1 2 3 4 5))
  '(5 4 3 2 1))
