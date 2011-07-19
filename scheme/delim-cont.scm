;	Delimited continiations
;
; $Id: delim-cont.scm 646 2005-04-21 02:18:32Z oleg $

; To get shift within Scheme48, do
; ,open escapes signals
; ,load /usr/local/lib/scheme48/misc/shift-reset.scm

;------------------------------------------------------------------------
; The H stuff and the emulation of various control operators

; data H a b = H a b | HV Value
; Conceptually, it's a algebraic data type with two constructors
; H and HV and a deconstructor case-H.

; Here's one implementation of the H datatype, as pure lambda-terms.

; Constructors
(define (H a b)
  (lambda (on-h)
    (lambda (on-hv)
      ((on-h a) b))))

(define (HV v)
  (lambda (on-h)
    (lambda (on-hv)
      (on-hv v))))

; Deconstructor
(define-syntax case-H
  (syntax-rules ()
    ((case-H e
       ((f x) on-h)
       (v on-hv))
      ((e (lambda (f) (lambda (x) on-h)))
	(lambda (v) on-hv)))))

; Another, potentially more efficient implementation of the H datatype,
; as an indiscriminated Scheme union

;; (define H-tag (list 'H-tag))

;; ; Constructors
;; (define (H a b) (cons H-tag (cons a b)))
;; (define-syntax HV			; just the identity
;;   (syntax-rules ()
;;     ((HV v) v)))

;; ; Deconstructor
;; (define-syntax case-H
;;   (syntax-rules ()
;;     ((case-H e
;;        ((f x) on-h)
;;        (v on-hv))
;;       (let ((val e))
;; 	(if (and (pair? val) (eq? (car val) H-tag))
;; 	  (let ((f (cadr val)) (x (cddr val))) on-h)
;; 	  (let ((v val)) on-hv))))))

;------------------------------------------------------------------------
; Generalized shift/reset parameterized by hr and hs

(define-syntax greset
  (syntax-rules ()
    ((greset hr e) (hr (reset (HV e))))))


(define-syntax gshift
  (syntax-rules ()
    ((gshift hs f e)
      (shift f* (H (lambda (x) (hs (f* x))) (lambda (f) e))))))

(define (hr-stop v)
  (case-H v
    ; on-h
    ((f x) (greset hr-stop (x f)))
    ; on-hv
    (v v)))

(define hs-stop hr-stop)

; Note that (greset hr-stop ...) is the regular reset,
; (gshift hs-stop ...) is the regular shift. The fact these definitions
; obey the standard reduction semantics for shift/reset is obvious.

; tests
(display "shift tests") (newline)

(display (+ 10 (greset hr-stop (+ 2 (gshift hs-stop k (+ 100 (k (k 3))))))))
(newline)
; --> 117

(display (* 10 (greset hr-stop (* 2 (gshift hs-stop g (greset hr-stop
				   (* 5 (gshift hs-stop f (+ (f 1) 1)))))))))
(newline)
; --> 60

(display (let ((f (lambda (x) (gshift hs-stop k (k (k x))))))
	   (+ 1 (greset hr-stop (+ 10 (f 100))))))
(newline)
; --> 121

(display (greset hr-stop 
	   (let ((x (gshift hs-stop f (cons 'a (f '())))))
	     (gshift hs-stop g x))))
(newline)
; ==> '(a)

(define (p x) (if (eq? x p) '(p p) `(p ,x)))
(define (shift* p) (gshift hs-stop f (p f)))
(greset hr-stop (display (let ((x 'abcde)) (eq? x ((shift* shift*) x)))))
(newline)

;------------------------------------------------------
; Dynamic delimited control

(define (hr-prop v)
  (case-H v
    ; on-h
    ((f x) (x f))
    ; on-hv
    (v v)))


(define (hs-prop v)
  (case-H v
    ; on-h
    ((f x)
      (shift g
	(H (lambda (y) (hs-prop (g (f y)))) x)))
    ; on-hv
    (v v)))


; particular expressions for control/prompt
(define-syntax prompt
  (syntax-rules ()
    ((prompt e) (greset hr-stop e))))

(define-syntax control
  (syntax-rules ()
    ((control f e) (gshift hs-prop f e))))

(display "control tests") (newline)

(display (+ 10 (prompt (+ 2 (control k (+ 100 (k (k 3))))))))
(newline)
; --> 117

(display (prompt (let ((x (control f (cons 'a (f '()))))) (control g x))))
(newline)
; ==> '()

(display (prompt ((lambda (x) (control l 2)) (control l (+ 1 (l 0))))))
(newline)
;  ==> 2
(display (prompt (control f (cons 'a (f '())))))
(newline)
; ==> '(a)
(display (prompt (let ((x (control f (cons 'a (f '()))))) (control g (g x)))))
(newline)
; ==> '(a)

(define (control* p) (control f (p f)))
(prompt (display (let ((x 'abcde)) (eq? x ((control* control*) x)))))
(newline)
(prompt (display (let ((x 'abcde)) (eq? x ((control* shift*) x)))))
(newline)
(prompt (display (let ((x 'abcde)) (eq? x ((shift* control*) x)))))
(newline)


(define-syntax prompt0
  (syntax-rules ()
    ((prompt0 e) (greset hr-prop e))))

(display "control0 tests") (newline)
(display (+ 10 (prompt0 (+ 2 (control k (+ 100 (k (k 3))))))))
(newline)
; --> 117

(display (prompt0 (prompt0 
		    (let ((x (control f (cons 'a (f '()))))) (control g x)))))
(newline)
; ==> '()

(define-syntax shift0
  (syntax-rules ()
    ((shift0 f e) (gshift hs-stop f e))))

(display "shift0 tests") (newline)
(display (+ 10 (prompt0 (+ 2 (shift0 k (+ 100 (k (k 3))))))))
(newline)
; --> 117

(display (prompt0 (cons 'a (prompt0 (shift0 f (shift0 g '()))))))
(newline)
; ==> '()

(display (prompt0 (cons 'a (prompt0 (prompt0 (shift0 f (shift0 g '())))))))
(newline)
; ==> '(a)


;------------------------------------------------------------------------
; Factorial

(define call/cc call-with-current-continuation)

(begin
  (define fact
    ((lambda (f)
       ((lambda (u) (u (lambda (x) (lambda (n) ((f (u x)) n)))))
	 (call/cc (call/cc (call/cc 
			     (call/cc (call/cc (lambda (x) x))))))))
      (lambda (f) (lambda (n)
		    (if (<= n 0) 1 (* n (f (- n 1))))))))
  (display (map fact '(5 6 7)))
  (newline))

(begin
  (define fact1
    ((lambda (f)
       ((lambda (u) (u (lambda (x) (lambda (n) ((f (u x)) n)))))
	 (call/cc (call/cc (call/cc 
		     (call/cc (call/cc (prompt (shift* control*)))))))))
      (lambda (f) (lambda (n)
		    (if (<= n 0) 1 (* n (f (- n 1))))))))
  (display (map fact1 '(5 6 7)))
  (newline))
 
 
(begin
  (define (cc p)
    (shift* (lambda (f) (f (p (lambda (x) (shift* (lambda (g) (f x)))))))))
  (prompt
    (let ((fact3
      ((lambda (f)
	 ((lambda (u) (u (lambda (x) (lambda (n) ((f (u x)) n)))))
	   (cc (cc (cc
		     (cc (lambda (x) x)))))))
      (lambda (f) (lambda (n)
		    (if (<= n 0) 1 (* n (f (- n 1)))))))))
  (display (map fact3 '(5 6 7)))
  (newline))))
 

(prompt
 (let ()
   (define (call/cc p)
    (shift* (lambda (f) (f (p (lambda (x) (shift* (lambda (g) (f x)))))))))
   (let ()
    (define fact4
      ((lambda (u)
	 (u (lambda (x)
	      (lambda (n) (if (<= n 0) 1 (* n ((u x) (- n 1))))))))
	(call/cc (call/cc (call/cc (call/cc (call/cc (shift* control*))))))))
      (display (map fact4 '(5 6 7)))
      (newline))))
 

;------------------------------------------------------------------------
;	701th delimited control operator: shift2
; It is an intermediate between shift and control: it has a weak
; delimiter reset2 and a strong delimiter reset1. Two weak delimiters
; act as one strong delimiter. A weak delimiter in a captured
; continuation becomes a stronmg delimiter.
;
; M[reset2 v] ==> v
; M[reset2 (C1 [reset2 C2[ shift2 f e]])] ==>
;     M[reset2 e'] where e' = let f x = (C1 [reset1 C2[x]]) in e
; M[reset1 v] ==> v
; M[reset1 (C1 [ shift2 f e])] ==>
;     M[reset1 e'] where e' = let f x = (C1 [x]) in e
; M[reset1 (C1 [reset2 C2[ shift2 f e]])] ==>
;     M[reset1 e'] where e' = let f x = (C1 [reset1 C2[x]]) in e


; data Counted v = Counted Int v
; actually we need the Either datatype.
; For simplicity, we just use a Scheme-style union
(define tag-counted (list 'tag-counted))
; constructor
(define (counted n x) (list tag-counted n x))
; deconstructor
(define-syntax case-counted
  (syntax-rules ()
    ((case-counted v ((n x) on-counted) (v* other))
     (let ((t v))
       (if (and (pair? t) (eq? (car t) tag-counted))
	 (let ((n (cadr t)) (x (caddr t))) on-counted)
	 (let ((v* t)) other))))))

; strong delimiter
(define-syntax reset1
  (syntax-rules ()
    ((reset1 e) (greset hr1 e))))

; weak delimiter
(define-syntax reset2
  (syntax-rules ()
    ((reset2 e) (greset hr2 e))))


(define (hr2 v)
  (case-H v
    ; on-h
    ((f x)
      (case-counted x
	((n x1) (reset2 (x1 f)))
	(x1 (shift g 
	      (H (lambda (y) (hr2 (g (reset1 (f y))))) (counted 1 x1))))))
    ; on-hv
    (v v)))

(define (hr1 v)
  (case-H v
    ; on-h
    ((f x)
      (case-counted x
	((n x1) (reset1 (x1 f)))
	(x1     (reset1 (x1 f)))))
    ; on-hv
    (v v)))

; shift2 is control

(display "shift2 tests") (newline)
(display (reset2 (+ 1 (reset2 (+ 2 (control f 3)))))) (newline)
; ==> 3
(display (reset2 (+ 4 (reset2 (+ 1 (reset2 (+ 2 (control f 3)))))))) (newline)
; ==> 7
(display (reset2 (+ 1 (reset1 (+ 2 (control f 3)))))) (newline)
; ==> 4
(display (reset2 (+ 1 (reset2 (+ 2 (control f (f 3))))))) (newline)
; ==> 6
(display (reset2 (+ 1 (reset2 ((control f (f (lambda () (control g 3)))))))))
(newline)
; ==> 4

(define (traverse-sh2 xs)
  (letrec ((visit
	     (lambda (xs)
	       (if (null? xs)
		 '()
		 (visit (control k
			  (cons (car xs) (k (cdr xs)))))))))
    (reset2 (reset2 (visit xs)))))

(display (traverse-sh2 '(1))) (newline)
(display (traverse-sh2 '(1 2))) (newline)
(display (traverse-sh2 '(1 2 3))) (newline)
(display (traverse-sh2 '(1 2 3 4))) (newline)
(display (traverse-sh2 '(1 2 3 4 5))) (newline)


(define (traverse-sh3 xs)
  (letrec ((visit
	     (lambda (xs)
	       (if (null? xs)
		 '()
		 (visit (control k
			  (cons (car xs) (k (cdr xs)))))))))
    (reset1 (visit xs))))
(display (traverse-sh3 '(1 2 3 4 5))) (newline)

(define (traverse-sh4 xs)
  (letrec ((visit
	     (lambda (xs)
	       (if (null? xs)
		 '()
		 (visit (control k
			  (reset2 (cons (car xs) (k (cdr xs))))))))))
    (reset1 (visit xs))))
(display (traverse-sh4 '(1 2 3 4 5))) (newline)
