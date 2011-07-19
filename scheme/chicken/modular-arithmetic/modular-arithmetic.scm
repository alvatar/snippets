(require-library
 srfi-1
 numbers matchable)

(module modular-arithmetic
  (xgcd mod+ mod- mod* mod/ modexpt with-modulus)
  (import
   (except scheme
	   + - * / = > < >= <=
	   number->string string->number
	   exp log sin cos tan asin acos atan expt sqrt
	   quotient modulo remainder numerator denominator
	   abs max min gcd lcm
	   positive? negative? odd? even? zero? exact? inexact?
	   floor ceiling truncate round
	   inexact->exact exact->inexact
	   number? complex? real? rational? integer?
	   real-part imag-part magnitude)
   (except chicken
	   add1 sub1 signum
	   bitwise-and bitwise-ior bitwise-xor bitwise-not
	   arithmetic-shift)
   srfi-1
   numbers)
  (import-for-syntax
   scheme chicken matchable)

;; Extended GCD
(define (xgcd a b)
  (let-values ([(q m) (quotient&modulo a b)])
    (if (zero? m)
	(values 0 1)
	(let-values ([(x y) (xgcd b m)])
	  (values y (- x (* y q)))))))

;; Modular addition generator
(define ((mod+ modulus) . ns)
  (fold (lambda (n a) (modulo (+ a n) modulus)) 0 ns))

;; Modular subtraction and negation generator
(define (mod- modulus)
  (case-lambda
    [(a)
     (modulo (- a) modulus)]
    [(a . ns)
     (fold (lambda (n a) (modulo (- a n) modulus)) a ns)]))

;; Modular multiplication generator
(define ((mod* modulus) . ns)
  (fold (lambda (n a) (modulo (* a n) modulus)) 1 ns))

;; Modular division and inversion generator
(define (mod/ modulus)
  (define (inverse a)
    (if (= a 1)
	1
	(let-values ([(1/a n) (xgcd a modulus)])
	  (if (zero? n)
	      (error 'mod/ "operand and modulus are not coprime" a modulus)
	      1/a))))
  (case-lambda
    [(a)
     (modulo (inverse a) modulus)]
    [(a . ns)
     (fold (lambda (n a) (modulo (* a (inverse n)) modulus)) a ns)]))

;; Modular exponentiation generator
(define (modexpt modulus)
  (let ([* (mod* modulus)]
        [/ (mod/ modulus)])
    (lambda (base exponent)
      (let loop ([a 1]
                 [base (if (negative? exponent) (/ base) base)]
                 [exponent (abs exponent)])
        (if (positive? exponent)
	    (loop (if (zero? (bitwise-and exponent 1)) a (* a base))
                  (* base base)
                  (arithmetic-shift exponent -1))
            a)))))

;; Syntax to overload +, add1, -, sub1, *, / and expt with modular versions
(define-syntax with-modulus
  (er-macro-transformer
   (lambda (stx rename id=)
     (match stx
       [(with-modulus modulus body ...)
        (let ([~let (rename 'let)]
	      [~letrec (rename 'letrec)]
	      [~modulus (rename 'modulus)])
          `(,~let ([,~modulus ,modulus])
             (,~letrec ([+ (,(rename 'mod+) ,~modulus)]
			[add1 (lambda (n) (+ n 1))]
			[- (,(rename 'mod-) ,~modulus)]
			[sub1 (lambda (n) (- n 1))]
			[* (,(rename 'mod*) ,~modulus)]
			[/ (,(rename 'mod/) ,~modulus)]
			[expt (,(rename 'modexpt) ,~modulus)])
               ,@body)))]))))

)
