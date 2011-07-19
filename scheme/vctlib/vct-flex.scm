;;; -*-scheme-*-
;;; vct-flex.scm
;;;
;;; vector library for gambit scheme
;;; 
;;; Copyright 2005-2008 Pierre-Alexandre Fournier
;;; (fournier@carretechnologies.com)
;;; All rights reserved.
;;; 
;;; $Id:  Exp $

;;; (include "../srfi/srfi-1.scm")
;;; (include "utils.scm")

(include "homogeneous-vectors-macros.scm")

(declare 
 (standard-bindings)
 (extended-bindings)
 (inline)
 (inlining-limit 100)
 (constant-fold)
 (lambda-lift)  
 (not safe) ;;WARNING: gives you a 40% speed-up, but you should debug
            ;;before
 (block))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; non-mutable set
(define (vector-set v i new-value)
  (with-homogeneous-vector
   v
   (let ((vcopy (vector-copy v)))
     (vector-set! vcopy i new-value)
     vcopy)))

;;; swaps two elements
(define (vector-swap! v i j)
  (with-homogeneous-vector
   v
   (let ((tmp (vector-ref v i)))
     (vector-set! v i (vector-ref v j))
     (vector-set! v j tmp))))

(define (vector-swap v i j)
  (with-homogeneous-vector
   v
   (let ((vcopy (vector-copy v)))
     (vector-swap! vcopy i j)
     vcopy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; map 
(define (vector-map f v)
  (with-homogeneous-vector
   v
   (let* ((vcopy (vector-copy v)))
     (vector-map! f vcopy))))

;;; mutating map
(define (vector-map! f v)
  (with-homogeneous-vector
   v
   (let* ((len (vector-length v)))
     (let loop ((i 0))
       (cond ((< i len)
	      (vector-set! v i (f (vector-ref v i)))
	      (loop (+ i 1)))
	     (else
	      v))))))

(define (vector-foldr f init v)
  (with-homogeneous-vector
   v
   (let ((len (vector-length v)))
     (let loop ((i 0)
		(acc init))
       (cond ((>= i len)
	      acc)
	     (else
	      (loop (+ i 1)
		    (f acc (vector-ref v i)))))))))
		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vector-subseq does right-side zero-padding 
(define (vector-subseq v start . end)
  (with-homogeneous-vector
   v
   (let* ((len      (vector-length v))
	  (start    (max start 0))
	  (end      (if (not (null? end)) 
			(car end) 
			(- (vector-length v) 1)))
	  (new-len  (- end start -1))
	  (max-iter (min new-len (- len start))))
     (cond ((> new-len 0)
	    (let ((new-v (make-vector new-len)))
	      (if (< max-iter 1)
		  new-v
		  (do ((i 0 (+ 1 i)))
		      ((= i max-iter) new-v)
		    (vector-set! new-v i (vector-ref v (+ i start)))))))
	   (else
	    (vector))))))

;;; vector-subseq-right-zp does right-side zero-padding on the right too.
(define (vector-subseq-right-zp v start . end)
  (with-homogeneous-vector
   v
   (let* ((len      (vector-length v))
	  (vstart   (max start 0))
	  (end      (if (not (null? end)) 
			(car end)
			(- len 1)))
	  (vend     (min end (- len 1)))
	  (new-len  (- end start -1))
	  (offset   (- start)))
     (cond ((> new-len 0)
	    (let ((new-v (make-vector new-len)))
	      (if (< (- vend vstart) 1)
		  new-v
		  (do ((i vstart (+ 1 i)))
		      ((> i vend) new-v)
		    (vector-set! new-v (+ i offset)
				 (vector-ref v i))))))
	   (else
	    (vector))))))


(define (vector-zero-padding v new-len)
  (with-homogeneous-vector
   v
   (let ((len (vector-length v)))
     (cond ((>= len new-len)
	    v)
	   (else
	    (let* ((pad-len (exact-floor (/ (- new-len len) 2)))
		   (new-vector (make-vector new-len)))
	      (vector-add-offset! new-vector v pad-len)
	      new-vector))))))

(define (vector-zero-padding-right v new-len)
  (with-homogeneous-vector
   v
   (let ((len (vector-length v)))
     (cond ((>= len new-len)
	    v)
	   (else
	    (let* ((pad-len (- new-len len))
		   (new-vector (make-vector new-len)))
	      (vector-add! new-vector v)
	      new-vector))))))

;;; argument vector is in (vector u8vector u16vector ... f64vector)
(define (vector-one-each-n vector len n . offset)
  (with-homogeneous-vector
   (vector)
   (let ((offset (if (null? offset) 0 (car offset)))
	 (v      (make-vector len)))
     (let loop ((i offset)
		(j 0))
       (cond ((>= i len)
	      v)
	     ((= (- n 1) (floor (modulo j n)))
	      (vector-set! v i 1.)
	      (loop (+ i 1)
		    (+ j 1)))
	     (else
	      (loop (+ i 1)
		    (+ j 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macro-vector-map!
(define-macro (macro-vector-map! f v)
  `(with-homogeneous-vector
    v
    (let* ((len (vector-length v)))
      (let loop ((i 0))
	(if (##fixnum.>= i len)
	    v
	    (begin
	      (vector-set! v i (,f (vector-ref v i)))
	      (loop (##fixnum.+ i 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;scale
(define (vector-scale! v k)
  (macro-vector-map! (lambda (x) (* k x)) v))

(define (vector-scale v k)
  (with-homogeneous-vector
   v
   (vector-scale! (vector-copy v) k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;offset
(define (vector-offset! v k)
  (macro-vector-map! (lambda (x) (+ k x)) v))

(define (vector-offset v k)
  (with-homogeneous-vector
   v 
   (vector-offset! (vector-copy v) k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;vector-remove-mean
(define (vector-remove-mean! v)
  (with-homogeneous-vector
   v 
   (vector-offset! v (- (vector-mean v)))))

(define (vector-remove-mean  v)
  (with-homogeneous-vector
   v 
   (vector-offset v (- (vector-mean v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on two vectors 
(define (vector-do! f v1 v2)
  (with-homogeneous-vectors
   (list v1 v2)
   (let* ((len (min (vector-length v1) (vector-length v2))))
     (let loop ((i 0))
       (cond ((##fixnum.>= i len)
	      v1)
	     (else
	      (vector-set! v1 i (f (vector-ref v1 i) (vector-ref v2 i)))
	      (loop (##fixnum.+ i 1))))))))

(define (vector-do f v1 v2)
  (with-homogeneous-vectors
   (list v1 v2)
   (let ((v1-copy (vector-copy v1)))
     (vector-do! f v1-copy v2))))


;;; with position offset
(define (vector-do-offset! f v1 v2 offset)
  (with-homogeneous-vectors
   (list v1 v2)
   (let* ((len1 (- (vector-length v1) offset))
	  (len2 (vector-length v2))
	  (len  (min len1 len2))
	  (offset (inexact->exact offset)))
     (let loop ((i 0))
       (if (>= i len)
	   v1
	   (let ((i1 (##fixnum.+ i offset)))
	     (vector-set! v1 i1 (f (vector-ref v1 i1) 
				   (vector-ref v2 i)))
	     (loop (##fixnum.+ i 1))))))))

(define (vector-do-offset f v1 v2 offset)
  (with-homogeneous-vectors
   (list v1 v2)
   (let ((v1-copy (vector-copy v1)))
     (vector-do-offset! f v1-copy v2 offset))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vector-add
(define (vector-add! v1 v2) 
  (vector-do! + v1 v2))

(define (vector-add v1 v2)
  (with-homogeneous-vector
   v1
   (vector-add! (vector-copy v1) v2)))

;;; vector-add with position offset
(define (vector-add-offset! v1 v2 offset)
  (vector-do-offset! + v1 v2 offset))

(define (vector-add-offset  v1 v2 offset) 
  (with-homogeneous-vector
   v1
   (vector-add-offset! (vector-copy v1) v2 offset)))

;;; add many vectors, no offset
(define (vectors-add! v1 . vectors)
  (let loop ((vectors vectors))
    (cond ((null? vectors)
	   v1)
	  (else
	   (vectors-add! (vector-add! v1 (car vectors)))
	   (loop (cdr vectors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vector-subtract
(define (vector-subtract! v1 v2) 
  (vector-do! - v1 v2))

(define (vector-subtract v1 v2)
  (with-homogeneous-vector
   v1
   (vector-subtract! (vector-copy v1) v2)))

;;; vector-subtract with position offset
(define (vector-subtract-offset! v1 v2 offset)  
  (vector-do-offset! - v1 v2 offset))

(define (vector-subtract-offset  v1 v2 offset) 
  (with-homogeneous-vector
   v1
   (vector-subtract-offset! (vector-copy v1) v2 offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vector-multiply
(define (vector-multiply! v1 v2) 
  (vector-do! * v1 v2))

(define (vector-multiply v1 v2) 
  (with-homogeneous-vector
   v1
   (vector-multiply! (vector-copy v1) v2)))

;;; vector-multiply with position offset
(define (vector-multiply-offset! v1 v2 offset)  
  (vector-do-offset! * v1 v2 offset))

(define (vector-multiply-offset v1 v2 offset) 
  (with-homogeneous-vector
   v1
   (vector-multiply-offset! (vector-copy v1) v2 offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Division
(define (vector-divide! v1 v2) 
  (vector-do! / v1 v2))

(define (vector-divide v1 v2)
  (with-homogeneous-vector
   v1
   (vector-divide! (vector-copy v1) v2)))

(define (vector-divide-offset! v1 v2 offset)
  (vector-do-offset! / v1 v2 offset))

(define (vector-divide-offset  v1 v2 offset)
  (with-homogeneous-vector
   v1
   (vector-divide-offset! (vector-copy v1) v2 offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scalar product
(define (vector-product v1 v2)
  (with-homogeneous-vector
   v1
   (vector-sum (vector-multiply! (vector-copy v1) v2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replicate vector n times
(define (vector-replicate v n)
  (with-homogeneous-vector
   v
   (let loop ((i 1)
	      (new-v v))
     (cond ((< i n)
	    (loop (+ i 1)
		  (vector-append new-v v)))
	   (else
	    new-v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; progressive sum of a vector
(define (vector-progressive-sum v)
  (with-homogeneous-vector
   v
   (let* ((len   (vector-length v))
	  (new-v (make-vector len)))
     (let loop ((i 0))
       (cond ((< i len)
	      (vector-set! new-v i (vector-sum (vector-subseq v 0 i)))
	      (loop (+ i 1)))
	     (else
	      new-v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vector initialisation

;;; vector-fill with thunk
(define (vector-fill vector len thunk)
  (with-homogeneous-vector
   (vector)
   (if (< len 1)
       (vector)
       (let ((v (make-vector len)))
	 (let loop ((i 0))
	   (cond ((< i len)
		  (vector-set! v i (thunk))
		  (loop (+ i 1)))
		 (else
		  v)))))))

(define (vector-zeros vector len)
  (with-homogeneous-vector
   (vector)
   (make-vector len)))

(define (vector-ones vector len)
  (with-homogeneous-vector
   (vector)
   (vector-offset! (make-vector len) 1)))

;;; vector-iota (based on srfi-1's iota function)
(define (vector-iota vector . args)
  (with-homogeneous-vector
   (vector)
   (list->vector (apply iota args))))

;;; add constant points at the beginning and end of vector
(define (vector-stretch v pad-len) 
  (with-homogeneous-vector
   v
   (let* ((len     (vector-length v))
	  (new-len (+ len (* 2 pad-len)))
	  (new-v   (make-vector new-len)))
     (let loop ((i 0))
       (cond ((>= i new-len)
	      new-v)
	     ((< i pad-len)
	      (vector-set! new-v i (vector-ref v 0))
	      (loop (+ i 1)))
	     ((>= i (+ len pad-len))
	      (vector-set! new-v i (vector-ref v (- len 1)))
	      (loop (+ i 1)))
	     (else
	      (vector-set! new-v i (vector-ref v (- i pad-len)))
	      (loop (+ i 1))))))))

;;; (vector-from-to 2 5) returns #f32(2. 3. 4. 5.)
(define (vector-from-to vector from to)
  (with-homogeneous-vector
   (vector)
   (list->vector (from-to from to))))

;;; (vector-from-to 2 5 0.5) returns #f32(2. 2.5 3. 3.5 4. 4.5 5.)
(define (vector-from-to-step vector from to step)
  (with-homogeneous-vector
   (vector)
   (list->vector (from-to-step from to step))))

;;; (vector-from-to-length -1 2 5) returns #f32(-1. -.25 .5 1.25 2.)
(define (vector-from-to-length vector from to len) 
  (with-homogeneous-vector
   (vector)
   (let ((v (vector-iota vector len)))
     (vector-offset (vector-scale v (/ (- to from) (- len 1))) from))))

;;; random reals between 0. and k
(define (vector-random vector len k)
  (with-homogeneous-vector
   (vector)
   (if homogeneous-vector-is-flonum?
       (vector-fill vector len (lambda () (* k (random-real))))
       (vector-fill vector len (lambda () (random-integer k))))))


;;; something that looks like gaussian noise (** flonum only!)
(define (vector-gauss vector len nrg-bidon)
  (with-homogeneous-vector
   (vector)
   (let ((v (make-vector len)))
     (do ((i 0 (1+ i)))
	 ((= i 10) v)
       (vector-add! v (vector-random vector len 1)))
     (vector-offset! v (- (vector-mean v)))
     (vector-scale! v (sqrt (/ nrg-bidon (vector-energy v)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Split

;vector-split: v is a vector, pts is a list of positions (ex: (list 0 5 14))
(define (vector-split v pts)
  (with-homogeneous-vector
   v
   (let ((len (vector-length v)))
     (cond ((null? pts)
	    '())
	   ((>= (car pts) len)
	    '())
	   (else
	    (let ((start (car pts))
		  (end (if (null? (cdr pts)) 
			   (- len 1)
			   (min (- (cadr pts) 1) 
				(- len 1)))))
	      (cons (vector-subseq v start end)
		    (vector-split v (cdr pts)))))))))

;;; chop chop!
(define (vector-chop v blk-len)
   (define (chop acc v)
     (with-homogeneous-vector
      v
      (let ((len (vector-length v)))
	(cond ((<= len blk-len)
	       (reverse (cons v acc)))
	      (else
	       (chop (cons (vector-subseq v 0 (- blk-len 1)) acc)
		     (vector-subseq v blk-len)))))))
   (cond ((< blk-len 1)
	  '())
	 (else
	  (chop '() v))))

;;; chop overlap
(define (vector-chop-overlap v blk-len overlap)
  (define (chop acc v)
    (with-homogeneous-vector
     v
     (let ((len (vector-length v)))
       (cond ((<= len blk-len)
	      (reverse (cons v acc)))
	     (else
	      (chop (cons (vector-subseq v 0 (- blk-len 1)) acc)
		    (vector-subseq v (- blk-len overlap))))))))
  (cond ((< blk-len 1)
	 '())
	(else
	 (chop '() v))))


;;;;naive subsampling
;(define (vector-subsampling v rate)
;  (with-homogeneous-vector
;   v
;   (let* ((k (min 1. rate))
;	  (new-len (exact-ceiling (* (vector-length v) k)))
;	  (new-v   (make-vector new-len)))
;     (do ((i 0 (+ 1 i)))
;	 ((= i new-len) new-v)
;       (vector-set! new-v i (vector-ref v (exact-floor (/ i k))))))))

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;statistics
(define (vector-mean v) 
  (with-homogeneous-vector
   v
   (let ((len (max 1 (vector-length v))))
     (/ (vector-sum v) len))))

(define (vector-median v) 
  (with-homogeneous-vector
   v
   (median (vector->list v))))

;;; Note variance normalized with N, not N-1
(define (vector-variance v)
  (let ((x (vector-copy v))
	(len (vector-length v)))
    (/ (vector-sum-square (vector-offset! x (- (vector-mean x)))) len)))

;;; Note standard deviation normalized with N, not N-1
(define (vector-std v) (sqrt (vector-variance v)))
(define (vector-rms v) (vector-std v))

(define (vector-square! v) (vector-multiply! v v))
(define (vector-square  v) (vector-multiply  v v))

;;; *** careful: sqrt may return a flonum!
(define (vector-sqrt! v) (vector-map! sqrt v))
(define (vector-sqrt  v) (vector-map  sqrt v)) 

(define (vector-abs! v) (vector-map! abs v))
(define (vector-abs  v) (vector-map  abs v))

;;; *** careful: inverse may return a flonum!
(define (vector-inverse! v) (vector-map! inverse v))
(define (vector-inverse  v) (vector-map  inverse v))

(define (vector-fmodulo! v x) (vector-map! (lambda-fmodulo x) v))
(define (vector-fmodulo  v x) (vector-map  (lambda-fmodulo x) v))

(define (vector-round! v) (vector-map! round v))
(define (vector-round  v) (vector-map  round v))

(define (vector-pos-lin! v) (vector-map! pos-lin v))
(define (vector-pos-lin  v) (vector-map  pos-lin v))

(define (vector-correlation v1 v2)
  (with-homogeneous-vectors
   (list v1 v2)
   (correlation-coeff (vector->list v1) (vector->list v2))))

;;product of all terms in vector
(define (vector-prod v)
  (with-homogeneous-vector
   v
   (let ((len  (vector-length v))
	 (prod  1))
     (do ((i 0 (+ 1 i)))
	 ((= i len) prod)
       (set! prod (* prod (vector-ref v i)))))))

;;;fastest version when compiled with '(not safe)
(define (vector-sum v)
  (with-homogeneous-vector
   v
   (let ((len (vector-length v))
	 (sum 0))
     (do ((i 0 (+ 1 i)))
	 ((= i len) sum)
       (set! sum (+ sum (vector-ref v i)))))))

;; *** careful: (/ 1 s) may return a fraction incompatible with vector type!
(define (vector-normalize! v) 
  (with-homogeneous-vector
   v
   (let ((s (vector-sum v)))
     (if (= s 0)
	 v
	 (vector-scale! v (/ 1 s))))))

(define (vector-normalize v)  
  (with-homogeneous-vector
   v
   (vector-normalize! (vector-copy v))))

;; *** careful: (/ 1 m) may return a fraction incompatible with vector type!
(define (vector-normalize-max! v) 
  (with-homogeneous-vector
   v
   (let ((m (cadr (vector-max v))))
     (if (= m 0)
	 v
	 (vector-scale! v (/ 1 m))))))

(define (vector-normalize-max v) 
  (with-homogeneous-vector
   v
   (vector-normalize-max! (vector-copy v))))

(define (vector-sum-square v)  
  (vector-sum (vector-square v)))

(define (vector-norm v) 
  (sqrt (vector-sum-square v)))

(define (vector-mean-square v)
  (vector-mean (vector-square v)))

(define vector-energy vector-mean-square)

;; *** careful: (/ 1 energy) may return a fraction incompatible with vector type!
(define (vector-normalize-energy! v)
  (let ((energy (vector-energy v)))
    (if (= energy 0)
	v
	(vector-scale! v (/ 1. (sqrt energy))))))

(define (vector-normalize-energy v)
  (with-homogeneous-vector
   v
   (vector-normalize-energy! (vector-copy v))))

;; *** careful: subtraction may return negative numbers incompatible
;; with unsigned vector type!
(define (vector-energy-diff v1 v2)
  (vector-energy
   (vector-subtract v1 v2)))

;; *** careful: incompatible with fixnum
(define (vector-normalized-energy-diff v1 v2)
  (vector-energy 
   (vector-subtract (vector-normalize-energy v1)
		    (vector-normalize-energy v2))))

;; *** careful: incompatible with fixnum
;;; compress vector as a whole, not point by point.
(define (vector-power-compressor v threshold ratio)
  (let ((power (vector-energy v)))
    (cond ((< power threshold)
	   v)
	  (else
	   (let* ((new-power (+ threshold (* ratio (- power threshold))))
		  (coeff (sqrt (/ new-power power))))
	     (vector-scale v coeff))))))

;find 'most f' element according to f: (f (vector-ref v i) extreme)
(define (vector-extreme f v)
  (with-homogeneous-vector
   v
   (let ((len (vector-length v)))
     (if (= 0 len)
	 #f
	 (let ((extreme (vector-ref v 0))
	       (pos 0))	
	   (do ((i 1 (+ 1 i)))
	       ((= i len))
	     (cond ((f (vector-ref v i) extreme)
		    (set! extreme (vector-ref v i))
		    (set! pos i))))
	   (list pos extreme))))))

(define (vector-extreme-value f v)
  (with-homogeneous-vector
   v
   (let ((len (vector-length v)))
     (if (= 0 len)
	 #f
	 (let ((extreme (vector-ref v 0))
	       (pos 0))
	   (do ((i 1 (+ 1 i)))
	       ((= i len))
	     (cond ((f (vector-ref v i) extreme)
		    (set! extreme (vector-ref v i)))))
	   extreme)))))

(define (vector-max v) (vector-extreme > v))
(define (vector-min v) (vector-extreme < v))
(define (vector-max-value v) (vector-extreme-value > v))
(define (vector-min-value v) (vector-extreme-value < v))

(define (vector-max-abs v)
  (vector-extreme (lambda (x y)
		    (> (abs x) (abs y)))
		  v))

(define (vector-threshold! v thres)
  (with-homogeneous-vector
   v
   (let ((len (vector-length v)))
     (if (= 0 len)
	 v
	 (let* ((zero (vector-ref (make-vector 1) 0))  ;; hack to get a flonum/fixnum zero
		(one  (+ zero 1)))
	   (do ((i 0 (+ i 1)))
	       ((= i len) v)
	     (if (< (vector-ref v i) thres)
		 (vector-set! v i zero)  
		 (vector-set! v i one))))))))

(define (vector-threshold v thres)
  (with-homogeneous-vector
   v
   (vector-threshold! (vector-copy v) thres)))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; trigonometric functions
(define (vector-sin!  v) (vector-map! sin v))
(define (vector-sin   v) (vector-map  sin v))
(define (vector-cos!  v) (vector-map! cos v))
(define (vector-cos   v) (vector-map! cos v))
(define (vector-tan!  v) (vector-map! tan v))
(define (vector-tan   v) (vector-map  tan v))
(define (vector-asin! v) (vector-map! asin v))
(define (vector-asin  v) (vector-map  asin v))
(define (vector-acos! v) (vector-map! acos v))
(define (vector-acos  v) (vector-map! acos v))
(define (vector-atan! v) (vector-map! atan v))
(define (vector-atan  v) (vector-map  atan v))

(define (vector-log   v) (vector-map log v))
(define (vector-log10 v) (vector-map log10 v))
 
(define (vector-invert v)
  (vector-map (lambda (x) (- x)) v))

;;; naive vector-reverse
(define (vector-reverse v)
  (with-homogeneous-vector
   v
   (list->vector (reverse (vector->list v)))))

;;; vector convolution
(define (vector-conv v1 v2)
  (with-homogeneous-vectors
   (list v1 v2)
   (let* ((len1 (vector-length v1))
	  (len2 (vector-length v2))
	  (len3 (+ len1 len2 -1))
	  (vc1  (vector-append (make-vector (- len2 1))
			       v1))
	  (vc2  (vector-reverse (vector-copy v2)))
	  (vc3  (make-vector len3)))
     (let loop ((i 0))
       (cond ((>= i len3)
	      vc3)
	     (else
	      (vector-set! vc3
			   i
			   (vector-sum
			    (vector-multiply!
			     (vector-copy vc2)
			     (vector-subseq vc1
					    i (+ i (- len2 1))))))
	      (loop (1+ i))))))))

;;; speed up vector-convolution with a step > 1
(define (vector-conv-step v1 v2 step)
  (with-homogeneous-vectors
   (list v1 v2)
   (let* ((len1 (vector-length v1))
	  (len2 (vector-length v2))
	  (len3 (exact-ceiling (/ (+ len1 len2 -1) step)))
	  (vc1  (vector-append (make-vector (- len2 1))
			       v1))
	  (vc2 (vector-reverse (vector-copy v2)))
	  (vc3 (make-vector len3)))
     (let loop ((i 0))
       (cond ((>= i len3)
	      vc3)
	     (else
	      (vector-set! vc3
			   i
			   (vector-sum
			    (vector-multiply!
			     (vector-copy vc2)
			     (vector-subseq vc1
					    (* i step) (+ (* i step) (- len2 1))))))
	      (loop (+ i 1))))))))

(define (vector-autocorrelation v)
  (vector-conv (vector-reverse v) v))

;; ;;; vector*function convolution (function f2 is (f2 i) -> vector) <- how do I handle the vector returned?
;; (define (vector-f-conv v1 f2)
;;   (with-homogeneous-vector
;;    v1
;;    (let* ((len  (vector-length v1))
;; 	  (vout (make-vector len)))
;;      (let loop ((i 0))
;;        (cond ((>= i len)
;; 	      vout)
;; 	     (else
;; 	      (let* ((v2     (f2 i)) ;; convolution function
;; 		     (len2   (vector-length v2))
;; 		     (start  (- i (exact-floor (/ len2 2))))
;; 		     (end    (+ start len2 -1))
;; 		     (value  (vector-sum
;; 			      (vector-multiply
;; 			       v2
;; 			       (vector-subseq-right-zp v1 start end))))) ;; v1 around i
;; 		(vector-set! vout
;; 			     i
;; 			     value)
;; 		(loop (1+ i)))))))))


;;; higher order sliding function 
;;; f argument is a list (f (list x ...))
(define (vector-sliding-f f v width)
  (with-homogeneous-vector
   v
   (let* ((len     (vector-length v))
	  (new-len (- len width -1))
	  (new-vector (make-vector new-len)))
     (let loop ((i 0))
       (cond ((>= i new-len)
	      new-vector)
	     (else
	      (vector-set! new-vector i 
			   (f (vector->list (vector-subseq v i (+ i width -1)))))
	      (loop (+ i 1))))))))

;;; specializations
(define (vector-sliding-median v width)
  (with-homogeneous-vector
   v
   (let* ((len     (vector-length v))
	  (new-len (- len width -1))
	  (new-vector (make-vector new-len)))
     (let loop ((i 0))
       (cond ((>= i new-len)
	      new-vector)
	     (else
	      (vector-set! new-vector i 
			   (vector-median (vector-subseq v i (+ i width -1))))
	      (loop (+ i 1))))))))

(define (vector-sliding-mean v width)
  (with-homogeneous-vector
   v
   (let* ((len     (vector-length v))
	  (new-len (- len width -1))
	  (new-vector (make-vector new-len)))
     (let loop ((i 0))
       (cond ((>= i new-len)
	      new-vector)
	     (else
	      (vector-set! new-vector i 
			   (vector-mean (vector-subseq v i (+ i width -1))))
	      (loop (+ i 1))))))))


(define (vector-sliding-min v width)
  (vector-sliding-f (lambda (lst) (apply min lst))
		    v
		    width))

(define (vector-sliding-max v width)
  (vector-sliding-f (lambda (lst) (apply max lst))
		    v
		    width))

;;;find n maxima in vector v
(define (find-n-max v n)
  (with-homogeneous-vector
   v
   (let ((minimum (cadr (vector-min v)))
	 (v2 (vector-copy v)))
     (let loop ((i n)
		(max-lst '()))
       (cond ((< i 1)
	      (reverse max-lst))
	     (else
	      (let ((max-pair (vector-max v2)))
		(vector-set! v2 (car max-pair) minimum)
		(loop (- i 1)
		      (cons max-pair
			    max-lst)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; File utilities (uses Guillaume Germain's wave.scm)
;;(define (file->vct filename . args)
;;  (cond ((null? args)
;;	 (wave-data (read-wave-file filename)))
;;	(else
;;	 (wave-data (read-wave-file filename (car args))))))
;;
;;(define (vct->file vct filename)
;;  (let ((wave (vct->wave vct)))
;;    (write-wave-file wave filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; other types of vectors for gambit-C

;;vct->ulaw: vct elements should be limited to [-8192 8191]
(define (vct->ulaw v)
  (define (clip-ulaw x)
    (cond ((> x  8191)  8191)
	  ((< x -8192) -8192)
	  (else         x)))
  (let* ((len (vct-length v))
	 (u8-ulaw (make-u8vector len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (u8vector-set! u8-ulaw i (linear14->ulaw 
				       (clip-ulaw
					(exact-round (vct-ref v i)))))
	     (loop (+ i 1)))
	    (else u8-ulaw)))))

;;vct->alaw: vct elements should be limited to [-4096 4095]
(define (vct->alaw v)
  (define (clip-alaw x)
    (cond ((> x  4095)  4095)
	  ((< x -4096) -4096)
	  (else         x)))
  (let* ((len (vct-length v))
	 (u8-alaw (make-u8vector len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (u8vector-set! u8-alaw i (linear13->alaw 
				       (clip-alaw
					(exact-round (vct-ref v i)))))
	     (loop (+ i 1)))
	    (else u8-alaw)))))

;;ulaw->vct
(define (ulaw->vct u8v)
  (let* ((len (u8vector-length u8v))
	 (v   (make-vct len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (vct-set! v i (exact->inexact
			    (ulaw->linear16 
			     (u8vector-ref u8v i))))
	     (loop (+ i 1)))
	    (else v)))))

;;alaw->vct
(define (alaw->vct u8v)
  (let* ((len (u8vector-length u8v))
	 (v   (make-vct len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (vct-set! v i (exact->inexact
			    (alaw->linear16
			     (u8vector-ref u8v i))))
	     (loop (+ i 1)))
	    (else v)))))

;;;cool graphics showing ulaw encoding quantification 
;(vct-gnuplot (vct-scale! (ulaw->vct (vct->ulaw (vct-from-to-step -10000 10000 10))) 0.25))
;(vct-gnuplot (vct-scale! (alaw->vct (vct->alaw (vct-from-to-step -7000 7000 10))) 0.125))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; u8vector functions: I use those for ulaw and A-law encoding
;; 
;; (define (u8vector-iota len)
;;   (list->u8vector
;;    (map (lambda (x) (modulo x 256))
;; 	(iota len))))
;; 
;; ;;;write a (u8vector) in a file
;; (define (u8vector->file u8v filename)
;;   (let ((file (open-output-file filename)))
;;     (write-subu8vector u8v 0 (u8vector-length u8v) file)
;;     (close-port file)))
;; 
;; ;;;reads a (u8vector) from a file
;; (define (file->u8vector filename)
;;   (let* ((len  (file-size filename))
;; 	 (file (open-input-file filename))
;; 	 (u8v  (make-u8vector len)))
;;     (read-subu8vector u8v 0 (- len 1) file)
;;     (close-port file)
;;     u8v))
;; 
;; (define (u8vector-subseq v start . end)
;;   (let* ((len      (u8vector-length v))
;; 	 (start    (max start 0))
;; 	 (end      (if (not (null? end)) 
;; 		       (car end) 
;; 		       (- (u8vector-length v) 1)))
;; 	 (new-len  (- end start -1))
;; 	 (max-iter (min new-len (- len start))))
;;     (cond ((> new-len 0)
;; 	   (let ((new-v (make-u8vector new-len 0)))
;; 	     (if (< max-iter 1)
;; 		 new-v
;; 		 (do ((i 0 (+ 1 i)))
;; 		     ((= i max-iter) new-v)
;; 		   (u8vector-set! new-v i (u8vector-ref v (+ i start)))))))
;; 	  (else
;; 	   (u8vector)))))
;; 
;; 
;; (define (simple-u8vector->string u8v)
;;   (list->string 
;;    (map byte->char
;; 	(u8vector->list u8v))))
;; 
;; ;;;optimized for small memory usage
;; (define (u8vector->string u8v . args)
;;   (let* ((len (if (length> args 0) (car args) (u8vector-length u8v)))
;; 	 (max-u8read 1000)
;; 	 (n-loop1 (exact-ceiling (/ len max-u8read)))
;; 	 (n-loop2 (modulo len max-u8read)))
;;     (let loop1 ((i1 0) 
;; 		(str ""))
;;       (cond ((>= i1 n-loop1)
;; 	     str)
;; 	    (else
;; 	     (let* ((start2 (* i1 max-u8read))
;; 		    (end2   (min (+ start2 max-u8read) len)))
;; 	       (let loop2 ((i2 start2)
;; 			   (lst '()))
;; 		 (cond ((= end2 i2)
;; 			(loop1 (+ i1 1)
;; 			       (string-append (list->string (reverse lst))
;; 					      str)))
;; 		       (else
;; 			(loop2 (+ i2 1)
;; 			       (cons (byte->char 
;; 				      (u8vector-ref u8v i2))
;; 				     lst)))))))))))
;; 
;; (define (u8vector->integer u8v)
;;   (let ((len (u8vector-length u8v)))
;;     (let loop ((i   0)
;; 	       (sum 0))
;;       (cond ((< i len)
;; 	     (loop (+ i 1)
;; 		   (+ sum (arithmetic-shift (u8vector-ref u8v i) (* i 8)))))
;; 	    (else sum)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; play sound on /dev/audio
;;(define (play-vct v)
;;  (play-ulaw (vct->ulaw v)))
;;
;;(define (play-ulaw u8v)
;;  (let ((dsp "/dev/audio"))
;;    (cond ((not (file-exists? dsp))
;;	   (display "Audio output /dev/audio is not available.\n"))
;;	  (else
;;	   (let ((audio-output (open-output-file dsp))
;;		 (len          (u8vector-length u8v)))
;;	     (write-subu8vector u8v 0 len audio-output)
;;	     (close-port audio-output))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some tests (sand-box section)

;;; fixed and floating point vectors
; (define vs (s32vector 0 1 2 3 4))
; (define vu (u32vector 0 1 2 3 4))
; (define vf (f32vector 0. 1. 2. 3. 4.))
; (vector-set vu 0 5)
; (vector-set vf 0 5.)
; (vector-swap! vu 0 4)
; (vector-swap! vf 0 4)
; (vector-swap vu 0 4)
; (vector-swap vf 0 4)
; (vector-map square vu)
; (vector-map square vf)
; (vector-foldr + 0  vu)
; (vector-foldr + 0. vf)
