;;; -*-scheme-*-
;;; f64vector.scm
;;;
;;; f64vector functions for gambit scheme
;;; 
;;; Copyright 2005-2007 Pierre-Alexandre Fournier
;;; (fournier@carretechnologies.com)
;;; All rights reserved.
;;; 
;;; $Id:  $

;;; (include "../srfi/srfi-1.scm")
;;; (include "utils.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic vector functions

;(define f64vector          f64vector)
;(define f64vector?         f64vector?)
;(define make-f64vector     make-f64vector)
;(define f64vector-length   f64vector-length)
;
;(define f64vector-ref      f64vector-ref)
;(define f64vector-set!     f64vector-set!)
;
;(define f64vector->list    f64vector->list)
;(define list->f64vector    list->f64vector)

(define (f64vector->vector v) 
  (list->vector (f64vector->list v)))
(define (vector->f64vector v) 
  (list->f64vector (map exact->inexact (vector->list v))))

(define f64vector+ ##flonum.+)
(define f64vector- ##flonum.-)
(define f64vector* ##flonum.*)
(define f64vector/ ##flonum./)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; non-mutable set
(define (f64vector-set v i new-value)
  (let ((new-v (f64vector-copy v)))
    (f64vector-set! new-v i new-value)
    new-v))

;;; swaps two elements
(define (f64vector-swap! v i j)
  (let ((tmp (f64vector-ref v i)))
    (f64vector-set! v i (f64vector-ref v j))
    (f64vector-set! v j tmp)))

(define (f64vector-swap v i j)
  (let ((new-v (f64vector-copy v)))
    (f64vector-swap! new-v i j)
    new-v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; map for one or two f64vectors
(define (f64vector-map f . args)
  (let* ((v1 (f64vector-copy (car args)))
	 (v2 (if (length> args 1) (cadr args) #f)))
    (if v2
	(f64vector-map! f v1 v2)
	(f64vector-map! f v1))))

;;; mutating map for one or two f64vectors
(define (f64vector-map! f . args)
  (let* ((v1    (car args))
	 (v2    (if (length> args 1) (cadr args) #f))
	 (len   (f64vector-length v1)))
    (if v2  ;;check dimensions
	(let* ((len2 (f64vector-length v2)))
	  (if (not (= len len2))
	      (error "f64vector-map!: f64vectors v1 and v2 must have same length.\n"))))
    (let loop ((i 0))
      (cond ((< i len)
	     (f64vector-set! v1 i 
		       (if v2
			   (f (f64vector-ref v1 i) 
			      (f64vector-ref v2 i))
			   (f (f64vector-ref v1 i))))
	     (loop (+ i 1)))
	    (else
	     v1)))))

(define (f64vector-foldr f init v)
  (let ((len (f64vector-length v)))
    (cond ((< len 1)
	   #f) ;should report error here
	  (else
	   (let loop ((i 0)
		      (acc init))
	     (cond ((= i len)
		    acc)
		   (else
		    (loop (+ i 1)
			  (f acc (f64vector-ref v i))))))))))
		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f64vector-subseq does right-side zero-padding 
(define (f64vector-subseq v start . end)
  (let* ((len      (f64vector-length v))
	 (start    (max start 0))
	 (end      (if (not (null? end)) 
		       (car end) 
		       (- (f64vector-length v) 1)))
	 (new-len  (- end start -1))
	 (max-iter (min new-len (- len start))))
    (cond ((> new-len 0)
	   (let ((new-v (make-f64vector new-len 0.)))
	     (if (< max-iter 1)
		 new-v
		 (do ((i 0 (+ 1 i)))
		     ((= i max-iter) new-v)
		   (f64vector-set! new-v i (f64vector-ref v (+ i start)))))))
	  (else
	   (f64vector)))))

;;; NOT GOOD, BUGGY
;(define (f64vector-subseq v start . end)
;  (define (copy-in-place! v1 v2)
;    (let ((len (min (f64vector-length v1) (f64vector-length v2))))
;      (let loop ((i 0))
;	(cond ((>= i len)
;	       #t)
;	      (else
;	       (f64vector-set! v1 i
;			 (f64vector-ref v2 i))
;	       (loop (+ i 1)))))))
;
;  (let* ((len      (f64vector-length v))
;	 (start    (max start 0))
;	 (end      (if (not (null? end)) 
;		       (car end) 
;		       (f64vector-length v)))
;	 (new-len  (- end start -1))
;	 (max-iter (min new-len (- len start))))
;    (cond ((> new-len 0)
;	   (let ((new-v (make-f64vector new-len 0.)))
;	     (copy-in-place! new-v (subf64vector v start (min (+ 1 end) (f64vector-length v))))
;	     new-v))
;	  (else
;	   (f64vector)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macro-f64vector-map!
(define-macro (macro-f64vector-map! f v)
  `(let* ((len (f64vector-length v)))
     (let loop ((i 0))
       (if (##fixnum.>= i len)
	   v
	   (begin
	     (f64vector-set! v i (,f (f64vector-ref v i)))
	     (loop (##fixnum.+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;scale
(define (f64vector-scale! v k)
  (let ((k (exact->inexact k)))
    (macro-f64vector-map! (lambda (x) (f64vector* k x)) v)))

(define (f64vector-scale v k)
  (let ((v-copy (f64vector-copy v)))
    (f64vector-scale! v-copy k)
    v-copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;offset
(define (f64vector-offset! v k)
  (let ((k (exact->inexact k)))
    (macro-f64vector-map! (lambda (x) (f64vector+ k x)) v)))

(define (f64vector-offset v k)
  (f64vector-offset! (f64vector-copy v) k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;f64vector-remove-mean
(define (f64vector-remove-mean! v)
  (f64vector-offset! v (- (f64vector-mean v))))

(define (f64vector-remove-mean  v)
  (f64vector-offset  v (- (f64vector-mean v))))

;;;f64vector-do accepts f64vectors of different length, returns a f64vector of length
;;;(f64vector-length v1) OBSOLETE 20070201: use f64vector-do below instead
;(define (f64vector-do f v1 v2 . offset)
;  (let* ((offset (if (null? offset) 0 (car offset)))
;	 (len1 (- (f64vector-length v1) offset))
;	 (len2 (f64vector-length v2))
;	 (len  (min len1 len2))
;	 (new-v (f64vector-copy v1)))
;    (let loop ((i 0))
;      (if (= i len)
;	  new-v
;	  (begin
;	    (f64vector-set! new-v (+ i offset) (f (f64vector-ref v1 (+ i offset)) 
;					    (f64vector-ref v2 i)))
;	    (loop (+ i 1)))))))
;
;(define (f64vector-do! f v1 v2 . offset)
;  (let* ((offset (if (null? offset) 0 (car offset)))
;	 (len1 (- (f64vector-length v1) offset))
;	 (len2 (f64vector-length v2))
;	 (len  (min len1 len2)))
;    (let loop ((i 0))
;      (if (= i len)
;	  v1
;	  (begin
;	    (f64vector-set! v1 (+ i offset) (f (f64vector-ref v1 (+ i offset)) 
;					 (f64vector-ref v2 i)))
;	    (loop (+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on two vectors 
(define (f64vector-do! f v1 v2)
  (let* ((len (min (f64vector-length v1) (f64vector-length v2))))
    (let loop ((i 0))
      (cond ((##fixnum.>= i len)
	     v1)
	    (else
	     (f64vector-set! v1 i (f (f64vector-ref v1 i) (f64vector-ref v2 i)))
	     (loop (##fixnum.+ i 1)))))))

(define (f64vector-do f v1 v2)
  (let ((v1-copy (f64vector-copy v1)))
    (f64vector-do! f v1 v2)))

;;; with position offset
(define (f64vector-do-offset! f v1 v2 offset)
  (let* ((len1 (- (f64vector-length v1) offset))
	 (len2 (f64vector-length v2))
	 (len  (min len1 len2))
	 (offset (inexact->exact offset)))
    (let loop ((i 0))
      (if (>= i len)
	  v1
	  (let ((i1 (##fixnum.+ i offset)))
	    (f64vector-set! v1 i1 (f (f64vector-ref v1 i1) 
				(f64vector-ref v2 i)))
	    (loop (##fixnum.+ i 1)))))))

(define (f64vector-do-offset f v1 v2 offset)
  (let ((v1-copy (f64vector-copy v1)))
    (f64vector-do-offset! f v1 v2 offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f64vector-add
(define (f64vector-add! v1 v2) (f64vector-do! f64vector+ v1 v2))
(define (f64vector-add  v1 v2) (f64vector-add! (f64vector-copy v1) v2))

;;; f64vector-add with position offset
(define (f64vector-add-offset! v1 v2 offset)  
  (f64vector-do-offset! f64vector+ v1 v2 offset))
(define (f64vector-add-offset  v1 v2 offset) 
  (f64vector-add-offset! (f64vector-copy v1) v2 offset))

;;; add many f64vectors, no offset
(define (f64vectors-add! v1 . f64vectors)
  (let loop ((f64vectors f64vectors))
    (cond ((null? f64vectors)
	   v1)
	  (else
	   (f64vectors-add! (f64vector-add! v1 (car f64vectors)))
	   (loop (cdr f64vectors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f64vector-subtract
(define (f64vector-subtract! v1 v2) (f64vector-do! f64vector- v1 v2))
(define (f64vector-subtract  v1 v2) (f64vector-subtract! (f64vector-copy v1) v2))

;;; f64vector-subtract with position offset
(define (f64vector-subtract-offset! v1 v2 offset)  
  (f64vector-do-offset! f64vector- v1 v2 offset))
(define (f64vector-subtract-offset  v1 v2 offset) 
  (f64vector-subtract-offset! (f64vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f64vector-multiply
(define (f64vector-multiply! v1 v2) (f64vector-do! f64vector* v1 v2))
(define (f64vector-multiply  v1 v2) (f64vector-multiply! (f64vector-copy v1) v2))

;;; f64vector-multiply with position offset
(define (f64vector-multiply-offset! v1 v2 offset)  
  (f64vector-do-offset! f64vector* v1 v2 offset))
(define (f64vector-multiply-offset  v1 v2 offset) 
  (f64vector-multiply-offset! (f64vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Division
(define (f64vector-divide! v1 v2) (f64vector-do! f64vector/ v1 v2))
(define (f64vector-divide  v1 v2) (f64vector-divide! (f64vector-copy v1) v2))

(define (f64vector-divide-offset! v1 v2 . offset)
  (let ((offset (if (null? offset) 0 (car offset))))
    (f64vector-do-offset! f64vector/ v1 v2 offset)))
(define (f64vector-divide-offset  v1 v2 offset)
  (f64vector-divide-offset! (f64vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scalar product
(define (f64vector-product v1 v2)
  (f64vector-sum (f64vector-multiply! (f64vector-copy v1) v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replicate f64vector n times
(define (f64vector-replicate v n)
  (let loop ((i 1)
	     (new-v v))
    (cond ((< i n)
	   (loop (+ i 1)
		 (f64vector-append new-v v)))
	  (else
	   new-v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; progressive sum of a f64vector
(define (f64vector-progressive-sum v)
  (let* ((len   (f64vector-length v))
	 (new-v (make-f64vector len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (f64vector-set! new-v i (f64vector-sum (f64vector-subseq v 0 i)))
	     (loop (+ i 1)))
	    (else
	     new-v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f64vector initialisation

;;; f64vector-fill with thunk
(define (f64vector-fill len thunk)
  (let ((v (make-f64vector len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (f64vector-set! v i (thunk))
	     (loop (+ i 1)))
	    (else
	     v)))))

(define (f64vector-zeros len) (make-f64vector len 0.))
(define (f64vector-ones  len) (make-f64vector len 1.))

;;; f64vector-iota (based on srfi-1's iota function)
(define (f64vector-iota . args)
  (list->f64vector
   (map exact->inexact
	(apply iota args))))

;;; add constant points at the beginning and end of f64vector
(define (f64vector-stretch v pad-len) 
  (let* ((len     (f64vector-length v))
	 (new-len (+ len (* 2 pad-len)))
	 (new-v (make-f64vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-v)
	    ((< i pad-len)
	     (f64vector-set! new-v i (f64vector-ref v 0))
	     (loop (+ i 1)))
	    ((>= i (+ len pad-len))
	     (f64vector-set! new-v i (f64vector-ref v (- len 1)))
	     (loop (+ i 1)))
	    (else
	     (f64vector-set! new-v i (f64vector-ref v (- i pad-len)))
	     (loop (+ i 1)))))))

;;; (f64vector-from-to 2 5) returns #f64(2. 3. 4. 5.)
(define (f64vector-from-to from to)
  (list->f64vector (map exact->inexact (from-to from to))))

;;; (f64vector-from-to 2 5 0.5) returns #f64(2. 2.5 3. 3.5 4. 4.5 5.)
(define (f64vector-from-to-step from to step)
  (list->f64vector (map exact->inexact (from-to-step from to step))))

;;; (f64vector-from-to-length -1 2 5) returns #f64(-1. -.25 .5 1.25 2.)
(define (f64vector-from-to-length from to len)
  (let ((v (f64vector-iota len)))
    (f64vector-offset (f64vector-scale v (/ (- to from) (- len 1))) from)))
	
;;; random reals between 0. and k
(define (f64vector-random len k)
  (f64vector-fill len (lambda () (* k (random-real)))))

;;; something that looks like gaussian noise
(define (f64vector-gauss len nrg-bidon)
  (let ((v (make-f64vector len 0.)))
    (do ((i 0 (1+ i)))
	((= i 10) v)
      (f64vector-add! v (f64vector-random len 1.)))
    (f64vector-offset! v (- (f64vector-mean v)))
    (f64vector-scale! v (sqrt (/ nrg-bidon (f64vector-energy v))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Append, split
;f64vector-append
(define (f64vector-append . v)
  (cond ((null? v)
	 #f)
	(else
	 (let* ((lens (map f64vector-length v))
		(total-len (sum lens))
		(new-v (make-f64vector total-len 0.)))
	   (let loop ((i 0) (v v) (lens lens))
	     (cond ((null? v)
		    new-v)
		   (else
		    (f64vector-add-offset! new-v (car v) i)
		    (loop (+ i (car lens)) (cdr v) (cdr lens)))))))))

;f64vector-split: v is a f64vector, pts is a list of positions (ex: (list 0 5 14))
(define (f64vector-split v pts)
  (let ((len (f64vector-length v)))
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
	     (cons (f64vector-subseq v start end)
		   (f64vector-split v (cdr pts))))))))

;;; chop chop!
(define (f64vector-chop v blk-len)
  (define (chop acc v)
    (let ((len (f64vector-length v)))
      (cond ((<= len blk-len)
	     (reverse (cons v acc)))
	    (else
	     (chop (cons (f64vector-subseq v 0 (- blk-len 1)) acc)
		   (f64vector-subseq v blk-len))))))
  (cond ((< blk-len 1)
	 '())
	(else
	 (chop '() v))))

;;; chop overlap
(define (f64vector-chop-overlap v blk-len overlap)
  (define (chop acc v)
    (let ((len (f64vector-length v)))
      (cond ((<= len blk-len)
	     (reverse (cons v acc)))
	    (else
	     (chop (cons (f64vector-subseq v 0 (- blk-len 1)) acc)
		   (f64vector-subseq v (- blk-len overlap)))))))
  (cond ((< blk-len 1)
	 '())
	(else
	 (chop '() v))))


;;;;naive subsampling
;(define (f64vector-subsampling v rate)
;  (let* ((k (min 1. rate))
;	 (new-len (exact-ceiling (* (f64vector-length v) k)))
;	 (new-v   (make-f64vector new-len)))
;    (do ((i 0 (+ 1 i)))
;	((= i new-len) new-v)
;      (f64vector-set! new-v i (f64vector-ref v (exact-floor (/ i k)))))))

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;statistics
(define (f64vector-mean v) 
  (let ((len (max 1 (f64vector-length v))))
    (/ (f64vector-sum v) len)))

(define (f64vector-median v) 
  (median (f64vector->list v)))

;;; Note variance normalized with N, not N-1
(define (f64vector-variance v)
  (let ((x (f64vector-copy v))
	(len (f64vector-length v)))
    (/ (f64vector-sum-square (f64vector-offset! x (- (f64vector-mean x)))) len)))

;;; Note standard deviation normalized with N, not N-1
(define (f64vector-std v) (sqrt (f64vector-variance v)))
(define (f64vector-rms v) (f64vector-std v))

(define (f64vector-square! v) (f64vector-multiply! v v))
(define (f64vector-square  v)
  (let ((v-copy (f64vector-copy v)))
    (f64vector-square! v-copy)
    v-copy))

(define (f64vector-sqrt! v) (f64vector-map! sqrt v))
(define (f64vector-sqrt  v) 
  (let ((v-copy (f64vector-copy v)))
    (f64vector-sqrt! v-copy)
    v-copy))

(define (f64vector-abs! v) (f64vector-map! abs v))
(define (f64vector-abs  v) 
  (let ((v-copy (f64vector-copy v)))
    (f64vector-abs! v-copy)
    v-copy))

(define (f64vector-inverse  v) (f64vector-map  inverse v))
(define (f64vector-inverse! v) (f64vector-map! inverse v))

(define (f64vector-fmodulo  v x) (f64vector-map  (lambda-fmodulo x) v))
(define (f64vector-fmodulo! v x) (f64vector-map! (lambda-fmodulo x) v))

(define (f64vector-round  v) (f64vector-map  round v))
(define (f64vector-round! v) (f64vector-map! round v))

(define (f64vector-pos-lin  v) (f64vector-map  pos-lin v))
(define (f64vector-pos-lin! v) (f64vector-map! pos-lin v))

(define (f64vector-correlation v1 v2)
  (correlation-coeff (f64vector->list v1) (f64vector->list v2)))

;;product of all terms in f64vector
(define (f64vector-prod v)
  (let ((len  (f64vector-length v))
	(prod  1.0))
    (do ((i 0 (+ 1 i)))
	((= i len) prod)
      (set! prod (f64vector* prod (f64vector-ref v i))))))

;;;fastest version when compiled with '(not safe)
(define (f64vector-sum v)
  (let ((len (f64vector-length v))
	(sum 0.0))
    (do ((i 0 (+ 1 i)))
	((= i len) sum)
      (set! sum (f64vector+ sum (f64vector-ref v i))))))

(define (f64vector-normalize! v) (f64vector-scale! v (/ 1. (f64vector-sum v))))
(define (f64vector-normalize v)  (f64vector-normalize! (f64vector-copy v)))

(define (f64vector-normalize-max! v) (f64vector-scale! v (/ 1. (cadr (f64vector-max v)))))
(define (f64vector-normalize-max v)  (f64vector-normalize-max! (f64vector-copy v)))

(define (f64vector-norm v)  (sqrt (f64vector-sum (f64vector-square v))))
(define (f64vector-sum-square v)  (f64vector-sum  (f64vector-square v)))
(define (f64vector-mean-square v) (f64vector-mean (f64vector-square v)))
(define f64vector-energy f64vector-mean-square)

(define (f64vector-normalize-energy! v)
  (let ((energy (f64vector-energy v)))
    (f64vector-scale! v (/ 1. (sqrt energy)))))

(define (f64vector-normalize-energy v)
  (f64vector-normalize-energy! (f64vector-copy v)))

(define (f64vector-energy-diff v1 v2)
  (f64vector-energy (f64vector-subtract! (f64vector-copy v1) v2)))

(define (f64vector-normalized-energy-diff v1 v2)
  (f64vector-energy 
   (f64vector-subtract (f64vector-normalize-energy v1)
		 (f64vector-normalize-energy v2))))

(define (f64vector-power-compressor v threshold ratio)
  (let ((power (f64vector-energy v)))
    (cond ((< power threshold)
	   v)
	  (else
	   (let* ((new-power (+ threshold (* ratio (- power threshold))))
		  (coeff (sqrt (/ new-power power))))
	     (f64vector-scale v coeff))))))

;find 'most f' element according to f: (f (f64vector-ref v i) extreme)
(define (f64vector-extreme f v)
  (let ((extreme (f64vector-ref v 0))
	(pos 0)
	(len (f64vector-length v)))
    (do ((i 1 (+ 1 i)))
	((= i len))
      (cond ((f (f64vector-ref v i) extreme)
	     (set! extreme (f64vector-ref v i))
	     (set! pos i))))
    (list pos extreme)))

(define (f64vector-extreme-value f v)
  (let ((extreme (f64vector-ref v 0))
	(pos 0)
	(len (f64vector-length v)))
    (do ((i 1 (+ 1 i)))
	((= i len))
      (cond ((f (f64vector-ref v i) extreme)
	     (set! extreme (f64vector-ref v i)))))
    extreme))

(define (f64vector-max v) (f64vector-extreme > v))
(define (f64vector-min v) (f64vector-extreme < v))
(define (f64vector-max-value v) (f64vector-extreme-value > v))
(define (f64vector-min-value v) (f64vector-extreme-value < v))

(define (f64vector-max-abs v)
  (f64vector-extreme (lambda (x y)
		 (> (abs x) (abs y)))
	       v))

(define (f64vector-threshold! v thres)
  (let ((len (f64vector-length v)))
    (do ((i 1 (+ i 1)))
	((= i len) v)
      (if (< (f64vector-ref v i) thres)
	  (f64vector-set! v i 0.)  
	  (f64vector-set! v i 1.)))))

;;; trigonometric functions
(define (f64vector-sin!  v) (f64vector-map! sin v))
(define (f64vector-sin   v)
  (let ((v-copy (f64vector-copy v)))
    (f64vector-sin! v-copy)
    v-copy))

(define (f64vector-cos!  v) (f64vector-map! cos v))
(define (f64vector-cos   v)
  (let ((v-copy (f64vector-copy v)))
    (f64vector-cos! v-copy)
    v-copy))

(define (f64vector-tan!  v) (f64vector-map! tan v))
(define (f64vector-tan   v) (f64vector-map  tan v))
(define (f64vector-atan! v) (f64vector-map! atan v))
(define (f64vector-atan  v) (f64vector-map  atan v))

(define (f64vector-log   v) (f64vector-map log v))
(define (f64vector-log10 v) (f64vector-map log10 v))

(define (f64vector-invert v)
  (f64vector-map (lambda (x) (- x)) v))

;;; naive f64vector-reverse
(define (f64vector-reverse v)
  (list->f64vector (reverse (f64vector->list v))))

;;; f64vector convolution
(define (f64vector-conv v1 v2)
  (let* ((len1 (f64vector-length v1))
	 (len2 (f64vector-length v2))
	 (len3 (+ len1 len2 -1))
	 (vc1  (f64vector-append (f64vector-zeros (- len2 1))
			   v1))
	 (vc2  (f64vector-reverse (f64vector-copy v2)))
	 (vc3  (f64vector-zeros len3)))
    (let loop ((i 0))
      (cond ((>= i len3)
	     vc3)
	    (else
	     (f64vector-set! vc3
		       i
		       (f64vector-sum
			(f64vector-multiply!
			 (f64vector-copy vc2)
			 (f64vector-subseq vc1
				     i (+ i (- len2 1))))))
	     (loop (1+ i)))))))

;;; speed up f64vector-convolution with a step > 1
(define (f64vector-conv-step v1 v2 step)
  (let* ((len1 (f64vector-length v1))
	 (len2 (f64vector-length v2))
	 (len3 (exact-ceiling (/ (+ len1 len2 -1) step)))
	 (vc1  (f64vector-append (f64vector-zeros (- len2 1))
			   v1))
	 (vc2 (f64vector-reverse (f64vector-copy v2)))
	 (vc3 (f64vector-zeros len3)))
    (let loop ((i 0))
      (cond ((>= i len3)
	     vc3)
	    (else
	     (f64vector-set! vc3
		       i
		       (f64vector-sum
			(f64vector-multiply!
			 (f64vector-copy vc2)
			 (f64vector-subseq vc1
				     (* i step) (+ (* i step) (- len2 1))))))
	     (loop (+ i 1)))))))

(define (f64vector-autocorrelation v)
  (f64vector-conv (f64vector-reverse v) v))

;;; higher order sliding function (TODO: f argument is a list: change
;;; that)
(define (f64vector-sliding-f f v width)
  (let* ((len     (f64vector-length v))
	 (new-len (- len width -1))
	 (new-f64vector (make-f64vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-f64vector)
	    (else
	     (f64vector-set! new-f64vector i 
		       (f (f64vector->list (f64vector-subseq v i (+ i width -1)))))
	     (loop (+ i 1)))))))

;;; specialization
(define (f64vector-sliding-median v width)
  (let* ((len     (f64vector-length v))
	 (new-len (- len width -1))
	 (new-f64vector (make-f64vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-f64vector)
	    (else
	     (f64vector-set! new-f64vector i 
		       (f64vector-median (f64vector-subseq v i (+ i width -1))))
	     (loop (+ i 1)))))))

(define (f64vector-sliding-mean v width)
  (let* ((len     (f64vector-length v))
	 (new-len (- len width -1))
	 (new-f64vector (make-f64vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-f64vector)
	    (else
	     (f64vector-set! new-f64vector i 
		       (f64vector-mean (f64vector-subseq v i (+ i width -1))))
	     (loop (+ i 1)))))))


(define (f64vector-sliding-min v width)
  (f64vector-sliding-f (lambda (lst) (apply min lst))
		       v
		       width))

(define (f64vector-sliding-max v width)
  (f64vector-sliding-f (lambda (lst) (apply max lst))
		 v
		 width))
