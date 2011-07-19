;;; -*-scheme-*-
;;; f32vector.scm
;;;
;;; f32vector functions for gambit scheme
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

;(define f32vector          f32vector)
;(define f32vector?         f32vector?)
;(define make-f32vector     make-f32vector)
;(define f32vector-length   f32vector-length)
;
;(define f32vector-ref      f32vector-ref)
;(define f32vector-set!     f32vector-set!)
;
;(define f32vector->list    f32vector->list)
;(define list->f32vector    list->f32vector)

(define (f32vector->vector v) 
  (list->vector (f32vector->list v)))
(define (vector->f32vector v) 
  (list->f32vector (map exact->inexact (vector->list v))))

(define f32vector+ ##flonum.+)
(define f32vector- ##flonum.-)
(define f32vector* ##flonum.*)
(define f32vector/ ##flonum./)

;;; copy
(define f32vector-copy f32vector-copy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; non-mutable set
(define (f32vector-set v i new-value)
  (let ((new-v (f32vector-copy v)))
    (f32vector-set! new-v i new-value)
    new-v))

;;; swaps two elements
(define (f32vector-swap! v i j)
  (let ((tmp (f32vector-ref v i)))
    (f32vector-set! v i (f32vector-ref v j))
    (f32vector-set! v j tmp)))

(define (f32vector-swap v i j)
  (let ((new-v (f32vector-copy v)))
    (f32vector-swap! new-v i j)
    new-v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; map for one or two f32vectors
(define (f32vector-map f . args)
  (let* ((v1 (f32vector-copy (car args)))
	 (v2 (if (length> args 1) (cadr args) #f)))
    (if v2
	(f32vector-map! f v1 v2)
	(f32vector-map! f v1))))

;;; mutating map for one or two f32vectors
(define (f32vector-map! f . args)
  (let* ((v1    (car args))
	 (v2    (if (length> args 1) (cadr args) #f))
	 (len   (f32vector-length v1)))
    (if v2  ;;check dimensions
	(let* ((len2 (f32vector-length v2)))
	  (if (not (= len len2))
	      (error "f32vector-map!: f32vectors v1 and v2 must have same length.\n"))))
    (let loop ((i 0))
      (cond ((< i len)
	     (f32vector-set! v1 i 
		       (if v2
			   (f (f32vector-ref v1 i) 
			      (f32vector-ref v2 i))
			   (f (f32vector-ref v1 i))))
	     (loop (+ i 1)))
	    (else
	     v1)))))

(define (f32vector-foldr f init v)
  (let ((len (f32vector-length v)))
    (cond ((< len 1)
	   #f) ;should report error here
	  (else
	   (let loop ((i 0)
		      (acc init))
	     (cond ((= i len)
		    acc)
		   (else
		    (loop (+ i 1)
			  (f acc (f32vector-ref v i))))))))))
		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f32vector-subseq does right-side zero-padding 
(define (f32vector-subseq v start . end)
  (let* ((len      (f32vector-length v))
	 (start    (max start 0))
	 (end      (if (not (null? end)) 
		       (car end) 
		       (- (f32vector-length v) 1)))
	 (new-len  (- end start -1))
	 (max-iter (min new-len (- len start))))
    (cond ((> new-len 0)
	   (let ((new-v (make-f32vector new-len 0.)))
	     (if (< max-iter 1)
		 new-v
		 (do ((i 0 (+ 1 i)))
		     ((= i max-iter) new-v)
		   (f32vector-set! new-v i (f32vector-ref v (+ i start)))))))
	  (else
	   (f32vector)))))

;;; NOT GOOD, BUGGY
;(define (f32vector-subseq v start . end)
;  (define (copy-in-place! v1 v2)
;    (let ((len (min (f32vector-length v1) (f32vector-length v2))))
;      (let loop ((i 0))
;	(cond ((>= i len)
;	       #t)
;	      (else
;	       (f32vector-set! v1 i
;			 (f32vector-ref v2 i))
;	       (loop (+ i 1)))))))
;
;  (let* ((len      (f32vector-length v))
;	 (start    (max start 0))
;	 (end      (if (not (null? end)) 
;		       (car end) 
;		       (f32vector-length v)))
;	 (new-len  (- end start -1))
;	 (max-iter (min new-len (- len start))))
;    (cond ((> new-len 0)
;	   (let ((new-v (make-f32vector new-len 0.)))
;	     (copy-in-place! new-v (subf32vector v start (min (+ 1 end) (f32vector-length v))))
;	     new-v))
;	  (else
;	   (f32vector)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macro-f32vector-map!
(define-macro (macro-f32vector-map! f v)
  `(let* ((len (f32vector-length v)))
     (let loop ((i 0))
       (if (##fixnum.>= i len)
	   v
	   (begin
	     (f32vector-set! v i (,f (f32vector-ref v i)))
	     (loop (##fixnum.+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;scale
(define (f32vector-scale! v k)
  (let ((k (exact->inexact k)))
    (macro-f32vector-map! (lambda (x) (f32vector* k x)) v)))

(define (f32vector-scale v k)
  (let ((v-copy (f32vector-copy v)))
    (f32vector-scale! v-copy k)
    v-copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;offset
(define (f32vector-offset! v k)
  (let ((k (exact->inexact k)))
    (macro-f32vector-map! (lambda (x) (f32vector+ k x)) v)))

(define (f32vector-offset v k)
  (f32vector-offset! (f32vector-copy v) k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;f32vector-remove-mean
(define (f32vector-remove-mean! v)
  (f32vector-offset! v (- (f32vector-mean v))))

(define (f32vector-remove-mean  v)
  (f32vector-offset  v (- (f32vector-mean v))))

;;;f32vector-do accepts f32vectors of different length, returns a f32vector of length
;;;(f32vector-length v1) OBSOLETE 20070201: use f32vector-do below instead
;(define (f32vector-do f v1 v2 . offset)
;  (let* ((offset (if (null? offset) 0 (car offset)))
;	 (len1 (- (f32vector-length v1) offset))
;	 (len2 (f32vector-length v2))
;	 (len  (min len1 len2))
;	 (new-v (f32vector-copy v1)))
;    (let loop ((i 0))
;      (if (= i len)
;	  new-v
;	  (begin
;	    (f32vector-set! new-v (+ i offset) (f (f32vector-ref v1 (+ i offset)) 
;					    (f32vector-ref v2 i)))
;	    (loop (+ i 1)))))))
;
;(define (f32vector-do! f v1 v2 . offset)
;  (let* ((offset (if (null? offset) 0 (car offset)))
;	 (len1 (- (f32vector-length v1) offset))
;	 (len2 (f32vector-length v2))
;	 (len  (min len1 len2)))
;    (let loop ((i 0))
;      (if (= i len)
;	  v1
;	  (begin
;	    (f32vector-set! v1 (+ i offset) (f (f32vector-ref v1 (+ i offset)) 
;					 (f32vector-ref v2 i)))
;	    (loop (+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on two vectors 
(define (f32vector-do! f v1 v2)
  (let* ((len (min (f32vector-length v1) (f32vector-length v2))))
    (let loop ((i 0))
      (cond ((##fixnum.>= i len)
	     v1)
	    (else
	     (f32vector-set! v1 i (f (f32vector-ref v1 i) (f32vector-ref v2 i)))
	     (loop (##fixnum.+ i 1)))))))

(define (f32vector-do f v1 v2)
  (let ((v1-copy (f32vector-copy v1)))
    (f32vector-do! f v1 v2)))

;;; with position offset
(define (f32vector-do-offset! f v1 v2 offset)
  (let* ((len1 (- (f32vector-length v1) offset))
	 (len2 (f32vector-length v2))
	 (len  (min len1 len2))
	 (offset (inexact->exact offset)))
    (let loop ((i 0))
      (if (>= i len)
	  v1
	  (let ((i1 (##fixnum.+ i offset)))
	    (f32vector-set! v1 i1 (f (f32vector-ref v1 i1) 
				(f32vector-ref v2 i)))
	    (loop (##fixnum.+ i 1)))))))

(define (f32vector-do-offset f v1 v2 offset)
  (let ((v1-copy (f32vector-copy v1)))
    (f32vector-do-offset! f v1 v2 offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f32vector-add
(define (f32vector-add! v1 v2) (f32vector-do! f32vector+ v1 v2))
(define (f32vector-add  v1 v2) (f32vector-add! (f32vector-copy v1) v2))

;;; f32vector-add with position offset
(define (f32vector-add-offset! v1 v2 offset)  
  (f32vector-do-offset! f32vector+ v1 v2 offset))
(define (f32vector-add-offset  v1 v2 offset) 
  (f32vector-add-offset! (f32vector-copy v1) v2 offset))

;;; add many f32vectors, no offset
(define (f32vectors-add! v1 . f32vectors)
  (let loop ((f32vectors f32vectors))
    (cond ((null? f32vectors)
	   v1)
	  (else
	   (f32vectors-add! (f32vector-add! v1 (car f32vectors)))
	   (loop (cdr f32vectors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f32vector-subtract
(define (f32vector-subtract! v1 v2) (f32vector-do! f32vector- v1 v2))
(define (f32vector-subtract  v1 v2) (f32vector-subtract! (f32vector-copy v1) v2))

;;; f32vector-subtract with position offset
(define (f32vector-subtract-offset! v1 v2 offset)  
  (f32vector-do-offset! f32vector- v1 v2 offset))
(define (f32vector-subtract-offset  v1 v2 offset) 
  (f32vector-subtract-offset! (f32vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f32vector-multiply
(define (f32vector-multiply! v1 v2) (f32vector-do! f32vector* v1 v2))
(define (f32vector-multiply  v1 v2) (f32vector-multiply! (f32vector-copy v1) v2))

;;; f32vector-multiply with position offset
(define (f32vector-multiply-offset! v1 v2 offset)  
  (f32vector-do-offset! f32vector* v1 v2 offset))
(define (f32vector-multiply-offset  v1 v2 offset) 
  (f32vector-multiply-offset! (f32vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Division
(define (f32vector-divide! v1 v2) (f32vector-do! f32vector/ v1 v2))
(define (f32vector-divide  v1 v2) (f32vector-divide! (f32vector-copy v1) v2))

(define (f32vector-divide-offset! v1 v2 . offset)
  (let ((offset (if (null? offset) 0 (car offset))))
    (f32vector-do-offset! f32vector/ v1 v2 offset)))
(define (f32vector-divide-offset  v1 v2 offset)
  (f32vector-divide-offset! (f32vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scalar product
(define (f32vector-product v1 v2)
  (f32vector-sum (f32vector-multiply! (f32vector-copy v1) v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replicate f32vector n times
(define (f32vector-replicate v n)
  (let loop ((i 1)
	     (new-v v))
    (cond ((< i n)
	   (loop (+ i 1)
		 (f32vector-append new-v v)))
	  (else
	   new-v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; progressive sum of a f32vector
(define (f32vector-progressive-sum v)
  (let* ((len   (f32vector-length v))
	 (new-v (make-f32vector len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (f32vector-set! new-v i (f32vector-sum (f32vector-subseq v 0 i)))
	     (loop (+ i 1)))
	    (else
	     new-v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f32vector initialisation

;;; f32vector-fill with thunk
(define (f32vector-fill len thunk)
  (let ((v (make-f32vector len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (f32vector-set! v i (thunk))
	     (loop (+ i 1)))
	    (else
	     v)))))

(define (f32vector-zeros len) (make-f32vector len 0.))
(define (f32vector-ones  len) (make-f32vector len 1.))

;;; f32vector-iota (based on srfi-1's iota function)
(define (f32vector-iota . args)
  (list->f32vector
   (map exact->inexact
	(apply iota args))))

;;; add constant points at the beginning and end of f32vector
(define (f32vector-stretch v pad-len) 
  (let* ((len     (f32vector-length v))
	 (new-len (+ len (* 2 pad-len)))
	 (new-v (make-f32vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-v)
	    ((< i pad-len)
	     (f32vector-set! new-v i (f32vector-ref v 0))
	     (loop (+ i 1)))
	    ((>= i (+ len pad-len))
	     (f32vector-set! new-v i (f32vector-ref v (- len 1)))
	     (loop (+ i 1)))
	    (else
	     (f32vector-set! new-v i (f32vector-ref v (- i pad-len)))
	     (loop (+ i 1)))))))

;;; (f32vector-from-to 2 5) returns #f32(2. 3. 4. 5.)
(define (f32vector-from-to from to)
  (list->f32vector (map exact->inexact (from-to from to))))

;;; (f32vector-from-to 2 5 0.5) returns #f32(2. 2.5 3. 3.5 4. 4.5 5.)
(define (f32vector-from-to-step from to step)
  (list->f32vector (map exact->inexact (from-to-step from to step))))

;;; (f32vector-from-to-length -1 2 5) returns #f32(-1. -.25 .5 1.25 2.)
(define (f32vector-from-to-length from to len)
  (let ((v (f32vector-iota len)))
    (f32vector-offset (f32vector-scale v (/ (- to from) (- len 1))) from)))
	
;;; random reals between 0. and k
(define (f32vector-random len k)
  (f32vector-fill len (lambda () (* k (random-real)))))

;;; something that looks like gaussian noise
(define (f32vector-gauss len nrg-bidon)
  (let ((v (make-f32vector len 0.)))
    (do ((i 0 (1+ i)))
	((= i 10) v)
      (f32vector-add! v (f32vector-random len 1.)))
    (f32vector-offset! v (- (f32vector-mean v)))
    (f32vector-scale! v (sqrt (/ nrg-bidon (f32vector-energy v))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Append, split
;f32vector-append
(define (f32vector-append . v)
  (cond ((null? v)
	 #f)
	(else
	 (let* ((lens (map f32vector-length v))
		(total-len (sum lens))
		(new-v (make-f32vector total-len 0.)))
	   (let loop ((i 0) (v v) (lens lens))
	     (cond ((null? v)
		    new-v)
		   (else
		    (f32vector-add-offset! new-v (car v) i)
		    (loop (+ i (car lens)) (cdr v) (cdr lens)))))))))

;f32vector-split: v is a f32vector, pts is a list of positions (ex: (list 0 5 14))
(define (f32vector-split v pts)
  (let ((len (f32vector-length v)))
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
	     (cons (f32vector-subseq v start end)
		   (f32vector-split v (cdr pts))))))))

;;; chop chop!
(define (f32vector-chop v blk-len)
  (define (chop acc v)
    (let ((len (f32vector-length v)))
      (cond ((<= len blk-len)
	     (reverse (cons v acc)))
	    (else
	     (chop (cons (f32vector-subseq v 0 (- blk-len 1)) acc)
		   (f32vector-subseq v blk-len))))))
  (cond ((< blk-len 1)
	 '())
	(else
	 (chop '() v))))

;;; chop overlap
(define (f32vector-chop-overlap v blk-len overlap)
  (define (chop acc v)
    (let ((len (f32vector-length v)))
      (cond ((<= len blk-len)
	     (reverse (cons v acc)))
	    (else
	     (chop (cons (f32vector-subseq v 0 (- blk-len 1)) acc)
		   (f32vector-subseq v (- blk-len overlap)))))))
  (cond ((< blk-len 1)
	 '())
	(else
	 (chop '() v))))


;;;;naive subsampling
;(define (f32vector-subsampling v rate)
;  (let* ((k (min 1. rate))
;	 (new-len (exact-ceiling (* (f32vector-length v) k)))
;	 (new-v   (make-f32vector new-len)))
;    (do ((i 0 (+ 1 i)))
;	((= i new-len) new-v)
;      (f32vector-set! new-v i (f32vector-ref v (exact-floor (/ i k)))))))

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;statistics
(define (f32vector-mean v) 
  (let ((len (max 1 (f32vector-length v))))
    (/ (f32vector-sum v) len)))

(define (f32vector-median v) 
  (median (f32vector->list v)))

;;; Note variance normalized with N, not N-1
(define (f32vector-variance v)
  (let ((x (f32vector-copy v))
	(len (f32vector-length v)))
    (/ (f32vector-sum-square (f32vector-offset! x (- (f32vector-mean x)))) len)))

;;; Note standard deviation normalized with N, not N-1
(define (f32vector-std v) (sqrt (f32vector-variance v)))
(define (f32vector-rms v) (f32vector-std v))

(define (f32vector-square! v) (f32vector-multiply! v v))
(define (f32vector-square  v)
  (let ((v-copy (f32vector-copy v)))
    (f32vector-square! v-copy)
    v-copy))

(define (f32vector-sqrt! v) (f32vector-map! sqrt v))
(define (f32vector-sqrt  v) 
  (let ((v-copy (f32vector-copy v)))
    (f32vector-sqrt! v-copy)
    v-copy))

(define (f32vector-abs! v) (f32vector-map! abs v))
(define (f32vector-abs  v) 
  (let ((v-copy (f32vector-copy v)))
    (f32vector-abs! v-copy)
    v-copy))

(define (f32vector-inverse  v) (f32vector-map  inverse v))
(define (f32vector-inverse! v) (f32vector-map! inverse v))

(define (f32vector-fmodulo  v x) (f32vector-map  (lambda-fmodulo x) v))
(define (f32vector-fmodulo! v x) (f32vector-map! (lambda-fmodulo x) v))

(define (f32vector-round  v) (f32vector-map  round v))
(define (f32vector-round! v) (f32vector-map! round v))

(define (f32vector-pos-lin  v) (f32vector-map  pos-lin v))
(define (f32vector-pos-lin! v) (f32vector-map! pos-lin v))

(define (f32vector-correlation v1 v2)
  (correlation-coeff (f32vector->list v1) (f32vector->list v2)))

;;product of all terms in f32vector
(define (f32vector-prod v)
  (let ((len  (f32vector-length v))
	(prod  1.0))
    (do ((i 0 (+ 1 i)))
	((= i len) prod)
      (set! prod (f32vector* prod (f32vector-ref v i))))))

;;;fastest version when compiled with '(not safe)
(define (f32vector-sum v)
  (let ((len (f32vector-length v))
	(sum 0.0))
    (do ((i 0 (+ 1 i)))
	((= i len) sum)
      (set! sum (f32vector+ sum (f32vector-ref v i))))))

(define (f32vector-normalize! v) (f32vector-scale! v (/ 1. (f32vector-sum v))))
(define (f32vector-normalize v)  (f32vector-normalize! (f32vector-copy v)))

(define (f32vector-normalize-max! v) (f32vector-scale! v (/ 1. (cadr (f32vector-max v)))))
(define (f32vector-normalize-max v)  (f32vector-normalize-max! (f32vector-copy v)))

(define (f32vector-norm v)  (sqrt (f32vector-sum (f32vector-square v))))
(define (f32vector-sum-square v)  (f32vector-sum  (f32vector-square v)))
(define (f32vector-mean-square v) (f32vector-mean (f32vector-square v)))
(define f32vector-energy f32vector-mean-square)

(define (f32vector-normalize-energy! v)
  (let ((energy (f32vector-energy v)))
    (f32vector-scale! v (/ 1. (sqrt energy)))))

(define (f32vector-normalize-energy v)
  (f32vector-normalize-energy! (f32vector-copy v)))

(define (f32vector-energy-diff v1 v2)
  (f32vector-energy (f32vector-subtract! (f32vector-copy v1) v2)))

(define (f32vector-normalized-energy-diff v1 v2)
  (f32vector-energy 
   (f32vector-subtract (f32vector-normalize-energy v1)
		 (f32vector-normalize-energy v2))))

(define (f32vector-power-compressor v threshold ratio)
  (let ((power (f32vector-energy v)))
    (cond ((< power threshold)
	   v)
	  (else
	   (let* ((new-power (+ threshold (* ratio (- power threshold))))
		  (coeff (sqrt (/ new-power power))))
	     (f32vector-scale v coeff))))))

;find 'most f' element according to f: (f (f32vector-ref v i) extreme)
(define (f32vector-extreme f v)
  (let ((extreme (f32vector-ref v 0))
	(pos 0)
	(len (f32vector-length v)))
    (do ((i 1 (+ 1 i)))
	((= i len))
      (cond ((f (f32vector-ref v i) extreme)
	     (set! extreme (f32vector-ref v i))
	     (set! pos i))))
    (list pos extreme)))

(define (f32vector-extreme-value f v)
  (let ((extreme (f32vector-ref v 0))
	(pos 0)
	(len (f32vector-length v)))
    (do ((i 1 (+ 1 i)))
	((= i len))
      (cond ((f (f32vector-ref v i) extreme)
	     (set! extreme (f32vector-ref v i)))))
    extreme))

(define (f32vector-max v) (f32vector-extreme > v))
(define (f32vector-min v) (f32vector-extreme < v))
(define (f32vector-max-value v) (f32vector-extreme-value > v))
(define (f32vector-min-value v) (f32vector-extreme-value < v))

(define (f32vector-max-abs v)
  (f32vector-extreme (lambda (x y)
		 (> (abs x) (abs y)))
	       v))

(define (f32vector-threshold! v thres)
  (let ((len (f32vector-length v)))
    (do ((i 1 (+ i 1)))
	((= i len) v)
      (if (< (f32vector-ref v i) thres)
	  (f32vector-set! v i 0.)  
	  (f32vector-set! v i 1.)))))

;;; trigonometric functions
(define (f32vector-sin!  v) (f32vector-map! sin v))
(define (f32vector-sin   v)
  (let ((v-copy (f32vector-copy v)))
    (f32vector-sin! v-copy)
    v-copy))

(define (f32vector-cos!  v) (f32vector-map! cos v))
(define (f32vector-cos   v)
  (let ((v-copy (f32vector-copy v)))
    (f32vector-cos! v-copy)
    v-copy))

(define (f32vector-tan!  v) (f32vector-map! tan v))
(define (f32vector-tan   v) (f32vector-map  tan v))
(define (f32vector-atan! v) (f32vector-map! atan v))
(define (f32vector-atan  v) (f32vector-map  atan v))

(define (f32vector-log   v) (f32vector-map log v))
(define (f32vector-log10 v) (f32vector-map log10 v))

(define (f32vector-invert v)
  (f32vector-map (lambda (x) (- x)) v))

;;; naive f32vector-reverse
(define (f32vector-reverse v)
  (list->f32vector (reverse (f32vector->list v))))

;;; f32vector convolution
(define (f32vector-conv v1 v2)
  (let* ((len1 (f32vector-length v1))
	 (len2 (f32vector-length v2))
	 (len3 (+ len1 len2 -1))
	 (vc1  (f32vector-append (f32vector-zeros (- len2 1))
			   v1))
	 (vc2  (f32vector-reverse (f32vector-copy v2)))
	 (vc3  (f32vector-zeros len3)))
    (let loop ((i 0))
      (cond ((>= i len3)
	     vc3)
	    (else
	     (f32vector-set! vc3
		       i
		       (f32vector-sum
			(f32vector-multiply!
			 (f32vector-copy vc2)
			 (f32vector-subseq vc1
				     i (+ i (- len2 1))))))
	     (loop (1+ i)))))))

;;; speed up f32vector-convolution with a step > 1
(define (f32vector-conv-step v1 v2 step)
  (let* ((len1 (f32vector-length v1))
	 (len2 (f32vector-length v2))
	 (len3 (exact-ceiling (/ (+ len1 len2 -1) step)))
	 (vc1  (f32vector-append (f32vector-zeros (- len2 1))
			   v1))
	 (vc2 (f32vector-reverse (f32vector-copy v2)))
	 (vc3 (f32vector-zeros len3)))
    (let loop ((i 0))
      (cond ((>= i len3)
	     vc3)
	    (else
	     (f32vector-set! vc3
		       i
		       (f32vector-sum
			(f32vector-multiply!
			 (f32vector-copy vc2)
			 (f32vector-subseq vc1
				     (* i step) (+ (* i step) (- len2 1))))))
	     (loop (+ i 1)))))))

(define (f32vector-autocorrelation v)
  (f32vector-conv (f32vector-reverse v) v))

;;; higher order sliding function (TODO: f argument is a list: change
;;; that)
(define (f32vector-sliding-f f v width)
  (let* ((len     (f32vector-length v))
	 (new-len (- len width -1))
	 (new-f32vector (make-f32vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-f32vector)
	    (else
	     (f32vector-set! new-f32vector i 
		       (f (f32vector->list (f32vector-subseq v i (+ i width -1)))))
	     (loop (+ i 1)))))))

;;; specialization
(define (f32vector-sliding-median v width)
  (let* ((len     (f32vector-length v))
	 (new-len (- len width -1))
	 (new-f32vector (make-f32vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-f32vector)
	    (else
	     (f32vector-set! new-f32vector i 
		       (f32vector-median (f32vector-subseq v i (+ i width -1))))
	     (loop (+ i 1)))))))

(define (f32vector-sliding-mean v width)
  (let* ((len     (f32vector-length v))
	 (new-len (- len width -1))
	 (new-f32vector (make-f32vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-f32vector)
	    (else
	     (f32vector-set! new-f32vector i 
		       (f32vector-mean (f32vector-subseq v i (+ i width -1))))
	     (loop (+ i 1)))))))


(define (f32vector-sliding-min v width)
  (f32vector-sliding-f (lambda (lst) (apply min lst))
		       v
		       width))

(define (f32vector-sliding-max v width)
  (f32vector-sliding-f (lambda (lst) (apply max lst))
		 v
		 width))
