;;; -*-scheme-*-
;;; u64vector.scm
;;;
;;; u64vector functions for gambit scheme
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

;(define u64vector          u64vector)
;(define u64vector?         u64vector?)
;(define make-u64vector     make-u64vector)
;(define u64vector-length   u64vector-length)
;
;(define u64vector-ref      u64vector-ref)
;(define u64vector-set!     u64vector-set!)
;
;(define u64vector->list    u64vector->list)
;(define list->u64vector    list->u64vector)

(define (u64vector->vector v) 
  (list->vector (u64vector->list v)))
(define (vector->u64vector v) 
  (list->u64vector (map exact->inexact (vector->list v))))

(define u64vector+ ##flonum.+)
(define u64vector- ##flonum.-)
(define u64vector* ##flonum.*)
(define u64vector/ ##flonum./)

;;; copy
(define u64vector-copy u64vector-copy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; non-mutable set
(define (u64vector-set v i new-value)
  (let ((new-v (u64vector-copy v)))
    (u64vector-set! new-v i new-value)
    new-v))

;;; swaps two elements
(define (u64vector-swap! v i j)
  (let ((tmp (u64vector-ref v i)))
    (u64vector-set! v i (u64vector-ref v j))
    (u64vector-set! v j tmp)))

(define (u64vector-swap v i j)
  (let ((new-v (u64vector-copy v)))
    (u64vector-swap! new-v i j)
    new-v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; map for one or two u64vectors
(define (u64vector-map f . args)
  (let* ((v1 (u64vector-copy (car args)))
	 (v2 (if (length> args 1) (cadr args) #f)))
    (if v2
	(u64vector-map! f v1 v2)
	(u64vector-map! f v1))))

;;; mutating map for one or two u64vectors
(define (u64vector-map! f . args)
  (let* ((v1    (car args))
	 (v2    (if (length> args 1) (cadr args) #f))
	 (len   (u64vector-length v1)))
    (if v2  ;;check dimensions
	(let* ((len2 (u64vector-length v2)))
	  (if (not (= len len2))
	      (error "u64vector-map!: u64vectors v1 and v2 must have same length.\n"))))
    (let loop ((i 0))
      (cond ((< i len)
	     (u64vector-set! v1 i 
		       (if v2
			   (f (u64vector-ref v1 i) 
			      (u64vector-ref v2 i))
			   (f (u64vector-ref v1 i))))
	     (loop (+ i 1)))
	    (else
	     v1)))))

(define (u64vector-foldr f init v)
  (let ((len (u64vector-length v)))
    (cond ((< len 1)
	   #f) ;should report error here
	  (else
	   (let loop ((i 0)
		      (acc init))
	     (cond ((= i len)
		    acc)
		   (else
		    (loop (+ i 1)
			  (f acc (u64vector-ref v i))))))))))
		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; u64vector-subseq does right-side zero-padding 
(define (u64vector-subseq v start . end)
  (let* ((len      (u64vector-length v))
	 (start    (max start 0))
	 (end      (if (not (null? end)) 
		       (car end) 
		       (- (u64vector-length v) 1)))
	 (new-len  (- end start -1))
	 (max-iter (min new-len (- len start))))
    (cond ((> new-len 0)
	   (let ((new-v (make-u64vector new-len 0.)))
	     (if (< max-iter 1)
		 new-v
		 (do ((i 0 (+ 1 i)))
		     ((= i max-iter) new-v)
		   (u64vector-set! new-v i (u64vector-ref v (+ i start)))))))
	  (else
	   (u64vector)))))

;;; NOT GOOD, BUGGY
;(define (u64vector-subseq v start . end)
;  (define (copy-in-place! v1 v2)
;    (let ((len (min (u64vector-length v1) (u64vector-length v2))))
;      (let loop ((i 0))
;	(cond ((>= i len)
;	       #t)
;	      (else
;	       (u64vector-set! v1 i
;			 (u64vector-ref v2 i))
;	       (loop (+ i 1)))))))
;
;  (let* ((len      (u64vector-length v))
;	 (start    (max start 0))
;	 (end      (if (not (null? end)) 
;		       (car end) 
;		       (u64vector-length v)))
;	 (new-len  (- end start -1))
;	 (max-iter (min new-len (- len start))))
;    (cond ((> new-len 0)
;	   (let ((new-v (make-u64vector new-len 0.)))
;	     (copy-in-place! new-v (subu64vector v start (min (+ 1 end) (u64vector-length v))))
;	     new-v))
;	  (else
;	   (u64vector)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macro-u64vector-map!
(define-macro (macro-u64vector-map! f v)
  `(let* ((len (u64vector-length v)))
     (let loop ((i 0))
       (if (##fixnum.>= i len)
	   v
	   (begin
	     (u64vector-set! v i (,f (u64vector-ref v i)))
	     (loop (##fixnum.+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;scale
(define (u64vector-scale! v k)
  (let ((k (exact->inexact k)))
    (macro-u64vector-map! (lambda (x) (u64vector* k x)) v)))

(define (u64vector-scale v k)
  (let ((v-copy (u64vector-copy v)))
    (u64vector-scale! v-copy k)
    v-copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;offset
(define (u64vector-offset! v k)
  (let ((k (exact->inexact k)))
    (macro-u64vector-map! (lambda (x) (u64vector+ k x)) v)))

(define (u64vector-offset v k)
  (u64vector-offset! (u64vector-copy v) k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;u64vector-remove-mean
(define (u64vector-remove-mean! v)
  (u64vector-offset! v (- (u64vector-mean v))))

(define (u64vector-remove-mean  v)
  (u64vector-offset  v (- (u64vector-mean v))))

;;;u64vector-do accepts u64vectors of different length, returns a u64vector of length
;;;(u64vector-length v1) OBSOLETE 20070201: use u64vector-do below instead
;(define (u64vector-do f v1 v2 . offset)
;  (let* ((offset (if (null? offset) 0 (car offset)))
;	 (len1 (- (u64vector-length v1) offset))
;	 (len2 (u64vector-length v2))
;	 (len  (min len1 len2))
;	 (new-v (u64vector-copy v1)))
;    (let loop ((i 0))
;      (if (= i len)
;	  new-v
;	  (begin
;	    (u64vector-set! new-v (+ i offset) (f (u64vector-ref v1 (+ i offset)) 
;					    (u64vector-ref v2 i)))
;	    (loop (+ i 1)))))))
;
;(define (u64vector-do! f v1 v2 . offset)
;  (let* ((offset (if (null? offset) 0 (car offset)))
;	 (len1 (- (u64vector-length v1) offset))
;	 (len2 (u64vector-length v2))
;	 (len  (min len1 len2)))
;    (let loop ((i 0))
;      (if (= i len)
;	  v1
;	  (begin
;	    (u64vector-set! v1 (+ i offset) (f (u64vector-ref v1 (+ i offset)) 
;					 (u64vector-ref v2 i)))
;	    (loop (+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on two vectors 
(define (u64vector-do! f v1 v2)
  (let* ((len (min (u64vector-length v1) (u64vector-length v2))))
    (let loop ((i 0))
      (cond ((##fixnum.>= i len)
	     v1)
	    (else
	     (u64vector-set! v1 i (f (u64vector-ref v1 i) (u64vector-ref v2 i)))
	     (loop (##fixnum.+ i 1)))))))

(define (u64vector-do f v1 v2)
  (let ((v1-copy (u64vector-copy v1)))
    (u64vector-do! f v1 v2)))

;;; with position offset
(define (u64vector-do-offset! f v1 v2 offset)
  (let* ((len1 (- (u64vector-length v1) offset))
	 (len2 (u64vector-length v2))
	 (len  (min len1 len2))
	 (offset (inexact->exact offset)))
    (let loop ((i 0))
      (if (>= i len)
	  v1
	  (let ((i1 (##fixnum.+ i offset)))
	    (u64vector-set! v1 i1 (f (u64vector-ref v1 i1) 
				(u64vector-ref v2 i)))
	    (loop (##fixnum.+ i 1)))))))

(define (u64vector-do-offset f v1 v2 offset)
  (let ((v1-copy (u64vector-copy v1)))
    (u64vector-do-offset! f v1 v2 offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; u64vector-add
(define (u64vector-add! v1 v2) (u64vector-do! u64vector+ v1 v2))
(define (u64vector-add  v1 v2) (u64vector-add! (u64vector-copy v1) v2))

;;; u64vector-add with position offset
(define (u64vector-add-offset! v1 v2 offset)  
  (u64vector-do-offset! u64vector+ v1 v2 offset))
(define (u64vector-add-offset  v1 v2 offset) 
  (u64vector-add-offset! (u64vector-copy v1) v2 offset))

;;; add many u64vectors, no offset
(define (u64vectors-add! v1 . u64vectors)
  (let loop ((u64vectors u64vectors))
    (cond ((null? u64vectors)
	   v1)
	  (else
	   (u64vectors-add! (u64vector-add! v1 (car u64vectors)))
	   (loop (cdr u64vectors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; u64vector-subtract
(define (u64vector-subtract! v1 v2) (u64vector-do! u64vector- v1 v2))
(define (u64vector-subtract  v1 v2) (u64vector-subtract! (u64vector-copy v1) v2))

;;; u64vector-subtract with position offset
(define (u64vector-subtract-offset! v1 v2 offset)  
  (u64vector-do-offset! u64vector- v1 v2 offset))
(define (u64vector-subtract-offset  v1 v2 offset) 
  (u64vector-subtract-offset! (u64vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; u64vector-multiply
(define (u64vector-multiply! v1 v2) (u64vector-do! u64vector* v1 v2))
(define (u64vector-multiply  v1 v2) (u64vector-multiply! (u64vector-copy v1) v2))

;;; u64vector-multiply with position offset
(define (u64vector-multiply-offset! v1 v2 offset)  
  (u64vector-do-offset! u64vector* v1 v2 offset))
(define (u64vector-multiply-offset  v1 v2 offset) 
  (u64vector-multiply-offset! (u64vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Division
(define (u64vector-divide! v1 v2) (u64vector-do! u64vector/ v1 v2))
(define (u64vector-divide  v1 v2) (u64vector-divide! (u64vector-copy v1) v2))

(define (u64vector-divide-offset! v1 v2 . offset)
  (let ((offset (if (null? offset) 0 (car offset))))
    (u64vector-do-offset! u64vector/ v1 v2 offset)))
(define (u64vector-divide-offset  v1 v2 offset)
  (u64vector-divide-offset! (u64vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scalar product
(define (u64vector-product v1 v2)
  (u64vector-sum (u64vector-multiply! (u64vector-copy v1) v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replicate u64vector n times
(define (u64vector-replicate v n)
  (let loop ((i 1)
	     (new-v v))
    (cond ((< i n)
	   (loop (+ i 1)
		 (u64vector-append new-v v)))
	  (else
	   new-v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; progressive sum of a u64vector
(define (u64vector-progressive-sum v)
  (let* ((len   (u64vector-length v))
	 (new-v (make-u64vector len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (u64vector-set! new-v i (u64vector-sum (u64vector-subseq v 0 i)))
	     (loop (+ i 1)))
	    (else
	     new-v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; u64vector initialisation

;;; u64vector-fill with thunk
(define (u64vector-fill len thunk)
  (let ((v (make-u64vector len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (u64vector-set! v i (thunk))
	     (loop (+ i 1)))
	    (else
	     v)))))

(define (u64vector-zeros len) (make-u64vector len 0.))
(define (u64vector-ones  len) (make-u64vector len 1.))

;;; u64vector-iota (based on srfi-1's iota function)
(define (u64vector-iota . args)
  (list->u64vector
   (map exact->inexact
	(apply iota args))))

;;; add constant points at the beginning and end of u64vector
(define (u64vector-stretch v pad-len) 
  (let* ((len     (u64vector-length v))
	 (new-len (+ len (* 2 pad-len)))
	 (new-v (make-u64vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-v)
	    ((< i pad-len)
	     (u64vector-set! new-v i (u64vector-ref v 0))
	     (loop (+ i 1)))
	    ((>= i (+ len pad-len))
	     (u64vector-set! new-v i (u64vector-ref v (- len 1)))
	     (loop (+ i 1)))
	    (else
	     (u64vector-set! new-v i (u64vector-ref v (- i pad-len)))
	     (loop (+ i 1)))))))

;;; (u64vector-from-to 2 5) returns #u64(2. 3. 4. 5.)
(define (u64vector-from-to from to)
  (list->u64vector (map exact->inexact (from-to from to))))

;;; (u64vector-from-to 2 5 0.5) returns #u64(2. 2.5 3. 3.5 4. 4.5 5.)
(define (u64vector-from-to-step from to step)
  (list->u64vector (map exact->inexact (from-to-step from to step))))

;;; (u64vector-from-to-length -1 2 5) returns #u64(-1. -.25 .5 1.25 2.)
(define (u64vector-from-to-length from to len)
  (let ((v (u64vector-iota len)))
    (u64vector-offset (u64vector-scale v (/ (- to from) (- len 1))) from)))
	
;;; random reals between 0. and k
(define (u64vector-random len k)
  (u64vector-fill len (lambda () (* k (random-real)))))

;;; something that looks like gaussian noise
(define (u64vector-gauss len nrg-bidon)
  (let ((v (make-u64vector len 0.)))
    (do ((i 0 (1+ i)))
	((= i 10) v)
      (u64vector-add! v (u64vector-random len 1.)))
    (u64vector-offset! v (- (u64vector-mean v)))
    (u64vector-scale! v (sqrt (/ nrg-bidon (u64vector-energy v))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Append, split
;u64vector-append
(define (u64vector-append . v)
  (cond ((null? v)
	 #f)
	(else
	 (let* ((lens (map u64vector-length v))
		(total-len (sum lens))
		(new-v (make-u64vector total-len 0.)))
	   (let loop ((i 0) (v v) (lens lens))
	     (cond ((null? v)
		    new-v)
		   (else
		    (u64vector-add-offset! new-v (car v) i)
		    (loop (+ i (car lens)) (cdr v) (cdr lens)))))))))

;u64vector-split: v is a u64vector, pts is a list of positions (ex: (list 0 5 14))
(define (u64vector-split v pts)
  (let ((len (u64vector-length v)))
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
	     (cons (u64vector-subseq v start end)
		   (u64vector-split v (cdr pts))))))))

;;; chop chop!
(define (u64vector-chop v blk-len)
  (define (chop acc v)
    (let ((len (u64vector-length v)))
      (cond ((<= len blk-len)
	     (reverse (cons v acc)))
	    (else
	     (chop (cons (u64vector-subseq v 0 (- blk-len 1)) acc)
		   (u64vector-subseq v blk-len))))))
  (cond ((< blk-len 1)
	 '())
	(else
	 (chop '() v))))

;;; chop overlap
(define (u64vector-chop-overlap v blk-len overlap)
  (define (chop acc v)
    (let ((len (u64vector-length v)))
      (cond ((<= len blk-len)
	     (reverse (cons v acc)))
	    (else
	     (chop (cons (u64vector-subseq v 0 (- blk-len 1)) acc)
		   (u64vector-subseq v (- blk-len overlap)))))))
  (cond ((< blk-len 1)
	 '())
	(else
	 (chop '() v))))


;;;;naive subsampling
;(define (u64vector-subsampling v rate)
;  (let* ((k (min 1. rate))
;	 (new-len (exact-ceiling (* (u64vector-length v) k)))
;	 (new-v   (make-u64vector new-len)))
;    (do ((i 0 (+ 1 i)))
;	((= i new-len) new-v)
;      (u64vector-set! new-v i (u64vector-ref v (exact-floor (/ i k)))))))

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;statistics
(define (u64vector-mean v) 
  (let ((len (max 1 (u64vector-length v))))
    (/ (u64vector-sum v) len)))

(define (u64vector-median v) 
  (median (u64vector->list v)))

;;; Note variance normalized with N, not N-1
(define (u64vector-variance v)
  (let ((x (u64vector-copy v))
	(len (u64vector-length v)))
    (/ (u64vector-sum-square (u64vector-offset! x (- (u64vector-mean x)))) len)))

;;; Note standard deviation normalized with N, not N-1
(define (u64vector-std v) (sqrt (u64vector-variance v)))
(define (u64vector-rms v) (u64vector-std v))

(define (u64vector-square! v) (u64vector-multiply! v v))
(define (u64vector-square  v)
  (let ((v-copy (u64vector-copy v)))
    (u64vector-square! v-copy)
    v-copy))

(define (u64vector-sqrt! v) (u64vector-map! sqrt v))
(define (u64vector-sqrt  v) 
  (let ((v-copy (u64vector-copy v)))
    (u64vector-sqrt! v-copy)
    v-copy))

(define (u64vector-abs! v) (u64vector-map! abs v))
(define (u64vector-abs  v) 
  (let ((v-copy (u64vector-copy v)))
    (u64vector-abs! v-copy)
    v-copy))

(define (u64vector-inverse  v) (u64vector-map  inverse v))
(define (u64vector-inverse! v) (u64vector-map! inverse v))

(define (u64vector-fmodulo  v x) (u64vector-map  (lambda-fmodulo x) v))
(define (u64vector-fmodulo! v x) (u64vector-map! (lambda-fmodulo x) v))

(define (u64vector-round  v) (u64vector-map  round v))
(define (u64vector-round! v) (u64vector-map! round v))

(define (u64vector-pos-lin  v) (u64vector-map  pos-lin v))
(define (u64vector-pos-lin! v) (u64vector-map! pos-lin v))

(define (u64vector-correlation v1 v2)
  (correlation-coeff (u64vector->list v1) (u64vector->list v2)))

;;product of all terms in u64vector
(define (u64vector-prod v)
  (let ((len  (u64vector-length v))
	(prod  1.0))
    (do ((i 0 (+ 1 i)))
	((= i len) prod)
      (set! prod (u64vector* prod (u64vector-ref v i))))))

;;;fastest version when compiled with '(not safe)
(define (u64vector-sum v)
  (let ((len (u64vector-length v))
	(sum 0.0))
    (do ((i 0 (+ 1 i)))
	((= i len) sum)
      (set! sum (u64vector+ sum (u64vector-ref v i))))))

(define (u64vector-normalize! v) (u64vector-scale! v (/ 1. (u64vector-sum v))))
(define (u64vector-normalize v)  (u64vector-normalize! (u64vector-copy v)))

(define (u64vector-normalize-max! v) (u64vector-scale! v (/ 1. (cadr (u64vector-max v)))))
(define (u64vector-normalize-max v)  (u64vector-normalize-max! (u64vector-copy v)))

(define (u64vector-norm v)  (sqrt (u64vector-sum (u64vector-square v))))
(define (u64vector-sum-square v)  (u64vector-sum  (u64vector-square v)))
(define (u64vector-mean-square v) (u64vector-mean (u64vector-square v)))
(define u64vector-energy u64vector-mean-square)

(define (u64vector-normalize-energy! v)
  (let ((energy (u64vector-energy v)))
    (u64vector-scale! v (/ 1. (sqrt energy)))))

(define (u64vector-normalize-energy v)
  (u64vector-normalize-energy! (u64vector-copy v)))

(define (u64vector-energy-diff v1 v2)
  (u64vector-energy (u64vector-subtract! (u64vector-copy v1) v2)))

(define (u64vector-normalized-energy-diff v1 v2)
  (u64vector-energy 
   (u64vector-subtract (u64vector-normalize-energy v1)
		 (u64vector-normalize-energy v2))))

(define (u64vector-power-compressor v threshold ratio)
  (let ((power (u64vector-energy v)))
    (cond ((< power threshold)
	   v)
	  (else
	   (let* ((new-power (+ threshold (* ratio (- power threshold))))
		  (coeff (sqrt (/ new-power power))))
	     (u64vector-scale v coeff))))))

;find 'most f' element according to f: (f (u64vector-ref v i) extreme)
(define (u64vector-extreme f v)
  (let ((extreme (u64vector-ref v 0))
	(pos 0)
	(len (u64vector-length v)))
    (do ((i 1 (+ 1 i)))
	((= i len))
      (cond ((f (u64vector-ref v i) extreme)
	     (set! extreme (u64vector-ref v i))
	     (set! pos i))))
    (list pos extreme)))

(define (u64vector-extreme-value f v)
  (let ((extreme (u64vector-ref v 0))
	(pos 0)
	(len (u64vector-length v)))
    (do ((i 1 (+ 1 i)))
	((= i len))
      (cond ((f (u64vector-ref v i) extreme)
	     (set! extreme (u64vector-ref v i)))))
    extreme))

(define (u64vector-max v) (u64vector-extreme > v))
(define (u64vector-min v) (u64vector-extreme < v))
(define (u64vector-max-value v) (u64vector-extreme-value > v))
(define (u64vector-min-value v) (u64vector-extreme-value < v))

(define (u64vector-max-abs v)
  (u64vector-extreme (lambda (x y)
		 (> (abs x) (abs y)))
	       v))

(define (u64vector-threshold! v thres)
  (let ((len (u64vector-length v)))
    (do ((i 1 (+ i 1)))
	((= i len) v)
      (if (< (u64vector-ref v i) thres)
	  (u64vector-set! v i 0.)  
	  (u64vector-set! v i 1.)))))

;;; trigonometric functions
(define (u64vector-sin!  v) (u64vector-map! sin v))
(define (u64vector-sin   v)
  (let ((v-copy (u64vector-copy v)))
    (u64vector-sin! v-copy)
    v-copy))

(define (u64vector-cos!  v) (u64vector-map! cos v))
(define (u64vector-cos   v)
  (let ((v-copy (u64vector-copy v)))
    (u64vector-cos! v-copy)
    v-copy))

(define (u64vector-tan!  v) (u64vector-map! tan v))
(define (u64vector-tan   v) (u64vector-map  tan v))
(define (u64vector-atan! v) (u64vector-map! atan v))
(define (u64vector-atan  v) (u64vector-map  atan v))

(define (u64vector-log   v) (u64vector-map log v))
(define (u64vector-log10 v) (u64vector-map log10 v))

(define (u64vector-invert v)
  (u64vector-map (lambda (x) (- x)) v))

;;; naive u64vector-reverse
(define (u64vector-reverse v)
  (list->u64vector (reverse (u64vector->list v))))

;;; u64vector convolution
(define (u64vector-conv v1 v2)
  (let* ((len1 (u64vector-length v1))
	 (len2 (u64vector-length v2))
	 (len3 (+ len1 len2 -1))
	 (vc1  (u64vector-append (u64vector-zeros (- len2 1))
			   v1))
	 (vc2  (u64vector-reverse (u64vector-copy v2)))
	 (vc3  (u64vector-zeros len3)))
    (let loop ((i 0))
      (cond ((>= i len3)
	     vc3)
	    (else
	     (u64vector-set! vc3
		       i
		       (u64vector-sum
			(u64vector-multiply!
			 (u64vector-copy vc2)
			 (u64vector-subseq vc1
				     i (+ i (- len2 1))))))
	     (loop (1+ i)))))))

;;; speed up u64vector-convolution with a step > 1
(define (u64vector-conv-step v1 v2 step)
  (let* ((len1 (u64vector-length v1))
	 (len2 (u64vector-length v2))
	 (len3 (exact-ceiling (/ (+ len1 len2 -1) step)))
	 (vc1  (u64vector-append (u64vector-zeros (- len2 1))
			   v1))
	 (vc2 (u64vector-reverse (u64vector-copy v2)))
	 (vc3 (u64vector-zeros len3)))
    (let loop ((i 0))
      (cond ((>= i len3)
	     vc3)
	    (else
	     (u64vector-set! vc3
		       i
		       (u64vector-sum
			(u64vector-multiply!
			 (u64vector-copy vc2)
			 (u64vector-subseq vc1
				     (* i step) (+ (* i step) (- len2 1))))))
	     (loop (+ i 1)))))))

(define (u64vector-autocorrelation v)
  (u64vector-conv (u64vector-reverse v) v))

;;; higher order sliding function (TODO: f argument is a list: change
;;; that)
(define (u64vector-sliding-f f v width)
  (let* ((len     (u64vector-length v))
	 (new-len (- len width -1))
	 (new-u64vector (make-u64vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-u64vector)
	    (else
	     (u64vector-set! new-u64vector i 
		       (f (u64vector->list (u64vector-subseq v i (+ i width -1)))))
	     (loop (+ i 1)))))))

;;; specialization
(define (u64vector-sliding-median v width)
  (let* ((len     (u64vector-length v))
	 (new-len (- len width -1))
	 (new-u64vector (make-u64vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-u64vector)
	    (else
	     (u64vector-set! new-u64vector i 
		       (u64vector-median (u64vector-subseq v i (+ i width -1))))
	     (loop (+ i 1)))))))

(define (u64vector-sliding-mean v width)
  (let* ((len     (u64vector-length v))
	 (new-len (- len width -1))
	 (new-u64vector (make-u64vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-u64vector)
	    (else
	     (u64vector-set! new-u64vector i 
		       (u64vector-mean (u64vector-subseq v i (+ i width -1))))
	     (loop (+ i 1)))))))


(define (u64vector-sliding-min v width)
  (u64vector-sliding-f (lambda (lst) (apply min lst))
		       v
		       width))

(define (u64vector-sliding-max v width)
  (u64vector-sliding-f (lambda (lst) (apply max lst))
		 v
		 width))
