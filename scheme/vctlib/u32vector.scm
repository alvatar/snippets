;;; -*-scheme-*-
;;; u32vector.scm
;;;
;;; u32vector functions for gambit scheme
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

;(define u32vector          u32vector)
;(define u32vector?         u32vector?)
;(define make-u32vector     make-u32vector)
;(define u32vector-length   u32vector-length)
;
;(define u32vector-ref      u32vector-ref)
;(define u32vector-set!     u32vector-set!)
;
;(define u32vector->list    u32vector->list)
;(define list->u32vector    list->u32vector)

(define (u32vector->vector v) 
  (list->vector (u32vector->list v)))
(define (vector->u32vector v) 
  (list->u32vector (map exact->inexact (vector->list v))))

(define u32vector+ ##fixnum.+)
(define u32vector- ##fixnum.-)
(define u32vector* ##fixnum.*)
(define u32vector/ /)

;;; copy
(define u32vector-copy u32vector-copy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; non-mutable set
(define (u32vector-set v i new-value)
  (let ((new-v (u32vector-copy v)))
    (u32vector-set! new-v i new-value)
    new-v))

;;; swaps two elements
(define (u32vector-swap! v i j)
  (let ((tmp (u32vector-ref v i)))
    (u32vector-set! v i (u32vector-ref v j))
    (u32vector-set! v j tmp)))

(define (u32vector-swap v i j)
  (let ((new-v (u32vector-copy v)))
    (u32vector-swap! new-v i j)
    new-v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; map for one or two u32vectors
(define (u32vector-map f . args)
  (let* ((v1 (u32vector-copy (car args)))
	 (v2 (if (length> args 1) (cadr args) #f)))
    (if v2
	(u32vector-map! f v1 v2)
	(u32vector-map! f v1))))

;;; mutating map for one or two u32vectors
(define (u32vector-map! f . args)
  (let* ((v1    (car args))
	 (v2    (if (length> args 1) (cadr args) #f))
	 (len   (u32vector-length v1)))
    (if v2  ;;check dimensions
	(let* ((len2 (u32vector-length v2)))
	  (if (not (= len len2))
	      (error "u32vector-map!: u32vectors v1 and v2 must have same length.\n"))))
    (let loop ((i 0))
      (cond ((< i len)
	     (u32vector-set! v1 i 
		       (if v2
			   (f (u32vector-ref v1 i) 
			      (u32vector-ref v2 i))
			   (f (u32vector-ref v1 i))))
	     (loop (+ i 1)))
	    (else
	     v1)))))

(define (u32vector-foldr f init v)
  (let ((len (u32vector-length v)))
    (cond ((< len 1)
	   #f) ;should report error here
	  (else
	   (let loop ((i 0)
		      (acc init))
	     (cond ((= i len)
		    acc)
		   (else
		    (loop (+ i 1)
			  (f acc (u32vector-ref v i))))))))))
		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; u32vector-subseq does right-side zero-padding 
(define (u32vector-subseq v start . end)
  (let* ((len      (u32vector-length v))
	 (start    (max start 0))
	 (end      (if (not (null? end)) 
		       (car end) 
		       (- (u32vector-length v) 1)))
	 (new-len  (- end start -1))
	 (max-iter (min new-len (- len start))))
    (cond ((> new-len 0)
	   (let ((new-v (make-u32vector new-len 0)))
	     (if (< max-iter 1)
		 new-v
		 (do ((i 0 (+ 1 i)))
		     ((= i max-iter) new-v)
		   (u32vector-set! new-v i (u32vector-ref v (+ i start)))))))
	  (else
	   (u32vector)))))

;;; NOT GOOD, BUGGY
;(define (u32vector-subseq v start . end)
;  (define (copy-in-place! v1 v2)
;    (let ((len (min (u32vector-length v1) (u32vector-length v2))))
;      (let loop ((i 0))
;	(cond ((>= i len)
;	       #t)
;	      (else
;	       (u32vector-set! v1 i
;			 (u32vector-ref v2 i))
;	       (loop (+ i 1)))))))
;
;  (let* ((len      (u32vector-length v))
;	 (start    (max start 0))
;	 (end      (if (not (null? end)) 
;		       (car end) 
;		       (u32vector-length v)))
;	 (new-len  (- end start -1))
;	 (max-iter (min new-len (- len start))))
;    (cond ((> new-len 0)
;	   (let ((new-v (make-u32vector new-len 0)))
;	     (copy-in-place! new-v (subu32vector v start (min (+ 1 end) (u32vector-length v))))
;	     new-v))
;	  (else
;	   (u32vector)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macro-u32vector-map!
(define-macro (macro-u32vector-map! f v)
  `(let* ((len (u32vector-length v)))
     (let loop ((i 0))
       (if (>= i len)
	   v
	   (begin
	     (u32vector-set! v i (,f (u32vector-ref v i)))
	     (loop (+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;scale
(define (u32vector-scale! v k)
  (macro-u32vector-map! (lambda (x) (exact-round (* k x))) v))

(define (u32vector-scale v k)
  (let ((v-copy (u32vector-copy v)))
    (u32vector-scale! v-copy k)
    v-copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;offset
(define (u32vector-offset! v k)
  (cond ((inexact? k)
	 (error "k should be exact."))
	(else
	 (macro-u32vector-map! (lambda (x) (u32vector+ k x)) v))))

(define (u32vector-offset v k)
  (u32vector-offset! (u32vector-copy v) k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;u32vector-remove-mean
(define (u32vector-remove-mean! v)
  (u32vector-offset! v (- (exact-round (u32vector-mean v)))))

(define (u32vector-remove-mean  v)
  (u32vector-offset  v (- (u32vector-mean v))))

;;;u32vector-do accepts u32vectors of different length, returns a u32vector of length
;;;(u32vector-length v1) OBSOLETE 20070201: use u32vector-do below instead
;(define (u32vector-do f v1 v2 . offset)
;  (let* ((offset (if (null? offset) 0 (car offset)))
;	 (len1 (- (u32vector-length v1) offset))
;	 (len2 (u32vector-length v2))
;	 (len  (min len1 len2))
;	 (new-v (u32vector-copy v1)))
;    (let loop ((i 0))
;      (if (= i len)
;	  new-v
;	  (begin
;	    (u32vector-set! new-v (+ i offset) (f (u32vector-ref v1 (+ i offset)) 
;					    (u32vector-ref v2 i)))
;	    (loop (+ i 1)))))))
;
;(define (u32vector-do! f v1 v2 . offset)
;  (let* ((offset (if (null? offset) 0 (car offset)))
;	 (len1 (- (u32vector-length v1) offset))
;	 (len2 (u32vector-length v2))
;	 (len  (min len1 len2)))
;    (let loop ((i 0))
;      (if (= i len)
;	  v1
;	  (begin
;	    (u32vector-set! v1 (+ i offset) (f (u32vector-ref v1 (+ i offset)) 
;					 (u32vector-ref v2 i)))
;	    (loop (+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on two vectors 
(define (u32vector-do! f v1 v2)
  (let* ((len (min (u32vector-length v1) (u32vector-length v2))))
    (let loop ((i 0))
      (cond ((>= i len)
	     v1)
	    (else
	     (u32vector-set! v1 i (f (u32vector-ref v1 i) (u32vector-ref v2 i)))
	     (loop (+ i 1)))))))

(define (u32vector-do f v1 v2)
  (let ((v1-copy (u32vector-copy v1)))
    (u32vector-do! f v1 v2)))

;;; with position offset
(define (u32vector-do-offset! f v1 v2 offset)
  (let* ((len1 (- (u32vector-length v1) offset))
	 (len2 (u32vector-length v2))
	 (len  (min len1 len2))
	 (offset (inexact->exact offset)))
    (let loop ((i 0))
      (if (>= i len)
	  v1
	  (let ((i1 (+ i offset)))
	    (u32vector-set! v1 i1 (f (u32vector-ref v1 i1) 
				(u32vector-ref v2 i)))
	    (loop (+ i 1)))))))

(define (u32vector-do-offset f v1 v2 offset)
  (let ((v1-copy (u32vector-copy v1)))
    (u32vector-do-offset! f v1 v2 offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; u32vector-add
(define (u32vector-add! v1 v2) (u32vector-do! u32vector+ v1 v2))
(define (u32vector-add  v1 v2) (u32vector-add! (u32vector-copy v1) v2))

;;; u32vector-add with position offset
(define (u32vector-add-offset! v1 v2 offset)  
  (u32vector-do-offset! u32vector+ v1 v2 offset))
(define (u32vector-add-offset  v1 v2 offset) 
  (u32vector-add-offset! (u32vector-copy v1) v2 offset))

;;; add many u32vectors, no offset
(define (u32vectors-add! v1 . u32vectors)
  (let loop ((u32vectors u32vectors))
    (cond ((null? u32vectors)
	   v1)
	  (else
	   (u32vectors-add! (u32vector-add! v1 (car u32vectors)))
	   (loop (cdr u32vectors))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; u32vector-subtract
(define (u32vector-subtract! v1 v2) (u32vector-do! u32vector- v1 v2))
(define (u32vector-subtract  v1 v2) (u32vector-subtract! (u32vector-copy v1) v2))

;;; u32vector-subtract with position offset
(define (u32vector-subtract-offset! v1 v2 offset)  
  (u32vector-do-offset! u32vector- v1 v2 offset))
(define (u32vector-subtract-offset  v1 v2 offset) 
  (u32vector-subtract-offset! (u32vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; u32vector-multiply
(define (u32vector-multiply! v1 v2) (u32vector-do! u32vector* v1 v2))
(define (u32vector-multiply  v1 v2) (u32vector-multiply! (u32vector-copy v1) v2))

;;; u32vector-multiply with position offset
(define (u32vector-multiply-offset! v1 v2 offset)  
  (u32vector-do-offset! u32vector* v1 v2 offset))
(define (u32vector-multiply-offset  v1 v2 offset) 
  (u32vector-multiply-offset! (u32vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Division
(define (u32vector-divide! v1 v2) (u32vector-do! u32vector/ v1 v2))
(define (u32vector-divide  v1 v2) (u32vector-divide! (u32vector-copy v1) v2))

(define (u32vector-divide-offset! v1 v2 . offset)
  (let ((offset (if (null? offset) 0 (car offset))))
    (u32vector-do-offset! u32vector/ v1 v2 offset)))
(define (u32vector-divide-offset  v1 v2 offset)
  (u32vector-divide-offset! (u32vector-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scalar product
(define (u32vector-product v1 v2)
  (u32vector-sum (u32vector-multiply! (u32vector-copy v1) v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replicate u32vector n times
(define (u32vector-replicate v n)
  (let loop ((i 1)
	     (new-v v))
    (cond ((< i n)
	   (loop (+ i 1)
		 (u32vector-append new-v v)))
	  (else
	   new-v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; progressive sum of a u32vector
(define (u32vector-progressive-sum v)
  (let* ((len   (u32vector-length v))
	 (new-v (make-u32vector len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (u32vector-set! new-v i (u32vector-sum (u32vector-subseq v 0 i)))
	     (loop (+ i 1)))
	    (else
	     new-v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; u32vector initialisation

;;; u32vector-fill with thunk
(define (u32vector-fill len thunk)
  (let ((v (make-u32vector len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (u32vector-set! v i (thunk))
	     (loop (+ i 1)))
	    (else
	     v)))))

(define (u32vector-zeros len) (make-u32vector len 0))
(define (u32vector-ones  len) (make-u32vector len 1))

;;; u32vector-iota (based on srfi-1's iota function)
(define (u32vector-iota . args)
  (list->u32vector
   (map exact->inexact
	(apply iota args))))

;;; add constant points at the beginning and end of u32vector
(define (u32vector-stretch v pad-len) 
  (let* ((len     (u32vector-length v))
	 (new-len (+ len (* 2 pad-len)))
	 (new-v (make-u32vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-v)
	    ((< i pad-len)
	     (u32vector-set! new-v i (u32vector-ref v 0))
	     (loop (+ i 1)))
	    ((>= i (+ len pad-len))
	     (u32vector-set! new-v i (u32vector-ref v (- len 1)))
	     (loop (+ i 1)))
	    (else
	     (u32vector-set! new-v i (u32vector-ref v (- i pad-len)))
	     (loop (+ i 1)))))))

;;; (u32vector-from-to 2 5) returns #u32(2 3 4 5)
(define (u32vector-from-to from to)
  (list->u32vector (map exact->inexact (from-to from to))))

;;; (u32vector-from-to-step 2 15 3) returns #u32(2 5 8 11 14)
(define (u32vector-from-to-step from to step)
  (cond ((any inexact? (list from to step))
	 (error "Arguments should be exact numbers."))
	(else
	 (list->u32vector (from-to-step from to step)))))

;;; (u32vector-from-to-length -1 2 5) returns #u32(-1. -.25 .5 1.25 2.)
;;; (define (u32vector-from-to-length from to len)
;;;   (let ((v (u32vector-iota len)))
;;;     (u32vector-offset (u32vector-scale v (/ (- to from) (- len 1))) from)))
	
;;; random reals between 0 and k
(define (u32vector-random len k)
  (u32vector-fill len (lambda () (random-integer k))))

;;; something that looks like gaussian noise
(define (u32vector-gauss len nrg-bidon)
  (let ((v (make-u32vector len 0)))
    (do ((i 0 (1+ i)))
	((= i 10) v)
      (u32vector-add! v (u32vector-random len nrg-bidon))
      (u32vector-offset! v (- (u32vector-mean v))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Append, split
;u32vector-append
(define (u32vector-append . v)
  (cond ((null? v)
	 #f)
	(else
	 (let* ((lens (map u32vector-length v))
		(total-len (sum lens))
		(new-v (make-u32vector total-len 0)))
	   (let loop ((i 0) (v v) (lens lens))
	     (cond ((null? v)
		    new-v)
		   (else
		    (u32vector-add-offset! new-v (car v) i)
		    (loop (+ i (car lens)) (cdr v) (cdr lens)))))))))

;u32vector-split: v is a u32vector, pts is a list of positions (ex: (list 0 5 14))
(define (u32vector-split v pts)
  (let ((len (u32vector-length v)))
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
	     (cons (u32vector-subseq v start end)
		   (u32vector-split v (cdr pts))))))))

;;; chop chop!
(define (u32vector-chop v blk-len)
  (define (chop acc v)
    (let ((len (u32vector-length v)))
      (cond ((<= len blk-len)
	     (reverse (cons v acc)))
	    (else
	     (chop (cons (u32vector-subseq v 0 (- blk-len 1)) acc)
		   (u32vector-subseq v blk-len))))))
  (cond ((< blk-len 1)
	 '())
	(else
	 (chop '() v))))

;;; chop overlap
(define (u32vector-chop-overlap v blk-len overlap)
  (define (chop acc v)
    (let ((len (u32vector-length v)))
      (cond ((<= len blk-len)
	     (reverse (cons v acc)))
	    (else
	     (chop (cons (u32vector-subseq v 0 (- blk-len 1)) acc)
		   (u32vector-subseq v (- blk-len overlap)))))))
  (cond ((< blk-len 1)
	 '())
	(else
	 (chop '() v))))


;;;;naive subsampling
;(define (u32vector-subsampling v rate)
;  (let* ((k (min 1 rate))
;	 (new-len (exact-ceiling (* (u32vector-length v) k)))
;	 (new-v   (make-u32vector new-len)))
;    (do ((i 0 (+ 1 i)))
;	((= i new-len) new-v)
;      (u32vector-set! new-v i (u32vector-ref v (exact-floor (/ i k)))))))

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;statistics
(define (u32vector-mean v) 
  (let ((len (max 1 (u32vector-length v))))
    (/ (u32vector-sum v) len)))

(define (u32vector-median v) 
  (median (u32vector->list v)))

;;; Note variance normalized with N, not N-1
(define (u32vector-variance v)
  (let ((x (u32vector-copy v))
	(len (u32vector-length v)))
    (/ (u32vector-sum-square (u32vector-offset! x (- (u32vector-mean x)))) len)))

;;; Note standard deviation normalized with N, not N-1
(define (u32vector-std v) (sqrt (u32vector-variance v)))
(define (u32vector-rms v) (u32vector-std v))

(define (u32vector-square! v) (u32vector-multiply! v v))
(define (u32vector-square  v)
  (let ((v-copy (u32vector-copy v)))
    (u32vector-square! v-copy)
    v-copy))

(define (u32vector-sqrt! v) (u32vector-map! sqrt v))
(define (u32vector-sqrt  v) 
  (let ((v-copy (u32vector-copy v)))
    (u32vector-sqrt! v-copy)
    v-copy))

(define (u32vector-abs! v) (u32vector-map! abs v))
(define (u32vector-abs  v) 
  (let ((v-copy (u32vector-copy v)))
    (u32vector-abs! v-copy)
    v-copy))

(define (u32vector-inverse  v) (u32vector-map  inverse v))
(define (u32vector-inverse! v) (u32vector-map! inverse v))

(define (u32vector-fmodulo  v x) (u32vector-map  (lambda-fmodulo x) v))
(define (u32vector-fmodulo! v x) (u32vector-map! (lambda-fmodulo x) v))

(define (u32vector-round  v) (u32vector-map  round v))
(define (u32vector-round! v) (u32vector-map! round v))

(define (u32vector-pos-lin  v) (u32vector-map  pos-lin v))
(define (u32vector-pos-lin! v) (u32vector-map! pos-lin v))

(define (u32vector-correlation v1 v2)
  (correlation-coeff (u32vector->list v1) (u32vector->list v2)))

;;product of all terms in u32vector
(define (u32vector-prod v)
  (let ((len  (u32vector-length v))
	(prod  1))
    (do ((i 0 (+ 1 i)))
	((= i len) prod)
      (set! prod (u32vector* prod (u32vector-ref v i))))))

;;;fastest version when compiled with '(not safe)
(define (u32vector-sum v)
  (let ((len (u32vector-length v))
	(sum 0))
    (do ((i 0 (+ 1 i)))
	((= i len) sum)
      (set! sum (u32vector+ sum (u32vector-ref v i))))))

(define (u32vector-normalize! v) (u32vector-scale! v (/ 1 (u32vector-sum v))))
(define (u32vector-normalize v)  (u32vector-normalize! (u32vector-copy v)))

(define (u32vector-normalize-max! v) (u32vector-scale! v (/ 1 (cadr (u32vector-max v)))))
(define (u32vector-normalize-max v)  (u32vector-normalize-max! (u32vector-copy v)))

(define (u32vector-norm v)  (sqrt (u32vector-sum (u32vector-square v))))
(define (u32vector-sum-square v)  (u32vector-sum  (u32vector-square v)))
(define (u32vector-mean-square v) (u32vector-mean (u32vector-square v)))
(define u32vector-energy u32vector-mean-square)

(define (u32vector-normalize-energy! v)
  (let ((energy (u32vector-energy v)))
    (u32vector-scale! v (/ 1. (sqrt energy)))))

(define (u32vector-normalize-energy v)
  (u32vector-normalize-energy! (u32vector-copy v)))

(define (u32vector-energy-diff v1 v2)
  (u32vector-energy (u32vector-subtract! (u32vector-copy v1) v2)))

(define (u32vector-normalized-energy-diff v1 v2)
  (u32vector-energy 
   (u32vector-subtract (u32vector-normalize-energy v1)
		 (u32vector-normalize-energy v2))))

(define (u32vector-power-compressor v threshold ratio)
  (let ((power (u32vector-energy v)))
    (cond ((< power threshold)
	   v)
	  (else
	   (let* ((new-power (+ threshold (* ratio (- power threshold))))
		  (coeff (sqrt (/ new-power power))))
	     (u32vector-scale v coeff))))))

;find 'most f' element according to f: (f (u32vector-ref v i) extreme)
(define (u32vector-extreme f v)
  (let ((extreme (u32vector-ref v 0))
	(pos 0)
	(len (u32vector-length v)))
    (do ((i 1 (+ 1 i)))
	((= i len))
      (cond ((f (u32vector-ref v i) extreme)
	     (set! extreme (u32vector-ref v i))
	     (set! pos i))))
    (list pos extreme)))

(define (u32vector-extreme-value f v)
  (let ((extreme (u32vector-ref v 0))
	(pos 0)
	(len (u32vector-length v)))
    (do ((i 1 (+ 1 i)))
	((= i len))
      (cond ((f (u32vector-ref v i) extreme)
	     (set! extreme (u32vector-ref v i)))))
    extreme))

(define (u32vector-max v) (u32vector-extreme-value > v))
(define (u32vector-min v) (u32vector-extreme-value < v))
(define (u32vector-max-value v) (u32vector-extreme-value > v))
(define (u32vector-min-value v) (u32vector-extreme-value < v))

(define (u32vector-max-abs v)
  (u32vector-extreme (lambda (x y)
		 (> (abs x) (abs y)))
	       v))

(define (u32vector-threshold! v thres)
  (let ((len (u32vector-length v)))
    (do ((i 1 (+ i 1)))
	((= i len) v)
      (if (< (u32vector-ref v i) thres)
	  (u32vector-set! v i 0.)  
	  (u32vector-set! v i 1.)))))

;;; trigonometric functions
(define (u32vector-sin!  v) (u32vector-map! sin v))
(define (u32vector-sin   v)
  (let ((v-copy (u32vector-copy v)))
    (u32vector-sin! v-copy)
    v-copy))

(define (u32vector-cos!  v) (u32vector-map! cos v))
(define (u32vector-cos   v)
  (let ((v-copy (u32vector-copy v)))
    (u32vector-cos! v-copy)
    v-copy))

(define (u32vector-tan!  v) (u32vector-map! tan v))
(define (u32vector-tan   v) (u32vector-map  tan v))
(define (u32vector-atan! v) (u32vector-map! atan v))
(define (u32vector-atan  v) (u32vector-map  atan v))

(define (u32vector-log   v) (u32vector-map log v))
(define (u32vector-log10 v) (u32vector-map log10 v))

(define (u32vector-invert v)
  (u32vector-map (lambda (x) (- x)) v))

;;; naive u32vector-reverse
(define (u32vector-reverse v)
  (list->u32vector (reverse (u32vector->list v))))

;;; u32vector convolution
(define (u32vector-conv v1 v2)
  (let* ((len1 (u32vector-length v1))
	 (len2 (u32vector-length v2))
	 (len3 (+ len1 len2 -1))
	 (vc1  (u32vector-append (u32vector-zeros (- len2 1))
			   v1))
	 (vc2  (u32vector-reverse (u32vector-copy v2)))
	 (vc3  (u32vector-zeros len3)))
    (let loop ((i 0))
      (cond ((>= i len3)
	     vc3)
	    (else
	     (u32vector-set! vc3
		       i
		       (u32vector-sum
			(u32vector-multiply!
			 (u32vector-copy vc2)
			 (u32vector-subseq vc1
				     i (+ i (- len2 1))))))
	     (loop (1+ i)))))))

;;; speed up u32vector-convolution with a step > 1
(define (u32vector-conv-step v1 v2 step)
  (let* ((len1 (u32vector-length v1))
	 (len2 (u32vector-length v2))
	 (len3 (exact-ceiling (/ (+ len1 len2 -1) step)))
	 (vc1  (u32vector-append (u32vector-zeros (- len2 1))
			   v1))
	 (vc2 (u32vector-reverse (u32vector-copy v2)))
	 (vc3 (u32vector-zeros len3)))
    (let loop ((i 0))
      (cond ((>= i len3)
	     vc3)
	    (else
	     (u32vector-set! vc3
		       i
		       (u32vector-sum
			(u32vector-multiply!
			 (u32vector-copy vc2)
			 (u32vector-subseq vc1
				     (* i step) (+ (* i step) (- len2 1))))))
	     (loop (+ i 1)))))))

(define (u32vector-autocorrelation v)
  (u32vector-conv (u32vector-reverse v) v))

;;; higher order sliding function (TODO: f argument is a list: change
;;; that)
(define (u32vector-sliding-f f v width)
  (let* ((len     (u32vector-length v))
	 (new-len (- len width -1))
	 (new-u32vector (make-u32vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-u32vector)
	    (else
	     (u32vector-set! new-u32vector i 
		       (f (u32vector->list (u32vector-subseq v i (+ i width -1)))))
	     (loop (+ i 1)))))))

;;; specialization
(define (u32vector-sliding-median v width)
  (let* ((len     (u32vector-length v))
	 (new-len (- len width -1))
	 (new-u32vector (make-u32vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-u32vector)
	    (else
	     (u32vector-set! new-u32vector i 
		       (u32vector-median (u32vector-subseq v i (+ i width -1))))
	     (loop (+ i 1)))))))

(define (u32vector-sliding-mean v width)
  (let* ((len     (u32vector-length v))
	 (new-len (- len width -1))
	 (new-u32vector (make-u32vector new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-u32vector)
	    (else
	     (u32vector-set! new-u32vector i 
		       (u32vector-mean (u32vector-subseq v i (+ i width -1))))
	     (loop (+ i 1)))))))


(define (u32vector-sliding-min v width)
  (u32vector-sliding-f (lambda (lst) (apply min lst))
		       v
		       width))

(define (u32vector-sliding-max v width)
  (u32vector-sliding-f (lambda (lst) (apply max lst))
		 v
		 width))
