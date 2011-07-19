;;; -*-scheme-*-
;;; vct.scm
;;;
;;; vct type definition for gambit scheme
;;; 
;;; Copyright 2005-2007 Pierre-Alexandre Fournier
;;; (fournier@carretechnologies.com)
;;; All rights reserved.
;;; 
;;; $Id: vct.scm,v 1.5 2006/04/24 05:14:53 paf Exp $

;;; (include "srfi-1.scm")
;;; (include "utils.scm")

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
;;; basic vector functions (vct floating point type
;;; abstraction). Change f32vector for f64vector in the next ~20 lines
;;; if you want 32 or 64 bits floating point numbers in for the vct
;;; type.
(define vct          f32vector)
(define vct?         f32vector?)
(define make-vct     make-f32vector)
(define vct-length   f32vector-length)

(define vct-ref      f32vector-ref)
(define vct-set!     f32vector-set!)

(define vct->list    f32vector->list)
(define list->vct    list->f32vector)
(define (vct->vector v) 
  (list->vector (vct->list v)))
(define (vector->vct v) 
  (list->f32vector (map exact->inexact (vector->list v))))

(define vct+ ##flonum.+)
(define vct- ##flonum.-)
(define vct* ##flonum.*)
(define vct/ ##flonum./)

;;; copy
(define vct-copy f32vector-copy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; non-mutable set
(define (vct-set v i new-value)
  (let ((new-v (vct-copy v)))
    (vct-set! new-v i new-value)
    new-v))

;;; swaps two elements
(define (vct-swap! v i j)
  (let ((tmp (vct-ref v i)))
    (vct-set! v i (vct-ref v j))
    (vct-set! v j tmp)))

(define (vct-swap v i j)
  (let ((new-v (vct-copy v)))
    (vct-swap! new-v i j)
    new-v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; map for one or two vcts
(define (vct-map f . args)
  (let* ((v1 (vct-copy (car args)))
	 (v2 (if (length> args 1) (cadr args) #f)))
    (if v2
	(vct-map! f v1 v2)
	(vct-map! f v1))))

;;; mutating map for one or two vcts
(define (vct-map! f . args)
  (let* ((v1    (car args))
	 (v2    (if (length> args 1) (cadr args) #f))
	 (len   (vct-length v1)))
    (if v2  ;;check dimensions
	(let* ((len2 (vct-length v2)))
	  (if (not (= len len2))
	      (error "vct-map!: vcts v1 and v2 must have same length.\n"))))
    (let loop ((i 0))
      (cond ((< i len)
	     (vct-set! v1 i 
		       (if v2
			   (f (vct-ref v1 i) 
			      (vct-ref v2 i))
			   (f (vct-ref v1 i))))
	     (loop (+ i 1)))
	    (else
	     v1)))))

(define (vct-foldr f init v)
  (let ((len (vct-length v)))
    (cond ((< len 1)
	   #f) ;should report error here
	  (else
	   (let loop ((i 0)
		      (acc init))
	     (cond ((= i len)
		    acc)
		   (else
		    (loop (+ i 1)
			  (f acc (vct-ref v i))))))))))
		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vct-subseq does right-side zero-padding 
(define (vct-subseq v start . end)
  (let* ((len      (vct-length v))
	 (start    (max start 0))
	 (end      (if (not (null? end)) 
		       (car end) 
		       (- (vct-length v) 1)))
	 (new-len  (- end start -1))
	 (max-iter (min new-len (- len start))))
    (cond ((> new-len 0)
	   (let ((new-v (make-vct new-len 0.)))
	     (if (< max-iter 1)
		 new-v
		 (do ((i 0 (+ 1 i)))
		     ((= i max-iter) new-v)
		   (vct-set! new-v i (vct-ref v (+ i start)))))))
	  (else
	   (vct)))))

;;; vct-subseq-right-zp does right-side zero-padding on the right too.
(define (vct-subseq-right-zp v start . end)
  (let* ((len      (vct-length v))
	 (vstart   (max start 0))
	 (end      (if (not (null? end)) 
		       (car end)
		       (- len 1)))
	 (vend     (min end (- len 1)))
	 (new-len  (- end start -1))
	 (offset   (- start)))
    (cond ((> new-len 0)
	   (let ((new-v (make-vct new-len 0.)))
	     (if (< (- vend vstart) 1)
		 new-v
		 (do ((i vstart (+ 1 i)))
		     ((> i vend) new-v)
		   (vct-set! new-v (+ i offset)
			     (vct-ref v i))))))
	  (else
	   (vct)))))


(define (vct-zero-padding v new-len)
  (let ((len     (vct-length v)))
    (cond ((>= len new-len)
	   v)
	  (else
	   (let* ((pad-len (exact-floor (/ (- new-len len) 2)))
		  (new-vct (make-vct new-len)))
	     (vct-add-offset! new-vct v pad-len)
	     new-vct)))))

(define (vct-zero-padding-right v new-len)
  (let ((len     (vct-length v)))
    (cond ((>= len new-len)
	   v)
	  (else
	   (let* ((pad-len (- new-len len))
		  (new-vct (make-vct new-len)))
	     (vct-add! new-vct v)
	     new-vct)))))

(define (vct-one-each-n len n . offset)
  (let ((offset (if (null? offset) 0 (car offset)))
	(v      (make-vct len)))
    (let loop ((i offset)
	       (j 0))
      (cond ((>= i len)
	     v)
	    ((= (- n 1) (floor (modulo j n)))
	     (vct-set! v i 1.)
	     (loop (+ i 1)
		   (+ j 1)))
	    (else
	     (loop (+ i 1)
		   (+ j 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macro-vct-map!
(define-macro (macro-vct-map! f v)
  `(let* ((len (vct-length v)))
     (let loop ((i 0))
       (if (##fixnum.>= i len)
	   v
	   (begin
	     (vct-set! v i (,f (vct-ref v i)))
	     (loop (##fixnum.+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;scale
(define (vct-scale! v k)
  (let ((k (exact->inexact k)))
    (macro-vct-map! (lambda (x) (vct* k x)) v)))

(define (vct-scale v k)
  (let ((v-copy (vct-copy v)))
    (vct-scale! v-copy k)
    v-copy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;offset
(define (vct-offset! v k)
  (let ((k (exact->inexact k)))
    (macro-vct-map! (lambda (x) (vct+ k x)) v)))

(define (vct-offset v k)
  (vct-offset! (vct-copy v) k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;vct-remove-mean
(define (vct-remove-mean! v)
  (vct-offset! v (- (vct-mean v))))

(define (vct-remove-mean  v)
  (vct-offset  v (- (vct-mean v))))

;;;vct-do accepts vcts of different length, returns a vct of length
;;;(vct-length v1) OBSOLETE 20070201: use vct-do below instead
;(define (vct-do f v1 v2 . offset)
;  (let* ((offset (if (null? offset) 0 (car offset)))
;	 (len1 (- (vct-length v1) offset))
;	 (len2 (vct-length v2))
;	 (len  (min len1 len2))
;	 (new-v (vct-copy v1)))
;    (let loop ((i 0))
;      (if (= i len)
;	  new-v
;	  (begin
;	    (vct-set! new-v (+ i offset) (f (vct-ref v1 (+ i offset)) 
;					    (vct-ref v2 i)))
;	    (loop (+ i 1)))))))
;
;(define (vct-do! f v1 v2 . offset)
;  (let* ((offset (if (null? offset) 0 (car offset)))
;	 (len1 (- (vct-length v1) offset))
;	 (len2 (vct-length v2))
;	 (len  (min len1 len2)))
;    (let loop ((i 0))
;      (if (= i len)
;	  v1
;	  (begin
;	    (vct-set! v1 (+ i offset) (f (vct-ref v1 (+ i offset)) 
;					 (vct-ref v2 i)))
;	    (loop (+ i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on two vectors 
(define (vct-do! f v1 v2)
  (let* ((len (min (vct-length v1) (vct-length v2))))
    (let loop ((i 0))
      (cond ((##fixnum.>= i len)
	     v1)
	    (else
	     (vct-set! v1 i (f (vct-ref v1 i) (vct-ref v2 i)))
	     (loop (##fixnum.+ i 1)))))))

(define (vct-do f v1 v2)
  (let ((v1-copy (vct-copy v1)))
    (vct-do! f v1 v2)))

;;; with position offset
(define (vct-do-offset! f v1 v2 offset)
  (let* ((len1 (- (vct-length v1) offset))
	 (len2 (vct-length v2))
	 (len  (min len1 len2))
	 (offset (inexact->exact offset)))
    (let loop ((i 0))
      (if (>= i len)
	  v1
	  (let ((i1 (##fixnum.+ i offset)))
	    (vct-set! v1 i1 (f (vct-ref v1 i1) 
				(vct-ref v2 i)))
	    (loop (##fixnum.+ i 1)))))))

(define (vct-do-offset f v1 v2 offset)
  (let ((v1-copy (vct-copy v1)))
    (vct-do-offset! f v1 v2 offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vct-add
(define (vct-add! v1 v2) (vct-do! vct+ v1 v2))
(define (vct-add  v1 v2) (vct-add! (vct-copy v1) v2))

;;; vct-add with position offset
(define (vct-add-offset! v1 v2 offset)  
  (vct-do-offset! vct+ v1 v2 offset))
(define (vct-add-offset  v1 v2 offset) 
  (vct-add-offset! (vct-copy v1) v2 offset))

;;; add many vcts, no offset
(define (vcts-add! v1 . vcts)
  (let loop ((vcts vcts))
    (cond ((null? vcts)
	   v1)
	  (else
	   (vcts-add! (vct-add! v1 (car vcts)))
	   (loop (cdr vcts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vct-subtract
(define (vct-subtract! v1 v2) (vct-do! vct- v1 v2))
(define (vct-subtract  v1 v2) (vct-subtract! (vct-copy v1) v2))

;;; vct-subtract with position offset
(define (vct-subtract-offset! v1 v2 offset)  
  (vct-do-offset! vct- v1 v2 offset))
(define (vct-subtract-offset  v1 v2 offset) 
  (vct-subtract-offset! (vct-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vct-multiply
(define (vct-multiply! v1 v2) (vct-do! vct* v1 v2))
(define (vct-multiply  v1 v2) (vct-multiply! (vct-copy v1) v2))

;;; vct-multiply with position offset
(define (vct-multiply-offset! v1 v2 offset)  
  (vct-do-offset! vct* v1 v2 offset))
(define (vct-multiply-offset  v1 v2 offset) 
  (vct-multiply-offset! (vct-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Division
(define (vct-divide! v1 v2) (vct-do! vct/ v1 v2))
(define (vct-divide  v1 v2) (vct-divide! (vct-copy v1) v2))

(define (vct-divide-offset! v1 v2 . offset)
  (let ((offset (if (null? offset) 0 (car offset))))
    (vct-do-offset! vct/ v1 v2 offset)))
(define (vct-divide-offset  v1 v2 offset)
  (vct-divide-offset! (vct-copy v1) v2 offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scalar product
(define (vct-product v1 v2)
  (vct-sum (vct-multiply! (vct-copy v1) v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; replicate vct n times
(define (vct-replicate v n)
  (let loop ((i 1)
	     (new-v v))
    (cond ((< i n)
	   (loop (+ i 1)
		 (vct-append new-v v)))
	  (else
	   new-v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; progressive sum of a vct
(define (vct-progressive-sum v)
  (let* ((len   (vct-length v))
	 (new-v (make-vct len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (vct-set! new-v i (vct-sum (vct-subseq v 0 i)))
	     (loop (+ i 1)))
	    (else
	     new-v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vct initialisation

;;; vct-fill with thunk
(define (vct-fill len thunk)
  (let ((v (make-vct len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (vct-set! v i (thunk))
	     (loop (+ i 1)))
	    (else
	     v)))))

(define (vct-zeros len) (make-vct len 0.))
(define (vct-ones  len) (make-vct len 1.))

;;; vct-iota (based on srfi-1's iota function)
(define (vct-iota . args)
  (list->vct
   (map exact->inexact
	(apply iota args))))

;;; add constant points at the beginning and end of vct
(define (vct-stretch v pad-len) 
  (let* ((len     (vct-length v))
	 (new-len (+ len (* 2 pad-len)))
	 (new-v (make-vct new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-v)
	    ((< i pad-len)
	     (vct-set! new-v i (vct-ref v 0))
	     (loop (+ i 1)))
	    ((>= i (+ len pad-len))
	     (vct-set! new-v i (vct-ref v (- len 1)))
	     (loop (+ i 1)))
	    (else
	     (vct-set! new-v i (vct-ref v (- i pad-len)))
	     (loop (+ i 1)))))))

;;; (vct-from-to 2 5) returns #f32(2. 3. 4. 5.)
(define (vct-from-to from to)
  (list->vct (map exact->inexact (from-to from to))))

;;; (vct-from-to 2 5 0.5) returns #f32(2. 2.5 3. 3.5 4. 4.5 5.)
(define (vct-from-to-step from to step)
  (list->vct (map exact->inexact (from-to-step from to step))))

;;; (vct-from-to-length -1 2 5) returns #f32(-1. -.25 .5 1.25 2.)
(define (vct-from-to-length from to len)
  (let ((v (vct-iota len)))
    (vct-offset (vct-scale v (/ (- to from) (- len 1))) from)))
	
;;; random reals between 0. and k
(define (vct-random len k)
  (vct-fill len (lambda () (* k (random-real)))))

;;; something that looks like gaussian noise
(define (vct-gauss len nrg-bidon)
  (let ((v (make-vct len 0.)))
    (do ((i 0 (1+ i)))
	((= i 10) v)
      (vct-add! v (vct-random len 1.)))
    (vct-offset! v (- (vct-mean v)))
    (vct-scale! v (sqrt (/ nrg-bidon (vct-energy v))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Append, split
;vct-append
(define (vct-append . v)
  (cond ((null? v)
	 #f)
	(else
	 (let* ((lens (map vct-length v))
		(total-len (sum lens))
		(new-v (make-vct total-len 0.)))
	   (let loop ((i 0) (v v) (lens lens))
	     (cond ((null? v)
		    new-v)
		   (else
		    (vct-add-offset! new-v (car v) i)
		    (loop (+ i (car lens)) (cdr v) (cdr lens)))))))))

;vct-split: v is a vct, pts is a list of positions (ex: (list 0 5 14))
(define (vct-split v pts)
  (let ((len (vct-length v)))
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
	     (cons (vct-subseq v start end)
		   (vct-split v (cdr pts))))))))

;;; chop chop!
(define (vct-chop v blk-len)
  (define (chop acc v)
    (let ((len (vct-length v)))
      (cond ((<= len blk-len)
	     (reverse (cons v acc)))
	    (else
	     (chop (cons (vct-subseq v 0 (- blk-len 1)) acc)
		   (vct-subseq v blk-len))))))
  (cond ((< blk-len 1)
	 '())
	(else
	 (chop '() v))))

;;; chop overlap
(define (vct-chop-overlap v blk-len overlap)
  (define (chop acc v)
    (let ((len (vct-length v)))
      (cond ((<= len blk-len)
	     (reverse (cons v acc)))
	    (else
	     (chop (cons (vct-subseq v 0 (- blk-len 1)) acc)
		   (vct-subseq v (- blk-len overlap)))))))
  (cond ((< blk-len 1)
	 '())
	(else
	 (chop '() v))))


;;;;naive subsampling
;(define (vct-subsampling v rate)
;  (let* ((k (min 1. rate))
;	 (new-len (exact-ceiling (* (vct-length v) k)))
;	 (new-v   (make-vct new-len)))
;    (do ((i 0 (+ 1 i)))
;	((= i new-len) new-v)
;      (vct-set! new-v i (vct-ref v (exact-floor (/ i k)))))))

    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;statistics
(define (vct-mean v) 
  (let ((len (max 1 (vct-length v))))
    (/ (vct-sum v) len)))

(define (vct-median v) 
  (median (vct->list v)))

;;; Note variance normalized with N, not N-1
(define (vct-variance v)
  (let ((x (vct-copy v))
	(len (vct-length v)))
    (/ (vct-sum-square (vct-offset! x (- (vct-mean x)))) len)))

;;; Note standard deviation normalized with N, not N-1
(define (vct-std v) (sqrt (vct-variance v)))
(define (vct-rms v) (vct-std v))

(define (vct-square! v) (vct-multiply! v v))
(define (vct-square  v)
  (let ((v-copy (vct-copy v)))
    (vct-square! v-copy)
    v-copy))

(define (vct-sqrt! v) (vct-map! sqrt v))
(define (vct-sqrt  v) 
  (let ((v-copy (vct-copy v)))
    (vct-sqrt! v-copy)
    v-copy))

(define (vct-abs! v) (vct-map! abs v))
(define (vct-abs  v) 
  (let ((v-copy (vct-copy v)))
    (vct-abs! v-copy)
    v-copy))

(define (vct-inverse  v) (vct-map  inverse v))
(define (vct-inverse! v) (vct-map! inverse v))

(define (vct-fmodulo  v x) (vct-map  (lambda-fmodulo x) v))
(define (vct-fmodulo! v x) (vct-map! (lambda-fmodulo x) v))

(define (vct-round  v) (vct-map  round v))
(define (vct-round! v) (vct-map! round v))

(define (vct-pos-lin  v) (vct-map  pos-lin v))
(define (vct-pos-lin! v) (vct-map! pos-lin v))

(define (vct-correlation v1 v2)
  (correlation-coeff (vct->list v1) (vct->list v2)))

;;product of all terms in vct
(define (vct-prod v)
  (let ((len  (vct-length v))
	(prod  1.0))
    (do ((i 0 (+ 1 i)))
	((= i len) prod)
      (set! prod (vct* prod (vct-ref v i))))))

;;;fastest version when compiled with '(not safe)
(define (vct-sum v)
  (let ((len (vct-length v))
	(sum 0.0))
    (do ((i 0 (+ 1 i)))
	((= i len) sum)
      (set! sum (vct+ sum (vct-ref v i))))))

(define (vct-normalize! v) (vct-scale! v (/ 1. (vct-sum v))))
(define (vct-normalize v)  (vct-normalize! (vct-copy v)))

(define (vct-normalize-max! v) (vct-scale! v (/ 1. (cadr (vct-max v)))))
(define (vct-normalize-max v)  (vct-normalize-max! (vct-copy v)))

(define (vct-norm v)  (sqrt (vct-sum (vct-square v))))
(define (vct-sum-square v)  (vct-sum  (vct-square v)))
(define (vct-mean-square v) (vct-mean (vct-square v)))
(define vct-energy vct-mean-square)

(define (vct-normalize-energy! v)
  (let ((energy (vct-energy v)))
    (vct-scale! v (/ 1. (sqrt energy)))))

(define (vct-normalize-energy v)
  (vct-normalize-energy! (vct-copy v)))

(define (vct-normalize-energy-safe! v)
  (let ((energy (vct-energy v)))
    (if (> energy 0.)
	(vct-scale! v (/ 1. (sqrt energy)))
	v)))

(define (vct-normalize-energy-safe v)
  (vct-normalize-energy-safe! (vct-copy v)))

(define (vct-energy-diff v1 v2)
  (vct-energy (vct-subtract! (vct-copy v1) v2)))

(define (vct-normalized-energy-diff v1 v2)
  (vct-energy 
   (vct-subtract (vct-normalize-energy v1)
		 (vct-normalize-energy v2))))

(define (vct-normalized-energy-diff-safe v1 v2)
  (vct-energy 
   (vct-subtract (vct-normalize-energy-safe v1)
		 (vct-normalize-energy-safe v2))))

(define (vct-power-compressor v threshold ratio)
  (let ((power (vct-energy v)))
    (cond ((< power threshold)
	   v)
	  (else
	   (let* ((new-power (+ threshold (* ratio (- power threshold))))
		  (coeff (sqrt (/ new-power power))))
	     (vct-scale v coeff))))))

;find 'most f' element according to f: (f (vct-ref v i) extreme)
(define (vct-extreme f v)
  (let ((extreme (vct-ref v 0))
	(pos 0)
	(len (vct-length v)))
    (do ((i 1 (+ 1 i)))
	((= i len))
      (cond ((f (vct-ref v i) extreme)
	     (set! extreme (vct-ref v i))
	     (set! pos i))))
    (list pos extreme)))

(define (vct-extreme-value f v)
  (let ((extreme (vct-ref v 0))
	(pos 0)
	(len (vct-length v)))
    (do ((i 1 (+ 1 i)))
	((= i len))
      (cond ((f (vct-ref v i) extreme)
	     (set! extreme (vct-ref v i)))))
    extreme))

(define (vct-max v) (vct-extreme > v))
(define (vct-min v) (vct-extreme < v))
(define (vct-max-value v) (vct-extreme-value > v))
(define (vct-min-value v) (vct-extreme-value < v))

(define (vct-max-abs v)
  (vct-extreme (lambda (x y)
		 (> (abs x) (abs y)))
	       v))

(define (vct-threshold! v thres)
  (let ((len (vct-length v)))
    (do ((i 1 (+ i 1)))
	((= i len) v)
      (if (< (vct-ref v i) thres)
	  (vct-set! v i 0.)  
	  (vct-set! v i 1.)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; trigonometric functions
(define (vct-sin!  v) (vct-map! sin v))
(define (vct-sin   v)
  (let ((v-copy (vct-copy v)))
    (vct-sin! v-copy)
    v-copy))

(define (vct-cos!  v) (vct-map! cos v))
(define (vct-cos   v)
  (let ((v-copy (vct-copy v)))
    (vct-cos! v-copy)
    v-copy))

(define (vct-tan!  v) (vct-map! tan v))
(define (vct-tan   v) (vct-map  tan v))
(define (vct-atan! v) (vct-map! atan v))
(define (vct-atan  v) (vct-map  atan v))

(define (vct-log   v) (vct-map log v))
(define (vct-log10 v) (vct-map log10 v))

(define (vct-invert v)
  (vct-map (lambda (x) (- x)) v))

;;; naive vct-reverse
(define (vct-reverse v)
  (list->vct (reverse (vct->list v))))

;;; vct convolution
(define (vct-conv v1 v2)
  (let* ((len1 (vct-length v1))
	 (len2 (vct-length v2))
	 (len3 (+ len1 len2 -1))
	 (vc1  (vct-append (vct-zeros (- len2 1))
			   v1))
	 (vc2  (vct-reverse (vct-copy v2)))
	 (vc3  (vct-zeros len3)))
    (let loop ((i 0))
      (cond ((>= i len3)
	     vc3)
	    (else
	     (vct-set! vc3
		       i
		       (vct-sum
			(vct-multiply!
			 (vct-copy vc2)
			 (vct-subseq vc1
				     i (+ i (- len2 1))))))
	     (loop (1+ i)))))))

;;; speed up vct-convolution with a step > 1
(define (vct-conv-step v1 v2 step)
  (let* ((len1 (vct-length v1))
	 (len2 (vct-length v2))
	 (len3 (exact-ceiling (/ (+ len1 len2 -1) step)))
	 (vc1  (vct-append (vct-zeros (- len2 1))
			   v1))
	 (vc2 (vct-reverse (vct-copy v2)))
	 (vc3 (vct-zeros len3)))
    (let loop ((i 0))
      (cond ((>= i len3)
	     vc3)
	    (else
	     (vct-set! vc3
		       i
		       (vct-sum
			(vct-multiply!
			 (vct-copy vc2)
			 (vct-subseq vc1
				     (* i step) (+ (* i step) (- len2 1))))))
	     (loop (+ i 1)))))))

(define (vct-autocorrelation v)
  (vct-conv (vct-reverse v) v))

;;; vct*function convolution (function f2 is (f2 i) -> vct)
(define (vct-f-conv v1 f2)
  (let* ((len (vct-length v1))
	 (vout (make-vct len)))
    (let loop ((i 0))
      (cond ((>= i len)
	     vout)
	    (else
	     (let* ((v2     (f2 i)) ;; convolution function
		    (len2   (vct-length v2))
		    (start  (- i (exact-floor (/ len2 2))))
		    (end    (+ start len2 -1))
		    (value  (vct-sum
			     (vct-multiply
			      v2
			      (vct-subseq-right-zp v1 start end))))) ;; v1 around i
	       (vct-set! vout
			 i
			 value)
	       (loop (1+ i))))))))
	    


;;; higher order sliding function (TODO: f argument is a list: change
;;; that)
(define (vct-sliding-f f v width)
  (let* ((len     (vct-length v))
	 (new-len (- len width -1))
	 (new-vct (make-vct new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-vct)
	    (else
	     (vct-set! new-vct i 
		       (f (vct->list (vct-subseq v i (+ i width -1)))))
	     (loop (+ i 1)))))))

;;; specialization
(define (vct-sliding-median v width)
  (let* ((len     (vct-length v))
	 (new-len (- len width -1))
	 (new-vct (make-vct new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-vct)
	    (else
	     (vct-set! new-vct i 
		       (vct-median (vct-subseq v i (+ i width -1))))
	     (loop (+ i 1)))))))

(define (vct-sliding-mean v width)
  (let* ((len     (vct-length v))
	 (new-len (- len width -1))
	 (new-vct (make-vct new-len)))
    (let loop ((i 0))
      (cond ((>= i new-len)
	     new-vct)
	    (else
	     (vct-set! new-vct i 
		       (vct-mean (vct-subseq v i (+ i width -1))))
	     (loop (+ i 1)))))))


(define (vct-sliding-min v width)
  (vct-sliding-f (lambda (lst) (apply min lst))
		 v
		 width))

(define (vct-sliding-max v width)
  (vct-sliding-f (lambda (lst) (apply max lst))
		 v
		 width))

;;;find n maxima in vct v
(define (find-n-max v n)
  (let ((minimum (cadr (vct-min v)))
	(v2 (vct-copy v)))
    (let loop ((i n)
	       (max-lst '()))
      (cond ((< i 1)
	     (reverse max-lst))
	    (else
	     (let ((max-pair (vct-max v2)))
	       (vct-set! v2 (car max-pair) minimum)
	       (loop (- i 1)
		     (cons max-pair
			   max-lst))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File utilities (uses Guillaume Germain's wave.scm)
(define (file->vct filename . args)
  (cond ((null? args)
	 (wave-data (read-wave-file filename)))
	(else
	 (wave-data (read-wave-file filename (car args))))))

(define (vct->file vct filename)
  (let ((wave (vct->wave vct)))
    (write-wave-file wave filename)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; u8vector functions: I use those for ulaw and A-law encoding

(define (u8vector-iota len)
  (list->u8vector
   (map (lambda (x) (modulo x 256))
	(iota len))))

;;;write a (u8vector) in a file
(define (u8vector->file u8v filename)
  (let ((file (open-output-file filename)))
    (write-subu8vector u8v 0 (u8vector-length u8v) file)
    (close-port file)))

;;;reads a (u8vector) from a file
(define (file->u8vector filename)
  (let* ((len  (file-size filename))
	 (file (open-input-file filename))
	 (u8v  (make-u8vector len)))
    (read-subu8vector u8v 0 (- len 1) file)
    (close-port file)
    u8v))

(define (u8vector-subseq v start . end)
  (let* ((len      (u8vector-length v))
	 (start    (max start 0))
	 (end      (if (not (null? end)) 
		       (car end) 
		       (- (u8vector-length v) 1)))
	 (new-len  (- end start -1))
	 (max-iter (min new-len (- len start))))
    (cond ((> new-len 0)
	   (let ((new-v (make-u8vector new-len 0)))
	     (if (< max-iter 1)
		 new-v
		 (do ((i 0 (+ 1 i)))
		     ((= i max-iter) new-v)
		   (u8vector-set! new-v i (u8vector-ref v (+ i start)))))))
	  (else
	   (u8vector)))))


(define (simple-u8vector->string u8v)
  (list->string 
   (map byte->char
	(u8vector->list u8v))))

;;;optimized for small memory usage
(define (u8vector->string u8v . args)
  (let* ((len (if (length> args 0) (car args) (u8vector-length u8v)))
	 (max-u8read 1000)
	 (n-loop1 (exact-ceiling (/ len max-u8read)))
	 (n-loop2 (modulo len max-u8read)))
    (let loop1 ((i1 0) 
		(str ""))
      (cond ((>= i1 n-loop1)
	     str)
	    (else
	     (let* ((start2 (* i1 max-u8read))
		    (end2   (min (+ start2 max-u8read) len)))
	       (let loop2 ((i2 start2)
			   (lst '()))
		 (cond ((= end2 i2)
			(loop1 (+ i1 1)
			       (string-append (list->string (reverse lst))
					      str)))
		       (else
			(loop2 (+ i2 1)
			       (cons (byte->char 
				      (u8vector-ref u8v i2))
				     lst)))))))))))

(define (u8vector->integer u8v)
  (let ((len (u8vector-length u8v)))
    (let loop ((i   0)
	       (sum 0))
      (cond ((< i len)
	     (loop (+ i 1)
		   (+ sum (arithmetic-shift (u8vector-ref u8v i) (* i 8)))))
	    (else sum)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; play sound on /dev/audio
;(define (play-vct v)
;  (play-ulaw (vct->ulaw v)))
;
;(define (play-ulaw u8v)
;  (let ((dsp "/dev/audio"))
;    (cond ((not (file-exists? dsp))
;	   (display "Audio output /dev/audio is not available.\n"))
;	  (else
;	   (let ((audio-output (open-output-file dsp))
;		 (len          (u8vector-length u8v)))
;	     (write-subu8vector u8v 0 len audio-output)
;	     (close-port audio-output))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some tests (sand-box section)
;(define va (vct 0 1 2 3 4 5 6 7 8 9))
;(define vb (vct 0 1 2 3 4 5 6 7 8 9))
;(define vc (vct 0 1 2))
;(vct-add! va vb 2)
;(vct-add! va vc)

