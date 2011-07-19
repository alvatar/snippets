;;; -*-scheme-*-
;;; matrix-ndim.scm
;;;
;;; n-dimensional matrix type definition for gambit
;;; 
;;; Copyright 2005-2007 Pierre-Alexandre Fournier
;;; (fournier@carretechnologies.com)
;;; All rights reserved.
;;;

;;; (include "../srfi/srfi-1.scm")
;;; (include "utils.scm")
;;; (include "f64vector.scm")
;;; (include "u32vector.scm")

;;; TODO: write macros so f64vector.scm can be expanded in any type of
;;; homogeneous vector, so we have u32vector.scm,
;;; u64vector.scm,. f32vector.scm, f64vector.scm, etc.


(define-type matrix
  id: c3baf372-53d2-47a7-ab86-18c2da2bc8d5 ;;;NEVER CHANGE THAT UNIQUE NUMBER!
  (dims  dims  dims!) ;;u32vector vector, repenser le nom matrix:dims ?
  (data  data  data!) ;;f64vector type data, repenser le nom matrix:data ?
  ;getter  (Brad Lucier's idea)
  ;setter 
  ;domain ?!? non.
  )

;;; fonctions already defined:
;;; (make-matrix n-dim dims data)
;;; (matrix? m)


; dims is a fixnum vector
(define (make-matrix dims . args)
  (cond ((or (< (u32vector-min dims) 1)
	     (not (u32vector? dims)))
	 (make-matrix dims #f))
	(else	 
	 (let* ((len  (u32vector-prod dims))
		(init (if (null? args) 0. (car args)))
		(data (make-f64vector len init)))
	   (make-matrix dims data)))))

(define (matrix-copy m)
  (make-matrix (u32vector-copy (dims m))
	       (f64vector-copy (data m))))

(define (matrix-ndims m)
  (u32vector-length (dims m)))

;;;dimensions are defined as #(rows columns dim3 dim4 ...)
(define (matrix-rows m)
  (if (< 0 (matrix-ndims m))
      (u32vector-ref (dims m) 0)
      (error (list *matrix-empty-dimensions* m))))

(define (matrix-columns m)
  (if (< 1 (matrix-ndims m))
      (u32vector-ref (dims m) 1)
      (error (list *matrix-one-dimension* m))))

(define (matrix-dimsize m dim)
  (if (< dim (matrix-ndims m))
      (u32vector-ref (dims m) dim)
      (error (list  *matrix-not-enough-dimensions* m))))

;;; coordinates conversion i j k, etc. -> position in f64vector
(define (matrix-pos m . args)
  (cond ((not (= (matrix-ndims m)
		 (length args)))
	 (error (list "matrix: matrix-pos needs " 
		      (number->string (matrix-ndims m))
		      " position arguments.\n")))
	(else
	 (let loop ((i   (cdr args))
		    (ni  (u32vector->list (dims m)))
		    (pos (car args)))
	   (cond ((null? i)
		  pos)
		 (else
		  (loop (cdr i)
			(cdr ni)
			(+ pos (* (car ni) (car i))))))))))

;;; matrix-ref
(define (matrix-ref m . args)
  (let ((pos (apply matrix-pos (cons m args))))
    (f64vector-ref (data m) pos)))

;;; matrix-set!
(define (matrix-set! m . args)
  (let* ((coordinates (take args (- (length args) 1)))
	 (new-value   (last args))
	 (pos (apply matrix-pos (cons m coordinates))))
    (f64vector-set! (data m) pos new-value)
    m))

;;; matrix-set (copy the structure)
(define (matrix-set m . args)
  (let ((new-matrix (matrix-copy m)))
    (apply matrix-set! (cons new-matrix args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special initialized matrixes: rethink if I add setters/getters to
;;; the structure.

;;; Identity
(define (matrix-I dims)
  (cond 
   ((not (= 2 (u32vector-length dims)))
    (error *error-matrix-not-square*))
   ((not (= (u32vector-ref dims 0) (u32vector-ref dims 1)))
    (error *error-matrix-not-square*))
   (else
    (let ((new-m (make-matrix dims)))
      (map (lambda (i)
	     (matrix-set! new-m i i 1.0))
	   (iota (u32vector-ref dims 0)))
      new-m))))

(define (matrix-zeros dims) (make-matrix dims))
(define (matrix-ones  dims) (make-matrix dims 1.))
;(define (matrix-iota  dims)

;;; Random matrixes
(define (matrix-random-real dims)
  (let ((new-m (make-matrix dims)))
    (matrix-map (lambda (x) (random-real)) new-m)))

(define (matrix-random-integer dims max)
  (let ((new-m (make-matrix dims)))
    (matrix-map (lambda (x) 
		  (exact->inexact (random-integer max))) 
		new-m)))

(define (matrix-random-binary dims)
  (matrix-random-integer dims 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; submatrixes

;;; matrix-slice: get a slice in dimension dim (a vector), position i. 
;(define (matrix-slice m dim i)
;  (let ((v-incr (u32vector-copy (dims m))))
;    (u32vector-set! v-incr dim 1)

;matrix-subseq ou submatrix ?
;matrix-subseq-set! ou submatrix-set! ?
;dériver matrix-row-set, matrix-columns-set à partir de subseq-set

;(define (matrix-randomize-rows m)
;(define (matrix-randomize-columns m)
;(define (matrix-randomize m)


;(define (matrix-scale! m k)  (matrix-map! (lambda (x) (* k x)) m))
;(define (matrix-scale  m k)  (matrix-scale! (matrix-copy m) k))
;(define (matrix-offset! m k) (matrix-map! (lambda (x) (+ k x)) m))
;(define (matrix-offset  m k) (matrix-offset! (matrix-copy m) k))
;(define (matrix-round! m k)  (matrix-map! (lambda (x) (round x)) m))
;(define (matrix-round  m k)  (matrix-round! (matrix-copy m) k))
;(define (matrix-floor! m k)  (matrix-map! (lambda (x) (floor x)) m))
;(define (matrix-floor  m k)  (matrix-round! (matrix-copy m) k))
;(define (matrix-ceiling! m k)  (matrix-map! (lambda (x) (ceiling x)) m))
;(define (matrix-ceiling  m k)  (matrix-round! (matrix-copy m) k))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; higher order basic functions (map, fold)

;;; margs is a list of matrixes
(define (matrix-map f . margs)
  (let ((nmat (length margs)))
    (cond ((< nmat 2)
	   (make-matrix (dims m)
			(f64vector-map f (data m))))
	  (else
	   (foldr (lambda (m1 m2)
		    (cond ((and (= (u32vector-length (dims m1))
				   (u32vector-length (dims m2)))
				(true-list?
				 (map = (u32vector->list (dims m1))
				      (u32vector->list (dims m2)))))
			   (make-matrix (dims m1)
					;;; f64vector-do is OBSOLETE!
					(f64vector-do f (data m1)
						(data m2))))
			  (else
			   (error *matrix-error-dimensions*))))
		  (car margs)
		  (cdr margs))))))

;;; a fold on all elements
(define (matrix-fold f init m)
  (f64vector-foldr f init (data m)))

;;; reduce on one dimension (gives a n-1 dimensional matrix) (dim is a
;;; vector).
;(define (matrix-fold-dim dim f init m)
;  (


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pretty printer

;;; TODO: add slices, rethink
(define (matrix-pp m)
  (define (matrix-2d-pp m)
    (cond 
     ((not (= 2 (matrix-ndims m)))
      (error "matrix should have 2 dimensions."))
     (else
      (let ((rows (matrix-rows m))
	    (cols (matrix-columns m)))
	(let loop ((i 0)
		   (j 0)
		   (str ""))
	  (cond 
	   ((= j cols)
	    (loop (+ i 1) 0 (string-append str "]\n")))
	   ((= i rows)
	    (display "#<matrix dims: ")
	    (pp (dims m))
	    (display "data:\n")
	    (display str))
	   ((= j 0)
	    (loop i (+ j 1) 
		  (string-append 
		   str "[" (number->string (matrix-ref m i j)))))
	   (else
	    (loop i (+ j 1) 
		  (string-append 
		   str " " (number->string (matrix-ref m i j)))))))))))

  (let ((ndims (matrix-ndims m)))
    (cond ((= ndims 0) (pp m))
	  ((= ndims 1) (pp m))
	  ((= ndims 2) (matrix-2d-pp m))
	  (else        (pp m)))))


;;; transpose 2d-matrixes
(define (matrix-2d-transpose m)
  (cond ((not (= 2 (matrix-ndims m)))
	 (error "matrix: matrix should have 2 dimensions.\n"))
	(else
	 (let ((new-m (make-matrix (u32vector-reverse (dims m))
				   (make-f64vector (f64vector-length (data m)))))
	       (rows (matrix-rows m))
	       (cols (matrix-columns m)))
	   (let loop ((i 0)
		      (j 0))
	     (cond 
	      ((= i rows)
	       new-m)
	      ((= j cols)
	       (loop (+ 1 i) 0))
	      (else
	       (matrix-set! new-m j i (matrix-ref m i j))
	       (loop i (+ 1 j)))))))))

;(define (matrix-trace m)

;;;TODO
;matrix-flip (directions)
;matrix-slice (:,i,:)
;matrix-fold (directions)
;matrix-product (*)
;matrix-inverse (-1)
;matrix-svd
;row-set, column-set, subseq, subseq-set

;;;folds:
;matrix-sum, prod, etc.
;matrix-mean, matrix-var/std

;binary operations: and, or, xor, rotate, shift, not

;;; operations on one matrix
(define (matrix-log m)  (matrix-map log m))
(define (matrix-sin m)  (matrix-map sin m))
(define (matrix-cos m)  (matrix-map cos m))
(define (matrix-tan m)  (matrix-map tan m))
(define (matrix-logsig m)  (matrix-map logsig m))

;;; operations on many matrix
(define (matrix-add . args)      (apply matrix-map + args))
(define (matrix-multiply . args) (apply matrix-map * args))
(define (matrix-subtract . args)
  (define (simple-matrix-subtract m1 m2) 
    (matrix-map - m1 m2))
  (cond ((null? (cdr args))
	 (matrix-map - (car args)))
	(else
	 (foldr simple-matrix-subtract (car args) (cdr args)))))

;;; misc: append

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error messages
(define *matrix-error-dimensions*
  "matrix: all matrixes must be the same dimensions.\n")
(define *matrix-empty-dimensions*
  "matrix: matrix does not have dimensions.\n")
(define *matrix-one-dimension*
  "matrix: matrix has only one dimension.\n")
(define  *matrix-not-enough-dimensions*
  "matrix: matrix does not have enough dimensions.\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-matrix macro
(define-macro (with-matrix expr)
  `(let (;(*      matrix-product)
	 (+      matrix-add)
	 (-      matrix-subtract)
	 ;(:      matrix-subseq)
	 (.*     matrix-multiply)
	 ;(*k     matrix-scale)
	 ;(+k     matrix-offset)
	 (log    matrix-log)
	 (logsig matrix-logsig)
	 ;(sum    matrix-sum)
	 ;(conv   matrix-conv)
	 (rows   matrix-rows)
	 (cols   matrix-columns)
	 ;(trans  matrix-transpose)
	 ;(mref   matrix-ref)
	 ;(msub   matrix-subseq)
	 (mset!  matrix-set!))
     ,expr))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tests

(define (matrix-test)
  (define m  (matrix (u32vector 2 3 4) 1.25))
  (define m2 (matrix (u32vector 2 3) 1.5))
  (define m2b (matrix-copy m2))
  (matrix-pp m2)
  (matrix-ndims m)
  (matrix-rows m)
  (matrix-columns m)
  (matrix-dimsize m 2)
  (matrix-pos m 0 1 2)
  (matrix-ref m 0 1 2)
  (matrix-set! m 0 1 2 2.5)
  (matrix-ref m 0 1 2)
  (matrix-set m 0 1 2 3.5)
  (matrix-ref m 0 1 2)
  (matrix-pp (matrix-map + m2 m2))
  (matrix-pp (matrix-map * m2 m2))
  )

(define (sizeof obj)
  (u8vector-length (object->u8vector obj)))
