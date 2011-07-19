;;; -*-scheme-*-
;;; vct.scm
;;;
;;; vctlib wrappers for BLAS level 1. Compatible with Gambit-C and
;;; Chicken.
;;; 
;;; Copyright 2006-2007 Pierre-Alexandre Fournier
;;; (fournier@carretechnologies.com)
;;; All rights reserved.
;;; 
;;; $Id: $

;;; Requirements:
;;; gambit:  (load "blas-gambit-ffi")
;;; chicken: (require blas)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Level 1 BLAS

;;; ROT: no wrapper

;;; SWAP: no wrapper

;;; SCAL
(define (vct-scale! v alpha)
  (let ((N    (f32vector-length v))
	(incX 1))
    (blas:sscal N (exact->inexact alpha) v incX)
    v))

;;; COPY
(define (vct-copy X)
  (let* ((N    (f32vector-length X))
	 (incX 1)
	 (Y    (make-f32vector N))
	 (incY 1))
    (blas:scopy N X incX Y incY)
    Y))

;;; AXPY: no wrapper

;;; DOT
(define (vct-product X Y)
  (let* ((NX     (f32vector-length X))
	 (NY     (f32vector-length Y))
	 (N      (min NX NY))
	 (Y-copy (vct-copy X))
	 (incX   1)
	 (incY   1))
    (blas:sdot N X incX Y-copy incY)))

;;; SUM (abs)
;(define (vct-sum X)
;  (let ((N    (f32vector-length X))
;	(incX 1))
;    (blas:sasum N X incX)))

;;; NRM2
(define (vct-norm X)
  (let ((N    (f32vector-length X))
	(incX 1))
    (blas:snrm2 N X incX)))

;;; AMAX
(define (vct-max-pos X)
  (let ((N    (f32vector-length X))
	(incX 1))
    (blas:isamax N X incX)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Level 2 BLAS

;;; Matrix-Vector multiplication y <- A x
(define (matrix-vct-multiply A x)
  (let* ((M      (matrix-rows    A))
	 (N      (matrix-columns A))
	 (A-flat (matrix-flatten A))
	 (lda    (max N 1))
	 (y      (make-f32vector M))
	 (alpha  1.0)
	 (beta   1.0)
	 (incx   1)
	 (incy   1))
    (blas:sgemv blas:RowMajor
		blas:NoTrans
		M
		N
		alpha
		A-flat
		lda
		x
		incx
		beta
		y
		incy)
    y))


;;; Matrix-Vector multiplication y <- A^-1 x


;;; Matrix Inverse


;;; Vector Tensors A <- x y^T


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Level 3 BLAS


;*  LDA    - INTEGER.
;*           On entry, LDA specifies the first dimension of A as declared
;*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
;*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
;*           least  max( 1, k ).
;*           Unchanged on exit.

;;; Matrix-Matrix multiplication: seems like there is a stability
;;; problem (20070611) We'll have to investigate more (see
;;; run-blas-benchmarks.scm). There is also a performance problem
;;; related to the conversion from matrix to vct and vice versa.
;;; vct-subseq (in vct->matrixize) takes 99% of the computation time.

;(define (matrix-product A B)
;  (let* ((M      (matrix-rows    A))
;	 (K      (matrix-rows    B))
;	 (N      (matrix-columns B))
;	 (A-flat (matrix-flatten A))
;	 (B-flat (matrix-flatten B))
;	 (C-flat (make-f32vector (* M N)))
;	 (lda    (max K 1))
;	 (ldb    (max N 1))
;	 (ldc    (max N 1))
;	 (alpha  1.0)
;	 (beta   1.0)
;	 (incx   1)
;	 (incy   1))
;    (blas:sgemm blas:RowMajor
;		blas:NoTrans
;		blas:NoTrans
;		M
;		N
;		K
;		alpha
;		A-flat
;		lda
;		B-flat
;		ldb
;		beta
;		C-flat
;		ldc)
;    (vct->matrixize C-flat M N)))
