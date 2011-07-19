;;; -*-scheme-*-
;;; matrix.scm
;;;
;;; matrix type definition for gambit
;;; 
;;; Copyright 2005-2007 Pierre-Alexandre Fournier
;;; (fournier@carretechnologies.com)
;;; All rights reserved.
;;; 

;;; (include "utils.scm")
;;; (include "vct.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matrix functions
;;; matrixes are one-dimensional homogeneous vectors (ex: f32vector, f64vector, etc.)

;;; first args is default matrix element (float)
(define (make-matrix r c . args)
  (cond ((or (< r 1) (< c 1))
	 #f)
	(else
	 (let ((m    (make-vector r))
	       (init (if (null? args) 0. (car args))))
	   (let loop ((i 0))
	     (cond ((< i r)
		    (vector-set! m i (make-vct c init))
		    (loop (+ i 1)))
		   (else
		    m)))))))

(define (matrix? m)
  (and (vector? m)
       (apply-and (map vct?    (vector->list m)))
       (apply= (map vct-length (vector->list m)))))

(define (matrix-copy m)
  (vector-map vct-copy m))

;;; returns a pair (rows . columns)
(define (matrix-size m)     
  (cons (matrix-rows m) (matrix-columns m)))

(define (matrix-ref m i j) 
  (vct-ref (vector-ref m i) j))

;;; set! element at row i and column j
(define (matrix-set! m i j x)
  (let ((row (vector-ref m i)))
    (vct-set! row j (exact->inexact x))))

;;set non mutant
(define (matrix-set m i j x)
  (let ((new-m (matrix-copy m)))
    (matrix-set! new-m i j x)
    new-m))

(define (matrix-rows m)    
  (vector-length m))

(define (matrix-columns m) 
  (vct-length (vector-ref m 0)))

(define (matrix-append . args)
  (cond ((apply= (map matrix-columns args))
	 (apply vector-append args))
	(else
	 (display "matrix-append: Matrixes should have the same number of columns.\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; type conversion
(define (matrix->vct m)
  (let ((rows    (matrix-rows m))
	(columns (matrix-columns m)))
    (cond ((= rows 1)
	   (vector-ref m 0))
	  ((= columns 1)
	   (vector-ref (matrix-transpose m) 0))
	  (else
	   (display "matrix->vct: Matrix must have only 1 row or only 1 column.\n")))))

(define (vct->matrix v)
  (let ((m (make-vector 1)))
    (vector-set! m 0 v)
    m))

(define (vct->matrixize v m n)
  (let* ((len-v (vct-length v))
	 (len-m (* m n))
	 (v-tmp (make-vct len-m)))
    (list->vector
     (vct-chop (vct-add v-tmp v) n))))

(define (list-vct->matrix lst)
  (list->vector lst))

(define (list-list->matrix lst)
  (if (apply= (map length lst))
      (list->vector (map list->vct lst))
      (display "list-list->matrix: all lists must be the same length.\n")))

(define (matrix->list-list m)
  (vector->list (vector-map vct->list m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init functions
(define (matrix-I size)
  (let ((m (make-matrix size size)))
    (let loop ((i 0))
      (cond ((< i size)
	     (matrix-set! m i i 1.)
	     (loop (+ i 1)))
	    (else
	     m)))))

(define matrix-zeros make-matrix)

(define (matrix-ones rows columns)
  (make-matrix rows columns 1.))

(define (matrix-iota rows . columns)
  (let* ((columns (if (null? columns) rows (car columns)))
	 (m (make-matrix rows columns)))
    (let loop-rows ((i 0))
      (cond ((< i rows)
	     (let loop-columns ((j 0))
	       (cond ((< j columns)
		      (matrix-set! m i j (exact->inexact (+ i j)))
		      (loop-columns (+ j 1)))
		     (else
		      (loop-rows (+ i 1))))))
	    (else
	     m)))))

(define (matrix-fill rows columns proc)
  (let ((m (make-matrix rows columns)))
    (let loop-rows ((i 0))
      (cond ((< i rows)
	     (let loop-columns ((j 0))
	       (cond ((< j columns)
		      (matrix-set! m i j (proc))
		      (loop-columns (+ j 1)))
		     (else
		      (loop-rows (+ i 1))))))
	    (else
	     m)))))

(define (matrix-random-real rows columns)
  (matrix-fill rows columns random-real))
  
;note: not real integers, rounded floats because of vct type
(define (matrix-random-integer rows columns max)
  (matrix-fill rows columns 
	       (lambda () (exact->inexact (random-integer max)))))

(define (matrix-random-binary rows columns)
  (matrix-random-integer rows columns 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; row/column functions
(define (matrix-row m i) 
  (vector (vector-ref m i)))

(define (matrix-row-set m i v) 
  (let ((columns (matrix-columns m))
	(len     (vct-length v)))
    (cond ((= columns len) 
	   (let ((new-m (matrix-copy m)))
	     (if (< i (matrix-rows m))
		 (vector-set! new-m i v))
	     new-m))
	  (else
	   (display "matrix-row-set: vct v is not of proper length.\n")))))

(define (matrix-row-set! m i v) 
  (let ((columns (matrix-columns m))
	(len     (vct-length v)))
    (cond ((= columns len) 
	   (if (< i (matrix-rows m))
	       (vector-set! m i v))
	   m)
	  (else
	   (display "matrix-row-set!: vct v is not of proper length.\n")))))

(define (matrix-column m j)
  (let* ((len   (matrix-rows m))
	 (new-m (make-vector len)))
    (let loop ((i 0))
      (cond ((< i len)	     
	     (vector-set! new-m i (vct (matrix-ref m i j)))
	     (loop (+ i 1)))
	    (else
	     new-m)))))

(define (matrix-column-set m j v)
  (let ((rows (matrix-rows m))
	(len  (vct-length v)))
    (if (= rows len) 
	(let ((mt (matrix-transpose m)))
	  (matrix-transpose (matrix-row-set mt j v)))
	(display "matrix-column-set: vct v is not of proper length.\n"))))

(define (matrix-column-set! m j v)
  (let ((rows (matrix-rows m))
	(len  (vct-length v)))
    (if (= rows len) 
	(let loop-rows ((i 0))
	  (cond ((< i rows)
		 (matrix-set! m i j (vct-ref v i))
		 (loop-rows (+ i 1)))
		(else
		 m)))
	(display "matrix-column-set: vct v is not of proper length.\n"))))

;;;subsection of a matrix (analog to vct-subseq)
(define (matrix-subseq m i1 i2 j1 j2)
  (let* ((rows     (matrix-rows m))
	 (columns  (matrix-columns m))
	 (new-rows    (- i2 i1 -1))
	 (new-columns (- j2 j1 -1))
	 (new-m (make-matrix new-rows new-columns)))
    (let loop-rows ((i i1))
      (cond ((and (<= i i2) (< i rows))
	     (let loop-columns ((j j1))
	       (cond ((and (<= j j2) (< j columns))
		      (matrix-set! new-m (- i i1) (- j j1)
				   (matrix-ref m i j))
		      (loop-columns (+ j 1)))
		     (else
		      (loop-rows (+ i 1))))))
	    (else
	     new-m)))))

;;; set a subseq of matrix m with matrix m2
(define (matrix-subseq-set m m2 i2 j2)
  (let* ((rows     (matrix-rows m))
	 (columns  (matrix-columns m))
	 (rows2    (matrix-rows m2))
	 (columns2 (matrix-columns m2))
	 (new-m    (matrix-copy m)))
    (let loop-rows ((i i2))
      (cond ((and (< i rows) (< (- i i2) rows2))
	     (let loop-columns ((j j2))
	       (cond ((and (< j columns) (< (- j j2) columns2))
		      (matrix-set! new-m i j
				   (matrix-ref m2 (- i i2) (- j j2)))
		      (loop-columns (+ j 1)))
		     (else
		      (loop-rows (+ i 1))))))
	    (else
	     new-m)))))

;;; return all the rows concatenated in one vct
(define (matrix-flatten m)
  (define (copy-in-place! v1 v2 offset)
    (let ((len (vct-length v2)))
      (let loop ((i 0))
	(cond ((>= i len)
	       #t)
	      (else
	       (vct-set! v1 (+ i offset)
			 (vct-ref v2 i))
	       (loop (+ i 1)))))))

  (let* ((r (matrix-rows    m))
	 (c (matrix-columns m))
	 (v (make-vct (* r c))))
    (let loop ((i 0))
      (cond ((>= i r)
	     v)
	    (else
	     (copy-in-place! v (vector-ref (matrix-row m i) 0)
			     (* i c))
	     (loop (+ i 1)))))))
	     
;;; randomize matrix elements position
(define (matrix-randomize-rows m)
  (vector-randomize m))

(define (matrix-randomize-columns m)
  (matrix-transpose (matrix-randomize-rows (matrix-transpose m))))

(define (matrix-randomize m)
  (matrix-randomize-rows (matrix-randomize-columns m)))

(define (matrix-map! f . args)
  (let* ((m1      (car args))
	 (m2      (if (length> args 1) (cadr args) #f))
	 (rows    (matrix-rows m1))
	 (columns (matrix-columns m1)))
    (if m2 ;;check dimensions
	(let* ((rows2    (matrix-rows m2))
	       (columns2 (matrix-columns m2)))
	  (if (not (and (= columns columns2)
			(= rows    rows2)))
	      (error "matrix-map!: Matrix m1 and m2 have to be the same size.\n"))))
    ;;; compute
    (let loop-rows ((i 0))
      (cond ((< i rows)
	     (let loop-columns ((j 0))
	       (cond ((< j columns)
		      (matrix-set! m1 i j 
				   (if m2
				       (f (matrix-ref m1 i j) 
					  (matrix-ref m2 i j))
				       (f (matrix-ref m1 i j))))
		      (loop-columns (+ j 1)))
		     (else
		      (loop-rows (+ i 1))))))
	    (else
	     m1)))))

(define (matrix-for-each f m)
  (let ((rows    (matrix-rows    m))
	(columns (matrix-columns m)))
    ;;; compute
    (let loop-rows ((i 0))
      (cond ((< i rows)
	     (let loop-columns ((j 0))
	       (cond ((< j columns)
		      (f (matrix-ref m i j))
		      (loop-columns (+ j 1)))
		     (else
		      (loop-rows (+ i 1))))))
	    (else
	     #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; misc map functions
(define (matrix-map f m)
  (let ((new-m (matrix-copy m)))
    (matrix-map! f new-m)))

(define (matrix-scale! m k)  (matrix-map! (lambda (x) (* k x)) m))
(define (matrix-scale  m k)  (matrix-scale! (matrix-copy m) k))

(define (matrix-offset! m k) (matrix-map! (lambda (x) (+ k x)) m))
(define (matrix-offset  m k) (matrix-offset! (matrix-copy m) k))

(define (matrix-round! m k)  (matrix-map! (lambda (x) (round x)) m))
(define (matrix-round  m k)  (matrix-round! (matrix-copy m) k))

(define (matrix-sin! m) (matrix-map! sin m))
(define (matrix-sin  m) (matrix-map! sin (matrix-copy m)))

(define (matrix-cos! m) (matrix-map! cos m))
(define (matrix-cos  m) (matrix-map! cos (matrix-copy m)))

(define (matrix-tan! m) (matrix-map! tan m))
(define (matrix-tan  m) (matrix-map! tan (matrix-copy m)))

(define (matrix-log! m) (matrix-map! log m))
(define (matrix-log  m) (matrix-map! log (matrix-copy m)))

(define (matrix-log10! m) (matrix-map! log10 m))
(define (matrix-log10  m) (matrix-map! log10 (matrix-copy m)))

(define (matrix-logsig! m) (matrix-map! logsig m))
(define (matrix-logsig  m) (matrix-map! logsig (matrix-copy m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; linear algebra tools
(define (matrix-transpose m)
  (let* ((columns (matrix-rows m))
	 (rows    (matrix-columns m))
	 (new-m   (make-matrix rows columns)))
    (let loop-rows ((i 0))
      (cond ((< i rows)
	     (let loop-columns ((j 0))
	       (cond ((< j columns)
		      (matrix-set! new-m i j (matrix-ref m j i))
		      (loop-columns (+ j 1)))
		     (else
		      (loop-rows (+ i 1))))))
	    (else
	     new-m)))))

;;; matrix-trace: matrix diagonal elements
(define (matrix-trace m)
  (let* ((columns (matrix-rows m))
	 (rows    (matrix-columns m))
	 (len     (min rows columns))
	 (new-m   (make-matrix 1 len)))
    (let loop ((i 0))
      (cond ((< i len)
	     (matrix-set! new-m 0 i (matrix-ref m i i))
	     (loop (+ i 1)))
	    (else
	     new-m)))))

;;; matrix addition
(define (matrix-add m1 m2)
  (matrix-map! + (matrix-copy m1) m2))

;;; matrix subtraction
(define (matrix-subtract m1 m2)
  (matrix-map! - (matrix-copy m1) m2))

;;; matrix multiplication
(define (matrix-multiply m1 m2)
  (matrix-map! * (matrix-copy m1) m2))

;;; matrix multiplication
(define (matrix-product m1 m2)
  (let* ((rows1    (matrix-rows m1))
	 (columns1 (matrix-columns m1))
	 (rows2    (matrix-rows m2))
	 (columns2 (matrix-columns m2)))
    (cond ((not (= columns1 rows2))
	   (display "matrix-add: number of columns in matrix m1 must match number of rows in matrix m2.\n"))
	  (else
	   (let ((new-m (make-matrix rows1 columns2))
		 (m2t   (matrix-transpose m2)))
	     (let loop-rows ((i 0))
	       (cond ((< i rows1)
		      (let loop-columns ((j 0))
			(cond ((< j columns2)
			       (matrix-set! new-m i j 
					    (vct-product (vector-ref m1  i)
							 (vector-ref m2t j)))
			       (loop-columns (+ j 1)))
			      (else
			       (loop-rows (+ i 1))))))
		     (else
		      new-m))))))))

;;; f is a function that takes a vct parameter (f vct)
(define (matrix-map-rows f m)
  (vector-map vct (vector-map f m)))
(define (matrix-map-columns f m)
  (matrix-transpose (matrix-map-rows f (matrix-transpose m))))
;;; f takes a vct as parameter
(define (matrix-f f m)
  (matrix-ref (matrix-map-columns f (matrix-map-rows f m)) 0 0))

;;; sum of elements of matrix m
(define (matrix-sum-rows m)    (matrix-map-rows    vct-sum m))
(define (matrix-sum-columns m) (matrix-map-columns vct-sum m))
(define (matrix-sum m)         (matrix-f           vct-sum m))

;;; product of all elements for each row
(define (matrix-prod-rows m)    (matrix-map-rows    vct-prod m))
(define (matrix-prod-columns m) (matrix-map-columns vct-prod m))
(define (matrix-prod m)         (matrix-f           vct-prod m))

;;; mean of all elements for each row
(define (matrix-mean-rows m)    (matrix-map-rows    vct-mean m))
(define (matrix-mean-columns m) (matrix-map-columns vct-mean m))
(define (matrix-mean m)         (matrix-f           vct-mean m))

;;; mean of all elements for each row
(define (matrix-median-rows m)    (matrix-map-rows    vct-median m))
(define (matrix-median-columns m) (matrix-map-columns vct-median m))
(define (matrix-median m)         (matrix-f           vct-median m))

;;; variance of all elements for each row
(define (matrix-variance-rows m)    (matrix-map-rows    vct-variance m))
(define (matrix-variance-columns m) (matrix-map-columns vct-variance m))
(define (matrix-variance m)         (matrix-f           vct-variance m))

;;; position of mininum of all elements for each column
(define (matrix-min-pos-columns m)
  (let ((cols (matrix-columns m))
	(rows (matrix-rows    m)))
    (cond ((< rows 1)
	   (zeros cols))
	  (else
	   (vector-map (lambda (v) (car (vct-min v)))
		       (matrix-transpose m))))))

(define (matrix-extreme-value f m)
  (f (vector->vct (vector-map f m))))

(define (matrix-min-value m) (matrix-extreme-value vct-min-value m))
(define (matrix-max-value m) (matrix-extreme-value vct-max-value m))
  
;;; w is a vct of length equal to the number of rows in m
(define (matrix-weighted-map-rows f m w)
  (matrix-map-rows (lambda (v) (f (vct-multiply v w))) m))
(define (matrix-weighted-map-columns f m w)
  (matrix-map-columns (lambda (v) (f (vct-multiply v w))) m))

;;; weighted means
(define (matrix-weighted-mean-rows m v) 
  (matrix-weighted-map-rows    vct-mean m v))
(define (matrix-weighted-mean-columns m v) 
  (matrix-weighted-map-columns vct-mean m v))

;;; weighted sums
(define (matrix-weighted-sum-rows m v) 
  (matrix-weighted-map-rows    vct-sum m v))
(define (matrix-weighted-sum-columns m v) 
  (matrix-weighted-map-columns vct-sum m v))

;;; normalize matrix so value are 0..1
(define (matrix-normalize-min-max m min max)
  (let* ((amp  (- max min))
	 (mmin (matrix-min-value m))
	 (mmax (matrix-max-value m))
	 (mamp (/ amp (- mmax mmin)))
	 (moff (- min mmin)))
    (matrix-offset
     (matrix-scale (matrix-offset m (- mmin))
		   mamp)
     min)))

;;; normalize so sum of columns are equal to 1
(define (matrix-normalize-columns m)
  (let ((vs (vct-inverse (matrix->vct (matrix-sum-columns m)))))
    (matrix-vct-row-multiply m vs)))

;;; normalize so sum of rows are equal to 1
(define (matrix-normalize-rows m)
  (let ((vs (vct-inverse (matrix->vct (matrix-sum-rows m)))))
    (matrix-vct-column-multiply m vs)))

;;; normalize so norm of columns are equal to 1
(define (matrix-normalize-norm-columns m)
  (let ((vs (vct-inverse
	     (vct-sqrt
	      (matrix->vct	      
	       (matrix-sum-columns 
		(matrix-map square m)))))))
    (matrix-vct-row-multiply m vs)))

;;; normalize so norm of rows are equal to 1
(define (matrix-normalize-norm-rows m)
  (let ((vs (vct-inverse
	     (vct-sqrt
	      (matrix->vct
	       (matrix-sum-rows
		(matrix-map square m)))))))
    (matrix-vct-column-multiply m vs)))

;;; Outer product or "kronecker product" (returns a matrix)
(define (vct-outer-product v1 v2)
  (let* ((rows    (vct-length v2))
	 (columns (vct-length v1))
	 (m (make-matrix rows columns)))
    (let loop-rows ((i 0))
      (cond ((< i rows)
	     (let loop-columns ((j 0))
	       (cond ((< j columns)
		      (matrix-set! m i j (vct* (vct-ref v2 i)
					       (vct-ref v1 j)))
		      (loop-columns (+ j 1)))
		     (else
		      (loop-rows (+ i 1))))))
	    (else
	     m)))))

;;; vertical flip
(define (matrix-vertical-flip m)
  (let* ((rows    (matrix-rows    m))
	 (columns (matrix-columns m))
	 (new-m   (make-matrix rows columns)))
    (let loop-rows ((i 0))
      (cond ((< i rows)
	     (vector-set! new-m i (vct-reverse (vector-ref m i)))
	     (loop-rows (+ i 1)))
	    (else
	     new-m)))))

;;; horizontal flip
(define (matrix-horizontal-flip m)
  (vector-reverse m))

;;; replicate a matrix x times the rows and y times the columns
(define (matrix-replicate m x y)
  (define (row-replicate row y)
    (let ((r (vector-ref row 0)))
      (apply vct-append (repeat y (lambda () r)))))
  
  (let* ((rows        (matrix-rows    m))
	 (columns     (matrix-columns m))
	 (new-rows    (* x (matrix-rows    m)))
	 (new-columns (* y (matrix-columns m)))
	 (new-m       (make-vector new-rows)))
    (let loop-rows ((i 0))
      (cond ((< i new-rows)
	     (vector-set! new-m i
			  (row-replicate (matrix-row m (modulo i rows)) y))
	     (loop-rows (+ i 1)))
	    (else
	     new-m)))))

;;; progressive sum
(define (matrix-progressive-sum-rows m)
  (vector-map vct-progressive-sum m))

(define (matrix-progressive-sum-columns m)
  (matrix-transpose 
   (vector-map vct-progressive-sum (matrix-transpose m))))

;;;convolution: m2 should have <= dimensions than m1
(define (matrix-conv m1 m2)
  (let* ((r1 (matrix-rows    m1))
	 (c1 (matrix-columns m1))
	 (r2 (matrix-rows    m2))
	 (c2 (matrix-columns m2))
	 (rows    (- r1 r2 -1))
	 (columns (- c1 c2 -1))
	 (new-m (make-matrix rows columns)))
    (let loop-rows ((r 0))
      (cond ((< r rows)
	     (let loop-columns ((c 0))
	       (cond ((< c columns)
		      (matrix-set! new-m r c 
				   (matrix-sum
				    (matrix-multiply
				     (matrix-subseq m1
						    r (+ r r2 -1)
						    c (+ c c2 -1))
				     m2)))
		      (loop-columns (+ c 1)))
		     (else
		      (loop-rows (+ r 1))))))
	    (else
	     new-m)))))

;;; matrix-subsampling: subsampling by averagign of dr x dc matrix
;;; elements. Note: dr and dc should be dividers of dimensions "rows"
;;; and "columns".
(define (matrix-subsampling m dr dc)
  (let* ((r  (matrix-rows    m))
	 (c  (matrix-columns m))
	 (new-r (/ r dr))
	 (new-c (/ c dc)))
    (cond ((not (and (= (round new-r) new-r)
		     (= (round new-c) new-c)))
	   (error "matrix-subsampling: matrix dimensions r and c should multiples of dr and dc."))
	  (else
	   (let ((area (* dr dc))
		 (new-m (make-matrix new-r new-c)))
	     (let loop-rows ((r 0))
	       (cond ((< r new-r)
		      (let loop-columns ((c 0))
			(cond ((< c new-c)
			       (matrix-set! 
				new-m r c 
				(matrix-sum
				 (matrix-subseq m
						(* dr r) (- (* dr (+ 1 r)) 1)
						(* dc c) (- (* dc (+ 1 c)) 1))))
			       (loop-columns (+ c 1)))
			      (else
			       (loop-rows (+ r 1))))))
		     (else
		      (matrix-scale! new-m (/ 1. area))))))))))


(define (matrix-vct-row-map f m v)
  (let* ((r   (matrix-rows m))
	 (c   (matrix-columns m))
	 (len (vct-length v)))
    (cond ((not (= c len))
	   (error "the number columns of m and v length must be equal.\n"))
	  (else
	   (let ((m2 (matrix-replicate (vct->matrix v) r 1)))
	     (f m m2))))))

(define (matrix-vct-row-add m v)
  (matrix-vct-row-map matrix-add m v))
(define (matrix-vct-row-multiply m v)
  (matrix-vct-row-map matrix-multiply m v))
(define (matrix-vct-row-subtract m v)
  (matrix-vct-row-map matrix-subtract m v))

(define (matrix-vct-column-map f m v)
  (let* ((r   (matrix-rows m))
	 (c   (matrix-columns m))
	 (len (vct-length v)))
    (cond ((not (= r len))
	   (error "the number columns of m and v length must be equal.\n"))
	  (else
	   (let ((m2 (matrix-replicate (matrix-transpose (vct->matrix v)) 1 c)))
	     (f m m2))))))

(define (matrix-vct-column-add m v)      
  (matrix-vct-column-map matrix-add m v))
(define (matrix-vct-column-multiply m v)
  (matrix-vct-column-map matrix-multiply m v))
(define (matrix-vct-column-subtract m v)
  (matrix-vct-column-map matrix-subtract m v))


;;;; some matrix tests (not complete)
;(define m (make-matrix 5 4))
;(matrix? m)
;(matrix-set! m 0 0 1.1)
;(matrix-set! m 1 1 1.2)
;(matrix-set! m 4 3 1.3)
;(matrix-ref m 0 0)
;(matrix-ref m 0 3)
;(matrix-ref m 4 3)
;(matrix-rows m)
;(matrix-columns m)
;(matrix-size m)
;(matrix-row m 4)
;(matrix-column m 3)
;(matrix-row-set m 2 (vct-iota 4 4))
;(matrix-column-set m 2 (vct-iota 5 5))
;(matrix-transpose m)
;(matrix-trace m)
;(define m1 (make-matrix 2 4 1.))
;(define m2 (make-matrix 2 4 4.))
;(matrix-add m1 m2)
;(matrix-append m1 m2)
;(define m1 (make-matrix 2 3 1.))
;(define m2 (make-matrix 3 4 4.))
;(matrix-multiply m1 m2)
;(matrix-map! square m)
;(matrix-scale! m -1)
;(matrix-offset! m 2)
;(define m (matrix-iota 6 6))
;(matrix-subseq m 2 4 2 3)
;(matrix-subseq-set m (matrix-iota 3 3) 2 2)
;(matrix-sum-rows m)
;(matrix-sum-columns m)
;(matrix->vct (matrix-sum-columns m))
;(vct->matrix (vct 1. 2. 3.))
;(define ml (list-list->matrix (list (from-to 0. 4.) (from-to 2. 6.))))
;(matrix->list-list ml)
;(matrix-iota 5 5)
;(define m (vct-outer-product (vct-iota 5) (vct-from-to 2 5)))
;(matrix-vertical-flip m)
;(matrix-horizontal-flip m)
;(matrix-vertical-flip (matrix-horizontal-flip m))
;(matrix-replicate (matrix-iota 4 4) 5/2 4)
;(matrix-progressive-sum-rows m)
;(matrix-progressive-sum-columns m)
;(matrix-conv m1 m2)
;(matrix-subsampling (matrix-iota 20 20) 4 5)
;(define m3 (matrix-replicate (matrix-iota 10 1) 1 10))
;(define m1 (matrix-iota 4 5))
;(define v  (vct-iota 5))
;(matrix-vct-row-add m1 v)
;(define m1 (matrix-iota 5 4))
;(matrix-vct-column-add m1 v)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;macro pour simplifier l'écriture des formules
(define-macro (with-matrix expr)
  `(let ((*      matrix-product)
	 (+      matrix-add)
	 (-      matrix-subtract)
	 (:      matrix-subseq)
	 (.*     matrix-multiply)
	 (*k     matrix-scale)
	 (+k     matrix-offset)
	 (log    matrix-log)
	 (logsig matrix-logsig)
	 (sum    matrix-sum)
	 (conv   matrix-conv)
	 (rows   matrix-rows)
	 (cols   matrix-columns)
	 (trans  matrix-transpose)
	 (mref   matrix-ref)
	 (msub   matrix-subseq)
	 (mset!  matrix-set!))
     ,expr))

;;ex:	
;(define m1 (matrix-iota 5 4))
;(define m2 (matrix-iota 5 4))
;(with-matrix (*k m1 2.25))
;(with-matrix (+  m1 m2))
;(with-matrix (.* m1 m2))
;(with-matrix (*  m1 (.* m1 (*k m1 2.25))))
;(with-matrix (: m1 0 2 0 2))
;(with-matrix (sum m1))





