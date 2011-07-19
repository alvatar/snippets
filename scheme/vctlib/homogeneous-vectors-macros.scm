;;; -*-scheme-*-
;;; homogeneous-vectors-macros.scm
;;;
;;; These macros help you define generic functions that work with
;;; homogeneous vectors (srfi-4) and normal Scheme vectors. You can't
;;; mix different types of vectors in the same macro call though.
;;; 
;;; Copyright 2008 Pierre-Alexandre Fournier
;;; (fournier@carretechnologies.com)
;;; All rights reserved.
;;; 
;;; $Id:  $


;;; homogeneous vector function with tag
(define-macro (->procedure tag function)
  `,(string->symbol (string-append tag (symbol->string function))))

;(->procedure "u32" vector) --> u32vector

;;; check if tag begins with f (like f32 or f64)
(define-macro (tag-flonum? tag)
  `,(char=? (string-ref tag 0) '#\f))

;(tag-flonum? "f32") --> #t
;(tag-flonum? "u32") --> #f

(define-macro (let-with-homogeneous-vector tag body)
  `(let* ((make-vector   ,(string->symbol (string-append "make-"  tag "vector")))
	  (list->vector  ,(string->symbol (string-append "list->" tag "vector")))
	  (vector        (->procedure ,tag vector))
	  (vector?       (->procedure ,tag vector?))
	  (vector-length (->procedure ,tag vector-length))
	  (vector-ref    (->procedure ,tag vector-ref))
	  (vector-set!   (->procedure ,tag vector-set!))
	  (vector->list  (->procedure ,tag vector->list))
	  (vector-copy   (->procedure ,tag vector-copy))
	  (vector-append (->procedure ,tag vector-append)))
     (if (tag-flonum? ,tag)
	 (let ((homogeneous-vector-is-flonum? #t) ;;; this does not work...
	       (vector+ ##flonum.+)
	       (vector- ##flonum.-)
	       (vector* ##flonum.*)
	       (vector/ ##flonum./))
	   ,body)
	 (let ((homogeneous-vector-is-flonum? #f)
	       (vector+ ##fixnum.+)
	       (vector- ##fixnum.-)
	       (vector* ##fixnum.*)
	       (vector/ /)) ;;; ##fixnum./) ?
	   ,body))))

(define-macro (with-homogeneous-vector vector body)
  `(cond ((f32vector? ,vector)
	  (let-with-homogeneous-vector "f32" ,body))
	 ((f64vector? ,vector)
	  (let-with-homogeneous-vector "f64" ,body))
	 ((u8vector? ,vector)
	  (let-with-homogeneous-vector "u8"  ,body))
	 ((u16vector? ,vector)
	  (let-with-homogeneous-vector "u16" ,body))
	 ((u32vector? ,vector)
	  (let-with-homogeneous-vector "u32" ,body))
	 ((u64vector? ,vector)
	  (let-with-homogeneous-vector "u64" ,body))
	 ((s8vector? ,vector)
	  (let-with-homogeneous-vector "s8"  ,body))
	 ((s16vector? ,vector)
	  (let-with-homogeneous-vector "s16" ,body))
	 ((s32vector? ,vector)
	  (let-with-homogeneous-vector "s32" ,body))
	 ((s64vector? ,vector)
	  (let-with-homogeneous-vector "s64" ,body))
	 ((vector? ,vector)
	  (let ((homogeneous-vector-is-flonum? #t)) ;;; allow flonum for consistency
	    ,body))
	 (else
	  (error "vector argument is not a known homogeneous vector type."))))


(define-macro (with-homogeneous-vectors vectors body)
  `(cond ((every f32vector? ,vectors)
	  (let-with-homogeneous-vector "f32" ,body))
	 ((every f64vector? ,vectors)
	  (let-with-homogeneous-vector "f64" ,body))
	 ((every u8vector? ,vectors)
	  (let-with-homogeneous-vector "u8"  ,body))
	 ((every u16vector? ,vectors)
	  (let-with-homogeneous-vector "u16" ,body))
	 ((every u32vector? ,vectors)
	  (let-with-homogeneous-vector "u32" ,body))
	 ((every u64vector? ,vectors)
	  (let-with-homogeneous-vector "u64" ,body))
	 ((every s8vector? ,vectors)
	  (let-with-homogeneous-vector "s8"  ,body))
	 ((every s16vector? ,vectors)
	  (let-with-homogeneous-vector "s16" ,body))
	 ((every s32vector? ,vectors)
	  (let-with-homogeneous-vector "s32" ,body))
	 ((every s64vector? ,vectors)
	  (let-with-homogeneous-vector "s64" ,body))
	 ((every vector? ,vectors)
	  (let ((homogeneous-vector-is-flonum? #t)) ;;; allow flonum for consistency
	    ,body))
	 (else
	  (error "All homogeneous vectors must have the same type"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation

;;; these are the basis homogeneous vectors functions for f32vectors

;(define make-vector     make-f32vector)
;(define list->vector    list->f32vector)
;(define vector          f32vector)
;(define vector?         f32vector?)
;(define vector-length   f32vector-length)
;(define vector-ref      f32vector-ref)
;(define vector-set!     f32vector-set!)
;(define vector->list    f32vector->list)
;(define vector-copy     f32vector-copy)

;;; Specialization of arithmetic operators for vectors elements (fixed
;;; vs floating point)

;; flonum
;(define vector+ ##flonum.+)
;(define vector- ##flonum.-)
;(define vector* ##flonum.*)
;(define vector/ ##flonum./)
;; fixnum
;(define vector+ ##fixnum.+)
;(define vector- ##fixnum.-)
;(define vector* ##fixnum.*)
;(define vector/ /) ;;; ##fixnum./) ?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Examples and tests

;(define v (make-u32vector 5))
;(let-with-homogeneous-vector "u32" (vector-length v))
;(define v (f64vector 0. 1. 2. 3.))
;(let-with-homogeneous-vector "f64" (vector-ref v 2))
;(let-with-homogeneous-vector "f64" (make-vector 2))
;(let-with-homogeneous-vector "u64" (list->vector (list 1 2 3)))

;(define v (vector 1. 2. 3.))
;(define f (f32vector 1. 2. 3.))
;(define u (u32vector 1 2 3))
;
;(define (vector-sum v)
;  (with-homogeneous-vector 
;   v
;   (let ((len (vector-length v)))
;     (let loop ((i 0)
;		(s 0))
;       (cond ((>= i len)
;	      s)
;	     (else
;	      (loop (+ i 1)
;		    (+ s (vector-ref v i)))))))))

;(vector-sum (f32vector 1. 2. 3. 42.))
;(vector-sum (f64vector 1. 2. 3. 42.))
;(vector-sum (u8vector  1 2 3 42))
;(vector-sum (u16vector 1 2 3 42))
;(vector-sum (u32vector 1 2 3 42))
;(vector-sum (u64vector 1 2 3 42))
;(vector-sum (s8vector  1 2 3 42))
;(vector-sum (s16vector 1 2 3 42))
;(vector-sum (s32vector 1 2 3 42))
;(vector-sum (s64vector 1 2 3 42))
;(vector-sum (vector    1 2 3 42))

;(load "../srfi/srfi-1.scm")
;(load "../vctlib/utils.scm")
;
;(define (vector-add . vectors)
;  (with-homogeneous-vectors
;   vectors
;   (let ((len (vector-length (car vectors))))
;     (let loop ((i 0)
;		(v (make-vector len)))
;       (cond ((>= i len)
;	      v)
;	     (else
;	      (vector-set! v i (sum (map (lambda (v)
;					   (vector-ref v i))
;					 vectors)))
;	      (loop (+ i 1)
;		    v)))))))

;(define vu (make-u32vector 5))
;(vector-add vu vu vu)
;(define vf (f64vector 0. 1. 2. 3.))
;(vector-add vf vf vu)
;(vector-add u)
;(vector-add f)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OBSOLETE
;; ;;; macro for functions defined for homogeneous vectors arguments
;; (define-macro (with-homogeneous-vector function vector)
;;   `(cond ((f32vector? ,vector)
;; 	  (let ((vector-length f32vector-length)
;; 		(vector-ref  f32vector-ref)
;; 		(vector-set! f32vector-set!))
;; 	    (,function ,vector)))
;; 	 ((u32vector? ,vector)
;; 	  (let ((vector-length u32vector-length)
;; 		(vector-ref  u32vector-ref)
;; 		(vector-set! u32vector-set!))
;; 	    (,function ,vector)))
;; 	 ((vector? ,vector)
;; 	  (,function ,vector))))
;; 
;; (define-macro (with-homogeneous-vector* function vectors)
;;   `(cond ((f32vector? (car ,vectors))
;; 	  (let ((vector-length f32vector-length)
;; 		(vector-ref  f32vector-ref)
;; 		(vector-set! f32vector-set!))
;; 	    (apply ,function ,vectors)))
;; 	 ((u32vector? (car ,vectors))
;; 	  (let ((vector-length u32vector-length)
;; 		(vector-ref  u32vector-ref)
;; 		(vector-set! u32vector-set!))
;; 	    (apply ,function ,vectors)))
;; 	 ;;; could use a "every" condition
;; 	 ((vector? (car ,vectors))
;; 	  (apply ,function ,vectors))))
;; 
;; (define srfi-4-tags
;;   '("u8" "u16" "u32" "u64"
;;     "s8" "s16" "s32" "s64"
;;     "f32" "f64"))
;; 
;; (define srfi-4-functions
;;   '(vector
;;     vector?
;;     vector-length
;;     vector-ref
;;     vector-set!
;;     vector->list
;;     vector-copy))


;; (define-macro (foo x y)
;;   `(let ((z 2))
;;      (cond ((exact? ,y)	    
;; 	    (,x ,y))
;; 	   (else
;; 	    (,x z ,y)))))
