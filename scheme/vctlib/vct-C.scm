;;; -*-scheme-*-
;;; vct-C.scm 
;;; 
;;; vctlib functions rewritten in C for better performance (for
;;; gambit-C).
;;; 
;;; Copyright 2006-2007 Pierre-Alexandre Fournier
;;; (pierre-alexandre.fournier@polymtl.ca)
;;; All rights reserved.
;;; 
;;; $Id:  $

(c-declare "#include <math.h>")
(c-declare "#include <stdlib.h>") ;; for abs() C function


(define (vct-subseq input-f32vector start . end)
  (let* ((N   (vct-length input-f32vector))
	 (start (if (< start 0) 0 start))
	 (end   (if (null? end) (- N 1) (car end))))
    (cond
     ((< (- end start) 0)
      (vct))
     (else
      (let ((output-f32vector (make-vct (+ 1 (- end start)))))
	((c-lambda 
	  (scheme-object scheme-object int int int)
	  void
	  "
   ___F32* X    = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   ___F32* subX = ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED));
   int N     = ___arg3;
   int start = ___arg4;
   int end   = ___arg5;

   int stop  = ((end < N) ? end : (N-1));
   int i     = 0;
   int isub  = 0;

   for (i=start; i<=stop; i++){
     subX[isub] = X[i];
     isub++;
   }
   if (stop!=end){
     for (i=stop+1; i<=end; i++){
       subX[isub] = 0.0;
       isub++;
     }     
   }
   ")
	 input-f32vector output-f32vector N start end)
      output-f32vector)))))

;;; 4.7 times faster than vctlib version
(define (vct-square! input-f32vector)
  (let ((N (vct-length input-f32vector)))
    ((c-lambda 
      (scheme-object int)
      void
      "
   ___F32* X = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   int N     = ___arg2;
   int i     = 0;
   for (i=0; i<N; i++){
     X[i] = X[i]*X[i];
   }
   ")
     input-f32vector N)
    input-f32vector))

(define (vct-sqrt! input-f32vector)
  (let ((N (vct-length input-f32vector)))
    ((c-lambda 
      (scheme-object int)
      void
      "
   ___F32* X = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   int N     = ___arg2;
   int i     = 0;
   for (i=0; i<N; i++){
     X[i] = sqrt(X[i]);
   }
   ")
     input-f32vector N)
    input-f32vector))


(define (vct-abs! input-f32vector)
  (let ((N (vct-length input-f32vector)))
    ((c-lambda 
      (scheme-object int)
      void
      "
   ___F32* X = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   int N     = ___arg2;
   int i     = 0;
   for (i=0; i<N; i++){
     X[i] = fabs(X[i]);
   }
   ")
     input-f32vector N)
    input-f32vector))


(define (vct-sum input-f32vector)
  (let ((N (vct-length input-f32vector)))
    ((c-lambda 
      (scheme-object int)
      float
      "
   ___F32* X = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   int N     = ___arg2;
   int i     = 0;
   float s   = 0.0;
   
   for (i=0; i<N; i++){
     s += X[i];
   }
   ___result = s;
   ")
     input-f32vector N)))

;;; vct convolution
(define (vct-conv v1 v2)
  (let* ((len1 (vct-length v1))
	 (len2 (vct-length v2))
	 (len3 (+ len1 len2 -1))
	 (v2  (vct-reverse v2))
	 (v3  (make-vct len3)))
    ((c-lambda 
      (scheme-object int scheme-object int scheme-object int)
      void
      "
   ___F32* X1 = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   int len1   = ___arg2;
   ___F32* X2 = ___CAST(___F32*, ___BODY_AS(___arg3, ___tSUBTYPED));
   int len2   = ___arg4;
   ___F32* X3 = ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED));
   int len3   = ___arg6;
   int i,j,k;

   for (k=0; k<len3; k++){
     X3[k] = 0.0;
     i=k;
     for (j=len2-1; (j>=0 && i>=0); j--){
       if (i<len1){
         X3[k] += X1[i]*X2[j];
       }
       i--;
     }
   }
   ")
     v1 len1 v2 len2 v3 len3)
    v3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trigonometric functions
(define (vct-sin! input-f32vector)
  (let ((N (vct-length input-f32vector)))
    ((c-lambda 
      (scheme-object int)
      void
      "
   ___F32* X = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   int N     = ___arg2;
   int i     = 0;
   for (i=0; i<N; i++){
     X[i] = sin(X[i]);
   }
   ")
     input-f32vector N)
    input-f32vector))


(define (vct-cos! input-f32vector)
  (let ((N (vct-length input-f32vector)))
    ((c-lambda 
      (scheme-object int)
      void
      "
   ___F32* X = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   int N     = ___arg2;
   int i     = 0;
   for (i=0; i<N; i++){
     X[i] = cos(X[i]);
   }
   ")
     input-f32vector N)))


(define (vct-log! input-f32vector)
  (let ((N (vct-length input-f32vector)))
    ((c-lambda 
      (scheme-object int)
      void
      "
   ___F32* X = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   int N     = ___arg2;
   int i     = 0;
   for (i=0; i<N; i++){
     X[i] = log(X[i]);
   }
   ")
     input-f32vector N)
    input-f32vector))


(define (vct-iota N . args)
  (let ((X     (make-vct N))
	(start (if (< (length args) 1) 0. (car  args)))
	(step  (if (< (length args) 2) 1. (cadr args))))
    ((c-lambda 
      (scheme-object int float float)
      void
      "
   ___F32* X   = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   int N       = ___arg2;
   float start = ___arg3;
   float step  = ___arg4;
   int i=0;

   X[0] = start;
   for (i=1; i<N; i++){
     X[i] = X[i-1]+step;
   }
   ")
     X N (exact->inexact start) (exact->inexact step))
    X))

(define (vct-harm-sin f len output-srate)
  (let ((X (make-vct len)))
    ((c-lambda 
      (scheme-object int float float)
      void
      "
   ___F32* X = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   int N       = ___arg2;
   float f     = ___arg3;
   float srate = ___arg4;
   float omega = 2.0*M_PI*(f/srate);
   float phi   = 0.0;
   int i=0;

   for (i=0; i<N; i++){
     X[i] = sin(phi);
     phi += omega;
   }
   ")
     X len (exact->inexact f) (exact->inexact output-srate))
    X))

(define (vct-harm-cos f len output-srate)
  (let ((X (make-vct len)))
    ((c-lambda 
      (scheme-object int float float)
      void
      "
   ___F32* X = ___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED));
   int N       = ___arg2;
   float f     = ___arg3;
   float srate = ___arg4;
   float omega = 2.0*M_PI*(f/srate);
   float phi   = 0.0;
   int i=0;

   for (i=0; i<N; i++){
     X[i] = cos(phi);
     phi += omega;
   }
   ")
     X len (exact->inexact f) (exact->inexact output-srate))
    X))

