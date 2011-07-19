;;; -*-scheme-*-
;;; blas.scm
;;;
;;; BLAS Gambit-C Foreign Function Interface (FFI) Version 1.0
;;; 
;;; Copyright 2007 Pierre-Alexandre Fournier
;;; (fournier@carretechnologies.com)
;;; All rights reserved.
;;; 
;;; This file was generated using a pattern matcher over Chicken
;;; Scheme FFI for the cblas.h file.
;;; 
;;; For compilation, use library:  -L -lcblas

(c-declare  "#include <cblas.h>")

;;; BLAS enums
(define blas:RowMajor  101) 
(define blas:ColMajor  102) 

(define blas:NoTrans   111) 
(define blas:Trans     112) 
(define blas:ConjTrans 113) 

(define blas:Upper     121) 
(define blas:Lower     122) 

(define blas:NonUnit   131) 
(define blas:Unit      132) 

(define blas:Left      141) 
(define blas:Right     142) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Level 1 BLAS

(define (blas:sdsdot arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda
       (int float scheme-object int scheme-object int)
       float 
     "___result = cblas_sdsdot(___arg1, ___arg2, ___CAST(___F32*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6);")
   arg1 arg2 arg3 arg4 arg5 arg6)) 

(define (blas:dsdot arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (int scheme-object int scheme-object int)
       double 
     "___result = cblas_dsdot(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5);")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:sdot arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (int scheme-object int scheme-object int)
       float 
     "___result = cblas_sdot(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5);")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:ddot arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (int scheme-object int scheme-object int)
       double 
     "___result = cblas_ddot(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5);")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:cdotu_sub arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda
       (int scheme-object int scheme-object int scheme-object)
       void 
     "cblas_cdotu_sub(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6)) 

(define (blas:cdotc_sub arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda
       (int scheme-object int scheme-object int scheme-object)
       void 
     "cblas_cdotc_sub(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6)) 

(define (blas:zdotu_sub arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda
       (int scheme-object int scheme-object int scheme-object)
       void 
     "cblas_zdotu_sub(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6)) 

(define (blas:zdotc_sub arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda
       (int scheme-object int scheme-object int scheme-object)
       void 
     "cblas_zdotc_sub(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6)) 

(define (blas:snrm2 arg1 arg2 arg3) 
  ((c-lambda
       (int scheme-object int)
       float 
     "___result = cblas_snrm2(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:sasum arg1 arg2 arg3) 
  ((c-lambda
       (int scheme-object int)
       float 
     "___result = cblas_sasum(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:dnrm2 arg1 arg2 arg3) 
  ((c-lambda
       (int scheme-object int)
       double 
     "___result = cblas_dnrm2(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:dasum arg1 arg2 arg3) 
  ((c-lambda
       (int scheme-object int)
       double 
     "___result = cblas_dasum(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:scnrm2 arg1 arg2 arg3) 
  ((c-lambda
       (int scheme-object int)
       float 
     "___result = cblas_scnrm2(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:scasum arg1 arg2 arg3) 
  ((c-lambda
       (int scheme-object int)
       float 
     "___result = cblas_scasum(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:dznrm2 arg1 arg2 arg3) 
  ((c-lambda
       (int scheme-object int)
       double 
     "___result = cblas_dznrm2(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:dzasum arg1 arg2 arg3) 
  ((c-lambda
       (int scheme-object int)
       double 
     "___result = cblas_dzasum(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:isamax arg1 arg2 arg3) 
  ((c-lambda
       (int scheme-object int)
       int 
     "___result = cblas_isamax(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:idamax arg1 arg2 arg3) 
  ((c-lambda
       (int scheme-object int)
       int 
     "___result = cblas_idamax(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:icamax arg1 arg2 arg3) 
  ((c-lambda
       (int (pointer void) int)
       int 
     "___result = cblas_icamax(___arg1, ___arg2, ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:izamax arg1 arg2 arg3) 
  ((c-lambda
       (int (pointer void) int)
       int 
     "___result = cblas_izamax(___arg1, ___arg2, ___arg3);")
   arg1 arg2 arg3)) 

(define (blas:sswap arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (int scheme-object int scheme-object int)
       void 
     "cblas_sswap(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5);")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:scopy arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (int scheme-object int scheme-object int)
       void 
     "cblas_scopy(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5);")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:saxpy arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda
       (int float scheme-object int scheme-object int)
       void 
     "cblas_saxpy(___arg1, ___arg2, ___CAST(___F32*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6);")
   arg1 arg2 arg3 arg4 arg5 arg6)) 

(define (blas:dswap arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (int scheme-object int scheme-object int)
       void 
     "cblas_dswap(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5);")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:dcopy arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (int scheme-object int scheme-object int)
       void 
     "cblas_dcopy(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5);")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:daxpy arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda
       (int double scheme-object int scheme-object int)
       void 
     "cblas_daxpy(___arg1, ___arg2, ___CAST(___F64*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6);")
   arg1 arg2 arg3 arg4 arg5 arg6)) 

(define (blas:cswap arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (int scheme-object int scheme-object int)
       void 
     "cblas_cswap(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5);")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:ccopy arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (int scheme-object int scheme-object int)
       void 
     "cblas_ccopy(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5);")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:caxpy arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda
       (int scheme-object scheme-object int scheme-object int)
       void 
     "cblas_caxpy(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6);")
   arg1 arg2 arg3 arg4 arg5 arg6)) 

(define (blas:zswap arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (int scheme-object int scheme-object int)
       void 
     "cblas_zswap(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5);")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:zcopy arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (int scheme-object int scheme-object int)
       void 
     "cblas_zcopy(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5);")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:zaxpy arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda
       (int scheme-object scheme-object int scheme-object int)
       void 
     "cblas_zaxpy(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6);")
   arg1 arg2 arg3 arg4 arg5 arg6)) 

(define (blas:srotg arg1 arg2 arg3 arg4) 
  ((c-lambda
       (scheme-object scheme-object scheme-object scheme-object)
       void 
     "cblas_srotg(___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4)) 

(define (blas:srotmg arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (scheme-object scheme-object scheme-object float scheme-object)
       void 
     "cblas_srotmg(___CAST(___F32*, ___BODY_AS(___arg1, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:srot arg1 arg2 arg3 arg4 arg5 arg6 arg7) 
  ((c-lambda
       (int scheme-object int scheme-object int float float)
       void 
     "cblas_srot(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5, ___arg6, ___arg7);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7)) 

(define (blas:srotm arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda
       (int scheme-object int scheme-object int scheme-object)
       void 
     "cblas_srotm(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6)) 

(define (blas:drotg arg1 arg2 arg3 arg4) 
  ((c-lambda
       (scheme-object scheme-object scheme-object scheme-object)
       void 
     "cblas_drotg(___CAST(___F64*, ___BODY_AS(___arg1, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4)) 

(define (blas:drotmg arg1 arg2 arg3 arg4 arg5) 
  ((c-lambda
       (scheme-object scheme-object scheme-object double scheme-object)
       void 
     "cblas_drotmg(___CAST(___F64*, ___BODY_AS(___arg1, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5)) 

(define (blas:drot arg1 arg2 arg3 arg4 arg5 arg6 arg7) 
  ((c-lambda
       (int scheme-object int scheme-object int double double)
       void 
     "cblas_drot(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5, ___arg6, ___arg7);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7)) 

(define (blas:drotm arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda
       (int scheme-object int scheme-object int scheme-object)
       void 
     "cblas_drotm(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6)) 

(define (blas:sscal arg1 arg2 arg3 arg4) 
  ((c-lambda
       (int float scheme-object int)
       void 
     "cblas_sscal(___arg1, ___arg2, ___CAST(___F32*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4);")
   arg1 arg2 arg3 arg4)) 

(define (blas:dscal arg1 arg2 arg3 arg4) 
  ((c-lambda
       (int double scheme-object int)
       void 
     "cblas_dscal(___arg1, ___arg2, ___CAST(___F64*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4);")
   arg1 arg2 arg3 arg4)) 

(define (blas:cscal arg1 arg2 arg3 arg4) 
  ((c-lambda
       (int scheme-object scheme-object int)
       void 
     "cblas_cscal(___arg1, ___CAST(___F32*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4);")
   arg1 arg2 arg3 arg4)) 

(define (blas:zscal arg1 arg2 arg3 arg4) 
  ((c-lambda
       (int scheme-object scheme-object int)
       void 
     "cblas_zscal(___arg1, ___CAST(___F64*, ___BODY_AS(___arg2, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4);")
   arg1 arg2 arg3 arg4)) 

(define (blas:csscal arg1 arg2 arg3 arg4) 
  ((c-lambda
       (int float scheme-object int)
       void 
     "cblas_csscal(___arg1, ___arg2, ___CAST(___F32*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4);")
   arg1 arg2 arg3 arg4)) 

(define (blas:zdscal arg1 arg2 arg3 arg4) 
  ((c-lambda
       (int double scheme-object int)
       void 
     "cblas_zdscal(___arg1, ___arg2, ___CAST(___F64*, ___BODY_AS(___arg3, ___tSUBTYPED)), ___arg4);")
   arg1 arg2 arg3 arg4)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Level 2 BLAS

(define (blas:sgemv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int float scheme-object int scheme-object int float scheme-object int)
       void 
     "cblas_sgemv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___arg10, ___CAST(___F32*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:sgbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14) 
  ((c-lambda
       (int int int int int int float scheme-object int scheme-object int float scheme-object int)
       void 
     "cblas_sgbmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F32*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11, ___arg12, ___CAST(___F32*, ___BODY_AS(___arg13, ___tSUBTYPED)), ___arg14);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14)) 

(define (blas:strmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int int int scheme-object int scheme-object int)
       void 
     "cblas_strmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 

(define (blas:stbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int int int int scheme-object int scheme-object int)
       void 
     "cblas_stbmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:stpmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int)
       void 
     "cblas_stpmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:strsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int int int scheme-object int scheme-object int)
       void 
     "cblas_strsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 

(define (blas:stbsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int int int int scheme-object int scheme-object int)
       void 
     "cblas_stbsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:stpsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int)
       void 
     "cblas_stpsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:dgemv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int double scheme-object int scheme-object int double scheme-object int)
       void 
     "cblas_dgemv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___arg10, ___CAST(___F64*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:dgbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14) 
  ((c-lambda
       (int int int int int int double scheme-object int scheme-object int double scheme-object int)
       void 
     "cblas_dgbmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F64*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11, ___arg12, ___CAST(___F64*, ___BODY_AS(___arg13, ___tSUBTYPED)), ___arg14);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14)) 

(define (blas:dtrmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int int int scheme-object int scheme-object int)
       void 
     "cblas_dtrmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 

(define (blas:dtbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int int int int scheme-object int scheme-object int)
       void 
     "cblas_dtbmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:dtpmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int)
       void 
     "cblas_dtpmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:dtrsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int int int scheme-object int scheme-object int)
       void 
     "cblas_dtrsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 

(define (blas:dtbsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int int int int scheme-object int scheme-object int)
       void 
     "cblas_dtbsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:dtpsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int)
       void 
     "cblas_dtpsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:cgemv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_cgemv(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F32*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:cgbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14) 
  ((c-lambda
       (int int int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_cgbmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F32*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11, ___CAST(___F32*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg13, ___tSUBTYPED)), ___arg14);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14)) 

(define (blas:ctrmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int int int scheme-object int scheme-object int)
       void 
     "cblas_ctrmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 

(define (blas:ctbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int int int int scheme-object int scheme-object int)
       void 
     "cblas_ctbmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:ctpmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int)
       void 
     "cblas_ctpmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:ctrsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int int int scheme-object int scheme-object int)
       void 
     "cblas_ctrsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 

(define (blas:ctbsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int int int int scheme-object int scheme-object int)
       void 
     "cblas_ctbsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:ctpsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int)
       void 
     "cblas_ctpsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:zgemv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_zgemv(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F64*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:zgbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14) 
  ((c-lambda
       (int int int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_zgbmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F64*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11, ___CAST(___F64*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg13, ___tSUBTYPED)), ___arg14);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14)) 

(define (blas:ztrmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int int int scheme-object int scheme-object int)
       void 
     "cblas_ztrmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 

(define (blas:ztbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int int int int scheme-object int scheme-object int)
       void 
     "cblas_ztbmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:ztpmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int)
       void 
     "cblas_ztpmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:ztrsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int int int scheme-object int scheme-object int)
       void 
     "cblas_ztrsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 

(define (blas:ztbsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int int int int scheme-object int scheme-object int)
       void 
     "cblas_ztbsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:ztpsv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int)
       void 
     "cblas_ztpsv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:ssymv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11) 
  ((c-lambda
       (int int int float scheme-object int scheme-object int float scheme-object int)
       void 
     "cblas_ssymv(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___arg9, ___CAST(___F32*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11)) 

(define (blas:ssbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int float scheme-object int scheme-object int float scheme-object int)
       void 
     "cblas_ssbmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___arg10, ___CAST(___F32*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:sspmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int float scheme-object scheme-object int float scheme-object int)
       void 
     "cblas_sspmv(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:sger arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int float scheme-object int scheme-object int scheme-object int)
       void 
     "cblas_sger(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:ssyr arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int float scheme-object int scheme-object int)
       void 
     "cblas_ssyr(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:sspr arg1 arg2 arg3 arg4 arg5 arg6 arg7) 
  ((c-lambda
       (int int int float scheme-object int scheme-object)
       void 
     "cblas_sspr(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7)) 

(define (blas:ssyr2 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int float scheme-object int scheme-object int scheme-object int)
       void 
     "cblas_ssyr2(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:sspr2 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int float scheme-object int scheme-object int scheme-object)
       void 
     "cblas_sspr2(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 

(define (blas:dsymv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11) 
  ((c-lambda
       (int int int double scheme-object int scheme-object int double scheme-object int)
       void 
     "cblas_dsymv(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___arg9, ___CAST(___F64*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11)) 

(define (blas:dsbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int double scheme-object int scheme-object int double scheme-object int)
       void 
     "cblas_dsbmv(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___arg10, ___CAST(___F64*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:dspmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int double scheme-object scheme-object int double scheme-object int)
       void 
     "cblas_dspmv(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:dger arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int double scheme-object int scheme-object int scheme-object int)
       void 
     "cblas_dger(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:dsyr arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int double scheme-object int scheme-object int)
       void 
     "cblas_dsyr(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:dspr arg1 arg2 arg3 arg4 arg5 arg6 arg7) 
  ((c-lambda
       (int int int double scheme-object int scheme-object)
       void 
     "cblas_dspr(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7)) 

(define (blas:dsyr2 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int double scheme-object int scheme-object int scheme-object int)
       void 
     "cblas_dsyr2(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:dspr2 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int double scheme-object int scheme-object int scheme-object)
       void 
     "cblas_dspr2(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 

(define (blas:chemv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11) 
  ((c-lambda
       (int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_chemv(___arg1, ___arg2, ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11)) 

(define (blas:chbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_chbmv(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F32*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:chpmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int scheme-object scheme-object scheme-object int scheme-object scheme-object int)
       void 
     "cblas_chpmv(___arg1, ___arg2, ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:cgeru arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int scheme-object scheme-object int scheme-object int scheme-object int)
       void 
     "cblas_cgeru(___arg1, ___arg2, ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:cgerc arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int scheme-object scheme-object int scheme-object int scheme-object int)
       void 
     "cblas_cgerc(___arg1, ___arg2, ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:cher arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int float scheme-object int scheme-object int)
       void 
     "cblas_cher(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:chpr arg1 arg2 arg3 arg4 arg5 arg6 arg7) 
  ((c-lambda
       (int int int float scheme-object int scheme-object)
       void 
     "cblas_chpr(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7)) 

(define (blas:cher2 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int scheme-object scheme-object int scheme-object int scheme-object int)
       void 
     "cblas_cher2(___arg1, ___arg2, ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:chpr2 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int scheme-object scheme-object int scheme-object int scheme-object)
       void 
     "cblas_chpr2(___arg1, ___arg2, ___arg3, ___CAST(___F32*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 

(define (blas:zhemv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11) 
  ((c-lambda
       (int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_zhemv(___arg1, ___arg2, ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11)) 

(define (blas:zhbmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_zhbmv(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F64*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:zhpmv arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int scheme-object scheme-object scheme-object int scheme-object scheme-object int)
       void 
     "cblas_zhpmv(___arg1, ___arg2, ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:zgeru arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int scheme-object scheme-object int scheme-object int scheme-object int)
       void 
     "cblas_zgeru(___arg1, ___arg2, ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:zgerc arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int scheme-object scheme-object int scheme-object int scheme-object int)
       void 
     "cblas_zgerc(___arg1, ___arg2, ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:zher arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) 
  ((c-lambda
       (int int int double scheme-object int scheme-object int)
       void 
     "cblas_zher(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8)) 

(define (blas:zhpr arg1 arg2 arg3 arg4 arg5 arg6 arg7) 
  ((c-lambda
       (int int int double scheme-object int scheme-object)
       void 
     "cblas_zhpr(___arg1, ___arg2, ___arg3, ___arg4, ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7)) 

(define (blas:zher2 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10) 
  ((c-lambda
       (int int int scheme-object scheme-object int scheme-object int scheme-object int)
       void 
     "cblas_zher2(___arg1, ___arg2, ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)) 

(define (blas:zhpr2 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9) 
  ((c-lambda
       (int int int scheme-object scheme-object int scheme-object int scheme-object)
       void 
     "cblas_zhpr2(___arg1, ___arg2, ___arg3, ___CAST(___F64*, ___BODY_AS(___arg4, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg5, ___tSUBTYPED)), ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)));")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Level 3 BLAS

(define (blas:sgemm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14) 
  ((c-lambda
       (int int int int int int float scheme-object int scheme-object int float scheme-object int)
       void 
     "cblas_sgemm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F32*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11, ___arg12, ___CAST(___F32*, ___BODY_AS(___arg13, ___tSUBTYPED)), ___arg14);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14)) 

(define (blas:ssymm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int float scheme-object int scheme-object int float scheme-object int)
       void 
     "cblas_ssymm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___arg11, ___CAST(___F32*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13)) 

(define (blas:ssyrk arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11) 
  ((c-lambda
       (int int int int int float scheme-object int float scheme-object int)
       void 
     "cblas_ssyrk(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___arg9, ___CAST(___F32*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11)) 

(define (blas:ssyr2k arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int float scheme-object int scheme-object int float scheme-object int)
       void 
     "cblas_ssyr2k(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___arg11, ___CAST(___F32*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13)) 

(define (blas:strmm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int int int int float scheme-object int scheme-object int)
       void 
     "cblas_strmm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F32*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:strsm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int int int int float scheme-object int scheme-object int)
       void 
     "cblas_strsm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F32*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:dgemm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14) 
  ((c-lambda
       (int int int int int int double scheme-object int scheme-object int double scheme-object int)
       void 
     "cblas_dgemm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F64*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11, ___arg12, ___CAST(___F64*, ___BODY_AS(___arg13, ___tSUBTYPED)), ___arg14);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14)) 

(define (blas:dsymm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int double scheme-object int scheme-object int double scheme-object int)
       void 
     "cblas_dsymm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___arg11, ___CAST(___F64*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13)) 

(define (blas:dsyrk arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11) 
  ((c-lambda
       (int int int int int double scheme-object int double scheme-object int)
       void 
     "cblas_dsyrk(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___arg9, ___CAST(___F64*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11)) 

(define (blas:dsyr2k arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int double scheme-object int scheme-object int double scheme-object int)
       void 
     "cblas_dsyr2k(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___arg11, ___CAST(___F64*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13)) 

(define (blas:dtrmm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int int int int double scheme-object int scheme-object int)
       void 
     "cblas_dtrmm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F64*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:dtrsm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int int int int double scheme-object int scheme-object int)
       void 
     "cblas_dtrsm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F64*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:cgemm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14) 
  ((c-lambda
       (int int int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_cgemm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F32*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11, ___CAST(___F32*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg13, ___tSUBTYPED)), ___arg14);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14)) 

(define (blas:csymm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_csymm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F32*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13)) 

(define (blas:csyrk arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int scheme-object scheme-object int)
       void 
     "cblas_csyrk(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11)) 

(define (blas:csyr2k arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_csyr2k(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F32*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13)) 

(define (blas:ctrmm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int int int int scheme-object scheme-object int scheme-object int)
       void 
     "cblas_ctrmm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F32*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:ctrsm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int int int int scheme-object scheme-object int scheme-object int)
       void 
     "cblas_ctrsm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___CAST(___F32*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F32*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:zgemm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14) 
  ((c-lambda
       (int int int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_zgemm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___arg9, ___CAST(___F64*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11, ___CAST(___F64*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg13, ___tSUBTYPED)), ___arg14);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14)) 

(define (blas:zsymm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_zsymm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F64*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13)) 

(define (blas:zsyrk arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int scheme-object scheme-object int)
       void 
     "cblas_zsyrk(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11)) 

(define (blas:zsyr2k arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_zsyr2k(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F64*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13)) 

(define (blas:ztrmm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int int int int scheme-object scheme-object int scheme-object int)
       void 
     "cblas_ztrmm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F64*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:ztrsm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12) 
  ((c-lambda
       (int int int int int int int scheme-object scheme-object int scheme-object int)
       void 
     "cblas_ztrsm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___arg7, ___CAST(___F64*, ___BODY_AS(___arg8, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F64*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___arg12);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12)) 

(define (blas:chemm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_chemm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F32*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13)) 

(define (blas:cherk arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11) 
  ((c-lambda
       (int int int int int float scheme-object int float scheme-object int)
       void 
     "cblas_cherk(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___arg9, ___CAST(___F32*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11)) 

(define (blas:cher2k arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int scheme-object int float scheme-object int)
       void 
     "cblas_cher2k(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F32*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F32*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F32*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___arg11, ___CAST(___F32*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13)) 

(define (blas:zhemm arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int scheme-object int scheme-object scheme-object int)
       void 
     "cblas_zhemm(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___CAST(___F64*, ___BODY_AS(___arg11, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13)) 

(define (blas:zherk arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11) 
  ((c-lambda
       (int int int int int double scheme-object int double scheme-object int)
       void 
     "cblas_zherk(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___arg6, ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___arg9, ___CAST(___F64*, ___BODY_AS(___arg10, ___tSUBTYPED)), ___arg11);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11)) 

(define (blas:zher2k arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13) 
  ((c-lambda
       (int int int int int scheme-object scheme-object int scheme-object int double scheme-object int)
       void 
     "cblas_zher2k(___arg1, ___arg2, ___arg3, ___arg4, ___arg5, ___CAST(___F64*, ___BODY_AS(___arg6, ___tSUBTYPED)), ___CAST(___F64*, ___BODY_AS(___arg7, ___tSUBTYPED)), ___arg8, ___CAST(___F64*, ___BODY_AS(___arg9, ___tSUBTYPED)), ___arg10, ___arg11, ___CAST(___F64*, ___BODY_AS(___arg12, ___tSUBTYPED)), ___arg13);")
   arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13))
