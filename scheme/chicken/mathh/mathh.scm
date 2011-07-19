;;;; mathh.scm
;;;; Kon Lovett & John Cowen, '07 - '08
;;;; Kon Lovett, Mar '09

;;; Provides access to ISO C math functions in <math.h>
;;; that are not defined by the Chicken core.

;; Issues
;;
;; - Windows Visual C++ 2005 deprecates hypot, j0, j1, jn, y0, y1, yn
;; & renames _hypot, _j0, etc.
;;
;; - Windows does not provide log2, log1p, lgamma, tgamma. scalbn.
;;
;; - 'gamma' is deprecated in favor of 'tgamma' but not available
;; yet on some platforms, so we use 'gamma' for now.
;;
;; - Solaris log2 in <sunmath.h>.


;;; Prelude

(declare
	(usual-integrations)
  (inline)
  (local)
  (number-type generic)
  (no-procedure-checks)
  (bound-to-procedure
    ##sys#check-inexact ) )

#>
#include <math.h>
#include <float.h>
<#


;;; Unimplemented Support

(define-inline (%unimplemented-error name)
  (error name (##core#immutable '"this function is unavailable on this platform")) )


;;; Mathh

(module mathh (;export
  bessel-j0 bessel-j1 bessel-jn
  bessel-y0 bessel-y1 bessel-yn
  cosh sinh tanh
  hypot
  gamma tgamma lgamma
  log10 log2 make-log/base log1p
  fpmod modf ldexp scalbn frexp
  signbit copysign nextafter
  cbrt
  fpclassify
  fpclass)

(import scheme chicken foreign)

;;; Unimplemented Support

#; ;UNUSED
(define-syntax define-unimplemented
  (syntax-rules ()
    ((_ ?name)
     (define (?name . _) (%unimplemented-error ?name) ) ) ) )

(define-syntax lambda-unimplemented
  (syntax-rules ()
    ((_ ?name)
     (lambda _ (%unimplemented-error ?name) ) ) ) )

;;;

;; Bessel functions of the 1st kind

(define bessel-j0 (foreign-lambda double "j0" double))
(define bessel-j1 (foreign-lambda double "j1" double))
(define bessel-jn (foreign-lambda double "jn" int double))

;; Bessel functions of the 2nd kind

(define bessel-y0 (foreign-lambda double "y0" double))
(define bessel-y1 (foreign-lambda double "y1" double))
(define bessel-yn (foreign-lambda double "yn" int double))

;; Hyperbolic functions

(define cosh (foreign-lambda double "cosh" double))
(define sinh (foreign-lambda double "sinh" double))
(define tanh (foreign-lambda double "tanh" double))

;; Euclidean distance function

(define hypot (foreign-lambda double "hypot" double double))

;; Gamma function

(define gamma
  (cond-expand
    (windows  (lambda-unimplemented 'gamma) )
    (linux    (foreign-lambda double "tgamma" double) )
    (macosx   (foreign-lambda double "tgamma" double) )
    (else     (foreign-lambda double "gamma" double) ) ) )

(define tgamma gamma)

;; Ln Abs Gamma function

(define lgamma
  (cond-expand
    (windows  (lambda-unimplemented 'lgamma) )
    (else     (foreign-lambda double "lgamma" double) ) ) )

;; Base-10 logarithm

(define log10 (foreign-lambda double "log10" double))

;; Base-2 logarithm

(define log2
  (cond-expand
    ((or linux macosx bsd)
      (foreign-lambda double "log2" double) )
    (else
      (foreign-lambda* double ((double x)) "
        #ifndef M_LN2
        #define #define M_LN2 0.693147180559945309417232121458176568
        #endif
        return( log( x ) / M_LN2 );")
      #;
      (foreign-lambda* double ((double x)) "
        #ifndef M_PI
        # define M_PI 3.14159265358979323846264338327950288
        #endif
        #ifndef M_E
        # define M_E 2.71828182845904523536028747135266250 
        #endif
        return( (log( 2.0 * M_PI * x) / 2.0) + (x * log( x / M_E )) );") ) ) )

;; Natural logarithm of 1+x accurate for very small x

(define log1p
  (cond-expand
    (windows ; potentially inaccurate but ...
      (foreign-lambda* double ((double x)) "return( log( 1.0 + x ) );") )
    (else
		  (foreign-lambda double "log1p" double) ) ) )

;; Compute x * 2**n

(define ldexp (foreign-lambda double "ldexp" double integer))

;; Efficiently compute x * 2**n

(define scalbn
  (cond-expand
    (windows ; not efficient but ...
      (foreign-lambda* double ((double x) (integer n)) "return( ldexp( x, n ) );"))
    (else
      (foreign-lambda double "scalbn" double integer))) )

;; Log function for base n

(define (make-log/base b)
	(when (fixnum? b) (set! b (exact->inexact b)))
	(##sys#check-inexact b 'make-log/base)
	(cond ((fp= 2.0 b)    log2 )
        ((fp= 10.0 b)   log10 )
        (else
         (let ((lnb (log b)))
           (lambda (n)
             ((foreign-lambda* double ((double x) (double lnb)) "return( log( x ) / lnb );") n lnb)) ) ) ) )

;; Flonum remainder

(define fpmod (foreign-lambda double "fmod" double double))

;; Return integer & fraction (as multiple values) of a flonum

(define modf (foreign-primitive ((double x)) "
  double ipart;
  double result  = modf( x, &ipart );
  C_word* values = C_alloc( 2 * C_SIZEOF_FLONUM );
  C_word value1  = C_flonum( &values, ipart );
  C_word value2  = C_flonum( &values, result );
  C_values( 4, C_SCHEME_UNDEFINED, C_k, value1, value2 );
  ") )

;; Return mantissa & exponent (as multiple values) of a flonum

(define frexp (foreign-primitive ((double x)) "
  int exp;
  double result = frexp( x, &exp );
  C_word* values = C_alloc( C_SIZEOF_FLONUM );
  C_word value1 = C_flonum( &values, result );
  C_word value2 = C_fix( exp );
  C_values( 4, C_SCHEME_UNDEFINED, C_k, value1, value2 );
  ") )

;; Returns arg1 with same sign as arg2

(define copysign
  (cond-expand
    (windows (foreign-lambda double "_copysign" double double))
    (else (foreign-lambda double "copysign" double double)) ) )

;; Increments/decrements arg1 in the direction of arg2

(define nextafter
  (cond-expand
    (windows (foreign-lambda double "_nextafter" double double))
    (else (foreign-lambda double "nextafter" double double)) ) )

;; #t when negative, #f otherwise

(define signbit
  (cond-expand
    (windows (foreign-lambda* bool ((double n)) "return( _copysign( 1.0, n ) < 0 );"))
    (else (foreign-lambda bool "signbit" double)) ) )

;; Cube Root

(define cbrt
  (cond-expand
    (windows (lambda-unimplemented 'cbrt))
    (else (foreign-lambda double "cbrt" double)) ) )

;; Returns a symbol denoting the kind of floating-point number.

(define fpclass
  (cond-expand
    ((and windows (not cygwin))
      (foreign-lambda* symbol ((double x)) "
        char *name;
        switch (_fpclass( x )) {
        case _FPCLASS_SNAN:
          name = \"signaling-nan\";
          break;
        case _FPCLASS_QNAN:
          name = \"quiet-nan\";
          break;
        case _FPCLASS_NINF:
          name = \"negative-infinite\";
          break;
        case _FPCLASS_NN:
          name = \"negative-normal\";
          break;
        case _FPCLASS_ND:
          name = \"negative-subnormal\";
          break;
        case _FPCLASS_NZ:
          name = \"negative-zero\";
          break;
        case _FPCLASS_PZ:
          name = \"positive-zero\";
          break;
        case _FPCLASS_PD:
          name = \"positive-subnormal\";
          break;
        case _FPCLASS_PN:
          name = \"positive-normal\";
          break;
        case _FPCLASS_PINF:
          name = \"positive-infinite\";
          break;
        default:
          name = \"unclassified\";
          break;
        }
        return( name );") )
    (else
      (foreign-lambda* symbol ((double x)) "
        char *name;
        switch (fpclassify( x )) {
        case FP_INFINITE:
          name = x < 0 ? \"negative-infinite\" : \"positive-infinite\";
          break;
        case FP_NAN:
          /*FIXME A quiet nan can be distinguished by bit inspection*/
          name = \"signaling-nan\";
          break;
        case FP_NORMAL:
          name = x < 0 ? \"negative-normal\" : \"positive-normal\";
          break;
        case FP_SUBNORMAL:
          name = x < 0 ? \"negative-subnormal\" : \"positive-subnormal\";
          break;
        case FP_ZERO:
          name = signbit( x ) ? \"negative-zero\" : \"positive-zero\";
          break;
        default:
          name = \"unclassified\";
          break;
        }
        return( name );") ) ) )

;; Returns a symbol denoting the kind of floating-point number.

(define fpclassify
  (cond-expand
    ((and windows (not cygwin))
      (foreign-lambda* symbol ((double x)) "
        char *name;
        switch (_fpclass( x )) {
        case _FPCLASS_SNAN:
        case _FPCLASS_QNAN:
          name = \"nan\";
          break;
        case _FPCLASS_NINF:
        case _FPCLASS_PINF:
          name = \"infinite\";
          break;
        case _FPCLASS_NN:
        case _FPCLASS_PN:
          name = \"normal\";
          break;
        case _FPCLASS_ND:
        case _FPCLASS_PD:
          name = \"subnormal\";
          break;
        case _FPCLASS_NZ:
        case _FPCLASS_PZ:
          name = \"zero\";
          break;
        default:
          name = \"unclassified\";
          break;
        }
        return( name );") )
    (else
      (foreign-lambda* symbol ((double x)) "
        char *name;
        switch (fpclassify( x )) {
        case FP_INFINITE:
          name = \"infinite\";
          break;
        case FP_NAN:
          name = \"nan\";
          break;
        case FP_NORMAL:
          name = \"normal\";
          break;
        case FP_SUBNORMAL:
          name = \"subnormal\";
          break;
        case FP_ZERO:
          name = \"zero\";
          break;
        default:
          name = \"unclassified\";
          break;
        }
        return( name );") ) ) )

) ;module mathh
