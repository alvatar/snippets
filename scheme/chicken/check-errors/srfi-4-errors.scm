;;;; srfi-4-errors.scm
;;;; Kon Lovett, Dec '09

;;;

(module srfi-4-errors (;export
	error-s8vector
	error-u8vector
	error-s16vector
	error-u16vector
	error-s32vector
	error-u32vector
	#;error-s64vector
	#;error-u64vector
	error-f32vector
	error-f64vector)
  
  (import scheme
          chicken
          srfi-4
          (only type-errors define-error-type))
 
  (require-library srfi-4 type-errors)
  
(define-error-type s8vector)
(define-error-type u8vector)
(define-error-type s16vector)
(define-error-type u16vector)
(define-error-type s32vector)
(define-error-type u32vector)
#;(define-error-type s64vector)
#;(define-error-type u64vector)
(define-error-type f32vector)
(define-error-type f64vector)

) ;module srfi-4-errors
