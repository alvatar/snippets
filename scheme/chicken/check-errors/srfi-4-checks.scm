;;;; srfi-4-checks.scm
;;;; Kon Lovett, Dec '09

;;;

(module srfi-4-checks (;export
  check-s8vector
  check-u8vector
  check-s16vector
  check-u16vector
  check-s32vector
  check-u32vector
  #;check-s64vector
  #;check-u64vector
  check-f32vector
  check-f64vector)
  
  (import scheme
          chicken
          srfi-4
          (only type-checks define-check-type)
          srfi-4-errors)
 
  (require-library srfi-4 type-checks srfi-4-errors)
  
(define-check-type s8vector)
(define-check-type u8vector)
(define-check-type s16vector)
(define-check-type u16vector)
(define-check-type s32vector)
(define-check-type u32vector)
#;(define-check-type s64vector)
#;(define-check-type u64vector)
(define-check-type f32vector)
(define-check-type f64vector)

) ;module srfi-4-checks
