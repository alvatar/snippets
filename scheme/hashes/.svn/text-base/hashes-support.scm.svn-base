;;;; hashes-support.scm
;;;; Kon Lovett, Jan '06
;;;; Kon Lovett, Aug '10

;; Issues
;;
;; - This is cavalier w/ alignment, assumes uint8_t * arguments on
;; a 32-bit boundry. Chicken heap & nursery object data are so
;; constrained.

;; TBD
;;
;; - LITTLE/BIG-ENDIAN versions of all hash functions are not provided.
;;
;; - 64-bit versions of alll hash functions are not provided.
;;
;; - 64-bit optimization: Chicken object data is 64-bit aligned so
;; could process data to hash in 64-bit chunks, rather than 32-bit.
;;
;; - Better initval handling, what ever that means.

(module hashes-support

  (;export
    hash-seed
    unsigned-integer32-set!
    unsigned-integer32-ref
    hash-context-size
    ctx-hash-ref
    ctx-hash-set!
    generic-init
    generic-final)

  (import scheme chicken foreign)

  (use lolevel)

#>
#include "hashes.h"
#undef bitsizeof
<#

(include "hashes-macros")

;;;

;;

(define u32-cptr-ref
  (foreign-lambda* unsigned-integer32 ((c-pointer dat) (int idx))
    "C_return( ((uint32_t *) dat)[idx] );"))

(define u32-cptr-set!
  (foreign-lambda* void ((c-pointer dat) (unsigned-integer32 w32) (int idx))
    "((uint32_t *) dat)[idx] = (uint32_t) w32;"))

(define u32-sptr-ref
  (foreign-lambda* unsigned-integer32 ((scheme-pointer dat) (int idx))
    "C_return( ((uint32_t *) dat)[idx] );"))

(define u32-sptr-set!
  (foreign-lambda* void ((scheme-pointer dat) (unsigned-integer32 w32) (int idx))
    "((uint32_t *) dat)[idx] = (uint32_t) w32;"))

(define-inline (ptrtyp-if obj cptr-proc sptr-proc)
 (if (or (pointer? obj) (locative? obj)) cptr-proc
    sptr-proc ) )

(define-inline (%unsigned-integer32-set! obj num idx)
  ((ptrtyp-if obj u32-cptr-set! u32-sptr-set!) obj num idx) )

(define-inline (%unsigned-integer32-ref obj idx)
  ((ptrtyp-if obj u32-cptr-ref u32-sptr-ref) obj idx) )

;;;

(define +hash-seed+ DEFAULT-HASH-SEED)

(define (hash-seed #!optional seed)
  (if seed (set! +hash-seed+ seed)
    +hash-seed+ ) )

;;;

(define (unsigned-integer32-set! obj num #!optional (idx 0))
  (%unsigned-integer32-set! obj num idx) )

(define (unsigned-integer32-ref obj #!optional (idx 0))
  (%unsigned-integer32-ref obj idx) )

;;

(define hash-context-size (foreign-value "sizeof( hashctx )" int))

(define ctx-hash-ref
  (foreign-lambda* unsigned-integer32 ((c-pointer ctx))
   "return (((hashctx *) ctx)->hash);") )

(define ctx-hash-set!
  (foreign-lambda* void ((c-pointer ctx) (unsigned-integer32 val))
   "((hashctx *) ctx)->hash = val;"))

;;

(define (generic-init ctx)
	(ctx-hash-set! ctx +hash-seed+) )

(define (generic-final ctx result)
	(%unsigned-integer32-set! result (ctx-hash-ref ctx) 0) )

) ;module hashes-support
