;;;; TWUserMixHash.scm
;;;; Kon Lovett, Feb '08
;;;; Kon Lovett, Aug '10

(module TWUserMixHash

  (;export
    make-TWUserMixHash-primitive-procedure
    make-TWUserMixHash)

  (import scheme chicken foreign type-checks)

  (use hash-utils type-checks)

;;;

#>
#include "hashes.h"

static uint32_t
TWUserMixHash( uint8_t *data, uint32_t length, uint32_t key, C_word mixer )
{
    /* User 32-bit Mix Function Callback (as if `define-external) */
#   define MIX( key ) do { \
        C_word num; \
        C_word *ptr; \
        C_word buf[ C_SIZEOF_FLONUM ]; \
        C_callback_adjust_stack( buf, C_SIZEOF_FLONUM ); \
        ptr = buf; \
        num = C_unsigned_int_to_num( &ptr, (key) ); \
        C_save( num ); \
        num = C_callback( mixer, 1 ); \
        (key) = C_num_to_unsigned_int( num ); \
        } while (0)

    if (data) {
        /* Full word(s) */
        while (length >= sizeof( uint32_t )) {
            /* Chicken data on at least 32-bit boundary */
            key += *((uint32_t *) data);
            MIX( key );
            data += sizeof( uint32_t );
            length -= sizeof( uint32_t );
        }
        /* Partial word */
        if (length) {
            switch (length) {
            /* all the case statements fall through */
            case 3: key += (uint32_t) data[ 2 ] << 16;
            case 2: key += (uint32_t) data[ 1 ] << 8;
            case 1: key += (uint32_t) data[ 0 ];
            }
            MIX( key );
        }
    }

    return key;

#   undef MIX
}

#undef bitsizeof
<#

;;;

(define *TWUserMixHash
  (foreign-safe-lambda unsigned-integer32 "TWUserMixHash"
    scheme-pointer unsigned-integer32 unsigned-integer32 scheme-object))

;;;

;; Signatures:
;;
;; mix-procedure
;; (unsigned-integer32 -> unsigned-integer32)
;;
;; hash-primitive-procedure
;; (scheme-object unsigned-integer32 unsigned-integer32 -> unsigned-integer32)
;;
;; hash-procedure
;; (scheme-object #!optional unsigned-integer32 unsigned-integer32 -> unsigned-integer32)

;; Takes 1 argument:
;; mix-procedure
;;
;; Returns 1 value:
;; hash-primitive-procedure

(define (make-TWUserMixHash-primitive-procedure mix-proc #!optional unsafe?)
  (check-procedure 'make-TWUserMixHash-primitive-procedure mix-proc)
  (if unsafe? (cut *TWUserMixHash <> <> <> mix-proc)
    (let* ((*last-exception* #f)
           (mixer (lambda (key)
                    (handle-exceptions ex
                        (begin (set! *last-exception* ex) 0)
                      (set! *last-exception* #f)
                      (mix-proc key) ) ) ) )
      (lambda (data length initval)
        (let ((key (*TWUserMixHash data length initval mixer)))
          (if *last-exception* (abort *last-exception*)
              key ) ) ) ) ) )

;; Takes 1 argument:
;; mix-procedure
;;
;; Returns 3 values:
;; hash-primitive-procedure
;; hash-procedure
;; message-digest-primitive

(define (make-TWUserMixHash mix-proc #!optional unsafe?)
  (let ((prim-proc (make-TWUserMixHash-primitive-procedure mix-proc unsafe?)))
      (values
        prim-proc
        (make-hash-procedure prim-proc)
        (make-hash-message-digest-primitive prim-proc 'TWUserMixHash)) ) )

) ;module TWUserMixHash
