;;;; BRPHash.scm
;;;; Kon Lovett, Jan '06

(module BRPHash

  (;export
    *BRPHash
    BRPHash
    BRPHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/* Bruno R. Preiss */

static uint32_t
BRPHash( uint8_t *data, uint32_t length, uint32_t key )
{
#   define MASK (((uint32_t) (~0)) << (bitsizeof( uint32_t ) - 6))

    if (data) {
        for ( ; length; data++, length--) {
            key = (key & MASK) ^ (key << 6) ^ ((uint32_t) *data);
        }
    }

    return key;

#   undef MASK
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api BRPHash)
(gen-update-proc BRPHash)
(gen-md-api BRPHash)

) ;module BRPHash
