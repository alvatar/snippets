;;;; PJWHash.scm
;;;; Kon Lovett, Jan '06

(module PJWHash

  (;export
    *PJWHash
    PJWHash
    PJWHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/* This hash algorithm is based on work by Peter J. Weinberger of AT&T Bell Labs. */

static uint32_t
PJWHash( uint8_t *data, uint32_t length, uint32_t key )
{
    if (data) {
        for ( ; length; data++, length--) {
            uint32_t test;
            key = (key << 2) + ((uint32_t) *data);
            if ((test = (key & 0xC000))) {
                key = ((key ^ (test >> 12)) & 0x3FFF);
            }
        }
    }

    return key;
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api PJWHash)
(gen-update-proc PJWHash)
(gen-md-api PJWHash)

) ;module PJWHash
