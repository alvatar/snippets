;;;; BKDRHash.scm
;;;; Kon Lovett, Jan '06

(module BKDRHash

  (;export
    *BKDRHash
    BKDRHash
    BKDRHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/*
 * This hash function comes from Brian Kernighan and Dennis Ritchie's book "The C
 * Programming Language". It is a simple hash function using a strange set of
 * possible seeds which all constitute a pattern of 31....31...31 etc, it seems to
 * be very similar to the DJB hash function.
 */

static uint32_t
BKDRHash( uint8_t *data, uint32_t length, uint32_t key )
{
    if (data) {
        uint32_t seed = 131; /* 31 131 1313 13131 131313 etc.. */
        for ( ; length; data++, length--) {
            key = (key * seed) + ((uint32_t) *data);
        }
    }

    return key;
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api BKDRHash)
(gen-update-proc BKDRHash)
(gen-md-api BKDRHash)

) ;module BKDRHash
