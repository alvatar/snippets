;;;; PYHash.scm
;;;; Kon Lovett, Jan '06

(module PYHash

  (;export
    *PYHash
    PYHash
    PYHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/* Python Hash */

static uint32_t
PYHash( uint8_t *data, uint32_t length, uint32_t key )
{
    if (! key) key = ((uint32_t) *data) << 7;

    if (data == NULL) {
        int i;

        for (i = 0; i < length; data++, i++) {
            key = (1000003 * key) ^ ((uint32_t) *data);
        }

        key ^= length;
    }

    return (((uint32_t) -1) == key) ? ((uint32_t) -2) : key;
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api PYHash)
(gen-update-proc PYHash)
(gen-md-api PYHash)

) ;module PYHash
