;;;; JSHash.scm
;;;; Kon Lovett, Jan '06

(module JSHash

  (;export
    *JSHash
    JSHash
    JSHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/* A bitwise hash function written by Justin Sobel */

static uint32_t
JSHash( uint8_t *data, uint32_t length, uint32_t key )
{
    if (! key) key = 1315423911;

    if (data) {
        for ( ; length; data++, length--) {
            key ^= (key << 5) + ((uint32_t) *data) + (key >> 2);
        }
    }

    return key;
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api JSHash)
(gen-update-proc JSHash)
(gen-md-api JSHash)

) ;module JSHash
