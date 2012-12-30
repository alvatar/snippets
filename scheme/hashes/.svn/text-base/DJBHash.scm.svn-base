;;;; DJBHash.scm
;;;; Kon Lovett, Jan '06

(module DJBHash

  (;export
    *DJBHash
    DJBHash
    DJBHash-primitive)

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/*
 * An algorithm produced by Professor Daniel J. Bernstein and shown
 * first to the world on the usenet newsgroup comp.lang.c. It is one of
 * the most efficient hash functions ever published.
 */

static uint32_t
DJBHash( uint8_t *data, uint32_t length, uint32_t key )
{
    if (! key) key = 5381;

    if (data) {
        for ( ; length; data++, length--) {
            key = ((key << 5) + key) + ((uint32_t) *data);
        }
    }

    return key;
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api DJBHash)
(gen-update-proc DJBHash)
(gen-md-api DJBHash)

) ;module DJBHash
