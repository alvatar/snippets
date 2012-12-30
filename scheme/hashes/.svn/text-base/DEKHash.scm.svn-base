;;;; DEKHash.scm
;;;; Kon Lovett, Jan '06

(module DEKHash

  (;export
    *DEKHash
    DEKHash
    DEKHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/*
 * An algorithm proposed by Donald E. Knuth in The Art Of Computer
 * Programming Volume 3, under the topic of sorting and search chapter
 * 6.4.
 */

static uint32_t
DEKHash( uint8_t *data, uint32_t length, uint32_t key )
{
    if (! key) key = length;

    if (data) {
        for ( ; length; data++, length--) {
            key = ((key << 5) ^ (key >> 27)) ^ ((uint32_t) *data);
        }
    }

    return key;
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api DEKHash)
(gen-update-proc DEKHash)
(gen-md-api DEKHash)

) ;module DEKHash
