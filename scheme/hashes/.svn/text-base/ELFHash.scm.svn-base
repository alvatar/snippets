;;;; ELFHash.scm
;;;; Kon Lovett, Jan '06

(module ELFHash

  (;export
    *ELFHash
    ELFHash
    ELFHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/*
 * Similar to the PJW Hash function, but tweaked for 32-bit processors.
 * It's the hash function widely used on most UNIX systems.
 */

static uint32_t
ELFHash( uint8_t *data, uint32_t length, uint32_t key )
{
    if (data) {
        for ( ; length; data++, length--) {
            uint32_t test;
            key = (key << 4) + ((uint32_t) *data);
            if ((test = (key & 0xF0000000L))) {
                key ^= (test >> 24);
                key &= ~test;
            }
        }
    }

    return key;
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api ELFHash)
(gen-update-proc ELFHash)
(gen-md-api ELFHash)

) ;module ELFHash
