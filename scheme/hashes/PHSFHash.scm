;;;; PHSFHash.scm
;;;; Kon Lovett, Jan '06

(module PHSFHash

  (;export
    *PHSFHash
    PHSFHash
    PHSFHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/* Paul Hsieh's SuperFast */

static uint32_t
PHSFHash( uint8_t *data, uint32_t length, uint32_t key )
{
#   define BITS16_REF( d ) (*((uint16_t *) (d)))

    if (! key) key = length;

    if (data) {
        uint32_t remlen = length & 3;

        length >>= 2;

        /* Main loop */
        for (; length; length--) {
            uint32_t tmp;
            key  += BITS16_REF( data );
            tmp   = (BITS16_REF( data + sizeof( uint16_t ) ) << 11) ^ key;
            key   = (key << sizeof( uint16_t )) ^ tmp;
            data += 2 * sizeof( uint16_t );
            key  += key >> 11;
        }

        /* Handle end cases */
        switch (remlen) {
            case 3: key += BITS16_REF( data );
                    key ^= key << 16;
                    key ^= data[ sizeof( uint16_t ) ] << 18;
                    key += key >> 11;
                    break;
            case 2: key += BITS16_REF( data );
                    key ^= key << 11;
                    key += key >> 17;
                    break;
            case 1: key += *data;
                    key ^= key << 10;
                    key += key >> 1;
        }

        /* Force "avalanching" of final 127 bits */
        key ^= key << 3;
        key += key >> 5;
        key ^= key << 2;
        key += key >> 15;
        key ^= key << 10;
    }

    return key;

#   undef BITS16_REF
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api PHSFHash)
(gen-update-proc PHSFHash)
(gen-md-api PHSFHash)

) ;module PHSFHash
