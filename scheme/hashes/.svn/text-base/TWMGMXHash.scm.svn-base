;;;; TWMGMXHash.scm
;;;; Kon Lovett, Jan '06

(module TWMGMXHash

  (;export
    *TWMGMXHash
    TWMGMXHash
    TWMGMXHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

static uint32_t
TWMGMXHash( uint8_t *data, uint32_t length, uint32_t key )
{
    /* Merge 32 bit Mix Function */
#   define MIX( key ) { \
        (key) = ((key) + 0x7ED55D16) + ((key) << 12); \
        (key) = ((key) ^ 0xC761C23C) ^ ((key) >> 19); \
        (key) = ((key) + 0x165667B1) + ((key) << 5); \
        (key) = ((key) + 0xD3A2646C) ^ ((key) << 9); \
        (key) = ((key) + 0xFD7046C5) + ((key) << 3); \
        (key) = ((key) ^ 0xB55A4F09) ^ ((key) >> 16); \
        }

    if (data) {

        while (length >= sizeof( uint32_t )) {
            key += *((uint32_t *) data);
            MIX( key );
            data += sizeof( uint32_t );
            length -= sizeof( uint32_t );
        }

        switch (length) {
          /* all the case statements fall through */
        case 3 : key += (((uint32_t) data[ 2 ]) << 16);
        case 2 : key += (((uint32_t) data[ 1 ]) << 8);
        case 1 : key += (uint32_t) data[ 0 ];
          /* case 0: nothing left to add */
        }
        MIX( key );
    }

    return key;

#   undef MIX
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api TWMGMXHash)
(gen-update-proc TWMGMXHash)
(gen-md-api TWMGMXHash)

) ;module TWMGMXHash
