;;;; TWSHMLMXHash.scm
;;;; Kon Lovett, Jan '06

(module TWSHMLMXHash

  (;export
    *TWSHMLMXHash
    TWSHMLMXHash
    TWSHMLMXHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

static uint32_t
TWSHMLMXHash( uint8_t *data, uint32_t length, uint32_t key )
{
    /* Shift-Multiply 32 bit Mix Function */
#   define MIX( key ) { \
        (key) = ((key) ^ 61) ^ ((key) >> 16); \
        (key) = (key) + ((key) << 3); \
        (key) = (key) ^ ((key) >> 4); \
        (key) = (key) * 0x27D4EB2D; \
        (key) = (key) ^ ((key) >> 15); \
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

(gen-hash-api TWSHMLMXHash)
(gen-update-proc TWSHMLMXHash)
(gen-md-api TWSHMLMXHash)

) ;module TWSHMLMXHash
