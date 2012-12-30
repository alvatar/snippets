;;;; TWSHMXHash.scm
;;;; Kon Lovett, Jan '06

(module TWSHMXHash

  (;export
    *TWSHMXHash
    TWSHMXHash
    TWSHMXHash-primitive)

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

static uint32_t
TWSHMXHash( uint8_t *data, uint32_t length, uint32_t key )
{
    /* Shift 32 bit Mix Function */
#   define MIX( key ) { \
        (key) = ~(key) + ((key) << 15); \
        (key) = (key) ^ ((key) >> 12); \
        (key) = (key) + ((key) << 2); \
        (key) = (key) ^ ((key) >> 4); \
        (key) = (key) * 2057; \
        (key) = (key) ^ ((key) >> 16); \
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

(gen-hash-api TWSHMXHash)
(gen-update-proc TWSHMXHash)
(gen-md-api TWSHMXHash)

) ;module TWSHMXHash
