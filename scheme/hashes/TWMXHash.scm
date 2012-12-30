;;;; TWMXHash.scm
;;;; Kon Lovett, Jan '06

(module TWMXHash

  (;export
    *TWMXHash
    TWMXHash
    TWMXHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

static uint32_t
TWMXHash( uint8_t *data, uint32_t length, uint32_t key )
{
    /* Thomas Wang's 32 bit Mix Function */
# ifdef C_SIXTY_FOUR
#   define MIX( key ) { \
        (key) = ~(key) + ((key) << 21); \
        (key) ^= (key) >> 24; \
        (key) += ((key) << 3) + ((key) << 8); \
        (key) ^= (key) >> 14; \
        (key) += ((key) << 2) + ((key) << 4); \
        (key) ^= (key) >> 28; \
        (key) += (key) << 31; \
        }
# else
#   define MIX( key ) { \
        (key) += ~((key) << 15); \
        (key) ^= ((key) & 0x7FFFFFFF) >> 10; \
        (key) += (key) << 3; \
        (key) ^= ((key) & 0x7FFFFFFF) >> 6; \
        (key) += ~((key) << 11); \
        (key) ^= ((key) & 0x7FFFFFFF) >> 16; \
        }
# endif

    if (data) {

# ifdef C_SIXTY_FOUR
        uint64_t hash = key;
        while (length >= sizeof( uint64_t )) {
            hash += *((uint64_t *) data);
            MIX( hash );
            data += sizeof( uint64_t );
            length -= sizeof( uint64_t );
        }

        switch (length) {
          /* all the case statements fall through */
        case 7 : hash += (((uint64_t) data[ 6 ]) << 48);
        case 6 : hash += (((uint64_t) data[ 5 ]) << 40);
        case 5 : hash += (((uint64_t) data[ 4 ]) << 32);
        case 4 : hash += (((uint64_t) data[ 3 ]) << 24);
        case 3 : hash += (((uint64_t) data[ 2 ]) << 16);
        case 2 : hash += (((uint64_t) data[ 1 ]) << 8);
        case 1 : hash += (uint64_t) data[ 0 ];
          /* case 0: nothing left to add */
        }
        MIX( hash );
        key = (uint32_t) hash;
# else
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
# endif
    }

    return key;

#   undef MIX
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api TWMXHash)
(gen-update-proc TWMXHash)
(gen-md-api TWMXHash)

) ;module TWMXHash
