;;;; APHash.scm
;;;; Kon Lovett, Jan '06

(module APHash

  (;export
    *APHash
    APHash
    APHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/*
 * An algorithm produced by me Arash Partow.
 *
 * I took ideas from all of the above hash functions making a hybrid rotative and
 * additive hash function algorithm based around four primes 3,5,7 and 11. There
 * isn't any real mathematical analysis explaining why one should use this hash
 * function instead of the others described above other than the fact that I tired
 * to resemble the design as close as possible to a simple LFSR. An empirical
 * result which demonstrated the distributive abilities of the hash algorithm was
 * obtained using a hash-table with 100003 buckets, hashing The Project Gutenberg
 * E-text of Webster's Unabridged Dictionary, the longest encountered chain length
 * was 7, the average chain length was 2, the number of empty buckets was 4579.
*/

static uint32_t
APHash( uint8_t *data, uint32_t length, uint32_t key )
{
    if (data) {
        int i;
        for (i = 0; i < length; data++, i++) {
            key ^= ((i & 1) == 0) /* even? */
                      ? (  (key <<  7) ^ ((uint32_t) *data) ^ (key >> 3))
                      : (~((key << 11) ^ ((uint32_t) *data) ^ (key >> 5)));
        }
    }

    return key;
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api APHash)
(gen-update-proc APHash)
(gen-md-api APHash)

) ;module APHash
