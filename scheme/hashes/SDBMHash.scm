;;;; SDBMHash.scm
;;;; Kon Lovett, Jan '06

(module SDBMHash

  (;export
    *SDBMHash
    SDBMHash
    SDBMHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/* This is the algorithm of choice which is used in the open source SDBM
 project. The hash function seems to have a good over-all distribution
for many different data sets. It seems to work well in situations where
there is a high variance in the MSBs of the elements in a data set. */

static uint32_t
SDBMHash( uint8_t *data, uint32_t length, uint32_t key )
{
    if (data) {
        for ( ; length; data++, length--) {
            key = ((uint32_t) *data) + (key << 6) + (key << 16) - key;
        }
    }

    return key;
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api SDBMHash)
(gen-update-proc SDBMHash)
(gen-md-api SDBMHash)

) ;module SDBMHash
