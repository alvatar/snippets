;;;; NDJBHash.scm
;;;; Kon Lovett, Jan '06

(module NDJBHash

  (;export
    *NDJBHash
    NDJBHash
    NDJBHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/*
 * Now favored by Bernstein; the magic of number 33 (why it works better
 * than many other constants, prime or not) has never been adequately
 * explained.
 */

static uint32_t
NDJBHash( uint8_t *data, uint32_t length, uint32_t key )
{
    if (! key) key = 5381;

    if (data) {
        for ( ; length; data++, length--) {
            key = (key * 33) ^ ((uint32_t) *data);
        }
    }

    return key;
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api NDJBHash)
(gen-update-proc NDJBHash)
(gen-md-api NDJBHash)

) ;module NDJBHash
