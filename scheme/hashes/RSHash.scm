;;;; RSHash.scm
;;;; Kon Lovett, Jan '06

(module RSHash

  (;export
    *RSHash
    RSHash
    RSHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/*
 **************************************************************************
 *                                                                        *
 *          General Purpose Hash Function Algorithms Library              *
 *                                                                        *
 * Author: Arash Partow - 2002                                            *
 * URL: http://www.partow.net                                             *
 * URL: http://www.partow.net/programming/hashfunctions/index.html        *
 *                                                                        *
 * Copyright notice:                                                      *
 * Free use of the General Purpose Hash Function Algorithms Library is    *
 * permitted under the guidelines and in accordance with the most current *
 * version of the Common Public License.                                  *
 * http://www.opensource.org/licenses/cpl.php                             *
 *                                                                        *
 **************************************************************************
*/

/* A simple hash function from Robert Sedgwicks Algorithms in C book.
I've added some simple optimizations to the algorithm in order to speed
up its hashing process. */

static uint32_t
RSHash(uint8_t *data, uint32_t length, uint32_t key )
{
#   define A 63689U
#   define B 378551U

    if (data) {
        uint32_t a = A;
        for ( ; length; data++, length--) {
            key = key * a + ((uint32_t) *data);
            a *= B;
        }
    }

    return key;

#   undef B
#   undef A
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api RSHash)
(gen-update-proc RSHash)
(gen-md-api RSHash)

) ;module RSHash
