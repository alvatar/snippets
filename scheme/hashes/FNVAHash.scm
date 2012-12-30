;;;; FNVAHash.scm
;;;; Kon Lovett, Jan '06

(module FNVAHash

  (;export
    *FNVAHash
    FNVAHash
    FNVAHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/* Fowler/Noll/Vo hash
 *
 * The basis of this hash algorithm was taken from an idea sent
 * as reviewer comments to the IEEE POSIX P1003.2 committee by:
 *
 *      Phong Vo (http://www.research.att.com/info/kpv/)
 *      Glenn Fowler (http://www.research.att.com/~gsf/)
 *
 * In a subsequent ballot round:
 *
 *      Landon Curt Noll (http://www.isthe.com/chongo/)
 *
 * improved on their algorithm.  Some people tried this hash
 * and found that it worked rather well.  In an EMail message
 * to Landon, they named it the ``Fowler/Noll/Vo'' or FNV hash.
 *
 * FNV hashes are designed to be fast while maintaining a low
 * collision rate. The FNV speed allows one to quickly hash lots
 * of data while maintaining a reasonable collision rate.  See:
 *
 *      http://www.isthe.com/chongo/tech/comp/fnv/index.html
 *
 * for more details as well as other forms of the FNV hash.
 *
 * Please do not copyright this code.  This code is in the public domain.
 *
 * LANDON CURT NOLL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO
 * EVENT SHALL LANDON CURT NOLL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * By:
 *	chongo <Landon Curt Noll> /\oo/\
 *      http://www.isthe.com/chongo/
 *
 * Share and Enjoy!	:-)
 */

/*
 * 32 bit FNV-1 and FNV-1a non-zero initial basis
 *
 * The FNV-1 initial basis is the FNV-0 hash of the following 32 octets:
 *
 *              chongo <Landon Curt Noll> /\../\
 *
 * NOTE: The \'s above are not back-slashing escape characters.
 * They are literal ASCII  backslash 0x5c characters.
 *
 * NOTE: The FNV-1a initial basis is the same value as FNV-1 by definition.
 */

/*
	Useful with the gcc compiler with -O3 on many AMD & Intel CPUs.
	Undefine if this is the case.
*/

#define NO_FNV_GCC_OPTIMIZATION

static uint32_t
FNVAHash( uint8_t *data, uint32_t length, uint32_t key )
{
#   define FNV1_32A_INIT  ((uint32_t)0x811c9dc5)
#   define FNV_32_PRIME		((uint32_t)0x01000193)

    if (! key) key = FNV1_32A_INIT;

    if (data) {
        uint8_t *be = data + length;	/* beyond end of buffer */

        /* FNV-1a hash each octet in the buffer */
        while (data < be) {

            /* xor the bottom with the current octet */
            key ^= (uint32_t) *data++;

            /* multiply by the 32 bit FNV magic prime mod 2^32 */
#ifdef NO_FNV_GCC_OPTIMIZATION
            key *= FNV_32_PRIME;
#else
            key += (key << 1) + (key << 4) + (key << 7) + (key << 8) + (key << 24);
#endif
        }
    }

    /* return our new hash value */
    return key;

#   undef FNV_32_PRIME
#   undef FNV1_32A_INIT
}

#undef NO_FNV_GCC_OPTIMIZATION

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api FNVAHash)
(gen-update-proc FNVAHash)
(gen-md-api FNVAHash)

) ;module FNVAHash
