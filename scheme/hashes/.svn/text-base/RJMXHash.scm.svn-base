;;;; RJMXHash.scm
;;;; Kon Lovett, Jan '06

(module RJMXHash

  (;export
    *RJMXHash
    RJMXHash
    RJMXHash-primitive )

  (import scheme chicken foreign)

  (use message-digest hashes-support hash-utils)

#>
#include "hashes.h"

/*
--------------------------------------------------------------------
lookup2.c, by Bob Jenkins, December 1996, Public Domain.
You can use this free for any purpose. It has no warranty.
--------------------------------------------------------------------
*/

/*
--------------------------------------------------------------------
hash() -- hash a variable-length key into a 32-bit value
  k     : the key (the unaligned variable-length array of bytes)
  len   : the length of the key, counting by bytes
  level : can be any 4-byte value
Returns a 32-bit value.  Every bit of the key affects every bit of
the return value.  Every 1-bit and 2-bit delta achieves avalanche.
About 36+6len instructions.

The best hash table sizes are powers of 2.  There is no need to do
mod a prime (mod is sooo slow!).  If you need less than 32 bits,
use a bitmask.  For example, if you need only 10 bits, do
  h = (h & hashmask(10));
In which case, the hash table should have hashsize(10) elements.

If you are hashing n strings (byte **)k, do it like this:
  for (i=0, h=0; i<n; ++i) h = hash( k[i], len[i], h);

By Bob Jenkins, 1996.  bob_jenkins@burtleburtle.net.  You may use this
code any way you wish, private, educational, or commercial.  It's free.

See http://burlteburtle.net/bob/hash/evahash.html
Use for hash table lookup, or anything where one collision in 2^32 is
acceptable.  Do NOT use for cryptographic purposes.
--------------------------------------------------------------------
*/

static uint32_t
RJMXHash( uint8_t *data, uint32_t length, uint32_t key )
{
    /*
    --------------------------------------------------------------------
    mix -- mix 3 32-bit values reversibly.
    For every delta with one or two bit set, and the deltas of all three
      high bits or all three low bits, whether the original value of a,b,c
      is almost all zero or is uniformly distributed,
    * If mix() is run forward or backward, at least 32 bits in a,b,c
      have at least 1/4 probability of changing.
    * If mix() is run forward, every bit of c will change between 1/3 and
      2/3 of the time.  (Well, 22/100 and 78/100 for some 2-bit deltas.)
    mix() was built out of 36 single-cycle latency instructions in a
      structure that could supported 2x parallelism, like so:
          a -= b;
          a -= c; x = (c>>13);
          b -= c; a ^= x;
          b -= a; x = (a<<8);
          c -= a; b ^= x;
          c -= b; x = (b>>13);
          ...
      Unfortunately, superscalar Pentiums and Sparcs can't take advantage
      of that parallelism. They've also turned some of those single-cycle
      latency instructions into multi-cycle latency instructions.  Still,
      this is the fastest good hash I could find. There were about 2^^68
      to choose from. I only looked at a billion or so.
    --------------------------------------------------------------------
    */

#   define MIX( a, b, c ) { \
        (a) -= (b); (a) -= (c); (a) ^= ((c) >> 13); \
        (b) -= (c); (b) -= (a); (b) ^= ((a) << 8); \
        (c) -= (a); (c) -= (b); (c) ^= ((b) >> 13); \
        (a) -= (b); (a) -= (c); (a) ^= ((c) >> 12); \
        (b) -= (c); (b) -= (a); (b) ^= ((a) << 16); \
        (c) -= (a); (c) -= (b); (c) ^= ((b) >> 5); \
        (a) -= (b); (a) -= (c); (a) ^= ((c) >> 3); \
        (b) -= (c); (b) -= (a); (b) ^= ((a) << 10); \
        (c) -= (a); (c) -= (b); (c) ^= ((b) >> 15); \
        }

#   define GOLDEN_RATIO 0x9E3779B9

    if (! key) key = length;

    if (data) {
        uint32_t a, b;

        /* Set up the internal state */
        a = b = GOLDEN_RATIO;    /* an arbitrary value */

        /*---------------------------------------- handle most of the key */
        while (length >= 12) {
              a += *((uint32_t *) (data + 0));
              b += *((uint32_t *) (data + 4));
            key += *((uint32_t *) (data + 8));
            MIX( a, b, key );
            data += 12;
            length -= 12;
        }

        /*------------------------------------- handle the last 11 bytes */
        switch (length) {
          /* all the case statements fall through */
        case 11: key += ((uint32_t) data[ 10 ]) << 24;
        case 10: key += ((uint32_t) data[ 9 ]) << 16;
        case 9 : key += ((uint32_t) data[ 8 ]) << 8;
          /* the first byte of key is reserved for the length */
        case 8 :   b += ((uint32_t) data[ 7 ]) << 24;
        case 7 :   b += ((uint32_t) data[ 6 ]) << 16;
        case 6 :   b += ((uint32_t) data[ 5 ]) << 8;
        case 5 :   b +=  (uint32_t) data[ 4 ];
        case 4 :   a += ((uint32_t) data[ 3 ]) << 24;
        case 3 :   a += ((uint32_t) data[ 2 ]) << 16;
        case 2 :   a += ((uint32_t) data[ 1 ]) << 8;
        case 1 :   a +=  (uint32_t) data[ 0 ];
          /* case 0: nothing left to add */
        }

        MIX( a, b, key );
    }

    /*-------------------------------------------- report the result */
    return key;

# undef MIX
}

#undef bitsizeof
<#

(include "hashes-macros")

(gen-hash-api RJMXHash)
(gen-update-proc RJMXHash)
(gen-md-api RJMXHash)

) ;module RJMXHash
