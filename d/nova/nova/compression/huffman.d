//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.compression.huffman;

//
// Huffman coder/decoder
//
//  Example:
//   The input string "ABRACADABRA" has the following propabilities (frequency table):
//     A   5
//     B   2
//     C   1
//     D   1
//     R   2
//
//   After the construction phase one possible huffman tree is:
//
//          o
//         / \
//        A   o
//           / \
//          B   o
//             / \
//            R   o
//               / \
//              C   D
//
//   Possible huffman codes are:
//         Len                      InvCan   Canonical
//     A   1      0        1        0        1
//     B   2      10       01       10       01
//     C   4      1111     0001     1110     0000
//     D   4      1110     0000     1111     0001
//     R   3      110      001      110      001
//
//   The minimal number of bits the input string could be encoded
//   (aka. weighted outer pathlength) is the sum of the huffman code length
//   of each symbol in the input string.
//   Simply multibly the frequency of each symbol and the length of the
//   symbols huffman code and sum it up:
//
//     minbits = 1*5 + 2*2 + 4*1 + 4*1 + 3*2 = 21
//
//   The compressionratio is 21 * 100 / (11*8) = 23,8 %
//
//   Theoretically you have to transfer a huffman tree completely from a
//   encoder to a decoder to be able to decompress a compressed stream.
//   But if a encoder/decoder pair are using the same creation scheme all
//   you have to transfer are the codelengths of the huffman tree.
//   There are different possible creation schemes. Canonical huffman codes
//   is one that allows efficient decoding.
//
//   Canonical huffman codes follow these rules:
//    1. shorter codes have a numerical higher value, if filled with zeros
//       on the right
//    2. within the same length, numerical values increase with the alphabet
//
//   Many compressors in the wild seem to use what I call "inverted canonical" (InvCan).
//    1. shorter codes have a numerical lower value, if filled with zeros
//       on the right
//    2. the second rule is the same as for canonical hufmann codes
//
//-----------------------------------------------------------------------------
//
//   To calculate canononical huffman codes you have to calculate
//   the start values for each code length. This could be done by the
//   following algorithm:
//
//    1. Beginning with code 0 for the longest code length.
//    2. The next start value is determined by adding the number of codes for
//       this code length and shifting them to the left by one.
//    3. Do this for every code length down to lenght 1
//    4. The code for length 1 must be 1. If you get some other startcode
//       there is a bug in the given code length table.
//
//    Example:      Len                             startcode
//                  4                               0000
//                  3     (0000 + 10) = 0010 >> 1 = 001
//                  2     (001  +  1) = 010  >> 1 = 01
//                  1     (01   +  1) = 10   >> 1 = 1
//
//-----------------------------------------------------------------------------
//
//   To calculate inverted canononical huffman codes you have to calculate
//   the start values for each code length. This could be done by the
//   following algorithm:
//
//    1. Beginning with code 0 for the shortest code length.
//    2. The next start value is determined by adding the number of codes for
//       this code length and shifting them to the left by one.
//    3. Do this for every code length from shortest to longest
//    4. The code for the longest length must be all zero. If you get some
//       other startcode there is a bug in the given code length table.
//
//    Example:      Len                           startcode
//                  1                               0
//                  2     (0   +   1) = 1    << 1 = 10
//                  3     (10  +   1) = 11   << 1 = 110
//                  4     (110 +  10) = 1000 << 1 = 0000
//
//-----------------------------------------------------------------------------
//
//   With the table of calculated startcodes you could assign codes
//   to symbols by a simple iteration.
//
//-----------------------------------------------------------------------------
//
//   Using inverted canonical codes from above the string "ABRACADABRA" is
//   encoded as:
//    A B  R   A C    A D    A B  R   A
//    0 10 110 0 1110 0 1111 0 10 110 0  or
//
//    00110100 11110111 _0110100  = 0x34 0xF7 0x34
//
private import tango.core.Exception;

private import tango.io.model.IConduit;

private import nova.ds.priorityqueue;
private import nova.compression.bitio;

/// calculate canonical startcodes
void CanonicalStartCodes(int lencount[], int startcodes[])
in
{
  assert(lencount.length == startcodes.length);
}
out
{
  assert(startcodes[1] == 1);
}
body
{
  startcodes[startcodes.length-1] = 0;
  for (int idx = (lencount.length-1); idx > 0; idx--)
  {
    startcodes[idx-1] = (startcodes[idx] + lencount[idx]) >> 1;
  }
}

/// calculate invers canonical startcodes
void InversCanonicalStartCodes(int lencount[], int startcodes[])
in
{
  assert(lencount.length == startcodes.length);
}
body
{
  startcodes[1] = 0;
  for (int idx = 1; idx < (lencount.length-1); idx++)
  {
    startcodes[idx+1] = (startcodes[idx] + lencount[idx]) << 1;
  }
}

//
// Static Huffman encoder
//
// NUM_CODES: alphabet = {0, 1, 2, ..., NUM_CODES - 1}
//
class StaticHuffmanEncoder(int NUM_CODES)
{
  public:
   void dump()
   {
      Stdout("Encoder:").newline;
      foreach(int idx, len; mLength)
      {
        if (len)
        {
          Stdout.format("{0,5}: {} ", idx, len);
          for (ushort mask = (1 << (len-1)); mask > 0; mask >>= 1)
          {
            Stdout((mCode[idx] & mask) ? "1" : "0");
          }
          Stdout.newline;
        }
      }
   }

  private:
    ubyte   mLength [NUM_CODES];           // length and
    ushort  mCode   [NUM_CODES];           // code of huffmann codes

    //
    // Takes a field with symbol frequencies and creates the
    // huffman code/length tuples in mCode[]/mLength[]
    //
    void makeCodeFromFrequency( int     nsymbols,
                                int     frequency[])
    {
      PriorityQueue!(int) pq;     // priority queue to create prevalence tree

      // Create the huffman tree. Algorithm is taken from Sedgewicks "Algoritms in C++"
      // 1. Setup: Fill a priority queue with the symbols. Use the probability
      //           as priority
      int idx = 0;
      for (; idx < nsymbols; idx++)
      {
        if (frequency[idx])
        {
          pq.insert(frequency[idx], idx);
        }
      }

      // dad[] array contains the huffman tree
      int dad[2 * NUM_CODES - 1];
      for(;!pq.empty(); idx++)
      {
        // Remove the 2 least probable nodes from the priority queue.
        int code1 = pq.remove();
        int code2 = pq.remove();

        // Form a new intermediate node as parent of them.
        dad[idx]   = 0;
        dad[code1] = idx;
        dad[code2] = idx;

        // Probability of the new node is the sum of the probabilites
        // of the removed nodes.
        frequency[idx] = frequency[code1] + frequency[code2];

        // Insert the new node back into the priority queue.
        if (!pq.empty())
        {
          pq.insert(frequency[idx], idx);
        }

        // Repeat until only one node is left
      }

      // remember root
      int root = idx;

      // Now we reuse the dad[] array to determine the code lengths.
      // 1. Set code length of root to zero
      dad[root] = 0;

      // 2. Traverse intermediate nodes from the last to the first created
      //    and accumulate the length from the parent node.
      for (idx = root-1; idx >= nsymbols; idx--)
      {
        dad[idx] = dad[dad[idx]] + 1;
      }

      // 3. Traverse the leaves, determine the code length and remember
      //    how often a length had occured. We need that info to calculate
      //    the huffman codes in the next step.
      int lencount [18];
      for (; idx >= 0; idx--)
      {
        // check if leave has a parent
        if (dad[idx])
        {
          int len = dad[dad[idx]];

          mLength[idx] = len;
          lencount[len]++;
        }
      }

      // Calculate start codes for the given code lengths
      int startcodes[18];
      InversCanonicalStartCodes(lencount, startcodes);

      // 2. Traverse the symbols in ascending order and
      //    assign the appropriate code
      //
      for (idx = 0; idx < nsymbols; idx++)
      {
        int len = mLength[idx];
        if (len)
        {
          mCode[idx] = startcodes[len];
          startcodes[len]++;
        }
      }
    }

  public:
    //
    // Determines the symbol frequencies of a given
    // byte array and initializes the huffman encoder.
    //
    void makeHuffmanCode(ubyte buffer[])
    {
      // Receives the symbol count (frequency).
      // Note that this array is used to store the frequency
      // values of the intermediate nodes. Because of this we
      // have to expand the size to (2 * NUM_CODES - 1)
      int frequency[2 * NUM_CODES - 1];

      // initialize (only NUM_CODES necessary here, the rest is
      // initialized in makeHuffmanCode)
      for (int idx=0; idx < NUM_CODES; ++idx)
      {
        frequency[idx] = 0;
      }

      // determine symbol frequencies
      foreach(ch; buffer)
      {
        frequency[ch]++;
      }

      // create huffman codes
      makeCodeFromFrequency( NUM_CODES,
                             frequency );

    }

    //
    // Writes the length table of the huffman tree to
    // the given writer object.
    //
    void encodeLengthTable(OutputStream stream)
    {
      // TODO
    }

    //
    // Gets the length table
    //
    ubyte[] getLengthTable()
    {
      return mLength;
    }

    //
    // Encodes a symbol buffer and writes the
    // result to the given writer object.
    //
    void encode(ubyte buffer[], OutputStream stream)
    {
      auto writer = new BitWriter(stream);

      // encode buffer content
      foreach(ch; buffer)
      {
        writer.putbits(mLength[ch], mCode[ch]);
      }

      writer.flush();
    }

}


//
// Static Huffman decoder
//
// NUM_CODES: alphabet = {0, 1, 2, ..., NUM_CODES - 1}
//            includes the termination code
// TABLEBITS: bits of LUT to decode symbols
//
class StaticHuffmanDecoder(int NUM_CODES, int TABLEBITS)
{
  private:
    const  BITBUFSIZE = 16;
    const  TABLESIZE  = 1 << TABLEBITS;

    // contains the huffman tree
    ushort mLeft     [2 * NUM_CODES - 1];
    ushort mRight    [2 * NUM_CODES - 1];

    // lookup table to decode one huffman code per iteration
    ushort mTable    [TABLESIZE];

    // code lengths
    ushort mLength   [NUM_CODES];

  protected:
    //  Huffman decode routines
    int decodeSymbol(BitReader reader)
    {
      int code = reader.peekbits(TABLEBITS);
      int idx  = mTable[code];

      // check if code fits into the LUT
      if (idx >= NUM_CODES)
      {
        // checkbits needs a right aligned mask because
        ushort mask = 1;

       // traverse the huffman tree for the rest of the bits
        do
        {
          if (reader.checkbits(mask))
          {
            idx = mRight[idx];
          }
          else
          {
            idx = mLeft [idx];
          }
          mask <<= 1;
        }
        while (idx >= NUM_CODES);
      }

      // skip length bits
      code = reader.getbits(mLength[idx]);

      return idx;
    }

  public:
    this()
    {
    }

    //
    // Creates a table for decoding
    // codelengths:  length of codes
    //
    void makeHuffmanTable(ubyte codelengths[])
    {
      int    idx;

      // 1. Determine how often a length had occured. We need that info to
      //    calculate the huffman codes in the next step. This is the same
      //    calculation as in the encoder.
      // 2. Copy the length table in the same loop.
      int lencount[18];
      for (idx = 0; idx < codelengths.length; idx++)
      {
        lencount[codelengths[idx]]++;
        mLength[idx] = codelengths[idx];
      }

      // Calculate start codes for the given code lengths
      int startcodes[18];
      InversCanonicalStartCodes(lencount, startcodes);

      if (startcodes[16] != (1 << 16))
        throw new AssertException("Bad huffman code", __LINE__);

      // determine bits not covered by the LUT
      const ncbits = 16 - TABLEBITS;

      // Calculate start and weight for the codes
      ushort weight[17];
      for (idx = 1; idx <= TABLEBITS; idx++)
      {
        // left align startcodes
        startcodes[idx] <<= TABLEBITS - idx;

        // determine weight, used to align
        weight[idx]  = 1U << (TABLEBITS - idx);
      }
      while (idx <= 16)
      {
        weight[idx] = 1U << (16 - idx);
        idx++;
      }

      // clear the LUT
      idx = startcodes[TABLEBITS];
      if (idx != (1U << 16))
      {
        for (;idx != (1U << TABLEBITS); idx++)
          mTable[idx] = 0;
      }

      // for every code fill the LUT and create
      // the intermediate nodes of the huffman tree
      // for the bits not covered by the LUT
      uint avail = codelengths.length;
      uint mask  = 1U << (15 - TABLEBITS);

      for (int code = 0; code < codelengths.length; code++)
      {
        int   len = codelengths[code];
        if (0 == len)
          continue;

        uint nextcode = startcodes[len] + weight[len];

        if (len <= TABLEBITS)
        {
          // code in table
          if(nextcode > TABLESIZE)
            throw new ArrayBoundsException("Huffman-Table exceeded", __LINE__);

          // init LUT entries for this code
          for (idx = startcodes[len]; idx < nextcode; idx++)
          {
            mTable[idx] = code;
          }
        }
        else
        {
          // code not completely in table
          uint    curcode = startcodes[len];
          ushort *p       = &mTable[curcode];
          idx = len - TABLEBITS;

          // create the huffman tree
          // for the bits not covered by the LUT
          while (idx != 0)
          {
            if (*p == 0)
            {
              mRight[avail] = mLeft[avail] = 0;
              *p = avail++;
            }
            if (curcode & mask)
            {
              p = &mRight[*p];
            }
            else
            {
              p = &mLeft[*p];
            }
            curcode <<= 1;
            idx--;
          }
          *p = code;
        }
        startcodes[len] = nextcode;
      }
    }

    //
    // decode a huffman encoded sequence
    // reader        : Reader to read the sequence from.
    // unpackedlength: Expected length of unpacked symbol sequence.
    //                 -1 means reading until termination code occurs
    //
    ubyte[] decode(InputStream stream, int unpackedlength)
    {
      auto reader = new BitReader(stream);
      ubyte[] result;

      if (-1 == unpackedlength)
      {
        // read until termination code occurs
        int ch = decodeSymbol(reader);
        while (NUM_CODES != ch)
        {
          result ~= cast(ubyte)ch;
          ch = decodeSymbol(reader);
        }
      }
      else
      {
        // read till unpackedlength reached
        while (unpackedlength > 0)
        {
          int ch = decodeSymbol(reader);
          result ~= cast(ubyte)ch;
          --unpackedlength;
        }
      }
      return result;
    }
}

private import tango.io.Buffer;
private import tango.util.log.Log;
private import tango.io.Stdout;

unittest
{
  auto logger = Log.getLogger("unittests.huffman");
  logger.info("Start unittest");

  auto encoder = new StaticHuffmanEncoder!(256);

  ubyte[] source = cast(ubyte[])"ABRACADABRA";

  auto buffer = new Buffer(256);

  encoder.makeHuffmanCode(source);
  encoder.dump();
  encoder.encode(source, buffer);

  //
  // decode
  //
  auto decoder = new StaticHuffmanDecoder!(256, 4);

  ubyte lengths[] = encoder.getLengthTable();
  decoder.makeHuffmanTable(lengths);

  ubyte[] result = decoder.decode(buffer, source.length);

  assert(result == source);

  logger.info("Done");
}
