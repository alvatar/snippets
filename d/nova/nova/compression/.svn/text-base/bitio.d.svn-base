//////////////////////////////////////////////////////////////////////////
/**

  Autors:    Klaus Oberhofer
  License:   zlib/libpng license
  Copyright: 2005-2007 Klaus Oberhofer

  Nova - a library for (un)common data structures and algorithms in D

*/////////////////////////////////////////////////////////////////////////
module nova.compression.bitio;

private import tango.core.Exception;
private import tango.io.Buffer;

private import tango.util.log.Log;

private import tango.stdc.limits;

public class BitReader
{
  private:
    // the buffer associated with this reader. Note that this
    // should not change over the lifetime of the reader, since
    // it is assumed to be immutable elsewhere
    IBuffer     buffer;

    int         bitcount;
    ushort      bitbuf;
    ubyte       subbitbuf;

    ushort      readErrors;

    const       BITBUFBITS = bitbuf.sizeof * CHAR_BIT;

    invariant
    {
      assert(bitcount <= 16);
      assert(bitcount >= 0);
    }

  public:
    this (InputStream stream)
    {
      auto b = cast(IBuffer) stream;
      if (b is null)
          b = new Buffer (stream.conduit);
      buffer = b;

      // reset read errors
      readErrors = 0;

      // initialize input buffer
      bitbuf    = 0;
      subbitbuf = 0;
      bitcount  = 0;
      fillbuf(BITBUFBITS);
    }

    /// end of input reached
    bool eoi()
    {
      return (readErrors >= 5);
    }

    /// get n bits (0 <= n <= 16)
    ushort getbits (int n)
    in
    {
      assert((n >= 0) && (n <= 16));
    }
    body
    {
      ushort retval = bitbuf >> (BITBUFBITS - n);
      fillbuf(n);
      return retval;
    }

    /// peek at n bits (0 <= n <= 16)
    ushort peekbits (int n)
    in
    {
      assert((n >= 0) && (n <= 16));
    }
    body
    {
      return bitbuf >> (BITBUFBITS - n);
    }

    /// check bits against a mask
    ushort checkbits  (ushort mask)
    {
      return (bitbuf & mask);
    }

  protected:
    /// ensure that n valid bits are in the input buffer
    void fillbuf (int n)
    in
    {
      assert((n >= 0) && (n <= 16));
    }
    body
    {
      bitbuf <<= n;
      while (n > bitcount)
      {
        bitbuf |= subbitbuf << (n -= bitcount);
        try
        {
          buffer.readExact(&subbitbuf, subbitbuf.sizeof);
        }
        catch (IOException ex)
        {
          subbitbuf = 0;
          readErrors++;
        }

        bitcount = CHAR_BIT;
      }
      bitbuf |= subbitbuf >> (bitcount -= n);
    }
}

unittest
{
  auto logger = Log.getLogger("unittests.BitReader");
  logger.info("Start unittest");

  const ubyte source[] =
  [
    0x12,0x34,0x56,0x78
  ];

  auto buffer = new Buffer(source);

  auto reader = new BitReader(buffer);
  assert(0x1 == reader.getbits(4));
  assert(0x2 == reader.peekbits(4));
  assert(0x2 == reader.getbits(4));
  assert(0x3 == reader.getbits(4));
  assert(0x4 == reader.peekbits(4));
  assert(0x4 == reader.getbits(4));
  assert(0x5 == reader.getbits(4));
  assert(0x6 == reader.getbits(4));
  assert(0x7 == reader.getbits(4));
  assert(0x8 == reader.getbits(4));

  logger.info("Done");
}

public class BitWriter
{
  private:
    // the buffer associated with this reader. Note that this
    // should not change over the lifetime of the reader, since
    // it is assumed to be immutable elsewhere
    IBuffer     buffer;

    ushort      bitbuf;                  // used to decode input
    int         bitcount;

    const       BITBUFBITS = bitbuf.sizeof * CHAR_BIT;

    invariant
    {
      assert(bitcount <= 16);
      assert(bitcount >= 0);
    }

  public:
    this (OutputStream stream)
    {
      auto b = cast(IBuffer) stream;
      if (b is null)
          b = new Buffer (stream.conduit);

      buffer = b;

      bitbuf   = 0;
      bitcount = CHAR_BIT;
    }

    /// put n bits (0 <= n <= 16)
    void putbits (int n, ushort value)
    in
    {
      assert((n >= 0) && (n <= 16));
    }
    body
    {
      // Example with 6 bits to put and 4 already put bits
      //   value        : aaaaaa-- --------
      //   bitbuf       : bbbb---- --------
      //   1. add bits  : bbbbaaaa aa------
      //   2. write byte: aa------ --------
      //
      if (n < bitcount)
      {
        bitbuf |= value << (bitcount -= n);
      }
      else
      {
        scope byte[1] wb = [ cast(byte)(bitbuf | (value >> (n -= bitcount))) ];
        buffer.write(wb);
        if (n < CHAR_BIT)
        {
          bitbuf = value << (bitcount = CHAR_BIT - n);
        }
        else
        {
          wb[0] = cast(byte)(value >> (n - CHAR_BIT));
          buffer.append(wb);
          bitbuf = value << (bitcount = BITBUFBITS - n);
        }
      }
  }

    /// flushes the bit buffer
    void flush ()
    {
      // fill rest of an incomplete buffer byte with zero
      if (bitcount != CHAR_BIT)
        putbits (bitcount, 0);
    }
}

unittest
{
  auto logger = Log.getLogger("unittests.BitWriter");
  logger.info("Start unittest");

  const ubyte expected[] =
  [
    0x12,0x34,0x56,0x78
  ];

  auto buffer = new Buffer(256);

  BitWriter writer = new BitWriter(buffer);
  writer.putbits(4, 0x1);
  writer.putbits(16, 0x2345);
  writer.putbits(4, 0x6);
  writer.putbits(4, 0x7);
  writer.putbits(4, 0x8);
  writer.flush();

  ubyte[] result = cast(ubyte[])buffer.slice();
  assert (result == expected);

  logger.info("Done");
}
