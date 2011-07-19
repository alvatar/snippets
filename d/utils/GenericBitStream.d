module xf.utils.GenericBitStream;

private {
	import std.intrinsic;
	import tango.math.Math;
	import tango.core.Traits;
	import tango.stdc.string;
	import tango.stdc.stringz;
	
	import xf.utils.BitStream;
}

private {
	extern(C) int printf(char* fmt, ...);

	const uint LENGTH_SIZE = 3;

	// Wrap uint bsr(uint)
	uint firstBitSet(T)(T x) {
		static if(T.sizeof > uint.sizeof) {
			auto foo = bsr(x >> 8*uint.sizeof);
			return foo ? (foo + 8*uint.sizeof) : bsr(x);
		} else {
			return bsr(x);
		}
	}

	// Returns: the number of bits copied
	size_t copyBits(ubyte[] from, ubyte[] to, uint fromBegin, uint fromEnd, uint toBegin) {
		uint readByte = fromBegin / 8;
		uint readBit = fromBegin % 8;
		
		uint writeByte = toBegin / 8;
		uint writeBit = toBegin % 8;

		while(readByte*8+readBit < fromEnd) {
			// If our bytes line up, copy byte-by-byte
			// Disabled because 3-bit length storage makes this almost
			// never happen.
			/*if((writeBit == 0) && (readBit == 0) && ((fromEnd - (readBit + readByte * 8)) > 8)) {
			  to[writeByte] = from[readByte];

			  writeByte++;
			  readByte++;
			  } else {				// Otherwise, copy bit-by-bit*/
			if(from[readByte] & (1 << readBit)) {
				to[writeByte] |= 1 << writeBit;
			}
			
			readBit++;
			writeBit++;

			if(readBit >= 8) {
				readByte++;
				readBit = 0;
			}
			if(writeBit >= 8) {
				writeByte++;
				writeBit = 0;
			}
			//}
		}
		
		return fromEnd - fromBegin;
	}
}


class GenericBitStreamWriter : BitStreamWriter {
	GenericBitStreamWriter opCall(bool b) {
		return append([cast(ubyte)b], 1);
	}

	GenericBitStreamWriter opCall(byte i, byte min = byte.min, byte max = byte.max) {
		return integralWrite(i, min, max);
	}

	GenericBitStreamWriter opCall(ubyte i, ubyte min = ubyte.min, ubyte max = ubyte.max) {
		return integralWrite(i, min, max);
	}

	GenericBitStreamWriter opCall(short i, short min = short.min, short max = short.max) {
		return integralWrite(i, min, max);
	}

	GenericBitStreamWriter opCall(ushort i, ushort min = ushort.min, ushort max = ushort.max) {
		return integralWrite(i, min, max);
	}

	GenericBitStreamWriter opCall(int i, int min = int.min, int max = int.max) {
		return integralWrite(i, min, max);
	}

	GenericBitStreamWriter opCall(uint i, uint min = uint.min, uint max = uint.max) {
		return integralWrite(i, min, max);
	}

	GenericBitStreamWriter opCall(long i, long min = long.min, long max = long.max) {
		return integralWrite(i, min, max);
	}

	GenericBitStreamWriter opCall(ulong i, ulong min = ulong.min, ulong max = ulong.max) {
		return integralWrite(i, min, max);
	}

	
	GenericBitStreamWriter opCall(float i, float min = float.min, float max = float.max) {
		assert(max > min, "Invalid limits");
		// TODO: Float compression
		return append((cast(ubyte*)&i)[0..float.sizeof], 8*float.sizeof);
	}


	GenericBitStreamWriter opCall(char[] c, uint maxlen = uint.max) {
		assert(c.length <= maxlen, "Attempted to encode string longer than maximum length");

		void doAppend(ubyte bits) {
			assert(bits < 8);

			// Append length indicator before string
			this.opCall(c.length, 0U, cast(uint)ubyte.max/(1 << (8 - bits)));
			append(cast(ubyte[])c, 8*c.length);
		}

		// Ridiculous overoptimzation follows
		if(maxlen > cast(uint)(ubyte.max/(1 << 1))) {	// length requires at least one byte
			size_t len = c.length;
			// Ensure safe handling of mid-string null chars
			foreach(i, x; c) {
				if('\0' == x) {
					len = i;
					break;
				}
			}
			append(cast(ubyte[])c, 8*len);
			// Null terminator
			append(cast(ubyte[])"\0", 8*char.sizeof);
		} else if(maxlen > ubyte.max/(1 << 2)) { // 7 bits
			doAppend(7);
		} else if(maxlen > ubyte.max/(1 << 3)) { // 6 bits
			doAppend(6);
		} else if(maxlen > ubyte.max/(1 << 4)) { // 5 bits
			doAppend(5);
		} else if(maxlen > ubyte.max/(1 << 5)) { // 4 bits
			doAppend(4);
		} else if(maxlen > ubyte.max/(1 << 6)) { // 3 bits
			doAppend(3);
		} else if(maxlen > ubyte.max/(1 << 7)) { // 2 bits
			doAppend(2);
		} else {				// 1 bit! :D
			doAppend(1);
		}
		
		return this;
	}
	
	
	GenericBitStreamWriter raw(void[] data) {
		if (data.length > 0) {
			append((cast(ubyte[])data), 8*data.length);
		}
		return this;
	}


	GenericBitStreamWriter integral(T)(T foo, T min = T.min, T max = T.max) {
		return integralWrite(foo, min, max);
	}

	GenericBitStreamWriter other(T)(T foo) {
		return append((cast(ubyte*)(&foo))[0..T.sizeof], 8*T.sizeof);
	}
	

	// Number of bits defined
	uint length() {
		return writeBit;
	}

	ubyte[] data() {
		// Update length value
		ubyte len = (8 * stream.length) - writeBit;
		//printf("Sent length %u\n", stream.length * 8 - len);
		synchronized(this) copyBits((&len)[0..1], stream, 0, LENGTH_SIZE, 0);
		return stream;
	}
	
	GenericBitStreamWriter reset() {
		stream[] = 0;
		stream = stream[0..0];
		writeBit = LENGTH_SIZE;
		return this;
	}
	

protected:
	GenericBitStreamWriter append(ubyte[] data, uint bits) {
		auto newEnd = writeBit + bits;
		synchronized(this) {
			stream.length = newEnd / 8 + (newEnd % 8 ? 1 : 0);

			copyBits(data, stream, 0, bits, writeBit);
			writeBit = newEnd;
		}
		return this;
	}

private:
	GenericBitStreamWriter integralWrite(T)(T i, T min, T max) {
		assert(max > min, "Invalid limits");
		assert((i <= max) && (i >= min), "Value is not between limits");
		
		// Simplify i
		i -= min;
		
		auto width = firstBitSet(max - min) + 1;
		return append((cast(ubyte*)(&i))[0..T.sizeof], width);
	}

	// First three bits are reserved for length
	uint writeBit = LENGTH_SIZE;
	ubyte[] stream;
}

class GenericBitStreamReader : BitStreamReader {
	this(GenericBitStreamWriter bsw) {
		stream = bsw.stream;
		_length = bsw.length;
		// Skip unneeded length bits
		readBit = LENGTH_SIZE;
	}

	this(ubyte[] stream) {
		this.stream = stream;
		copyBits(stream, (cast(ubyte*)&_length)[0.._length.sizeof], 0, LENGTH_SIZE, 0);
		readBit += LENGTH_SIZE;
		_length = stream.length*8 - _length;
		//printf("Read length %u\n", length);
	}
	

	GenericBitStreamReader opCall(bool* b) {
		ubyte[1] buf;
		*b = read(1, buf)[0] != 0;
		return this;
	}

	GenericBitStreamReader opCall(byte* i, byte min = byte.min, byte max = byte.max) {
		return integralRead(i, min, max);
	}

	GenericBitStreamReader opCall(ubyte* i, ubyte min = ubyte.min, ubyte max = ubyte.max) {
		return integralRead(i, min, max);
	}

	GenericBitStreamReader opCall(short* i, short min = short.min, short max = short.max) {
		return integralRead(i, min, max);
	}

	GenericBitStreamReader opCall(ushort* i, ushort min = ushort.min, ushort max = ushort.max) {
		return integralRead(i, min, max);
	}

	GenericBitStreamReader opCall(int* i, int min = int.min, int max = int.max) {
		return integralRead(i, min, max);
	}

	GenericBitStreamReader opCall(uint* i, uint min = uint.min, uint max = uint.max) {
		return integralRead(i, min, max);
	}

	GenericBitStreamReader opCall(long* i, long min = long.min, long max = long.max) {
		return integralRead(i, min, max);
	}

	GenericBitStreamReader opCall(ulong* i, ulong min = ulong.min, ulong max = ulong.max) {
		return integralRead(i, min, max);
	}

	
	GenericBitStreamReader opCall(float* i, float min = -float.max, float max = float.max) {
		assert(max > min, "Invalid limits");
		auto width = 8*float.sizeof; // TODO: Float compression

		ubyte[float.sizeof] buf;
		*i = *(cast(float*)read(width, buf).ptr);
		return this;
	}


	GenericBitStreamReader opCall(char[]* i, uint maxlen = uint.max) {
		void doRead(uint lenbits) {
			assert(lenbits < 8);

			uint len;
			this.opCall(&len, 0, ubyte.max/(1 << (8 - lenbits)));
			(*i).length = len;
			//*i = cast(char[]) read(8*len, cast(ubyte[])(*i));
			read(8*len, cast(ubyte[])(*i));
		}

		// Ridiculous overoptimzation follows
		if(maxlen > cast(uint)(ubyte.max/(1 << 1))) {	// null terminated
			ubyte[char.sizeof] buf;
			char ch = (cast(char[])read(8*char.sizeof, buf))[0];
			while(ch != '\0') {
				*i ~= ch;
				ch = (cast(char[])read(8*char.sizeof, buf))[0];
			}
		} else {
			uint len;
			if(maxlen > ubyte.max/(1 << 2)) { // 7 bits
				doRead(7);
			} else if(maxlen > ubyte.max/(1 << 3)) { // 6 bits
				doRead(6);
			} else if(maxlen > ubyte.max/(1 << 4)) { // 5 bits
				doRead(5);
			} else if(maxlen > ubyte.max/(1 << 5)) { // 4 bits
				doRead(4);
			} else if(maxlen > ubyte.max/(1 << 6)) { // 3 bits
				doRead(3);
			} else if(maxlen > ubyte.max/(1 << 7)) { // 2 bits
				doRead(2);
			} else {				// 1 bit! :D
				doRead(1);
			}
		}

		return this;
	}


	GenericBitStreamReader raw(void[] data) {
		if (data.length > 0) {
			read(8*data.length, cast(ubyte[])data);
		}
		return this;
	}


	GenericBitStreamReader integral(T)(T* i, T min = T.min, T max = T.max) {
		return integralRead(i, min, max);
	}

	GenericBitStreamReader other(T)(T* i) {
		read(8*T.sizeof, (cast(ubyte*)i)[0..T.sizeof]);
		return this;
	}


	uint length() {
		return _length;
	}
	
	int bitsRemaining() {
		return length - readBit;
	}
	
	bool moreBits() {
		return readBit < length;
	}

	bool moreBytes() {			// DEPRECATED
		return readBit/8 < stream.length - 1;
	}
	

	ubyte[] stream;

protected:
	ubyte[] read(uint bits, ubyte[] dst = null) {
		{
			auto needed = bits / 8 + (bits % 8 ? 1 : 0);
			if (dst.length < needed) {
				assert (dst is null);
				dst.length = needed;
			}
		}
		
		dst[] = 0;	// copyBits will not do it :o
		
		copyBits(stream, dst, readBit, readBit+bits, 0);
		synchronized(this) {
			readBit += bits;
		}

		return dst;
	}

private:
	GenericBitStreamReader integralRead(T)(T* i, T min, T max) {
		assert(max > min, "Invalid limits");
		
		final int width = firstBitSet(max - min) + 1;
		read(width, // Width
			 (cast(ubyte*)i)[0..T.sizeof]);
		// Restore i
		*i += min;

		return this;
	}
	
	uint readBit = 0;
	uint _length = 0;				// Number of bits.  Should be immutable.
}


version (UnitTest) {
	import tango.math.random.Kiss;
	import tango.text.convert.Format;
}

unittest {
	{
		// Test general encoding/decoding; could use some expansion.
		long u = long.max, u2;
		int i = -42, iMin = -50, iMax = -12, i2;
		bool b = true, b2;
		float f = -float.max, f2;
		char[] cs = "sup lol", cs2;
		uint csMax = 47;

	
		auto wr = new GenericBitStreamWriter;
		wr(u);
		wr(i, iMin, iMax);
		wr(b);
		wr(f);
		wr(cs);
		wr(cs, 47);

	
		auto re = new GenericBitStreamReader(wr);
	
		re(&u2);
		assert(u == u2);

		re(&i2, iMin, iMax);
		assert(i == i2);

		re(&b2);
		assert(b == b2);
	
		re(&f2);
		assert(f == f2, Format("f={} f2={}", f, f2));

		re(&cs2);
		assert(cs == cs2);

		re(&cs2, csMax);
		assert(cs == cs2);

		// Test serialized length passing
		auto len = re.length;
		re = new GenericBitStreamReader(wr.data);
		assert(len == re.length);
	}

	{
		auto wr = new GenericBitStreamWriter;
		wr(true);
		wr(true);
		wr(true);
		
		const int numRandFloats = 100000;
		float[] randFloats = new float[numRandFloats];
		for (int i = 0; i < numRandFloats; ++i) {
			randFloats[i] = cast(float)Kiss.instance.fraction();
		}
		randFloats[0] = 0.f;
		randFloats[1] = -0.f;
		
		const int numRandBools = 100000;
		bool[] randBools = new bool[numRandBools];
		for (int i = 0; i < numRandBools; ++i) {
			randBools[i] = Kiss.instance.natural % 2 == 0;
		}
		
		for(ushort u = 0; u < ushort.max; u++) {
			bool blah = u % 2 == 0;
			wr(blah);
			wr(u);
		}
		for(short u = short.min; u < short.max; u++) {
			wr(u);
		}

		for (int i = 0; i < numRandFloats; ++i) {
			wr(randFloats[i]);
		}
		
		foreach (i, b; randBools) {
			wr(b);
		}
		
		wr(42U);
		wr(75834U);
		wr(473U);
		wr(uint.max);
		wr(-1);
		wr("onoz i can has string"[]);

		auto re = new GenericBitStreamReader(wr.data);
		bool meh;
		re(&meh);
		re(&meh);
		re(&meh);
		assert (meh);

		{
			for(ushort u = 0; u < ushort.max; u++) {
				bool blah;
				re(&blah);
				assert (blah == (u % 2 == 0));
				ushort v = 0;
				re(&v);
				//printf("Testing: %u read as %u\n", u, v);
				if(u != v) {
					printf("FAILED TEST: %u was read as %u\n", u, v);
					assert(false, "Failed exhaustive ushort test");
				}
			}
			for(short u = short.min; u < short.max; u++) {
				short v = 0;
				re(&v);
				//printf("Testing: %u read as %u\n", u, v);
				if(u != v) {
					printf("FAILED TEST: %u was read as %u\n", u, v);
					assert(false, "Failed exhaustive short test");
				}
			}
		}

		for (int i = 0; i < numRandFloats; ++i) {
			float r = 12345;
			re(&r);
			assert (randFloats[i] == r, Format("{}: got {}({:x}) instead of {}({:x})", i, r, *cast(uint*)&r, randFloats[i], *cast(uint*)&randFloats[i]));
		}

		foreach (i, b; randBools) {
			bool b2;
			re(&b2);
			assert (b == b2);
		}

		uint foo, bar, baz, eggs;
		int bacon;
		re(&foo);
		re(&bar);
		re(&baz);
		re(&eggs);
		re(&bacon);
		assert(foo == 42U);
		assert(bar == 75834U);
		assert(baz == 473U);
		assert(eggs == uint.max);
		assert(bacon == -1);
		
		char[] spam;
		re(&spam);
		assert("onoz i can has string" == spam);

		assert(wr.length == re.length, "Failed trans-serialization length test");
	}
	
	{
		auto wr = new GenericBitStreamWriter;
		wr.reset;
		wr(true);
		/+wr(false);
		wr(true);+/
		int zomg = 0;
		wr(zomg);
		/+wr(false);
		wr(true);+/
		//assert (3 + 3 + typeof(0).sizeof*8 == wr.length, Format("wr length: {}", wr.length));
		
		foreach (b; cast(ubyte[])wr.data) {
			printf("%2.2x ", cast(int)b);
		}
		printf("\n");
		
		auto re = new GenericBitStreamReader(wr.data);
		//assert (3 + 3 + typeof(0).sizeof*8 == re.length, Format("re length: {}", re.length));
		bool v;
		assert ((re(&v), v == true));
		/+assert ((re(&v), v == false));
		assert ((re(&v), v == true));+/
		int x;
		assert ((re(&x), x == 0));
		/+assert ((re(&v), v == false));
		assert ((re(&v), v == true));+/
		
		assert (!re.moreBits);
	}
}
