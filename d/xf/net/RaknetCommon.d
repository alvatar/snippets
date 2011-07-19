module xf.net.RaknetCommon;

private {
	import xf.utils.BitStream;
	import xf.net.raknet.RakCore;
}



class RakStreamWriter : BitStreamWriter {
	RakStreamWriter opCall(bool b) {
		bsWriteBool(bs, b);
		return this;
	}

	RakStreamWriter opCall(byte i, byte min, byte max) {
		bsWriteUbyte(bs, *cast(ubyte*)&i);
		return this;
	}
	
	RakStreamWriter opCall(ubyte i, ubyte min, ubyte max) {
		bsWriteUbyte(bs, i);
		return this;
	}

	RakStreamWriter opCall(short i, short min, short max) {
		bsWriteShort(bs, i);
		return this;
	}
	
	RakStreamWriter opCall(ushort i, ushort min, ushort max) {
		bsWriteUshort(bs, i);
		return this;
	}

	RakStreamWriter opCall(int i, int min, int max) {
		bsWriteInt(bs, i);
		return this;
	}
	
	RakStreamWriter opCall(uint i, uint min, uint max) {
		bsWriteUint(bs, i);
		return this;
	}

	RakStreamWriter opCall(long i, long min, long max) {
		bsWriteUint(bs, *(cast(uint*)&i));
		bsWriteUint(bs, *(cast(uint*)&i + 1));
		return this;
	}
	
	RakStreamWriter opCall(ulong i, ulong min, ulong max) {
		bsWriteUint(bs, *(cast(uint*)&i));
		bsWriteUint(bs, *(cast(uint*)&i + 1));
		return this;
	}

	RakStreamWriter opCall(float i, float min, float max) {
		bsWriteFloat(bs, i);
		return this;
	}

	RakStreamWriter opCall(char[] str, uint maxLen) {
		bsWriteUint(bs, str.length);
		foreach (ref char c; str) {
			bsWriteUbyte(bs, *cast(ubyte*)&c);
		}
		return this;
	}
	
	
	BitStream	bs;
}



class RakStreamReader : BitStreamReader {
	RakStreamReader opCall(bool* b) {
		bsReadBool(bs, b);
		return this;
	}

	RakStreamReader opCall(byte* i, byte min, byte max) {
		bsReadUbyte(bs, cast(ubyte*)i);
		return this;
	}
	
	RakStreamReader opCall(ubyte* i, ubyte min, ubyte max) {
		bsReadUbyte(bs, i);
		return this;
	}

	RakStreamReader opCall(short* i, short min, short max) {
		bsReadShort(bs, i);
		return this;
	}
	
	RakStreamReader opCall(ushort* i, ushort min, ushort max) {
		bsReadUshort(bs, i);
		return this;
	}

	RakStreamReader opCall(int* i, int min, int max) {
		bsReadInt(bs, i);
		return this;
	}
	
	RakStreamReader opCall(uint* i, uint min, uint max) {
		bsReadUint(bs, i);
		return this;
	}

	RakStreamReader opCall(long* i, long min, long max) {
		bsReadUint(bs, (cast(uint*)i));
		bsReadUint(bs, (cast(uint*)i + 1));
		return this;
	}
	
	RakStreamReader opCall(ulong* i, ulong min, ulong max) {
		bsReadUint(bs, (cast(uint*)i));
		bsReadUint(bs, (cast(uint*)i + 1));
		return this;
	}

	RakStreamReader opCall(float* i, float min, float max) {
		bsReadFloat(bs, i);
		return this;
	}

	RakStreamReader opCall(char[]* str, uint maxLen) {
		uint len;
		bsReadUint(bs, &len);
		(*str).length = len;
		foreach (ref char b; *str) {
			bsReadUbyte(bs, cast(ubyte*)&b);
		}
		return this;
	}

	bool moreBytes() {
		return bsGetNumberOfUnreadBits(bs) > 7;
	}
	
	
	BitStream	bs;
}



template MRecvPacket(Param) {
	private {
		BitStream	retainedBitstream;
		ubyte		retainedPacketId;
		Packet		retainedPacket;
	}
	
	bool recvPacket(Param handler) {
		BitStream	recvBs;
		ubyte		packetId;
		Packet		recvPacket;
		
		synchronized (this) {
			if (retainedBitstream !is null) {
				recvBs = retainedBitstream;
				retainedBitstream = null;
				
				packetId = retainedPacketId;
				recvPacket = retainedPacket;
			}
		}
		
		if (recvBs is null) {
			static if (is(typeof(rsiReceive(impl)))) {
				recvPacket = rsiReceive(impl);
			} else {
				recvPacket = rciReceive(impl);
			}
		
			if (recvPacket !is null) {				
				recvBs = bsGetBitStream3(ntPacketGetData(recvPacket), (ntPacketGetBitSize(recvPacket) - 1) / 8 + 1, false);
				bsReadUbyte(recvBs, &packetId);
			}
		}
		
		if (recvBs !is null) {
			StreamFate sfate = handlePacket(recvPacket, packetId, recvBs, handler);
				
			if (StreamFate.Dispose == sfate) {
				bsDestroyBitStream(recvBs);

				static if (is(typeof(rsiDeallocatePacket(impl, recvPacket)))) {
					rsiDeallocatePacket(impl, recvPacket);
				} else {
					rciDeallocatePacket(impl, recvPacket);
				}
			} else {
				synchronized (this) {
					assert (retainedBitstream is null, `Cannot retain more than one stream at a time...`);
					retainedBitstream = recvBs;
					retainedPacketId = packetId;
					retainedPacket = recvPacket;
					return false;
				}
			}
			
			return true;
		} else {
			return false;
		}
	}
}



template MStreamGetters() {
	// ----
		static RakStreamReader createBSR__(BitStream bs) {
			auto r = new RakStreamReader;
			r.bs = bs;
			return r;
		}
		static void reuseBSR__(RakStreamReader bsr, BitStream bs) {
			bsr.bs = bs;
		}
	ScopedResource!(ThreadsafePool!(createBSR__, reuseBSR__)) getStreamReader;
	// ----
		static RakStreamWriter createBSW__() {
			auto r = new RakStreamWriter;
			r.bs = bsGetBitStream();
			return r;
		}
		static void reuseBSW__(RakStreamWriter r) {
			bsReset(r.bs);
		}
	ScopedResource!(ThreadsafePool!(createBSW__, reuseBSW__)) getStreamWriter;
	// ----
}
