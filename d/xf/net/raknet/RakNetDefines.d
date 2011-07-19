module xf.net.raknet.RakNetDefines;



/// Define __BITSTREAM_NATIVE_END to NOT support endian swapping in the BitStream class.  This is faster and is what you should use
/// unless you actually plan to have different endianness systems connect to each other
/// Enabled by default.
const int __BITSTREAM_NATIVE_END = 1;  // = 1 -> nb

/// Maximum (stack) size to use with _alloca before using new and delete instead.
const int MAX_ALLOCA_STACK_ALLOCATION = 1048576;