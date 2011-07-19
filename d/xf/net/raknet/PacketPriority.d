module xf.net.raknet.PacketPriority;


// These enumerations are used to describe when packets are delivered.
enum PacketReliability {
	UNRELIABLE=0,
	UNRELIABLE_SEQUENCED=1,
	RELIABLE=2,
	RELIABLE_ORDERED=3,
	RELIABLE_SEQUENCED=4,
}


enum PacketPriority {
	SYSTEM_PRIORITY=0,
	HIGH_PRIORITY=1,
	MEDIUM_PRIORITY=2,
	LOW_PRIORITY=3,
	NUMBER_OF_PRIORITIES=4,
}
