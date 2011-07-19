module xf.net.raknet.RakCore;

private {
	import xf.net.raknet.PacketPriority;
	import tango.sys.SharedLib;
	
	import tango.stdc.stdio : printf;
	
	version (Windows) {
		const char[] LIBRARY = `RakCore.dll`;
	} else {
		static assert (false, `TODO`);
	}
}



extern (C) {
	typedef void* PlayerID;
	typedef void* NetworkID;
	typedef void* Packet;
	typedef void* BitStream;
	typedef void* PluginInterface;
	typedef void* RouterInterface;
	typedef void* RakNetStatisticsStruct;
	typedef void* RPCMap;
	typedef void* RPCParameters;

	typedef void* RakClientInterface;
	typedef void* RakServerInterface;
	typedef void* RakPeerInterface;
	typedef void* ConsoleServer;
	typedef void* ReplicaManager;
	typedef void* LogCommandParser;
	typedef void* PacketLogger;
	typedef void* RakNetCommandParser;
	typedef void* RakNetTransport;
	typedef void* TelnetTransport;
	typedef void* PacketConsoleLogger;
	typedef void* PacketFileLogger;
	typedef void* Router;
	typedef void* ConnectionGraph;

	alias ushort PlayerIndex;
	alias long RakNetTimeNS;
	alias uint RakNetTime;


	void function() dupa;

	/* RakServerInterface */

	bool function(RakServerInterface rsi, ushort AllowedPlayers, uint depreciated,
		int threadSleepTimer, ushort port, char *forceHostAddress = null) rsiStart;
	void function(RakServerInterface rsi, char *privateKeyE, char *privateKeyN) rsiInitializeSecurity;
	void function(RakServerInterface rsi) rsiDisableSecurity;
	void function(RakServerInterface rsi, char *_password) rsiSetPassword;
	bool function(RakServerInterface rsi) rsiHasPassword;
	void function(RakServerInterface rsi, uint blockDuration,
		ubyte orderingChannel) rsiDisconnect;
	bool function(RakServerInterface rsi, char *data, int length,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, PlayerID playerId, bool broadcast) rsiSend;
	bool function(RakServerInterface rsi, BitStream bitStream, PacketPriority priority,
		PacketReliability reliability, char orderingChannel, PlayerID playerId, bool broadcast) rsiSendBS;
	Packet function(RakServerInterface rsi) rsiReceive;
	void function(RakServerInterface rsi, PlayerID playerId) rsiKick;
	void function(RakServerInterface rsi, Packet packet) rsiDeallocatePacket;
	void function(RakServerInterface rsi, ushort AllowedPlayers) rsiSetAllowedPlayers;
	ushort function(RakServerInterface rsi) rsiGetAllowedPlayers;
	ushort function(RakServerInterface rsi) rsiGetConnectedPlayers;
	void function(RakServerInterface rsi, PlayerID playerId,
		char returnValue[ 22 ], ushort *port) rsiGetPlayerIPFromID;
	void function(RakServerInterface rsi, PlayerID playerId) rsiPingPlayer;
	int function(RakServerInterface rsi, PlayerID playerId) rsiGetAveragePing;
	int function(RakServerInterface rsi, PlayerID playerId) rsiGetLastPing;
	int function(RakServerInterface rsi, PlayerID playerId) rsiGetLowestPing;
	void function(RakServerInterface rsi) rsiStartOccasionalPing;
	void function(RakServerInterface rsi) rsiStopOccasionalPing;
	bool function(RakServerInterface rsi) rsiIsActive;
	uint function(RakServerInterface rsi) rsiGetSynchronizedRandomInteger;
	void function(RakServerInterface rsi) rsiStartSynchronizedRandomInteger;
	void function(RakServerInterface rsi) rsiStopSynchronizedRandomInteger;
	bool function(RakServerInterface rsi,
		uint inputFrequencyTable[ 256 ], bool inputLayer) rsiGenerateCompressionLayer;
	bool function(RakServerInterface rsi, bool inputLayer) rsiDeleteCompressionLayer;
	//__declspec(dllexport) void rsiRegisterAsRemoteProcedureCall(RakServerInterface rsi,
	//	char* uniqueID, void ( *functionPointer ) ( RPCParameters *rpcParms ))
	void function(RakServerInterface rsi, char* uniqueID,
		void* function(RPCParameters rpcParms) functionPointer) rsiRegisterAsRemoteProcedureCall;
	void function(RakServerInterface rsi, char* uniqueID, void *functionPointer) rsiRegisterClassMemberRPC;
	void function(RakServerInterface rsi, char* uniqueID) rsiUnregisterAsRemoteProcedureCall;
	bool function(RakServerInterface rsi, char* uniqueID, char *data, uint bitLength,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, PlayerID playerId, bool broadcast,
		bool shiftTimestamp, NetworkID networkID, BitStream replyFromTarget) rsiRPC;
	bool function(RakServerInterface rsi, char* uniqueID, BitStream bitStream,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, PlayerID playerId, bool broadcast,
		bool shiftTimestamp, NetworkID networkID, BitStream replyFromTarget) rsiRPCBS;
	void function(RakServerInterface rsi, bool b) rsiSetTrackFrequencyTable;
	bool function(RakServerInterface rsi, uint outputFrequencyTable[ 256 ]) rsiGetSendFrequencyTable;
	float function(RakServerInterface rsi) rsiGetCompressionRatio;
	float function(RakServerInterface rsi) rsiGetDecompressionRatio;
	void function(RakServerInterface rsi, PluginInterface messageHandler) rsiAttachPlugin;
	void function(RakServerInterface rsi, PluginInterface messageHandler) rsiDetachPlugin;
	BitStream function(RakServerInterface rsi) rsiGetStaticServerData;
	void function(RakServerInterface rsi, char *data, int length) rsiSetStaticServerData;
	void function(RakServerInterface rsi, bool b) rsiSetRelayStaticClientData;
	void function(RakServerInterface rsi, PlayerID playerId) rsiSendStaticServerDataToClient;
	void function(RakServerInterface rsi, char *data, uint length) rsiSetOfflinePingResponse;
	BitStream function(RakServerInterface rsi, PlayerID playerId) rsiGetStaticClientData;
	void function(RakServerInterface rsi, PlayerID playerId, char *data, int length) rsiSetStaticClientData;
	void function(RakServerInterface rsi,
		PlayerID playerChangedId, PlayerID playerToSendToId) rsiChangeStaticClientData;
	uint function(RakServerInterface rsi) rsiGetNumberOfAddresses;
	char* function(RakServerInterface rsi, uint index) rsiGetLocalIP;
	PlayerID function(RakServerInterface rsi)rsiGetInternalID;
	void function(RakServerInterface rsi, Packet packet, bool pushAtHead) rsiPushBackPacket;
	void function(RakServerInterface rsi, RouterInterface routerInterface) rsiSetRouterInterface;
	void function(RakServerInterface rsi, RouterInterface routerInterface) rsiRemoveRouterInterface;
	int function(RakServerInterface rsi, PlayerID playerId) rsiGetIndexFromPlayerID;
	PlayerID function(RakServerInterface rsi, int index) rsiGetPlayerIDFromIndex;
	void function(RakServerInterface rsi, char *IP) rsiAddToBanList;
	void function(RakServerInterface rsi, char *IP) rsiRemoveFromBanList;
	void function(RakServerInterface rsi) rsiClearBanList;
	bool function(RakServerInterface rsi, char *IP) rsiIsBanned;
	bool function(RakServerInterface rsi, PlayerID playerId) rsiIsActivePlayerID;
	void function(RakServerInterface rsi, RakNetTime timeMS, PlayerID target) rsiSetTimeoutTime;
	bool function(RakServerInterface rsi, int size) rsiSetMTUSize;
	int function(RakServerInterface rsi) rsiGetMTUSize;
	void function(RakServerInterface rsi, char *host,
		ushort remotePort, char *data, int dataLength) rsiAdvertiseSystem;
	RakNetStatisticsStruct function(RakServerInterface rsi, PlayerID playerId) rsiGetStatistics;
	void function(RakServerInterface rsi, double maxSendBPS, ushort minExtraPing,
		ushort extraPingVariance) rsiApplyNetworkSimulator;
	bool function(RakServerInterface rsi) rsiIsNetworkSimulatorActive;

	/* RakClientInterface */

	bool function(RakClientInterface rci, char* host, ushort serverPort,
		ushort clientPort, uint depreciated, int threadSleepTimer) rciConnect;
	void function(RakClientInterface rci, uint blockDuration, ubyte orderingChannel = 0) rciDisconnect;
	void function(RakClientInterface rci, char *privKeyP, char *privKeyQ) rciInitializeSecurity;
	void function(RakClientInterface rci, char *_password) rciSetPassword;
	bool function(RakClientInterface rci) rciHasPassword;
	bool function(RakClientInterface rci, char *data, int length,
		PacketPriority priority, PacketReliability reliability, char orderingChannel) rciSend;
	bool function(RakClientInterface rci, BitStream bitStream,
		PacketPriority priority, PacketReliability reliability, char orderingChannel) rciSendBS;
	Packet function(RakClientInterface rci) rciReceive;
	void function(RakClientInterface rci, Packet packet) rciDeallocatePacket;
	void function(RakClientInterface rci) rciPingServer;
	void function(RakClientInterface rci, char* host, ushort serverPort,
		ushort clientPort, bool onlyReplyOnAcceptingConnections) rciPingServerFull;
	int function(RakClientInterface rci) rciGetAveragePing;
	int function(RakClientInterface rci) rciGetLastPing;
	int function(RakClientInterface rci) rciGetLowestPing;
	int function(RakClientInterface rci, PlayerID playerId) rciGetPlayerPing;
	void function(RakClientInterface rci) rciStartOccasionalPing;
	void function(RakClientInterface rci) rciStopOccasionalPing;
	bool function(RakClientInterface rci) rciIsConnected;
	uint function(RakClientInterface rci) rciGetSynchronizedRandomInteger;
	bool function(RakClientInterface rci,
		uint inputFrequencyTable[ 256 ], bool inputLayer) rciGenerateCompressionLayer;
	bool function(RakClientInterface rci, bool inputLayer) rciDeleteCompressionLayer;
	//__declspec(dllexport) void RegisterAsRemoteProcedureCall(RakClientInterface rci, char* uniqueID,
	//	void ( *functionPointer ) ( RPCParameters *rpcParms ))
	void function(RakClientInterface rci, char* uniqueId,
		void* function(RPCParameters rpcParms) functionPointer) rciRegisterAsRemoteProcedureCall;
	void function(RakClientInterface rci, char* uniqueID, void *functionPointer) rciRegisterClassMemberRPC;
	void function(RakClientInterface rci, char* uniqueID) rciUnregisterAsRemoteProcedureCall;
	bool function(RakClientInterface rci, char* uniqueID, char *data,
		uint bitLength, PacketPriority priority, PacketReliability reliability, char orderingChannel,
		bool shiftTimestamp, NetworkID networkID, BitStream replyFromTarget) rciRPC;
	bool function(RakClientInterface rci, char* uniqueID, BitStream bitStream,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, bool shiftTimestamp,
		NetworkID networkID, BitStream replyFromTarget) rciRPCBS;
	void function(RakClientInterface rci, bool b) rciSetTrackFrequencyTable;
	bool function(RakClientInterface rci, uint outputFrequencyTable[ 256 ]) rciGetSendFrequencyTable;
	float function(RakClientInterface rci) rciGetCompressionRatio;
	float function(RakClientInterface rci) rciGetDecompressionRatio;
	void function(RakClientInterface rci, PluginInterface messageHandler) rciAttachPlugin;
	void function(RakClientInterface rci, PluginInterface messageHandler) rciDetachPlugin;
	BitStream function(RakClientInterface rci) rciGetStaticServerData;
	void function(RakClientInterface rci, char *data, int length) rciSetStaticServerData;
	BitStream function(RakClientInterface rci, PlayerID playerId) rciGetStaticClientData;
	void function(RakClientInterface rci, PlayerID playerId,
		char *data, int length) rciSetStaticClientData;
	void function(RakClientInterface rci) rciSendStaticClientDataToServer;
	PlayerID function(RakClientInterface rci) rciGetServerID;
	PlayerID function(RakClientInterface rci) rciGetPlayerID;
	PlayerID function(RakClientInterface rci) rciGetInternalID;
	char* function(RakClientInterface rci, PlayerID playerId) rciPlayerIDToDottedIP;
	void function(RakClientInterface rci, Packet packet, bool pushAtHead) rciPushBackPacket;
	void function(RakClientInterface rci, RouterInterface routerInterface) rciSetRouterInterface;
	void function(RakClientInterface rci, RouterInterface routerInterface) rciRemoveRouterInterface;
	void function(RakClientInterface rci, RakNetTime timeMS) rciSetTimeoutTime;
	bool function(RakClientInterface rci, int size) rciSetMTUSize;
	int function(RakClientInterface rci) rciGetMTUSize;
	void function(RakClientInterface rci, bool allow) rciAllowConnectionResponseIPMigration;
	void function(RakClientInterface rci, char *host,
		ushort remotePort, char *data, int dataLength) rciAdvertiseSystem;
	RakNetStatisticsStruct function(RakClientInterface rci) rciGetStatistics;
	void function(RakClientInterface rci, double maxSendBPS,
		ushort minExtraPing, ushort extraPingVariance) rciApplyNetworkSimulator;
	bool function(RakClientInterface rci) rciIsNetworkSimulatorActive;
	PlayerIndex function(RakClientInterface rci) rciGetPlayerIndex;

	/* RakPeerInterface */

	bool function(RakPeerInterface rpi, ushort maxConnections, ushort localPort,
		int _threadSleepTimer, char *forceHostAddress) rpiInitialize;
	void function(RakPeerInterface rpi, char *pubKeyE, char *pubKeyN,
		char *privKeyP, char *privKeyQ) rpiInitializeSecurity;
	void function(RakPeerInterface rpi) rpiDisableSecurity;
	void function(RakPeerInterface rpi, ushort numberAllowed) rpiSetMaximumIncomingConnections;
	ushort function(RakPeerInterface rpi) rpiGetMaximumIncomingConnections;
	void function(RakPeerInterface rpi, char* passwordData, int passwordDataLength) rpiSetIncomingPassword;
	void function(RakPeerInterface rpi, char* passwordData, int *passwordDataLength) rpiGetIncomingPassword;
	bool function(RakPeerInterface rpi, char* host, ushort remotePort, char* passwordData,
		int passwordDataLength) rpiConnect;
	void function(RakPeerInterface rpi, uint blockDuration, ubyte orderingChannel=0) rpiDisconnect;
	bool function(RakPeerInterface rpi) rpiIsActive;
	bool function(RakPeerInterface rpi, PlayerID remoteSystems,
		ushort *numberOfSystems) rpiGetConnectionList;
	bool function(RakPeerInterface rpi, char *data, int length, PacketPriority priority,
		PacketReliability reliability, char orderingChannel, PlayerID playerId, bool broadcast) rpiSend;
	bool function(RakPeerInterface rpi, BitStream bitStream, PacketPriority priority,
		PacketReliability reliability, char orderingChannel, PlayerID playerId, bool broadcast) rpiSendBS;
	Packet function(RakPeerInterface rpi) rpiReceive;
	void function(RakPeerInterface rpi, Packet packet) rpiDeallocatePacket;
	ushort function(RakPeerInterface rpi) rpiGetMaximumNumberOfPeers;
	//__declspec(dllexport) void RegisterAsRemoteProcedureCall(RakPeerInterface rpi, char* uniqueID,
	//	void ( *functionPointer ) ( RPCParameters *rpcParms ))
	void function(RakPeerInterface rpi, char* uniqueID,
		void* function(RPCParameters rpcParms) functionPointer) rpiRegisterAsRemoteProcedureCall;
	void function(RakPeerInterface rpi, char* uniqueID, void *functionPointer) rpiRegisterClassMemberRPC;
	void function(RakPeerInterface rpi, char* uniqueID) rpiUnregisterAsRemoteProcedureCall;
	bool function(RakPeerInterface rpi, char* uniqueID, char *data, uint bitLength,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, PlayerID playerId, bool broadcast,
		bool shiftTimestamp, NetworkID networkID, BitStream replyFromTarget) rpiRPC;
	bool function(RakPeerInterface rpi, char* uniqueID, BitStream bitStream,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, PlayerID playerId, bool broadcast,
		bool shiftTimestamp, NetworkID networkID, BitStream replyFromTarget) rpiRPCBS;
	void function(RakPeerInterface rpi,  PlayerID target, bool sendDisconnectionNotification,
		ubyte orderingChannel) rpiCloseConnection;
	int function(RakPeerInterface rpi,  PlayerID playerId) rpiGetIndexFromPlayerID;
	PlayerID function(RakPeerInterface rpi, int index) rpiGetPlayerIDFromIndex;
	void function(RakPeerInterface rpi, char *IP, RakNetTime milliseconds) rpiAddToBanList;
	void function(RakPeerInterface rpi, char *IP) rpiRemoveFromBanList;
	void function(RakPeerInterface rpi) rpiClearBanList;
	bool function(RakPeerInterface rpi, char *IP) rpiIsBanned;
	void function(RakPeerInterface rpi, PlayerID target) rpiPing;
	void function(RakPeerInterface rpi, char* host, ushort remotePort,
		bool onlyReplyOnAcceptingConnections) rpiPingFull;
	int function(RakPeerInterface rpi, PlayerID playerId) rpiGetAveragePing;
	int function(RakPeerInterface rpi, PlayerID playerId) rpiGetLastPing;
	int function(RakPeerInterface rpi, PlayerID playerId) rpiGetLowestPing;
	void function(RakPeerInterface rpi, bool doPing) rpiSetOccasionalPing;
	BitStream function(RakPeerInterface rpi, PlayerID playerId) rpiGetRemoteStaticData;
	void function(RakPeerInterface rpi, PlayerID playerId, char *data, int length) rpiSetRemoteStaticData;
	void function(RakPeerInterface rpi, PlayerID target) rpiSendStaticData;
	void function(RakPeerInterface rpi, char *data, uint length) rpiSetOfflinePingResponse;
	PlayerID function(RakPeerInterface rpi) rpiGetInternalID;
	PlayerID function(RakPeerInterface rpi, PlayerID target) rpiGetExternalID;
	void function(RakPeerInterface rpi, RakNetTime timeMS, PlayerID target) rpiSetTimeoutTime;
	bool function(RakPeerInterface rpi, int size) rpiSetMTUSize;
	int function(RakPeerInterface rpi) rpiGetMTUSize;
	uint function(RakPeerInterface rpi) rpiGetNumberOfAddresses;
	char* function(RakPeerInterface rpi, uint index) rpiGetLocalIP;
	char* function(RakPeerInterface rpi, PlayerID playerId) rpiPlayerIDToDottedIP;
	void function(RakPeerInterface rpi, char* host, ushort remotePort, PlayerID playerId) rpiIPToPlayerID;
	void function(RakPeerInterface rpi, bool allow) rpiAllowConnectionResponseIPMigration;
	void function(RakPeerInterface rpi, char *host, ushort remotePort,
		char *data, int dataLength) rpiAdvertiseSystem;
	void function(RakPeerInterface rpi, int interval) rpiSetSplitMessageProgressInterval;
	void function(RakPeerInterface rpi, RakNetTime timeoutMS) rpiSetUnreliableTimeout;
	void function(RakPeerInterface rpi, bool doCompile) rpiSetCompileFrequencyTable;
	bool function(RakPeerInterface rpi, uint outputFrequencyTable[ 256 ]) rpiGetOutgoingFrequencyTable;
	bool function(RakPeerInterface rpi,
		uint inputFrequencyTable[ 256 ], bool inputLayer) rpiGenerateCompressionLayer;
	bool function(RakPeerInterface rpi, bool inputLayer) rpiDeleteCompressionLayer;
	float function(RakPeerInterface rpi) rpiGetCompressionRatio;
	float function(RakPeerInterface rpi) rpiGetDecompressionRatio;
	void function(RakPeerInterface rpi, PluginInterface plugin) rpiAttachPlugin;
	void function(RakPeerInterface rpi, PluginInterface messageHandler) rpiDetachPlugin;
	void function(RakPeerInterface rpi, Packet packet, bool pushAtHead) rpiPushBackPacket;
	void function(RakPeerInterface rpi, RouterInterface routerInterface) rpiSetRouterInterface;
	void function(RakPeerInterface rpi, RouterInterface routerInterface) rpiRemoveRouterInterface;
	void function(RakPeerInterface rpi, double maxSendBPS,
		ushort minExtraPing, ushort extraPingVariance) rpiApplyNetworkSimulator;
	bool function(RakPeerInterface rpi) rpiIsNetworkSimulatorActive;
	RakNetStatisticsStruct function(RakPeerInterface rpi, PlayerID playerId) rpiGetStatistics;
	RPCMap function(RakPeerInterface rpi, PlayerID playerId) rpiGetRPCMap;

	/* RakNetworkFactory */

	RakClientInterface function() rnfGetRakClientInterface;
	RakServerInterface function() rnfGetRakServerInterface;
	RakPeerInterface function() rnfGetRakPeerInterface;
	ConsoleServer function() rnfGetConsoleServer;
	ReplicaManager function() rnfGetReplicaManager;
	LogCommandParser function() rnfGetLogCommandParser;
	PacketLogger function() rnfGetPacketLogger;
	RakNetCommandParser function() rnfGetRakNetCommandParser;
	RakNetTransport function() rnfGetRakNetTransport;
	TelnetTransport function() rnfGetTelnetTransport;
	PacketConsoleLogger function() rnfGetPacketConsoleLogger;
	PacketFileLogger function() rnfGetPacketFileLogger;
	Router function() rnfGetRouter;
	ConnectionGraph function() rnfGetConnectionGraph;

	void function(RakClientInterface i) rnfDestroyRakClientInterface;
	void function(RakServerInterface i) rnfDestroyRakServerInterface;
	void function(RakPeerInterface i)rnfDestroyRakPeerInterface;
	void function(ConsoleServer i) rnfDestroyConsoleServer;
	void function(ReplicaManager i) rnfDestroyReplicaManager;
	void function(LogCommandParser i) rnfDestroyLogCommandParser;
	void function(PacketLogger i) rnfDestroyPacketLogger;
	void function(RakNetCommandParser i) rnfDestroyRakNetCommandParser;
	void function(RakNetTransport i) rnfDestroyRakNetTransport;
	void function(TelnetTransport i) rnfDestroyTelnetTransport;
	void function(PacketConsoleLogger i) rnfDestroyPacketConsoleLogger;
	void function(PacketFileLogger i) rnfDestroyPacketFileLogger;
	void function(Router i) rnfDestroyRouter;
	void function(ConnectionGraph i) rnfDestroyConnectionGraph;

	/* NetworkTypes */

	/* Packet */
	Packet function() ntPacketCreatePacket;
	PlayerIndex function(Packet p) ntPacketGetPlayerIndex;
	void function(Packet p, PlayerIndex pi) ntPacketSetPlayerIndex;
	PlayerID function(Packet p) ntPacketGetPlayerId;
	void function(Packet p, PlayerID pi) ntPacketSetPlayerId;
	uint function(Packet p) ntPacketGetLength;
	void function(Packet p, uint len) ntPacketSetLength;
	uint function(Packet p) ntPacketGetBitSize;
	void function(Packet p, uint bitSize) ntPacketSetBitSize;
	ubyte* function(Packet p) ntPacketGetData;
	void function(Packet p, ubyte* data) ntPacketSetData;
	bool function(Packet p) ntPacketGetDeleteData;
	void function(Packet p, bool dd) ntPacketSetDeleteData;
	/* PlayerID */
	PlayerID function() ntPlayerIDCreatePlayerID;
	void function(PlayerID) ntDestroyPlayerID;
	uint function(PlayerID pid) ntPlayerIDGetBinaryAddress;
	void function(PlayerID pid, uint ba) ntPlayerIDSetBinaryAddress;
	void function(PlayerID pid, char[] str) ntPlayerIDSetBinaryAddressString;
	ushort function(PlayerID pid) ntPlayerIDGetPort;
	void function(PlayerID pid, ushort p) ntPlayerIDSetPort;
	int function(PlayerID pid1, PlayerID pid2) ntPlayerIDCompareTo;
	PlayerID function() ntPlayerIDGetUnassignedPlayerID;
	/* NetworkID */
	//NetworkID function() ntNetworkIDCreateNetworkID;
	//bool function(NetworkID nid) ntNetworkIDGetPeerToPeerMode;
	//void function(NetworkID nid, bool mode) ntNetworkIDSetPeerToPeerMode;
	PlayerID function(NetworkID nid) ntNetworkIDGetPlayerID;
	void function(NetworkID nid, PlayerID pid) ntNetworkIDSetPlayerID;
	ushort function(NetworkID nid) ntNetworkIDGetLocalSystemId;
	void function(NetworkID nid, ushort lsid) ntNetworkIDSetLocalSystemId;
	int function(NetworkID nid1, NetworkID nid2) ntNetworkIDCompareTo;

	/* BitStream */

	BitStream function() bsGetBitStream;
	BitStream function(int initialBytesToAllocate) bsGetBitStream1;
	BitStream function(ubyte* _data, uint lengthInBytes, bool _copyData) bsGetBitStream3;
	void function(BitStream bs) bsDestroyBitStream;
	bool function(BitStream bs, RakNetTime rnt) bsReadRakNetTime;
	void function(BitStream bs) bsReset;
	bool function(BitStream bs, bool writeToBitstream,  char* input, int numberOfBytes) bsSerialize;
	bool function(BitStream bs, bool writeToBitstream, ubyte* input,
		int numberOfBitsToSerialize, bool rightAlignedBits) bsSerializeBits;
	void function(BitStream bs, char* input, int numberOfBytes) bsWriteChars;
	void function(BitStream bs, BitStream bitStream, int numberOfBits) bsWriteBSn;
	void function(BitStream bs, BitStream bitStream) bsWriteBS;
	bool function(BitStream bs, char* output, int numberOfBytes) bsRead;			// BUG: why not bsReadChars ??  -h3
	void function(BitStream bs) bsResetReadPointer;
	void function(BitStream bs) bsResetWritePointer;
	void function(BitStream bs) bsAssertStreamEmpty;
	void function(BitStream bs) bsPrintBits;
	void function(BitStream bs, int numberOfBits) bsIgnoreBits;
	void function(BitStream bs, int offset) bsSetWriteOffset;
	int function(BitStream bs) bsGetNumberOfBitsUsed;
	int function(BitStream bs) bsGetWriteOffset;
	int function(BitStream bs) bsGetNumberOfBytesUsed;
	int function(BitStream bs) bsGetReadOffset;
	void function(BitStream bs, int newReadOffset) bsSetReadOffset;
	int function(BitStream bs) bsGetNumberOfUnreadBits;
	int function(BitStream bs, ubyte** _data) bsCopyData;  // watchout for this one
	void function(BitStream bs, ubyte *input) bsSetData;
	ubyte* function(BitStream bs) bsGetData;
	void function(BitStream bs, ubyte* input, int numberOfBitsToWrite,
		bool rightAlignedBits) bsWriteBits;
	void function(BitStream bs, ubyte *input, int numberOfBytesToWrite) bsWriteAlignedBytes;
	bool function(BitStream bs, ubyte *output, int numberOfBytesToRead) bsReadAlignedBytes;
	void function(BitStream bs) bsAlignWriteToByteBoundary;
	void function(BitStream bs) bsAlignReadToByteBoundary;
	bool function(BitStream bs, ubyte *output, int numberOfBitsToRead,
		bool alignBitsToRight) bsReadBits;
	void function(BitStream bs) bsWrite0;
	void function(BitStream bs) bsWrite1;

	bool function(BitStream bs, bool input) bsWriteBool;
	bool function(BitStream bs, ubyte input) bsWriteUbyte;
	bool function(BitStream bs, char input) bsWriteChar;
	bool function(BitStream bs, ushort input) bsWriteUshort;
	bool function(BitStream bs, short input) bsWriteShort;
	bool function(BitStream bs, uint input) bsWriteUint;
	bool function(BitStream bs, int input) bsWriteInt;
	//bool function(BitStream bs, ulong input) bsWriteUlong;
	//bool function(BitStream bs, long input) bsWriteLong;
	bool function(BitStream bs, float input) bsWriteFloat;
	bool function(BitStream bs, double input) bsWriteDouble;
	bool function(BitStream bs, NetworkID networkId) bsWriteNetworkID;
	bool function(BitStream bs, ubyte input) bsWriteCompressedUbyte;
	bool function(BitStream bs, char input) bsWriteCompressedChar;
	bool function(BitStream bs, ushort input) bsWriteCompressedUshort;
	bool function(BitStream bs, short input) bsWriteCompressedShort;
	bool function(BitStream bs, uint input) bsWriteCompressedUint;
	bool function(BitStream bs, int input) bsWriteCompressedInt;
	//bool function(BitStream bs, ulong input) bsWriteCompressedUlong;
	//bool function(BitStream bs, long input) bsWriteCompressedLong;
	bool function(BitStream bs, float input) bsWriteCompressedFloat;
	bool function(BitStream bs, float x, float y, float z) bsWriteNormVector;
	bool function(BitStream bs, float x, float y, float z) bsWriteVector;
	bool function(BitStream bs, float w, float x, float y, float z) bsWriteNormQuat;
	bool function(BitStream bs,
		float m00, float m01, float m02,
		float m10, float m11, float m12,
		float m20, float m21, float m22) bsWriteOrthMatrix;
	bool function(BitStream bs, double input) bsWriteCompressedDouble;

	bool function(BitStream bs, bool* input) bsReadBool;
	bool function(BitStream bs, ubyte* input) bsReadUbyte;
	bool function(BitStream bs, char* input) bsReadChar;
	bool function(BitStream bs, ushort* input) bsReadUshort;
	bool function(BitStream bs, short* input) bsReadShort;
	bool function(BitStream bs, uint* input) bsReadUint;
	bool function(BitStream bs, int* input) bsReadInt;
	//bool function(BitStream bs, ulong* input) bsReadUlong;
	//bool function(BitStream bs, long* input) bsReadLong;
	bool function(BitStream bs, float* input) bsReadFloat;
	bool function(BitStream bs, double* input) bsReadDouble;
	bool function(BitStream bs, NetworkID networkId) bsReadNetworkID;
	bool function(BitStream bs, ubyte* input) bsReadCompressedUbyte;
	bool function(BitStream bs, char* input) bsReadCompressedChar;
	bool function(BitStream bs, ushort* input) bsReadCompressedUshort;
	bool function(BitStream bs, short* input) bsReadCompressedShort;
	bool function(BitStream bs, uint* input) bsReadCompressedUint;
	bool function(BitStream bs, int* input) bsReadCompressedInt;
	//bool function(BitStream bs, ulong* input) bsReadCompressedUlong;
	//bool function(BitStream bs, long* input) bsReadCompressedLong;
	bool function(BitStream bs, float* input) bsReadCompressedFloat;
	bool function(BitStream bs, float* x, float* y, float* z) bsReadNormVector;
	bool function(BitStream bs, float* x, float* y, float* z) bsReadVector;
	bool function(BitStream bs, float* w, float* x, float* y, float* z) bsReadNormQuat;
	bool function(BitStream bs,
		float* m00, float* m01, float* m02,
		float* m10, float* m11, float* m12,
		float* m20, float* m21, float* m22) bsReadOrthMatrix;
	bool function(BitStream bs, double* input) bsReadCompressedDouble;

	bool function(BitStream bs) bsReadBit;
	void function(BitStream bs) bsAssertCopyData;
	void function(BitStream bs, uint lengthInBits) bsSetNumberOfBitsAllocated;
	void function(BitStream bs, int numberOfBitsToWrite) bsAddBitsAndReallocate;

	/* GetTime */

	RakNetTime function() gtGetTime;
	RakNetTimeNS function() gtGetTimeNS;

	/* RakNetStatisticsStruct */

	RakNetStatisticsStruct function() ntStatisticsCreateStatistics;
	uint* function(RakNetStatisticsStruct rnss) ntStatisticsGetMessageSendBuffer;
	void function(RakNetStatisticsStruct rnss, uint* data) ntStatisticsSetMessageSendBuffer;
	uint* function(RakNetStatisticsStruct rnss) ntStatisticsGetMessagesSent;
	void function(RakNetStatisticsStruct rnss, uint* data) ntStatisticsSetMessagesSent;
	uint* function(RakNetStatisticsStruct rnss) ntStatisticsGetMessageDataBitsSent;
	void function(RakNetStatisticsStruct rnss, uint* data) ntStatisticsSetMessageDataBitsSent;
	uint* function(RakNetStatisticsStruct rnss) ntStatisticsGetMessageTotalBitsSent;
	void function(RakNetStatisticsStruct rnss, uint* data) ntStatisticsSetMessageTotalBitsSent;

	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetPacketsContainingOnlyAcknowlegements;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetPacketsContainingOnlyAcknowlegements;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetAcknowlegementsSent;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetAcknowlegementsSent;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetAcknowlegementsPending;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetAcknowlegementsPending;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetAcknowlegementBitsSent;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetAcknowlegementBitsSent;

	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetPacketsContainingOnlyAcknowlegementsAndResends;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetPacketsContainingOnlyAcknowlegementsAndResends;

	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetMessageResends;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetMessageResends;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetMessageDataBitsResent;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetMessageDataBitsResent;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetMessagesTotalBitsResent;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetMessagesTotalBitsResent;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetMessagesOnResendQueue;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetMessagesOnResendQueue;

	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetNumberOfUnsplitMessages;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetNumberOfUnsplitMessages;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetNumberOfSplitMessages;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetNumberOfSplitMessages;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetTotalSplits;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetTotalSplits;

	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetPacketsSent;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetPacketsSent;

	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetEncryptionBitsSent;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetEncryptionBitsSent;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetTotalBitsSent;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetTotalBitsSent;

	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetSequencedMessagesOutOfOrder;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetSequencedMessagesOutOfOrder;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetSequencedMessagesInOrder;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetSequencedMessagesInOrder;

	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetOrderedMessagesOutOfOrder;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetOrderedMessagesOutOfOrder;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetOrderedMessagesInOrder;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetOrderedMessagesInOrder;

	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetPacketsReceived;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetPacketsReceived;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetPacketsWithBadCRCReceived;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetPacketsWithBadCRCReceived;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetBitsReceived;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetBitsReceived;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetBitsWithBadCRCReceived;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetBitsWithBadCRCReceived;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetAcknowlegementsReceived;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetAcknowlegementsReceived;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetDuplicateAcknowlegementsReceived;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetDuplicateAcknowlegementsReceived;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetMessagesReceived;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetMessagesReceived;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetInvalidMessagesReceived;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetInvalidMessagesReceived;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetDuplicateMessagesReceived;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetDuplicateMessagesReceived;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetMessagesWaitingForReassembly;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetMessagesWaitingForReassembly;
	uint function(RakNetStatisticsStruct rnss) ntStatisticsGetInternalOutputQueueSize;
	void function(RakNetStatisticsStruct rnss, uint data) ntStatisticsSetInternalOutputQueueSize;

	double function(RakNetStatisticsStruct rnss) ntStatisticsGetBitsPerSecond;
	void function(RakNetStatisticsStruct rnss, double data) ntStatisticsSetBitsPerSecond;
	
	void function(RakNetStatisticsStruct rnss, char* buffer, int level) ntStatisticsToString;

	RakNetTime function(RakNetStatisticsStruct rnss) ntStatisticsGetConnectionStartTime;
	void function(RakNetStatisticsStruct rnss, RakNetTime rnt) ntStatisticsSetConnectionStartTime;

}


private void loadSym(T)(inout T t, SharedLib lib, char[] name) {
	t = cast(T)lib.getSymbol(name.ptr);
	if (t is null) printf(`Could not load '%s'`\n, name.ptr);
}


void bindRaknetFunctions() {
	auto dll = SharedLib.load(LIBRARY);
	if (dll is null) {
		auto up = new Exception(`Could not load the raknet library, '` ~LIBRARY~`'`);
		throw up;
	}
	
	/* RakNetworkFactory */

	loadSym(rnfGetRakClientInterface, dll, `rnfGetRakClientInterface`);
	loadSym(rnfGetRakServerInterface, dll, `rnfGetRakServerInterface`);
	loadSym(rnfGetRakPeerInterface, dll, `rnfGetRakPeerInterface`);
	loadSym(rnfGetConsoleServer, dll, `rnfGetRakPeerInterface`);
	loadSym(rnfGetReplicaManager, dll, `rnfGetReplicaManager`);
	loadSym(rnfGetLogCommandParser, dll, `rnfGetLogCommandParser`);
	loadSym(rnfGetPacketLogger, dll, `rnfGetPacketLogger`);
	loadSym(rnfGetRakNetCommandParser, dll, `rnfGetRakNetCommandParser`);
	loadSym(rnfGetRakNetTransport, dll, `rnfGetRakNetTransport`);
	loadSym(rnfGetTelnetTransport, dll, `rnfGetTelnetTransport`);
	loadSym(rnfGetPacketConsoleLogger, dll, `rnfGetPacketConsoleLogger`);
	loadSym(rnfGetPacketFileLogger, dll, `rnfGetPacketFileLogger`);
	loadSym(rnfGetRouter, dll, `rnfGetRouter`);
	loadSym(rnfGetConnectionGraph, dll, `rnfGetConnectionGraph`);

	loadSym(rnfDestroyRakClientInterface, dll, `rnfDestroyRakClientInterface`);
	loadSym(rnfDestroyRakServerInterface, dll, `rnfDestroyRakServerInterface`);
	loadSym(rnfDestroyRakPeerInterface, dll, `rnfDestroyRakPeerInterface`);
	loadSym(rnfDestroyConsoleServer, dll, `rnfDestroyConsoleServer`);
	loadSym(rnfDestroyReplicaManager, dll, `rnfDestroyReplicaManager`);
	loadSym(rnfDestroyLogCommandParser, dll, `rnfDestroyLogCommandParser`);
	loadSym(rnfDestroyPacketLogger, dll, `rnfDestroyPacketLogger`);
	loadSym(rnfDestroyRakNetCommandParser, dll, `rnfDestroyRakNetCommandParser`);
	loadSym(rnfDestroyRakNetTransport, dll, `rnfDestroyRakNetTransport`);
	loadSym(rnfDestroyTelnetTransport, dll, `rnfDestroyTelnetTransport`);
	loadSym(rnfDestroyPacketConsoleLogger, dll, `rnfDestroyPacketConsoleLogger`);
	loadSym(rnfDestroyPacketFileLogger, dll, `rnfDestroyPacketFileLogger`);
	loadSym(rnfDestroyRouter, dll, `rnfDestroyRouter`);
	loadSym(rnfDestroyConnectionGraph, dll, `rnfDestroyConnectionGraph`);

	/* RakServerInterface */

	loadSym(rsiStart, dll, `rsiStart`);
	loadSym(rsiInitializeSecurity, dll, `rsiInitializeSecurity`);
	loadSym(rsiDisableSecurity, dll, `rsiDisableSecurity`);
	loadSym(rsiSetPassword, dll, `rsiSetPassword`);
	loadSym(rsiHasPassword, dll, `rsiHasPassword`);
	loadSym(rsiDisconnect, dll, `rsiDisconnect`);
	loadSym(rsiSend, dll, `rsiSend`);
	loadSym(rsiSendBS, dll, `rsiSendBS`);
	loadSym(rsiReceive, dll, `rsiReceive`);
	loadSym(rsiKick, dll, `rsiKick`);
	loadSym(rsiDeallocatePacket, dll, `rsiDeallocatePacket`);
	loadSym(rsiSetAllowedPlayers, dll, `rsiSetAllowedPlayers`);
	loadSym(rsiGetAllowedPlayers, dll, `rsiGetAllowedPlayers`);
	loadSym(rsiGetConnectedPlayers, dll, `rsiGetConnectedPlayers`);
	loadSym(rsiGetPlayerIPFromID, dll, `rsiGetPlayerIPFromID`);
	loadSym(rsiPingPlayer, dll, `rsiPingPlayer`);
	loadSym(rsiGetAveragePing, dll, `rsiGetAveragePing`);
	loadSym(rsiGetLastPing, dll, `rsiGetLastPing`);
	loadSym(rsiGetLowestPing, dll, `rsiGetLowestPing`);
	loadSym(rsiStartOccasionalPing, dll, `rsiStartOccasionalPing`);
	loadSym(rsiStopOccasionalPing, dll, `rsiStopOccasionalPing`);
	loadSym(rsiIsActive, dll, `rsiIsActive`);
	loadSym(rsiGetSynchronizedRandomInteger, dll, `rsiGetSynchronizedRandomInteger`);
	loadSym(rsiStartSynchronizedRandomInteger, dll, `rsiStartSynchronizedRandomInteger`);
	loadSym(rsiStopSynchronizedRandomInteger, dll, `rsiStopSynchronizedRandomInteger`);
	loadSym(rsiGenerateCompressionLayer, dll, `rsiGenerateCompressionLayer`);
	loadSym(rsiDeleteCompressionLayer, dll, `rsiDeleteCompressionLayer`);
	loadSym(rsiRegisterAsRemoteProcedureCall, dll, `rsiRegisterAsRemoteProcedureCall`);
	loadSym(rsiRegisterClassMemberRPC, dll, `rsiRegisterClassMemberRPC`);
	loadSym(rsiUnregisterAsRemoteProcedureCall, dll, `rsiUnregisterAsRemoteProcedureCall`);
	loadSym(rsiRPC, dll, `rsiRPC`);
	loadSym(rsiRPCBS, dll, `rsiRPCBS`);
	loadSym(rsiSetTrackFrequencyTable, dll, `rsiSetTrackFrequencyTable`);
	loadSym(rsiGetSendFrequencyTable, dll, `rsiGetSendFrequencyTable`);
	loadSym(rsiGetCompressionRatio, dll, `rsiGetCompressionRatio`);
	loadSym(rsiGetDecompressionRatio, dll, `rsiGetDecompressionRatio`);
	loadSym(rsiAttachPlugin, dll, `rsiAttachPlugin`);
	loadSym(rsiDetachPlugin, dll, `rsiDetachPlugin`);
	loadSym(rsiGetStaticServerData, dll, `rsiGetStaticServerData`);
	loadSym(rsiSetStaticServerData, dll, `rsiSetStaticServerData`);
	loadSym(rsiSetRelayStaticClientData, dll, `rsiSetRelayStaticClientData`);
	loadSym(rsiSendStaticServerDataToClient, dll, `rsiSendStaticServerDataToClient`);
	loadSym(rsiSetOfflinePingResponse, dll, `rsiSetOfflinePingResponse`);
	loadSym(rsiGetStaticClientData, dll, `rsiGetStaticClientData`);
	loadSym(rsiSetStaticClientData, dll, `rsiSetStaticClientData`);
	loadSym(rsiChangeStaticClientData, dll, `rsiChangeStaticClientData`);
	loadSym(rsiGetNumberOfAddresses, dll, `rsiGetNumberOfAddresses`);
	loadSym(rsiGetLocalIP, dll, `rsiGetLocalIP`);
	loadSym(rsiGetInternalID, dll, `rsiGetInternalID`);
	loadSym(rsiPushBackPacket, dll, `rsiPushBackPacket`);
	loadSym(rsiSetRouterInterface, dll, `rsiSetRouterInterface`);
	loadSym(rsiRemoveRouterInterface, dll, `rsiRemoveRouterInterface`);
	loadSym(rsiGetIndexFromPlayerID, dll, `rsiGetIndexFromPlayerID`);
	loadSym(rsiGetPlayerIDFromIndex, dll, `rsiGetPlayerIDFromIndex`);
	loadSym(rsiAddToBanList, dll, `rsiAddToBanList`);
	loadSym(rsiRemoveFromBanList, dll, `rsiRemoveFromBanList`);
	loadSym(rsiClearBanList, dll, `rsiClearBanList`);
	loadSym(rsiIsBanned, dll, `rsiIsBanned`);
	loadSym(rsiIsActivePlayerID, dll, `rsiIsActivePlayerID`);
	loadSym(rsiSetTimeoutTime, dll, `rsiSetTimeoutTime`);
	loadSym(rsiSetMTUSize, dll, `rsiSetMTUSize`);
	loadSym(rsiGetMTUSize, dll, `rsiGetMTUSize`);
	loadSym(rsiAdvertiseSystem, dll, `rsiAdvertiseSystem`);
	loadSym(rsiGetStatistics, dll, `rsiGetStatistics`);
	loadSym(rsiApplyNetworkSimulator, dll, `rsiApplyNetworkSimulator`);
	loadSym(rsiIsNetworkSimulatorActive, dll, `rsiIsNetworkSimulatorActive`);

	/* RakClientInterface */

	loadSym(rciConnect, dll, `rciConnect`);
	loadSym(rciDisconnect, dll, `rciDisconnect`);
	loadSym(rciInitializeSecurity, dll, `rciInitializeSecurity`);
	loadSym(rciSetPassword, dll, `rciSetPassword`);
	loadSym(rciHasPassword, dll, `rciHasPassword`);
	loadSym(rciSend, dll, `rciSend`);
	loadSym(rciSendBS, dll, `rciSendBS`);
	loadSym(rciReceive, dll, `rciReceive`);
	loadSym(rciDeallocatePacket, dll, `rciDeallocatePacket`);
	loadSym(rciPingServer, dll, `rciPingServer`);
	loadSym(rciPingServerFull, dll, `rciPingServerFull`);
	loadSym(rciGetAveragePing, dll, `rciGetAveragePing`);
	loadSym(rciGetLastPing, dll, `rciGetLastPing`);
	loadSym(rciGetLowestPing, dll, `rciGetLowestPing`);
	loadSym(rciGetPlayerPing, dll, `rciGetPlayerPing`);
	loadSym(rciStartOccasionalPing, dll, `rciStartOccasionalPing`);
	loadSym(rciStopOccasionalPing, dll, `rciStopOccasionalPing`);
	loadSym(rciIsConnected, dll, `rciIsConnected`);
	loadSym(rciGetSynchronizedRandomInteger, dll, `rciGetSynchronizedRandomInteger`);
	loadSym(rciGenerateCompressionLayer, dll, `rciGenerateCompressionLayer`);
	loadSym(rciDeleteCompressionLayer, dll, `rciDeleteCompressionLayer`);
	loadSym(rciRegisterAsRemoteProcedureCall, dll, `rciRegisterAsRemoteProcedureCall`);
	loadSym(rciRegisterClassMemberRPC, dll, `rciRegisterClassMemberRPC`);
	loadSym(rciUnregisterAsRemoteProcedureCall, dll, `rciUnregisterAsRemoteProcedureCall`);
	loadSym(rciRPC, dll, `rciRPC`);
	loadSym(rciRPCBS, dll, `rciRPCBS`);
	loadSym(rciSetTrackFrequencyTable, dll, `rciSetTrackFrequencyTable`);
	loadSym(rciGetSendFrequencyTable, dll, `rciGetSendFrequencyTable`);
	loadSym(rciGetCompressionRatio, dll, `rciGetCompressionRatio`);
	loadSym(rciGetDecompressionRatio, dll, `rciGetDecompressionRatio`);
	loadSym(rciAttachPlugin, dll, `rciAttachPlugin`);
	loadSym(rciDetachPlugin, dll, `rciDetachPlugin`);
	loadSym(rciGetStaticServerData, dll, `rciGetStaticServerData`);
	loadSym(rciSetStaticServerData, dll, `rciSetStaticServerData`);
	loadSym(rciGetStaticClientData, dll, `rciGetStaticClientData`);
	loadSym(rciSetStaticClientData, dll, `rciSetStaticClientData`);
	loadSym(rciSendStaticClientDataToServer, dll, `rciSendStaticClientDataToServer`);
	loadSym(rciGetServerID, dll, `rciGetServerID`);
	loadSym(rciGetPlayerID, dll, `rciGetPlayerID`);
	loadSym(rciGetInternalID, dll, `rciGetInternalID`);
	loadSym(rciPlayerIDToDottedIP, dll, `rciPlayerIDToDottedIP`);
	loadSym(rciPushBackPacket, dll, `rciPushBackPacket`);
	loadSym(rciSetRouterInterface, dll, `rciSetRouterInterface`);
	loadSym(rciRemoveRouterInterface, dll, `rciRemoveRouterInterface`);
	loadSym(rciSetTimeoutTime, dll, `rciSetTimeoutTime`);
	loadSym(rciSetMTUSize, dll, `rciSetMTUSize`);
	loadSym(rciGetMTUSize, dll, `rciGetMTUSize`);
	loadSym(rciAllowConnectionResponseIPMigration, dll, `rciAllowConnectionResponseIPMigration`);
	loadSym(rciAdvertiseSystem, dll, `rciAdvertiseSystem`);
	loadSym(rciGetStatistics, dll, `rciGetStatistics`);
	loadSym(rciApplyNetworkSimulator, dll, `rciApplyNetworkSimulator`);
	loadSym(rciIsNetworkSimulatorActive, dll, `rciIsNetworkSimulatorActive`);
	loadSym(rciGetPlayerIndex, dll, `rciGetPlayerIndex`);

	/* RakPeerInterface */

	loadSym(rpiInitialize, dll, `rpiInitialize`);
	loadSym(rpiInitializeSecurity, dll, `rpiInitializeSecurity`);
	loadSym(rpiDisableSecurity, dll, `rpiDisableSecurity`);
	loadSym(rpiSetMaximumIncomingConnections, dll, `rpiSetMaximumIncomingConnections`);
	loadSym(rpiGetMaximumIncomingConnections, dll, `rpiGetMaximumIncomingConnections`);
	loadSym(rpiSetIncomingPassword, dll, `rpiSetIncomingPassword`);
	loadSym(rpiGetIncomingPassword, dll, `rpiGetIncomingPassword`);
	loadSym(rpiConnect, dll, `rpiConnect`);
	loadSym(rpiDisconnect, dll, `rpiDisconnect`);
	loadSym(rpiIsActive, dll, `rpiIsActive`);
	loadSym(rpiGetConnectionList, dll, `rpiGetConnectionList`);
	loadSym(rpiSend, dll, `rpiSend`);
	loadSym(rpiSendBS, dll, `rpiSendBS`);
	loadSym(rpiReceive, dll, `rpiReceive`);
	loadSym(rpiDeallocatePacket, dll, `rpiDeallocatePacket`);
	loadSym(rpiGetMaximumNumberOfPeers, dll, `rpiGetMaximumNumberOfPeers`);
	loadSym(rpiRegisterAsRemoteProcedureCall, dll, `rpiRegisterAsRemoteProcedureCall`);
	loadSym(rpiRegisterClassMemberRPC, dll, `rpiRegisterClassMemberRPC`);
	loadSym(rpiUnregisterAsRemoteProcedureCall, dll, `rpiUnregisterAsRemoteProcedureCall`);
	loadSym(rpiRPC, dll, `rpiRPC`);
	loadSym(rpiRPCBS, dll, `rpiRPCBS`);
	loadSym(rpiCloseConnection, dll, `rpiCloseConnection`);
	loadSym(rpiGetIndexFromPlayerID, dll, `rpiGetIndexFromPlayerID`);
	loadSym(rpiGetPlayerIDFromIndex, dll, `rpiGetPlayerIDFromIndex`);
	loadSym(rpiAddToBanList, dll, `rpiAddToBanList`);
	loadSym(rpiRemoveFromBanList, dll, `rpiRemoveFromBanList`);
	loadSym(rpiClearBanList, dll, `rpiClearBanList`);
	loadSym(rpiIsBanned, dll, `rpiIsBanned`);
	loadSym(rpiPing, dll, `rpiPing`);
	loadSym(rpiPingFull, dll, `rpiPingFull`);
	loadSym(rpiGetAveragePing, dll, `rpiGetAveragePing`);
	loadSym(rpiGetLastPing, dll, `rpiGetLastPing`);
	loadSym(rpiGetLowestPing, dll, `rpiGetLowestPing`);
	loadSym(rpiSetOccasionalPing, dll, `rpiSetOccasionalPing`);
	loadSym(rpiGetRemoteStaticData, dll, `rpiGetRemoteStaticData`);
	loadSym(rpiSetRemoteStaticData, dll, `rpiSetRemoteStaticData`);
	loadSym(rpiSendStaticData, dll, `rpiSendStaticData`);
	loadSym(rpiSetOfflinePingResponse, dll, `rpiSetOfflinePingResponse`);
	loadSym(rpiGetInternalID, dll, `rpiGetInternalID`);
	loadSym(rpiGetExternalID, dll, `rpiGetExternalID`);
	loadSym(rpiSetTimeoutTime, dll, `rpiSetTimeoutTime`);
	loadSym(rpiSetMTUSize, dll, `rpiSetMTUSize`);
	loadSym(rpiGetMTUSize, dll, `rpiGetMTUSize`);
	loadSym(rpiGetNumberOfAddresses, dll, `rpiGetNumberOfAddresses`);
	loadSym(rpiGetLocalIP, dll, `rpiGetLocalIP`);
	loadSym(rpiPlayerIDToDottedIP, dll, `rpiPlayerIDToDottedIP`);
	loadSym(rpiIPToPlayerID, dll, `rpiIPToPlayerID`);
	loadSym(rpiAllowConnectionResponseIPMigration, dll, `rpiAllowConnectionResponseIPMigration`);
	loadSym(rpiAdvertiseSystem, dll, `rpiAdvertiseSystem`);
	loadSym(rpiSetSplitMessageProgressInterval, dll, `rpiSetSplitMessageProgressInterval`);
	loadSym(rpiSetUnreliableTimeout, dll, `rpiSetUnreliableTimeout`);
	loadSym(rpiSetCompileFrequencyTable, dll, `rpiSetCompileFrequencyTable`);
	loadSym(rpiGetOutgoingFrequencyTable, dll, `rpiGetOutgoingFrequencyTable`);
	loadSym(rpiGenerateCompressionLayer, dll, `rpiGenerateCompressionLayer`);
	loadSym(rpiDeleteCompressionLayer, dll, `rpiDeleteCompressionLayer`);
	loadSym(rpiGetCompressionRatio, dll, `rpiGetCompressionRatio`);
	loadSym(rpiGetDecompressionRatio, dll, `rpiGetDecompressionRatio`);
	loadSym(rpiAttachPlugin, dll, `rpiAttachPlugin`);
	loadSym(rpiDetachPlugin, dll, `rpiDetachPlugin`);
	loadSym(rpiPushBackPacket, dll, `rpiPushBackPacket`);
	loadSym(rpiSetRouterInterface, dll, `rpiSetRouterInterface`);
	loadSym(rpiRemoveRouterInterface, dll, `rpiRemoveRouterInterface`);
	loadSym(rpiApplyNetworkSimulator, dll, `rpiApplyNetworkSimulator`);
	loadSym(rpiIsNetworkSimulatorActive, dll, `rpiIsNetworkSimulatorActive`);
	loadSym(rpiGetStatistics, dll, `rpiGetStatistics`);
	loadSym(rpiGetRPCMap, dll, `rpiGetRPCMap`);

	/* NetworkTypes */

	/* Packet */
	loadSym(ntPacketCreatePacket, dll, `ntPacketCreatePacket`);
	loadSym(ntPacketGetPlayerIndex, dll, `ntPacketGetPlayerIndex`);
	loadSym(ntPacketSetPlayerIndex, dll, `ntPacketSetPlayerIndex`);
	loadSym(ntPacketGetPlayerId, dll, `ntPacketGetPlayerId`);
	loadSym(ntPacketSetPlayerId, dll, `ntPacketSetPlayerId`);
	loadSym(ntPacketGetLength, dll, `ntPacketGetLength`);
	loadSym(ntPacketSetLength, dll, `ntPacketSetLength`);
	loadSym(ntPacketGetBitSize, dll, `ntPacketGetBitSize`);
	loadSym(ntPacketSetBitSize, dll, `ntPacketSetBitSize`);
	loadSym(ntPacketGetData, dll, `ntPacketGetData`);
	loadSym(ntPacketSetData, dll, `ntPacketSetData`);
	loadSym(ntPacketGetDeleteData, dll, `ntPacketGetDeleteData`);
	loadSym(ntPacketSetDeleteData, dll, `ntPacketSetDeleteData`);
	/* PlayerID */
	loadSym(ntPlayerIDCreatePlayerID, dll, `ntPlayerIDCreatePlayerID`);
	loadSym(ntDestroyPlayerID, dll, `ntDestroyPlayerID`);
	loadSym(ntPlayerIDGetBinaryAddress, dll, `ntPlayerIDGetBinaryAddress`);
	loadSym(ntPlayerIDSetBinaryAddress, dll, `ntPlayerIDSetBinaryAddress`);
	loadSym(ntPlayerIDSetBinaryAddressString, dll, `ntPlayerIDSetBinaryAddressString`);
	loadSym(ntPlayerIDGetPort, dll, `ntPlayerIDGetPort`);
	loadSym(ntPlayerIDSetPort, dll, `ntPlayerIDSetPort`);
	loadSym(ntPlayerIDCompareTo, dll, `ntPlayerIDCompareTo`);
	loadSym(ntPlayerIDGetUnassignedPlayerID, dll, `ntPlayerIDGetUnassignedPlayerID`);
	/* NetworkID */
	//loadSym(ntNetworkIDCreateNetworkID, dll, `ntNetworkIDCreateNetworkID`);
	//loadSym(ntNetworkIDGetPeerToPeerMode, dll, `ntNetworkIDGetPeerToPeerMode`);
	//loadSym(ntNetworkIDSetPeerToPeerMode, dll, `ntNetworkIDSetPeerToPeerMode`);
	loadSym(ntNetworkIDGetPlayerID, dll, `ntNetworkIDGetPlayerID`);
	loadSym(ntNetworkIDSetPlayerID, dll, `ntNetworkIDSetPlayerID`);
	loadSym(ntNetworkIDGetLocalSystemId, dll, `ntNetworkIDGetLocalSystemId`);
	loadSym(ntNetworkIDSetLocalSystemId, dll, `ntNetworkIDSetLocalSystemId`);
	loadSym(ntNetworkIDCompareTo, dll, `ntNetworkIDCompareTo`);

	/* BitStream */
	loadSym(bsGetBitStream, dll, `bsGetBitStream`);
	loadSym(bsGetBitStream1, dll, `bsGetBitStream1`);
	loadSym(bsGetBitStream3, dll, `bsGetBitStream3`);
	loadSym(bsDestroyBitStream, dll, `bsDestroyBitStream`);
	loadSym(bsReadRakNetTime, dll, `bsReadRakNetTime`);
	loadSym(bsReset, dll, `bsReset`);
	loadSym(bsSerialize, dll, `bsSerialize`);
	loadSym(bsSerializeBits, dll, `bsSerializeBits`);
	loadSym(bsWriteChars, dll, `bsWriteChars`);
	loadSym(bsWriteBSn, dll, `bsWriteBSn`);
	loadSym(bsWriteBS, dll, `bsWriteBS`);
	loadSym(bsRead, dll, `bsRead`);
	loadSym(bsResetReadPointer, dll, `bsResetReadPointer`);
	loadSym(bsResetWritePointer, dll, `bsResetWritePointer`);
	loadSym(bsAssertStreamEmpty, dll, `bsAssertStreamEmpty`);
	loadSym(bsPrintBits, dll, `bsPrintBits`);
	loadSym(bsIgnoreBits, dll, `bsIgnoreBits`);
	loadSym(bsSetWriteOffset, dll, `bsSetWriteOffset`);
	loadSym(bsGetNumberOfBitsUsed, dll, `bsGetNumberOfBitsUsed`);
	loadSym(bsGetWriteOffset, dll, `bsGetWriteOffset`);
	loadSym(bsGetNumberOfBytesUsed, dll, `bsGetNumberOfBytesUsed`);
	loadSym(bsGetReadOffset, dll, `bsGetReadOffset`);
	loadSym(bsSetReadOffset, dll, `bsSetReadOffset`);
	loadSym(bsGetNumberOfUnreadBits, dll, `bsGetNumberOfUnreadBits`);
	loadSym(bsCopyData, dll, `bsCopyData`);
	loadSym(bsSetData, dll, `bsSetData`);
	loadSym(bsGetData, dll, `bsGetData`);
	loadSym(bsWriteBits, dll, `bsWriteBits`);
	loadSym(bsWriteAlignedBytes, dll, `bsWriteAlignedBytes`);
	loadSym(bsReadAlignedBytes, dll, `bsReadAlignedBytes`);
	loadSym(bsAlignWriteToByteBoundary, dll, `bsAlignWriteToByteBoundary`);
	loadSym(bsAlignReadToByteBoundary, dll, `bsAlignReadToByteBoundary`);
	loadSym(bsReadBits, dll, `bsReadBits`);
	loadSym(bsWrite0, dll, `bsWrite0`);
	loadSym(bsWrite1, dll, `bsWrite1`);

	loadSym(bsWriteBool, dll, `bsWriteBool`);
	loadSym(bsWriteUbyte, dll, `bsWriteUbyte`);
	loadSym(bsWriteChar, dll, `bsWriteChar`);
	loadSym(bsWriteUshort, dll, `bsWriteUshort`);
	loadSym(bsWriteShort, dll, `bsWriteShort`);
	loadSym(bsWriteUint, dll, `bsWriteUint`);
	loadSym(bsWriteInt, dll, `bsWriteInt`);
	//loadSym(bsWriteUlong, dll, `bsWriteUlong`);
	//loadSym(bsWriteLong, dll, `bsWriteLong`);
	loadSym(bsWriteFloat, dll, `bsWriteFloat`);
	loadSym(bsWriteDouble, dll, `bsWriteDouble`);
	loadSym(bsWriteNetworkID, dll, `bsWriteNetworkID`);
	loadSym(bsWriteCompressedUbyte, dll, `bsWriteCompressedUbyte`);
	loadSym(bsWriteCompressedChar, dll, `bsWriteCompressedChar`);
	loadSym(bsWriteCompressedUshort, dll, `bsWriteCompressedUshort`);
	loadSym(bsWriteCompressedShort, dll, `bsWriteCompressedShort`);
	loadSym(bsWriteCompressedUint, dll, `bsWriteCompressedUint`);
	loadSym(bsWriteCompressedInt, dll, `bsWriteCompressedInt`);
	//loadSym(bsWriteCompressedUlong, dll, `bsWriteCompressedUlong`);
	//loadSym(bsWriteCompressedLong, dll, `bsWriteCompressedLong`);
	loadSym(bsWriteCompressedFloat, dll, `bsWriteCompressedFloat`);
	loadSym(bsWriteNormVector, dll, `bsWriteNormVector`);
	loadSym(bsWriteVector, dll, `bsWriteVector`);
	loadSym(bsWriteNormQuat, dll, `bsWriteNormQuat`);
	loadSym(bsWriteOrthMatrix, dll, `bsWriteOrthMatrix`);
	loadSym(bsWriteCompressedDouble, dll, `bsWriteCompressedDouble`);

	loadSym(bsReadBool, dll, `bsReadBool`);
	loadSym(bsReadUbyte, dll, `bsReadUbyte`);
	loadSym(bsReadChar, dll, `bsReadChar`);
	loadSym(bsReadUshort, dll, `bsReadUshort`);
	loadSym(bsReadShort, dll, `bsReadShort`);
	loadSym(bsReadUint, dll, `bsReadUint`);
	loadSym(bsReadInt, dll, `bsReadInt`);
	//loadSym(bsReadUlong, dll, `bsReadUlong`);
	//loadSym(bsReadLong, dll, `bsReadLong`);
	loadSym(bsReadFloat, dll, `bsReadFloat`);
	loadSym(bsReadDouble, dll, `bsReadDouble`);
	loadSym(bsReadNetworkID, dll, `bsReadNetworkID`);
	loadSym(bsReadCompressedUbyte, dll, `bsReadCompressedUbyte`);
	loadSym(bsReadCompressedChar, dll, `bsReadCompressedChar`);
	loadSym(bsReadCompressedUshort, dll, `bsReadCompressedUshort`);
	loadSym(bsReadCompressedShort, dll, `bsReadCompressedShort`);
	loadSym(bsReadCompressedUint, dll, `bsReadCompressedUint`);
	loadSym(bsReadCompressedInt, dll, `bsReadCompressedInt`);
	//loadSym(bsReadCompressedUlong, dll, `bsReadCompressedUlong`);
	//loadSym(bsReadCompressedLong, dll, `bsReadCompressedLong`);
	loadSym(bsReadCompressedFloat, dll, `bsReadCompressedFloat`);
	loadSym(bsReadNormVector, dll, `bsReadNormVector`);
	loadSym(bsReadVector, dll, `bsReadVector`);
	loadSym(bsReadNormQuat, dll, `bsReadNormQuat`);
	loadSym(bsReadOrthMatrix, dll, `bsReadOrthMatrix`);
	loadSym(bsReadCompressedDouble, dll, `bsReadCompressedDouble`);

	loadSym(bsReadBit, dll, `bsReadBit`);
	loadSym(bsAssertCopyData, dll, `bsAssertCopyData`);
	loadSym(bsSetNumberOfBitsAllocated, dll, `bsSetNumberOfBitsAllocated`);
	loadSym(bsAddBitsAndReallocate, dll, `bsAddBitsAndReallocate`);

	/* GetTime */
	loadSym(gtGetTime, dll, `gtGetTime`);
	loadSym(gtGetTimeNS, dll, `gtGetTimeNS`);

	/* RakNetStatistics */
	loadSym(ntStatisticsCreateStatistics, dll, `ntStatisticsCreateStatistics`);
	loadSym(ntStatisticsGetMessageSendBuffer, dll, `ntStatisticsGetMessageSendBuffer`);
	loadSym(ntStatisticsSetMessageSendBuffer, dll, `ntStatisticsSetMessageSendBuffer`);
	loadSym(ntStatisticsGetMessagesSent, dll, `ntStatisticsGetMessagesSent`);
	loadSym(ntStatisticsSetMessagesSent, dll, `ntStatisticsSetMessagesSent`);
	loadSym(ntStatisticsGetMessageDataBitsSent, dll, `ntStatisticsGetMessageDataBitsSent`);
	loadSym(ntStatisticsSetMessageDataBitsSent, dll, `ntStatisticsSetMessageDataBitsSent`);
	loadSym(ntStatisticsGetMessageTotalBitsSent, dll, `ntStatisticsGetMessageTotalBitsSent`);
	loadSym(ntStatisticsSetMessageTotalBitsSent, dll, `ntStatisticsSetMessageTotalBitsSent`);

	loadSym(ntStatisticsGetPacketsContainingOnlyAcknowlegements, dll, `ntStatisticsGetPacketsContainingOnlyAcknowlegements`);
	loadSym(ntStatisticsSetPacketsContainingOnlyAcknowlegements, dll, `ntStatisticsSetPacketsContainingOnlyAcknowlegements`);
	loadSym(ntStatisticsGetAcknowlegementsSent, dll, `ntStatisticsGetAcknowlegementsSent`);
	loadSym(ntStatisticsSetAcknowlegementsSent, dll, `ntStatisticsSetAcknowlegementsSent`);
	loadSym(ntStatisticsGetAcknowlegementsPending, dll, `ntStatisticsGetAcknowlegementsPending`);
	loadSym(ntStatisticsSetAcknowlegementsPending, dll, `ntStatisticsSetAcknowlegementsPending`);
	loadSym(ntStatisticsGetAcknowlegementBitsSent, dll, `ntStatisticsGetAcknowlegementBitsSent`);
	loadSym(ntStatisticsSetAcknowlegementBitsSent, dll, `ntStatisticsSetAcknowlegementBitsSent`);

	loadSym(ntStatisticsGetPacketsContainingOnlyAcknowlegementsAndResends, dll, `ntStatisticsGetPacketsContainingOnlyAcknowlegementsAndResends`);
	loadSym(ntStatisticsSetPacketsContainingOnlyAcknowlegementsAndResends, dll, `ntStatisticsSetPacketsContainingOnlyAcknowlegementsAndResends`);

	loadSym(ntStatisticsGetMessageResends, dll, `ntStatisticsGetMessageResends`);
	loadSym(ntStatisticsSetMessageResends, dll, `ntStatisticsSetMessageResends`);
	loadSym(ntStatisticsGetMessageDataBitsResent, dll, `ntStatisticsGetMessageDataBitsResent`);
	loadSym(ntStatisticsSetMessageDataBitsResent, dll, `ntStatisticsSetMessageDataBitsResent`);
	loadSym(ntStatisticsGetMessagesTotalBitsResent, dll, `ntStatisticsGetMessagesTotalBitsResent`);
	loadSym(ntStatisticsSetMessagesTotalBitsResent, dll, `ntStatisticsSetMessagesTotalBitsResent`);
	loadSym(ntStatisticsGetMessagesOnResendQueue, dll, `ntStatisticsGetMessagesOnResendQueue`);
	loadSym(ntStatisticsSetMessagesOnResendQueue, dll, `ntStatisticsSetMessagesOnResendQueue`);

	loadSym(ntStatisticsGetNumberOfUnsplitMessages, dll, `ntStatisticsGetNumberOfUnsplitMessages`);
	loadSym(ntStatisticsSetNumberOfUnsplitMessages, dll, `ntStatisticsSetNumberOfUnsplitMessages`);
	loadSym(ntStatisticsGetNumberOfSplitMessages, dll, `ntStatisticsGetNumberOfSplitMessages`);
	loadSym(ntStatisticsSetNumberOfSplitMessages, dll, `ntStatisticsSetNumberOfSplitMessages`);
	loadSym(ntStatisticsGetTotalSplits, dll, `ntStatisticsGetTotalSplits`);
	loadSym(ntStatisticsSetTotalSplits, dll, `ntStatisticsSetTotalSplits`);

	loadSym(ntStatisticsGetPacketsSent, dll, `ntStatisticsGetPacketsSent`);
	loadSym(ntStatisticsSetPacketsSent, dll, `ntStatisticsSetPacketsSent`);

	loadSym(ntStatisticsGetEncryptionBitsSent, dll, `ntStatisticsGetEncryptionBitsSent`);
	loadSym(ntStatisticsSetEncryptionBitsSent, dll, `ntStatisticsSetEncryptionBitsSent`);
	loadSym(ntStatisticsGetTotalBitsSent, dll, `ntStatisticsGetTotalBitsSent`);
	//loadSym(ntStatisticsSetTotalBitsSent, dll, `ntStatisticsSetTotalBitsSent`);

	loadSym(ntStatisticsGetSequencedMessagesOutOfOrder, dll, `ntStatisticsGetSequencedMessagesOutOfOrder`);
	loadSym(ntStatisticsSetSequencedMessagesOutOfOrder, dll, `ntStatisticsSetSequencedMessagesOutOfOrder`);
	loadSym(ntStatisticsGetSequencedMessagesInOrder, dll, `ntStatisticsGetSequencedMessagesInOrder`);
	loadSym(ntStatisticsSetSequencedMessagesInOrder, dll, `ntStatisticsSetSequencedMessagesInOrder`);

	loadSym(ntStatisticsGetOrderedMessagesOutOfOrder, dll, `ntStatisticsGetOrderedMessagesOutOfOrder`);
	loadSym(ntStatisticsSetOrderedMessagesOutOfOrder, dll, `ntStatisticsSetOrderedMessagesOutOfOrder`);
	loadSym(ntStatisticsGetOrderedMessagesInOrder, dll, `ntStatisticsGetOrderedMessagesInOrder`);
	loadSym(ntStatisticsSetOrderedMessagesInOrder, dll, `ntStatisticsSetOrderedMessagesInOrder`);

	loadSym(ntStatisticsGetPacketsReceived, dll, `ntStatisticsGetPacketsReceived`);
	loadSym(ntStatisticsSetPacketsReceived, dll, `ntStatisticsSetPacketsReceived`);
	loadSym(ntStatisticsGetPacketsWithBadCRCReceived, dll, `ntStatisticsGetPacketsWithBadCRCReceived`);
	loadSym(ntStatisticsSetPacketsWithBadCRCReceived, dll, `ntStatisticsSetPacketsWithBadCRCReceived`);
	loadSym(ntStatisticsGetBitsReceived, dll, `ntStatisticsGetBitsReceived`);
	loadSym(ntStatisticsSetBitsReceived, dll, `ntStatisticsSetBitsReceived`);
	loadSym(ntStatisticsGetBitsWithBadCRCReceived, dll, `ntStatisticsGetBitsWithBadCRCReceived`);
	loadSym(ntStatisticsSetBitsWithBadCRCReceived, dll, `ntStatisticsSetBitsWithBadCRCReceived`);
	loadSym(ntStatisticsGetAcknowlegementsReceived, dll, `ntStatisticsGetAcknowlegementsReceived`);
	loadSym(ntStatisticsSetAcknowlegementsReceived, dll, `ntStatisticsSetAcknowlegementsReceived`);
	loadSym(ntStatisticsGetDuplicateAcknowlegementsReceived, dll, `ntStatisticsGetDuplicateAcknowlegementsReceived`);
	loadSym(ntStatisticsSetDuplicateAcknowlegementsReceived, dll, `ntStatisticsSetDuplicateAcknowlegementsReceived`);
	loadSym(ntStatisticsGetMessagesReceived, dll, `ntStatisticsGetMessagesReceived`);
	loadSym(ntStatisticsSetMessagesReceived, dll, `ntStatisticsSetMessagesReceived`);
	loadSym(ntStatisticsGetInvalidMessagesReceived, dll, `ntStatisticsGetInvalidMessagesReceived`);
	loadSym(ntStatisticsSetInvalidMessagesReceived, dll, `ntStatisticsSetInvalidMessagesReceived`);
	loadSym(ntStatisticsGetDuplicateMessagesReceived, dll, `ntStatisticsGetDuplicateMessagesReceived`);
	loadSym(ntStatisticsSetDuplicateMessagesReceived, dll, `ntStatisticsSetDuplicateMessagesReceived`);
	loadSym(ntStatisticsGetMessagesWaitingForReassembly, dll, `ntStatisticsGetMessagesWaitingForReassembly`);
	loadSym(ntStatisticsSetMessagesWaitingForReassembly, dll, `ntStatisticsSetMessagesWaitingForReassembly`);
	loadSym(ntStatisticsGetInternalOutputQueueSize, dll, `ntStatisticsGetInternalOutputQueueSize`);
	loadSym(ntStatisticsSetInternalOutputQueueSize, dll, `ntStatisticsSetInternalOutputQueueSize`);

	loadSym(ntStatisticsGetBitsPerSecond, dll, `ntStatisticsGetBitsPerSecond`);
	loadSym(ntStatisticsSetBitsPerSecond, dll, `ntStatisticsSetBitsPerSecond`);

	loadSym(ntStatisticsGetConnectionStartTime, dll, `ntStatisticsGetConnectionStartTime`);
	loadSym(ntStatisticsSetConnectionStartTime, dll, `ntStatisticsSetConnectionStartTime`);
	
	loadSym(ntStatisticsToString, dll, `ntStatisticsToString`);
}

