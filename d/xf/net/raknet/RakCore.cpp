#include "RakServerInterface.h"
#include "RakClientInterface.h"
#include "RakPeerInterface.h"
#include "RakNetworkFactory.h"
#include "BitStream.h"
#include "BitStream_NoTemplate.h"
#include "RakNetStatistics.h"
#include "GetTime.h"
#include <cstdio>
#include <cstring>
//#include <winsock2.h>

extern "C"
{
	__declspec(dllexport) void dupa()
	{
		printf("dupa\n");
	}

	/* RakServerInterface */

	__declspec(dllexport) bool rsiStart(RakServerInterface* rsi, unsigned short AllowedPlayers,
		unsigned int depreciated, int threadSleepTimer, unsigned short port, const char *forceHostAddress = 0)
	{
		return rsi->Start(AllowedPlayers, depreciated, threadSleepTimer, port, forceHostAddress);
	}

	__declspec(dllexport) void rsiInitializeSecurity(RakServerInterface* rsi, const char *privateKeyE, const char *privateKeyN)
	{
		rsi->InitializeSecurity(privateKeyE, privateKeyN);
	}

	__declspec(dllexport) void rsiDisableSecurity(RakServerInterface* rsi)
	{
		rsi->DisableSecurity();
	}

	__declspec(dllexport) void rsiSetPassword(RakServerInterface* rsi, const char *_password)
	{
		rsi->SetPassword(_password);
	}

	__declspec(dllexport) bool rsiHasPassword(RakServerInterface* rsi)
	{
		return rsi->HasPassword();
	}

	__declspec(dllexport) void rsiDisconnect(RakServerInterface* rsi, unsigned int blockDuration,
		unsigned char orderingChannel = 0)
	{
		rsi->Disconnect(blockDuration, orderingChannel);
	}

	__declspec(dllexport) bool rsiSend(RakServerInterface* rsi, const char *data, const int length,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, PlayerID* playerId, bool broadcast)
	{
		return rsi->Send(data, length, priority, reliability, orderingChannel, *playerId, broadcast);
	}

	__declspec(dllexport) bool rsiSendBS(RakServerInterface* rsi, RakNet::BitStream *bitStream, PacketPriority priority,
		PacketReliability reliability, char orderingChannel, PlayerID* playerId, bool broadcast)
	{
		return rsi->Send(bitStream, priority, reliability, orderingChannel, *playerId, broadcast);
	}

	__declspec(dllexport) Packet* rsiReceive(RakServerInterface* rsi)
	{
		return rsi->Receive();
	}

	__declspec(dllexport) void rsiKick(RakServerInterface* rsi, const PlayerID* playerId)
	{
		rsi->Kick(*playerId);
	}

	__declspec(dllexport) void rsiDeallocatePacket(RakServerInterface* rsi, Packet *packet)
	{
		rsi->DeallocatePacket(packet);
	}

	__declspec(dllexport) void rsiSetAllowedPlayers(RakServerInterface* rsi, unsigned short AllowedPlayers)
	{
		rsi->SetAllowedPlayers(AllowedPlayers);
	}

	__declspec(dllexport) unsigned short rsiGetAllowedPlayers(RakServerInterface* rsi)
	{
		return rsi->GetAllowedPlayers();
	}

	__declspec(dllexport) unsigned short rsiGetConnectedPlayers(RakServerInterface* rsi)
	{
		return rsi->GetConnectedPlayers();
	}

	__declspec(dllexport) void rsiGetPlayerIPFromID(RakServerInterface* rsi, const PlayerID* playerId,
		char returnValue[ 22 ], unsigned short *port)
	{
		rsi->GetPlayerIPFromID(*playerId, returnValue, port);
	}

	__declspec(dllexport) void rsiPingPlayer(RakServerInterface* rsi, const PlayerID* playerId)
	{
		rsi->PingPlayer(*playerId);
	}

	__declspec(dllexport) int rsiGetAveragePing(RakServerInterface* rsi, const PlayerID* playerId)
	{
		return rsi->GetAveragePing(*playerId);
	}

	__declspec(dllexport) int rsiGetLastPing(RakServerInterface* rsi, const PlayerID* playerId)
	{
		return rsi->GetLastPing(*playerId);
	}

	__declspec(dllexport) int rsiGetLowestPing(RakServerInterface* rsi, const PlayerID* playerId)
	{
		return rsi->GetLowestPing(*playerId);
	}

	__declspec(dllexport) void rsiStartOccasionalPing(RakServerInterface* rsi)
	{
		rsi->StartOccasionalPing();
	}

	__declspec(dllexport) void rsiStopOccasionalPing(RakServerInterface* rsi)
	{
		rsi->StopOccasionalPing();
	}

	__declspec(dllexport) bool rsiIsActive(RakServerInterface* rsi)
	{
		return rsi->IsActive();
	}

	__declspec(dllexport) unsigned int rsiGetSynchronizedRandomInteger(RakServerInterface* rsi)
	{
		return rsi->GetSynchronizedRandomInteger();
	}

	__declspec(dllexport) void rsiStartSynchronizedRandomInteger(RakServerInterface* rsi)
	{
		rsi->StartSynchronizedRandomInteger();
	}

	__declspec(dllexport) void rsiStopSynchronizedRandomInteger(RakServerInterface* rsi)
	{
		rsi->StopSynchronizedRandomInteger();
	}

	__declspec(dllexport) bool rsiGenerateCompressionLayer(RakServerInterface* rsi,
		unsigned int inputFrequencyTable[ 256 ], bool inputLayer)
	{
		return rsi->GenerateCompressionLayer(inputFrequencyTable, inputLayer);
	}

	__declspec(dllexport) bool rsiDeleteCompressionLayer(RakServerInterface* rsi, bool inputLayer)
	{
		return rsi->DeleteCompressionLayer(inputLayer);
	}

	__declspec(dllexport) void rsiRegisterAsRemoteProcedureCall(RakServerInterface* rsi,
		char* uniqueID, void ( *functionPointer ) ( RPCParameters *rpcParms ))
	{
		rsi->RegisterAsRemoteProcedureCall(uniqueID, functionPointer);
	}

	__declspec(dllexport) void rsiRegisterClassMemberRPC(RakServerInterface* rsi, char* uniqueID, void *functionPointer)
	{
		rsi->RegisterClassMemberRPC(uniqueID, functionPointer);
	}

	__declspec(dllexport) void rsiUnregisterAsRemoteProcedureCall(RakServerInterface* rsi, char* uniqueID)
	{
		rsi->UnregisterAsRemoteProcedureCall(uniqueID);
	}

	__declspec(dllexport) bool rsiRPC(RakServerInterface* rsi, char* uniqueID, const char *data, unsigned int bitLength,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, PlayerID* playerId, bool broadcast,
		bool shiftTimestamp, NetworkID* networkID, RakNet::BitStream *replyFromTarget)
	{
		return rsi->RPC(uniqueID, data, bitLength, priority, reliability, orderingChannel, *playerId,
			broadcast, shiftTimestamp, *networkID, replyFromTarget);
	}

	__declspec(dllexport) bool rsiRPCBS(RakServerInterface* rsi, char* uniqueID, RakNet::BitStream *bitStream,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, PlayerID* playerId, bool broadcast,
		bool shiftTimestamp, NetworkID* networkID, RakNet::BitStream *replyFromTarget)
	{
		return rsi->RPC(uniqueID, bitStream, priority, reliability, orderingChannel, *playerId, broadcast,
			shiftTimestamp, *networkID, replyFromTarget);
	}

	__declspec(dllexport) void rsiSetTrackFrequencyTable(RakServerInterface* rsi, bool b)
	{
		rsi->SetTrackFrequencyTable(b);
	}
	__declspec(dllexport) bool rsiGetSendFrequencyTable(RakServerInterface* rsi, unsigned int outputFrequencyTable[ 256 ])
	{
		return rsi->GetSendFrequencyTable(outputFrequencyTable);
	}

	__declspec(dllexport) float rsiGetCompressionRatio(RakServerInterface* rsi)
	{
		return rsi->GetCompressionRatio();
	}

	__declspec(dllexport) float rsiGetDecompressionRatio(RakServerInterface* rsi)
	{
		return rsi->GetDecompressionRatio();
	}

	__declspec(dllexport) void rsiAttachPlugin(RakServerInterface* rsi, PluginInterface *messageHandler)
	{
		rsi->AttachPlugin(messageHandler);
	}

	__declspec(dllexport) void rsiDetachPlugin(RakServerInterface* rsi, PluginInterface *messageHandler)
	{
		rsi->DetachPlugin(messageHandler);
	}

	__declspec(dllexport) RakNet::BitStream * rsiGetStaticServerData(RakServerInterface* rsi)
	{
		return rsi->GetStaticServerData();
	}

	__declspec(dllexport) void rsiSetStaticServerData(RakServerInterface* rsi, const char *data, const int length)
	{
		rsi->SetStaticServerData(data, length);
	}

	__declspec(dllexport) void rsiSetRelayStaticClientData(RakServerInterface* rsi, bool b)
	{
		rsi->SetRelayStaticClientData(b);
	}

	__declspec(dllexport) void rsiSendStaticServerDataToClient(RakServerInterface* rsi, const PlayerID* playerId)
	{
		rsi->SendStaticServerDataToClient(*playerId);
	}

	__declspec(dllexport) void rsiSetOfflinePingResponse(RakServerInterface* rsi, const char *data, const unsigned int length)
	{
		rsi->SetOfflinePingResponse(data, length);
	}

	__declspec(dllexport) RakNet::BitStream * rsiGetStaticClientData(RakServerInterface* rsi, const PlayerID* playerId)
	{
		return rsi->GetStaticClientData(*playerId);
	}

	__declspec(dllexport) void rsiSetStaticClientData(RakServerInterface* rsi, const PlayerID* playerId, const char *data, const int length)
	{
		rsi->SetStaticClientData(*playerId, data, length);
	}

	__declspec(dllexport) void rsiChangeStaticClientData(RakServerInterface* rsi,
		const PlayerID* playerChangedId, PlayerID* playerToSendToId)
	{
		rsi->ChangeStaticClientData(*playerChangedId, *playerToSendToId);
	}

	__declspec(dllexport) unsigned int rsiGetNumberOfAddresses(RakServerInterface* rsi)
	{
		return rsi->GetNumberOfAddresses();
	}

	__declspec(dllexport) const char* rsiGetLocalIP(RakServerInterface* rsi, unsigned int index)
	{
		return rsi->GetLocalIP(index);
	}

	__declspec(dllexport) PlayerID* rsiGetInternalID(RakServerInterface* rsi)
	{
		PlayerID* result = new PlayerID;
		*result = rsi->GetInternalID();
		return result;
	}

	__declspec(dllexport) void rsiPushBackPacket(RakServerInterface* rsi, Packet *packet, bool pushAtHead)
	{
		rsi->PushBackPacket(packet, pushAtHead);
	}

	__declspec(dllexport) void rsiSetRouterInterface(RakServerInterface* rsi, RouterInterface *routerInterface)
	{
		rsi->SetRouterInterface(routerInterface);
	}

	__declspec(dllexport) void rsiRemoveRouterInterface(RakServerInterface* rsi, RouterInterface *routerInterface)
	{
		rsi->RemoveRouterInterface(routerInterface);
	}

	__declspec(dllexport) int rsiGetIndexFromPlayerID(RakServerInterface* rsi, const PlayerID* playerId)
	{
		return rsi->GetIndexFromPlayerID(*playerId);
	}

	__declspec(dllexport) PlayerID* rsiGetPlayerIDFromIndex(RakServerInterface* rsi, int index)
	{
		PlayerID* result = new PlayerID;
		*result = rsi->GetPlayerIDFromIndex(index);
		return result;
	}

	__declspec(dllexport) void rsiAddToBanList(RakServerInterface* rsi, const char *IP)
	{
		rsi->AddToBanList(IP);
	}

	__declspec(dllexport) void rsiRemoveFromBanList(RakServerInterface* rsi, const char *IP)
	{
		rsi->RemoveFromBanList(IP);
	}

	__declspec(dllexport) void rsiClearBanList(RakServerInterface* rsi)
	{
		rsi->ClearBanList();
	}

	__declspec(dllexport) bool rsiIsBanned(RakServerInterface* rsi, const char *IP)
	{
		return rsi->IsBanned(IP);
	}

	__declspec(dllexport) bool rsiIsActivePlayerID(RakServerInterface* rsi, const PlayerID* playerId)
	{
		return rsi->IsActivePlayerID(*playerId);
	}

	__declspec(dllexport) void rsiSetTimeoutTime(RakServerInterface* rsi, RakNetTime timeMS, const PlayerID* target)
	{
		rsi->SetTimeoutTime(timeMS, *target);
	}

	__declspec(dllexport) bool rsiSetMTUSize(RakServerInterface* rsi, int size)
	{
		return rsi->SetMTUSize(size);
	}

	__declspec(dllexport) int rsiGetMTUSize(RakServerInterface* rsi)
	{
		return rsi->GetMTUSize();
	}

	__declspec(dllexport) void rsiAdvertiseSystem(RakServerInterface* rsi, const char *host,
		unsigned short remotePort, const char *data, int dataLength)
	{
		rsi->AdvertiseSystem(host, remotePort, data, dataLength);
	}

	__declspec(dllexport) RakNetStatisticsStruct * rsiGetStatistics(RakServerInterface* rsi, PlayerID* playerId)
	{
		return rsi->GetStatistics(*playerId);
	}

	__declspec(dllexport) void rsiApplyNetworkSimulator(RakServerInterface* rsi, double maxSendBPS, unsigned short minExtraPing,
		unsigned short extraPingVariance)
	{
		rsi->ApplyNetworkSimulator(maxSendBPS, minExtraPing, extraPingVariance);
	}

	__declspec(dllexport) bool rsiIsNetworkSimulatorActive(RakServerInterface* rsi)
	{
		return rsi->IsNetworkSimulatorActive();
	}

	/* RakClientInterface */

	__declspec(dllexport) bool rciConnect(RakClientInterface* rci, const char* host, unsigned short serverPort,
		unsigned short clientPort, unsigned int depreciated, int threadSleepTimer)
	{
		return rci->Connect(host, serverPort, clientPort, depreciated, threadSleepTimer);
	}

	__declspec(dllexport) void rciDisconnect(RakClientInterface* rci, unsigned int blockDuration, unsigned char orderingChannel=0)
	{
		rci->Disconnect(blockDuration, orderingChannel);
	}

	__declspec(dllexport) void rciInitializeSecurity(RakClientInterface* rci, const char *privKeyP, const char *privKeyQ)
	{
		rci->InitializeSecurity(privKeyP, privKeyQ);
	}

	__declspec(dllexport) void rciSetPassword(RakClientInterface* rci, const char *_password)
	{
		rci->SetPassword(_password);
	}

	__declspec(dllexport) bool rciHasPassword(RakClientInterface* rci)
	{
		return rci->HasPassword();
	}

	__declspec(dllexport) bool rciSend(RakClientInterface* rci, const char *data, const int length,
		PacketPriority priority, PacketReliability reliability, char orderingChannel)
	{
		return rci->Send(data, length, priority, reliability, orderingChannel);
	}

	__declspec(dllexport) bool rciSendBS(RakClientInterface* rci, RakNet::BitStream * bitStream,
		PacketPriority priority, PacketReliability reliability, char orderingChannel)
	{
		return rci->Send(bitStream, priority, reliability, orderingChannel);
	}

	__declspec(dllexport) Packet* rciReceive(RakClientInterface* rci)
	{
		return rci->Receive();
	}

	__declspec(dllexport) void rciDeallocatePacket(RakClientInterface* rci, Packet *packet)
	{
		rci->DeallocatePacket(packet);
	}

	__declspec(dllexport) void rciPingServer(RakClientInterface* rci)
	{
		rci->PingServer();
	}

	__declspec(dllexport) void rciPingServerFull(RakClientInterface* rci, const char* host, unsigned short serverPort,
		unsigned short clientPort, bool onlyReplyOnAcceptingConnections)
	{
		rci->PingServer(host, serverPort, clientPort, onlyReplyOnAcceptingConnections);
	}

	__declspec(dllexport) int rciGetAveragePing(RakClientInterface* rci)
	{
		return rci->GetAveragePing();
	}

	__declspec(dllexport) int rciGetLastPing(RakClientInterface* rci)
	{
		return rci->GetLastPing();
	}

	__declspec(dllexport) int rciGetLowestPing(RakClientInterface* rci)
	{
		return rci->GetLowestPing();
	}

	__declspec(dllexport) int rciGetPlayerPing(RakClientInterface* rci, const PlayerID* playerId)
	{
		return rci->GetPlayerPing(*playerId);
	}

	__declspec(dllexport) void rciStartOccasionalPing(RakClientInterface* rci)
	{
		rci->StartOccasionalPing();
	}

	__declspec(dllexport) void rciStopOccasionalPing(RakClientInterface* rci)
	{
		rci->StopOccasionalPing();
	}

	__declspec(dllexport) bool rciIsConnected(RakClientInterface* rci)
	{
		return rci->IsConnected();
	}

	__declspec(dllexport) unsigned int rciGetSynchronizedRandomInteger(RakClientInterface* rci)
	{
		return rci->GetSynchronizedRandomInteger();
	}

	__declspec(dllexport) bool rciGenerateCompressionLayer(RakClientInterface* rci,
		unsigned int inputFrequencyTable[ 256 ], bool inputLayer)
	{
		return rci->GenerateCompressionLayer(inputFrequencyTable, inputLayer);
	}

	__declspec(dllexport) bool rciDeleteCompressionLayer(RakClientInterface* rci, bool inputLayer)
	{
		return rci->DeleteCompressionLayer(inputLayer);
	}

	__declspec(dllexport) void rciRegisterAsRemoteProcedureCall(RakClientInterface* rci, char* uniqueID,
		void ( *functionPointer ) ( RPCParameters *rpcParms ))
	{
		rci->RegisterAsRemoteProcedureCall(uniqueID, functionPointer);
	}

	__declspec(dllexport) void rciRegisterClassMemberRPC(RakClientInterface* rci, char* uniqueID, void *functionPointer)
	{
		rci->RegisterClassMemberRPC(uniqueID, functionPointer);
	}

	__declspec(dllexport) void rciUnregisterAsRemoteProcedureCall(RakClientInterface* rci, char* uniqueID)
	{
		rci->UnregisterAsRemoteProcedureCall(uniqueID);
	}

	__declspec(dllexport) bool rciRPC(RakClientInterface* rci, char* uniqueID, const char *data,
		unsigned int bitLength, PacketPriority priority, PacketReliability reliability, char orderingChannel,
		bool shiftTimestamp, NetworkID* networkID, RakNet::BitStream *replyFromTarget)
	{
		return rci->RPC(uniqueID, data, bitLength, priority, reliability, orderingChannel,
			shiftTimestamp, *networkID, replyFromTarget);
	}

	__declspec(dllexport) bool rciRPCBS(RakClientInterface* rci, char* uniqueID, RakNet::BitStream *bitStream,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, bool shiftTimestamp,
		NetworkID* networkID, RakNet::BitStream *replyFromTarget)
	{
		return rci->RPC(uniqueID, bitStream, priority ,reliability, orderingChannel, shiftTimestamp,
			*networkID, replyFromTarget);
	}

	__declspec(dllexport) void rciSetTrackFrequencyTable(RakClientInterface* rci, bool b)
	{
		rci->SetTrackFrequencyTable(b);
	}

	__declspec(dllexport) bool rciGetSendFrequencyTable(RakClientInterface* rci, unsigned int outputFrequencyTable[ 256 ])
	{
		return rci->GetSendFrequencyTable(outputFrequencyTable);
	}

	__declspec(dllexport) float rciGetCompressionRatio(RakClientInterface* rci)
	{
		return rci->GetCompressionRatio();
	}

	__declspec(dllexport) float rciGetDecompressionRatio(RakClientInterface* rci)
	{
		return rci->GetDecompressionRatio();
	}

	__declspec(dllexport) void rciAttachPlugin(RakClientInterface* rci, PluginInterface *messageHandler)
	{
		rci->AttachPlugin(messageHandler);
	}

	__declspec(dllexport) void rciDetachPlugin(RakClientInterface* rci, PluginInterface *messageHandler)
	{
		rci->DetachPlugin(messageHandler);
	}

	__declspec(dllexport) RakNet::BitStream * rciGetStaticServerData(RakClientInterface* rci)
	{
		return rci->GetStaticServerData();
	}

	__declspec(dllexport) void rciSetStaticServerData(RakClientInterface* rci, const char *data, const int length)
	{
		rci->SetStaticServerData(data, length);
	}

	__declspec(dllexport) RakNet::BitStream * rciGetStaticClientData(RakClientInterface* rci, const PlayerID* playerId)
	{
		return rci->GetStaticClientData(*playerId);
	}

	__declspec(dllexport) void rciSetStaticClientData(RakClientInterface* rci, const PlayerID* playerId,
		const char *data, const int length)
	{
		rci->SetStaticClientData(*playerId, data, length);
	}

	__declspec(dllexport) void rciSendStaticClientDataToServer(RakClientInterface* rci)
	{
		rci->SendStaticClientDataToServer();
	}

	__declspec(dllexport) PlayerID* rciGetServerID(RakClientInterface* rci)
	{
		PlayerID* result = new PlayerID;
		*result = rci->GetServerID();
		return result;
	}

	__declspec(dllexport) PlayerID* rciGetPlayerID(RakClientInterface* rci)
	{
		PlayerID* output = new PlayerID;
		*output = rci->GetPlayerID();
		return output;
	}

	__declspec(dllexport) PlayerID* rciGetInternalID(RakClientInterface* rci)
	{
		PlayerID* output = new PlayerID;
		*output = rci->GetInternalID();
		return output;
	}

	__declspec(dllexport) const char* rciPlayerIDToDottedIP(RakClientInterface* rci, const PlayerID* playerId)
	{
		return rci->PlayerIDToDottedIP(*playerId);
	}

	__declspec(dllexport) void rciPushBackPacket(RakClientInterface* rci, Packet *packet, bool pushAtHead)
	{
		rci->PushBackPacket(packet, pushAtHead);
	}

	__declspec(dllexport) void rciSetRouterInterface(RakClientInterface* rci, RouterInterface *routerInterface)
	{
		rci->SetRouterInterface(routerInterface);
	}

	__declspec(dllexport) void rciRemoveRouterInterface(RakClientInterface* rci, RouterInterface *routerInterface)
	{
		rci->RemoveRouterInterface(routerInterface);
	}

	__declspec(dllexport) void rciSetTimeoutTime(RakClientInterface* rci, RakNetTime timeMS)
	{
		rci->SetTimeoutTime(timeMS);
	}

	__declspec(dllexport) bool rciSetMTUSize(RakClientInterface* rci, int size)
	{
		return rci->SetMTUSize(size);
	}

	__declspec(dllexport) int rciGetMTUSize(RakClientInterface* rci)
	{
		return rci->GetMTUSize();
	}

	__declspec(dllexport) void rciAllowConnectionResponseIPMigration(RakClientInterface* rci, bool allow)
	{
		rci->AllowConnectionResponseIPMigration(allow);
	}

	__declspec(dllexport) void rciAdvertiseSystem(RakClientInterface* rci, const char *host,
		unsigned short remotePort, const char *data, int dataLength)
	{
		rci->AdvertiseSystem(host, remotePort, data, dataLength);
	}

	__declspec(dllexport) RakNetStatisticsStruct * const rciGetStatistics(RakClientInterface* rci)
	{
		return rci->GetStatistics();
	}

	__declspec(dllexport) void rciApplyNetworkSimulator(RakClientInterface* rci, double maxSendBPS,
		unsigned short minExtraPing, unsigned short extraPingVariance)
	{
		rci->ApplyNetworkSimulator(maxSendBPS, minExtraPing, extraPingVariance);
	}

	__declspec(dllexport) bool rciIsNetworkSimulatorActive(RakClientInterface* rci)
	{
		return rci->IsNetworkSimulatorActive();
	}

	__declspec(dllexport) PlayerIndex rciGetPlayerIndex(RakClientInterface* rci)
	{
		return rci->GetPlayerIndex();
	}

	/* RakPeerInterface */

	__declspec(dllexport) bool rpiInitialize(RakPeerInterface* rpi, unsigned short maxConnections, unsigned short localPort,
		int _threadSleepTimer, const char *forceHostAddress=0)
	{
		return rpi->Initialize(maxConnections, localPort, _threadSleepTimer, forceHostAddress);
	}

	__declspec(dllexport) void rpiInitializeSecurity(RakPeerInterface* rpi, const char *pubKeyE, const char *pubKeyN,
		const char *privKeyP, const char *privKeyQ)
	{
		rpi->InitializeSecurity(pubKeyE, pubKeyN, privKeyP, privKeyQ);
	}

	__declspec(dllexport) void rpiDisableSecurity(RakPeerInterface* rpi)
	{
		rpi->DisableSecurity();
	}

	__declspec(dllexport) void rpiSetMaximumIncomingConnections(RakPeerInterface* rpi, unsigned short numberAllowed)
	{
		rpi->SetMaximumIncomingConnections(numberAllowed);
	}

	__declspec(dllexport) unsigned short rpiGetMaximumIncomingConnections(RakPeerInterface* rpi)
	{
		return rpi->GetMaximumIncomingConnections();
	}

	__declspec(dllexport) void rpiSetIncomingPassword(RakPeerInterface* rpi, const char* passwordData, int passwordDataLength)
	{
		rpi->SetIncomingPassword(passwordData, passwordDataLength);
	}

	__declspec(dllexport) void rpiGetIncomingPassword(RakPeerInterface* rpi, char* passwordData, int *passwordDataLength)
	{
		rpi->GetIncomingPassword(passwordData, passwordDataLength);
	}

	__declspec(dllexport) bool rpiConnect(RakPeerInterface* rpi, const char* host, unsigned short remotePort, char* passwordData,
		int passwordDataLength)
	{
		return rpi->Connect(host, remotePort, passwordData, passwordDataLength);
	}

	__declspec(dllexport) void rpiDisconnect(RakPeerInterface* rpi, unsigned int blockDuration, unsigned char orderingChannel=0)
	{
		rpi->Disconnect(blockDuration, orderingChannel);
	}

	__declspec(dllexport) bool rpiIsActive(RakPeerInterface* rpi)
	{
		return rpi->IsActive();
	}

	__declspec(dllexport) bool rpiGetConnectionList(RakPeerInterface* rpi, PlayerID *remoteSystems,
		unsigned short *numberOfSystems)
	{
		return rpi->GetConnectionList(remoteSystems, numberOfSystems);
	}

	__declspec(dllexport) bool rpiSend(RakPeerInterface* rpi, const char *data, const int length, PacketPriority priority,
		PacketReliability reliability, char orderingChannel, PlayerID* playerId, bool broadcast)
	{
		return rpi->Send(data, length, priority, reliability, orderingChannel, *playerId, broadcast);
	}

	__declspec(dllexport) bool rpiSendBS(RakPeerInterface* rpi, RakNet::BitStream * bitStream, PacketPriority priority,
		PacketReliability reliability, char orderingChannel, PlayerID* playerId, bool broadcast)
	{
		return rpi->Send(bitStream, priority, reliability, orderingChannel, *playerId, broadcast);
	}

	__declspec(dllexport) Packet* rpiReceive(RakPeerInterface* rpi)
	{
		return rpi->Receive();
	}

	__declspec(dllexport) void rpiDeallocatePacket(RakPeerInterface* rpi, Packet *packet)
	{
		rpi->DeallocatePacket(packet);
	}

	__declspec(dllexport) unsigned short rpiGetMaximumNumberOfPeers(RakPeerInterface* rpi)
	{
		return rpi->GetMaximumNumberOfPeers();
	}

	__declspec(dllexport) void rpiRegisterAsRemoteProcedureCall(RakPeerInterface* rpi, char* uniqueID,
		void ( *functionPointer ) ( RPCParameters *rpcParms ))
	{
		rpi->RegisterAsRemoteProcedureCall(uniqueID, functionPointer);
	}

	__declspec(dllexport) void rpiRegisterClassMemberRPC(RakPeerInterface* rpi, char* uniqueID, void *functionPointer)
	{
		rpi->RegisterClassMemberRPC(uniqueID, functionPointer);
	}

	__declspec(dllexport) void rpiUnregisterAsRemoteProcedureCall(RakPeerInterface* rpi, char* uniqueID)
	{
		rpi->UnregisterAsRemoteProcedureCall(uniqueID);
	}

	__declspec(dllexport) bool rpiRPC(RakPeerInterface* rpi, char* uniqueID, const char *data, unsigned int bitLength,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, PlayerID* playerId, bool broadcast,
		bool shiftTimestamp, NetworkID* networkID, RakNet::BitStream *replyFromTarget)
	{
		return rpi->RPC(uniqueID, data, bitLength, priority, reliability, orderingChannel, *playerId, broadcast,
			shiftTimestamp, *networkID, replyFromTarget);
	}

	__declspec(dllexport) bool rpiRPCBS(RakPeerInterface* rpi, char* uniqueID, RakNet::BitStream *bitStream,
		PacketPriority priority, PacketReliability reliability, char orderingChannel, PlayerID* playerId, bool broadcast,
		bool shiftTimestamp, NetworkID* networkID, RakNet::BitStream *replyFromTarget)
	{
		return rpi->RPC(uniqueID, bitStream, priority, reliability, orderingChannel, *playerId, broadcast,
			shiftTimestamp, *networkID, replyFromTarget);
	}

	__declspec(dllexport) void rpiCloseConnection(RakPeerInterface* rpi, const PlayerID* target, bool sendDisconnectionNotification,
		unsigned char orderingChannel=0)
	{
		rpi->CloseConnection(*target, sendDisconnectionNotification, orderingChannel);
	}

	__declspec(dllexport) int rpiGetIndexFromPlayerID(RakPeerInterface* rpi, const PlayerID* playerId)
	{
		return rpi->GetIndexFromPlayerID(*playerId);
	}

	__declspec(dllexport) PlayerID* rpiGetPlayerIDFromIndex(RakPeerInterface* rpi, int index)
	{
		PlayerID* output = new PlayerID;
		*output = rpi->GetPlayerIDFromIndex(index);
		return output;
	}

	__declspec(dllexport) void rpiAddToBanList(RakPeerInterface* rpi, const char *IP, RakNetTime milliseconds=0)
	{
		rpi->AddToBanList(IP, milliseconds);
	}

	__declspec(dllexport) void rpiRemoveFromBanList(RakPeerInterface* rpi, const char *IP)
	{
		rpi->RemoveFromBanList(IP);
	}

	__declspec(dllexport) void rpiClearBanList(RakPeerInterface* rpi)
	{
		rpi->ClearBanList();
	}

	__declspec(dllexport) bool rpiIsBanned(RakPeerInterface* rpi, const char *IP)
	{
		return rpi->IsBanned(IP);
	}

	__declspec(dllexport) void rpiPing(RakPeerInterface* rpi, const PlayerID* target)
	{
		rpi->Ping(*target);
	}

	__declspec(dllexport) void rpiPingFull(RakPeerInterface* rpi, const char* host, unsigned short remotePort,
		bool onlyReplyOnAcceptingConnections)
	{
		rpi->Ping(host, remotePort, onlyReplyOnAcceptingConnections);
	}

	__declspec(dllexport) int rpiGetAveragePing(RakPeerInterface* rpi, const PlayerID* playerId)
	{
		return rpi->GetAveragePing(*playerId);
	}

	__declspec(dllexport) int rpiGetLastPing(RakPeerInterface* rpi, const PlayerID* playerId)
	{
		return rpi->GetLastPing(*playerId);
	}

	__declspec(dllexport) int rpiGetLowestPing(RakPeerInterface* rpi, const PlayerID* playerId)
	{
		return rpi->GetLowestPing(*playerId);
	}

	__declspec(dllexport) void rpiSetOccasionalPing(RakPeerInterface* rpi, bool doPing)
	{
		rpi->SetOccasionalPing(doPing);
	}

	__declspec(dllexport) RakNet::BitStream * rpiGetRemoteStaticData(RakPeerInterface* rpi, const PlayerID* playerId)
	{
		return rpi->GetRemoteStaticData(*playerId);
	}

	__declspec(dllexport) void rpiSetRemoteStaticData(RakPeerInterface* rpi, const PlayerID* playerId, const char *data, const int length)
	{
		rpi->SetRemoteStaticData(*playerId, data, length);
	}

	__declspec(dllexport) void rpiSendStaticData(RakPeerInterface* rpi, const PlayerID* target)
	{
		rpi->SendStaticData(*target);
	}

	__declspec(dllexport) void rpiSetOfflinePingResponse(RakPeerInterface* rpi, const char *data, const unsigned int length)
	{
		rpi->SetOfflinePingResponse(data, length);
	}

	__declspec(dllexport) PlayerID* rpiGetInternalID(RakPeerInterface* rpi)
	{
		PlayerID* output = new PlayerID;
		*output = rpi->GetInternalID();
		return output;
	}

	__declspec(dllexport) PlayerID* rpiGetExternalID(RakPeerInterface* rpi, const PlayerID* target)
	{
		PlayerID* output = new PlayerID;
		*output = rpi->GetExternalID(*target);
		return output;
	}

	__declspec(dllexport) void rpiSetTimeoutTime(RakPeerInterface* rpi, RakNetTime timeMS, const PlayerID* target)
	{
		rpi->SetTimeoutTime(timeMS, *target);
	}

	__declspec(dllexport) bool rpiSetMTUSize(RakPeerInterface* rpi, int size)
	{
		return rpi->SetMTUSize(size);
	}

	__declspec(dllexport) int rpiGetMTUSize(RakPeerInterface* rpi)
	{
		return rpi->GetMTUSize();
	}

	__declspec(dllexport) unsigned rpiGetNumberOfAddresses(RakPeerInterface* rpi)
	{
		return rpi->GetNumberOfAddresses();
	}

	__declspec(dllexport) const char* rpiGetLocalIP(RakPeerInterface* rpi, unsigned int index)
	{
		return rpi->GetLocalIP(index);
	}

	__declspec(dllexport) const char* rpiPlayerIDToDottedIP(RakPeerInterface* rpi, const PlayerID* playerId)
	{
		return rpi->PlayerIDToDottedIP(*playerId);
	}

	__declspec(dllexport) void rpiIPToPlayerID(RakPeerInterface* rpi, const char* host, unsigned short remotePort, PlayerID* *playerId)
	{
		rpi->IPToPlayerID(host, remotePort, *playerId);
	}

	__declspec(dllexport) void rpiAllowConnectionResponseIPMigration(RakPeerInterface* rpi, bool allow)
	{
		rpi->AllowConnectionResponseIPMigration(allow);
	}

	__declspec(dllexport) void rpiAdvertiseSystem(RakPeerInterface* rpi, const char *host, unsigned short remotePort,
		const char *data, int dataLength)
	{
		rpi->AdvertiseSystem(host, remotePort, data, dataLength);
	}

	__declspec(dllexport) void rpiSetSplitMessageProgressInterval(RakPeerInterface* rpi, int interval)
	{
		rpi->SetSplitMessageProgressInterval(interval);
	}

	__declspec(dllexport) void rpiSetUnreliableTimeout(RakPeerInterface* rpi, RakNetTime timeoutMS)
	{
		rpi->SetUnreliableTimeout(timeoutMS);
	}

	__declspec(dllexport) void rpiSetCompileFrequencyTable(RakPeerInterface* rpi, bool doCompile)
	{
		rpi->SetCompileFrequencyTable(doCompile);
	}

	__declspec(dllexport) bool rpiGetOutgoingFrequencyTable(RakPeerInterface* rpi, unsigned int outputFrequencyTable[ 256 ])
	{
		return rpi->GetOutgoingFrequencyTable(outputFrequencyTable);
	}

	__declspec(dllexport) bool rpiGenerateCompressionLayer(RakPeerInterface* rpi,
		unsigned int inputFrequencyTable[ 256 ], bool inputLayer)
	{
		return rpi->GenerateCompressionLayer(inputFrequencyTable, inputLayer);
	}

	__declspec(dllexport) bool rpiDeleteCompressionLayer(RakPeerInterface* rpi, bool inputLayer)
	{
		return rpi->DeleteCompressionLayer(inputLayer);
	}

	__declspec(dllexport) float rpiGetCompressionRatio(RakPeerInterface* rpi)
	{
		return rpi->GetCompressionRatio();
	}

	__declspec(dllexport) float rpiGetDecompressionRatio(RakPeerInterface* rpi)
	{
		return rpi->GetDecompressionRatio();
	}

	__declspec(dllexport) void rpiAttachPlugin(RakPeerInterface* rpi, PluginInterface *plugin)
	{
		rpi->AttachPlugin(plugin);
	}

	__declspec(dllexport) void rpiDetachPlugin(RakPeerInterface* rpi, PluginInterface *messageHandler)
	{
		rpi->DetachPlugin(messageHandler);
	}

	__declspec(dllexport) void rpiPushBackPacket(RakPeerInterface* rpi, Packet *packet, bool pushAtHead)
	{
		rpi->PushBackPacket(packet, pushAtHead);
	}

	__declspec(dllexport) void rpiSetRouterInterface(RakPeerInterface* rpi, RouterInterface *routerInterface)
	{
		rpi->SetRouterInterface(routerInterface);
	}

	__declspec(dllexport) void rpiRemoveRouterInterface(RakPeerInterface* rpi, RouterInterface *routerInterface)
	{
		rpi->RemoveRouterInterface(routerInterface);
	}

	__declspec(dllexport) void rpiApplyNetworkSimulator(RakPeerInterface* rpi, double maxSendBPS,
		unsigned short minExtraPing, unsigned short extraPingVariance)
	{
		rpi->ApplyNetworkSimulator(maxSendBPS, minExtraPing, extraPingVariance);
	}

	__declspec(dllexport) bool rpiIsNetworkSimulatorActive(RakPeerInterface* rpi)
	{
		return rpi->IsNetworkSimulatorActive();
	}

	__declspec(dllexport) RakNetStatisticsStruct * const rpiGetStatistics(RakPeerInterface* rpi, const PlayerID* playerId)
	{
		return rpi->GetStatistics(*playerId);
	}

	__declspec(dllexport) RPCMap * rpiGetRPCMap(RakPeerInterface* rpi, const PlayerID* playerId)
	{
		return rpi->GetRPCMap(*playerId);
	}

	/* RakNetworkFactory */

	__declspec(dllexport) RakClientInterface* rnfGetRakClientInterface()
	{
		return RakNetworkFactory::GetRakClientInterface();
	}

	__declspec(dllexport) RakServerInterface* rnfGetRakServerInterface()
	{
		return RakNetworkFactory::GetRakServerInterface();
	}

	__declspec(dllexport) RakPeerInterface* rnfGetRakPeerInterface()
	{
		return RakNetworkFactory::GetRakPeerInterface();
	}

	__declspec(dllexport) ConsoleServer* rnfGetConsoleServer()
	{
		return RakNetworkFactory::GetConsoleServer();
	}

	__declspec(dllexport) ReplicaManager* rnfGetReplicaManager()
	{
		return RakNetworkFactory::GetReplicaManager();
	}

	__declspec(dllexport) LogCommandParser* rnfGetLogCommandParser()
	{
		return RakNetworkFactory::GetLogCommandParser();
	}

	__declspec(dllexport) PacketLogger* rnfGetPacketLogger()
	{
		return RakNetworkFactory::GetPacketLogger();
	}

	__declspec(dllexport) RakNetCommandParser* rnfGetRakNetCommandParser()
	{
		return RakNetworkFactory::GetRakNetCommandParser();
	}

	__declspec(dllexport) RakNetTransport* rnfGetRakNetTransport()
	{
		return RakNetworkFactory::GetRakNetTransport();
	}

	__declspec(dllexport) TelnetTransport* rnfGetTelnetTransport()
	{
		return RakNetworkFactory::GetTelnetTransport();
	}

	__declspec(dllexport) PacketConsoleLogger* rnfGetPacketConsoleLogger()
	{
		return RakNetworkFactory::GetPacketConsoleLogger();
	}

	__declspec(dllexport) PacketFileLogger* rnfGetPacketFileLogger()
	{
		return RakNetworkFactory::GetPacketFileLogger();
	}

	__declspec(dllexport) Router* rnfGetRouter()
	{
		return RakNetworkFactory::GetRouter();
	}

	__declspec(dllexport) ConnectionGraph* rnfGetConnectionGraph()
	{
		return RakNetworkFactory::GetConnectionGraph();
	}

	__declspec(dllexport) void rnfDestroyRakClientInterface(RakClientInterface* i)
	{
		RakNetworkFactory::DestroyRakClientInterface(i);
	}

	__declspec(dllexport) void rnfDestroyRakServerInterface(RakServerInterface* i)
	{
		RakNetworkFactory::DestroyRakServerInterface(i);
	}

	__declspec(dllexport) void rnfDestroyRakPeerInterface(RakPeerInterface* i)
	{
		RakNetworkFactory::DestroyRakPeerInterface(i);
	}

	__declspec(dllexport) void rnfDestroyConsoleServer(ConsoleServer* i)
	{
		RakNetworkFactory::DestroyConsoleServer(i);
	}

	__declspec(dllexport) void rnfDestroyReplicaManager(ReplicaManager* i)
	{
		RakNetworkFactory::DestroyReplicaManager(i);
	}

	__declspec(dllexport) void rnfDestroyLogCommandParser(LogCommandParser* i)
	{
		RakNetworkFactory::DestroyLogCommandParser(i);
	}

	__declspec(dllexport) void rnfDestroyPacketLogger(PacketLogger* i)
	{
		RakNetworkFactory::DestroyPacketLogger(i);
	}

	__declspec(dllexport) void rnfDestroyRakNetCommandParser(RakNetCommandParser* i)
	{
		RakNetworkFactory::DestroyRakNetCommandParser(i);
	}

	__declspec(dllexport) void rnfDestroyRakNetTransport(RakNetTransport* i)
	{
		RakNetworkFactory::DestroyRakNetTransport(i);
	}

	__declspec(dllexport) void rnfDestroyTelnetTransport(TelnetTransport* i)
	{
		RakNetworkFactory::DestroyTelnetTransport(i);
	}

	__declspec(dllexport) void rnfDestroyPacketConsoleLogger(PacketConsoleLogger* i)
	{
		RakNetworkFactory::DestroyPacketConsoleLogger(i);
	}

	__declspec(dllexport) void rnfDestroyPacketFileLogger(PacketFileLogger* i)
	{
		RakNetworkFactory::DestroyPacketFileLogger(i);
	}

	__declspec(dllexport) void rnfDestroyRouter(Router* i)
	{
		RakNetworkFactory::DestroyRouter(i);
	}

	__declspec(dllexport) void rnfDestroyConnectionGraph(ConnectionGraph* i)
	{
		RakNetworkFactory::DestroyConnectionGraph(i);
	}

	/* NetworkTypes */

	/* Packet */

	__declspec(dllexport) Packet* ntPacketCreatePacket()
	{
		return new Packet();
	}

	__declspec(dllexport) PlayerIndex ntPacketGetPlayerIndex(Packet* p)
	{
		return p->playerIndex;
	}

	__declspec(dllexport) void ntPacketSetPlayerIndex(Packet* p, PlayerIndex pi)
	{
		p->playerIndex = pi;
	}

	__declspec(dllexport) PlayerID* ntPacketGetPlayerId(Packet* p)
	{
		/*
		PlayerID* output = new PlayerID();
		*output = p->playerId;
		return output;
		*/
		return &(p->playerId);
	}

	__declspec(dllexport) void ntPacketSetPlayerId(Packet* p, PlayerID* pi)
	{
		memcpy(&(p->playerId), pi, sizeof(PlayerID));
	}

	__declspec(dllexport) unsigned int ntPacketGetLength(Packet* p)
	{
		return p->length;
	}

	__declspec(dllexport) void ntPacketSetLength(Packet* p, unsigned int len)
	{
		p->length = len;
	}

	__declspec(dllexport) unsigned int ntPacketGetBitSize(Packet* p)
	{
		return p->bitSize;
	}

	__declspec(dllexport) void ntPacketSetBitSize(Packet* p, unsigned int bitSize)
	{
		p->bitSize = bitSize;
	}

	__declspec(dllexport) unsigned char* ntPacketGetData(Packet* p)
	{
		return p->data;
	}

	__declspec(dllexport) void ntPacketSetData(Packet* p, unsigned char* data)
	{
		p->data = data;
	}

	__declspec(dllexport) bool ntPacketGetDeleteData(Packet* p)
	{
		return p->deleteData;
	}

	__declspec(dllexport) void ntPacketSetDeleteData(Packet* p, bool dd)
	{
		p->deleteData = dd;
	}

	/* RakNetStatistics */

	__declspec(dllexport) RakNetStatisticsStruct* ntStatisticsCreateStatistics()
	{
		return new RakNetStatisticsStruct();
	}

	__declspec(dllexport) void ntStatisticsSetMessageSendBuffer(RakNetStatisticsStruct* rnss, unsigned int* data)
	{
		memcpy(rnss->messageSendBuffer, data, sizeof(unsigned int) * NUMBER_OF_PRIORITIES);
	}

	__declspec(dllexport) unsigned int* ntStatisticsGetMessageSendBuffer(RakNetStatisticsStruct* rnss)
	{
		return rnss->messageSendBuffer;
	}
	
	__declspec(dllexport) unsigned int* ntStatisticsGetMessagesSent(RakNetStatisticsStruct* rnss)
	{
		return rnss->messagesSent;
	}

	__declspec(dllexport) void ntStatisticsSetMessagesSent(RakNetStatisticsStruct* rnss, unsigned int* data)
	{
		memcpy(rnss->messagesSent, data, sizeof(unsigned int) * NUMBER_OF_PRIORITIES);
	}

	__declspec(dllexport) unsigned int* ntStatisticsGetMessageDataBitsSent(RakNetStatisticsStruct* rnss)
	{
		return rnss->messageDataBitsSent;
	}

	__declspec(dllexport) void ntStatisticsSetMessageDataBitsSent(RakNetStatisticsStruct* rnss, unsigned int* data)
	{
		memcpy(rnss->messageDataBitsSent, data, sizeof(unsigned int) * NUMBER_OF_PRIORITIES);
	}

	__declspec(dllexport) unsigned int* ntStatisticsGetMessageTotalBitsSent(RakNetStatisticsStruct* rnss)
	{
		return rnss->messageTotalBitsSent;
	}

	__declspec(dllexport) void ntStatisticsSetMessageTotalBitsSent(RakNetStatisticsStruct* rnss, unsigned int* data)
	{
		memcpy(rnss->messageTotalBitsSent, data, sizeof(unsigned int) * NUMBER_OF_PRIORITIES);
	}

	__declspec(dllexport) unsigned int ntStatisticsGetPacketsContainingOnlyAcknowlegements(RakNetStatisticsStruct* rnss)
	{
		return rnss->packetsContainingOnlyAcknowlegements;
	}

	__declspec(dllexport) void ntStatisticsSetPacketsContainingOnlyAcknowlegements(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->packetsContainingOnlyAcknowlegements = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetAcknowlegementsSent(RakNetStatisticsStruct* rnss)
	{
		return rnss->acknowlegementsSent;
	}

	__declspec(dllexport) void ntStatisticsSetAcknowlegementsSent(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->acknowlegementsSent = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetAcknowlegementsPending(RakNetStatisticsStruct* rnss)
	{
		return rnss->acknowlegementsPending;
	}

	__declspec(dllexport) void ntStatisticsSetAcknowlegementsPending(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->acknowlegementsPending = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetAcknowlegementBitsSent(RakNetStatisticsStruct* rnss)
	{
		return rnss->acknowlegementBitsSent;
	}

	__declspec(dllexport) void ntStatisticsSetAcknowlegementBitsSent(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->acknowlegementBitsSent = data;
	}


	__declspec(dllexport) unsigned int ntStatisticsGetPacketsContainingOnlyAcknowlegementsAndResends(RakNetStatisticsStruct* rnss)
	{
		return rnss->packetsContainingOnlyAcknowlegementsAndResends;
	}

	__declspec(dllexport) void ntStatisticsSetPacketsContainingOnlyAcknowlegementsAndResends(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->packetsContainingOnlyAcknowlegementsAndResends = data;
	}


	__declspec(dllexport) unsigned int ntStatisticsGetMessageResends(RakNetStatisticsStruct* rnss)
	{
		return rnss->messageResends;
	}

	__declspec(dllexport) void ntStatisticsSetMessageResends(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->messageResends = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetMessageDataBitsResent(RakNetStatisticsStruct* rnss)
	{
		return rnss->messageDataBitsResent;
	}

	__declspec(dllexport) void ntStatisticsSetMessageDataBitsResent(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->messageDataBitsResent = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetMessagesTotalBitsResent(RakNetStatisticsStruct* rnss)
	{
		return rnss->messagesTotalBitsResent;
	}

	__declspec(dllexport) void ntStatisticsSetMessagesTotalBitsResent(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->messagesTotalBitsResent = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetMessagesOnResendQueue(RakNetStatisticsStruct* rnss)
	{
		return rnss->messagesOnResendQueue;
	}

	__declspec(dllexport) void ntStatisticsSetMessagesOnResendQueue(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->messagesOnResendQueue = data;
	}


	__declspec(dllexport) unsigned int ntStatisticsGetNumberOfUnsplitMessages(RakNetStatisticsStruct* rnss)
	{
		return rnss->numberOfUnsplitMessages;
	}

	__declspec(dllexport) void ntStatisticsSetNumberOfUnsplitMessages(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->numberOfUnsplitMessages = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetNumberOfSplitMessages(RakNetStatisticsStruct* rnss)
	{
		return rnss->numberOfSplitMessages;
	}

	__declspec(dllexport) void ntStatisticsSetNumberOfSplitMessages(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->numberOfSplitMessages = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetTotalSplits(RakNetStatisticsStruct* rnss)
	{
		return rnss->totalSplits;
	}

	__declspec(dllexport) void ntStatisticsSetTotalSplits(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->totalSplits = data;
	}


	__declspec(dllexport) unsigned int ntStatisticsGetPacketsSent(RakNetStatisticsStruct* rnss)
	{
		return rnss->packetsSent;
	}

	__declspec(dllexport) void ntStatisticsSetPacketsSent(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->packetsSent = data;
	}


	__declspec(dllexport) unsigned int ntStatisticsGetEncryptionBitsSent(RakNetStatisticsStruct* rnss)
	{
		return rnss->encryptionBitsSent;
	}

	__declspec(dllexport) void ntStatisticsSetEncryptionBitsSent(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->encryptionBitsSent = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetTotalBitsSent(RakNetStatisticsStruct* rnss)
	{
		return rnss->totalBitsSent;
	}

	__declspec(dllexport) void ntStatisticsSetTotalBitsSents(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->totalBitsSent = data;
	}


	__declspec(dllexport) unsigned int ntStatisticsGetSequencedMessagesOutOfOrder(RakNetStatisticsStruct* rnss)
	{
		return rnss->sequencedMessagesOutOfOrder;
	}

	__declspec(dllexport) void ntStatisticsSetSequencedMessagesOutOfOrder(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->sequencedMessagesOutOfOrder = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetSequencedMessagesInOrder(RakNetStatisticsStruct* rnss)
	{
		return rnss->sequencedMessagesInOrder;
	}

	__declspec(dllexport) void ntStatisticsSetSequencedMessagesInOrder(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->sequencedMessagesInOrder = data;
	}


	__declspec(dllexport) unsigned int ntStatisticsGetOrderedMessagesOutOfOrder(RakNetStatisticsStruct* rnss)
	{
		return rnss->orderedMessagesOutOfOrder;
	}

	__declspec(dllexport) void ntStatisticsSetOrderedMessagesOutOfOrder(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->orderedMessagesOutOfOrder = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetOrderedMessagesInOrder(RakNetStatisticsStruct* rnss)
	{
		return rnss->orderedMessagesInOrder;
	}

	__declspec(dllexport) void ntStatisticsSetOrderedMessagesInOrder(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->orderedMessagesInOrder = data;
	}


	__declspec(dllexport) unsigned int ntStatisticsGetPacketsReceived(RakNetStatisticsStruct* rnss)
	{
		return rnss->packetsReceived;
	}

	__declspec(dllexport) void ntStatisticsSetPacketsReceived(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->packetsReceived = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetPacketsWithBadCRCReceived(RakNetStatisticsStruct* rnss)
	{
		return rnss->packetsWithBadCRCReceived;
	}

	__declspec(dllexport) void ntStatisticsSetPacketsWithBadCRCReceived(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->packetsWithBadCRCReceived = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetBitsReceived(RakNetStatisticsStruct* rnss)
	{
		return rnss->bitsReceived;
	}

	__declspec(dllexport) void ntStatisticsSetBitsReceived(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->bitsReceived = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetBitsWithBadCRCReceived(RakNetStatisticsStruct* rnss)
	{
		return rnss->bitsWithBadCRCReceived;
	}

	__declspec(dllexport) void ntStatisticsSetBitsWithBadCRCReceived(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->bitsWithBadCRCReceived = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetAcknowlegementsReceived(RakNetStatisticsStruct* rnss)
	{
		return rnss->acknowlegementsReceived;
	}

	__declspec(dllexport) void ntStatisticsSetAcknowlegementsReceived(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->acknowlegementsReceived = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetDuplicateAcknowlegementsReceived(RakNetStatisticsStruct* rnss)
	{
		return rnss->duplicateAcknowlegementsReceived;
	}

	__declspec(dllexport) void ntStatisticsSetDuplicateAcknowlegementsReceived(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->duplicateAcknowlegementsReceived = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetMessagesReceived(RakNetStatisticsStruct* rnss)
	{
		return rnss->messagesReceived;
	}

	__declspec(dllexport) void ntStatisticsSetMessagesReceived(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->messagesReceived = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetInvalidMessagesReceived(RakNetStatisticsStruct* rnss)
	{
		return rnss->invalidMessagesReceived;
	}

	__declspec(dllexport) void ntStatisticsSetInvalidMessagesReceived(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->invalidMessagesReceived = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetDuplicateMessagesReceived(RakNetStatisticsStruct* rnss)
	{
		return rnss->duplicateMessagesReceived;
	}

	__declspec(dllexport) void ntStatisticsSetDuplicateMessagesReceived(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->duplicateMessagesReceived = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetMessagesWaitingForReassembly(RakNetStatisticsStruct* rnss)
	{
		return rnss->messagesWaitingForReassembly;
	}

	__declspec(dllexport) void ntStatisticsSetMessagesWaitingForReassembly(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->messagesWaitingForReassembly = data;
	}

	__declspec(dllexport) unsigned int ntStatisticsGetInternalOutputQueueSize(RakNetStatisticsStruct* rnss)
	{
		return rnss->internalOutputQueueSize;
	}

	__declspec(dllexport) void ntStatisticsSetInternalOutputQueueSize(RakNetStatisticsStruct* rnss, unsigned int data)
	{
		rnss->internalOutputQueueSize = data;
	}


	__declspec(dllexport) double ntStatisticsGetBitsPerSecond(RakNetStatisticsStruct* rnss)
	{
		return rnss->bitsPerSecond;
	}

	__declspec(dllexport) void ntStatisticsSetBitsPerSecond(RakNetStatisticsStruct* rnss, double data)
	{
		rnss->bitsPerSecond = data;
	}


	__declspec(dllexport) RakNetTime* ntStatisticsGetConnectionStartTime(RakNetStatisticsStruct* rnss)
	{
		return &(rnss->connectionStartTime);
	}

	__declspec(dllexport) void ntStatisticsSetConnectionStartTime(RakNetStatisticsStruct* rnss, RakNetTime* rnt)
	{
		memcpy(&(rnss->connectionStartTime), rnt, sizeof(RakNetTime));
	}

	__declspec(dllexport) void ntStatisticsToString(RakNetStatisticsStruct* rnss, char* buffer, int level)
	{
		StatisticsToString(rnss, buffer, level);
	}

	/* PlayerID */

	__declspec(dllexport) PlayerID* ntPlayerIDCreatePlayerID()
	{
		return new PlayerID();
	}

	__declspec(dllexport) void ntDestroyPlayerID(PlayerID* pid)
	{
		delete pid;
	}

	__declspec(dllexport) unsigned int ntPlayerIDGetBinaryAddress(PlayerID* pid)
	{
		return pid->binaryAddress;
	}

	__declspec(dllexport) void ntPlayerIDSetBinaryAddress(PlayerID* pid, unsigned int ba)
	{
		pid->binaryAddress = ba;
	}

	__declspec(dllexport) void ntPlayerIDSetBinaryAddressString(PlayerID* pid, char *str)
	{
	//	pid->binaryAddress = inet_addr(str);
	}

	__declspec(dllexport) unsigned short ntPlayerIDGetPort(PlayerID* pid)
	{
		return pid->port;
	}

	__declspec(dllexport) void ntPlayerIDSetPort(PlayerID* pid, unsigned short port)
	{
		pid->port = port;
	}

	__declspec(dllexport) int ntPlayerIDCompareTo(PlayerID* pid1, PlayerID* pid2)
	{
		return ( (pid1->binaryAddress - pid2->binaryAddress) || ( (pid1->binaryAddress == pid2->binaryAddress)
			&& (pid1->port - pid2->port) ) );
	}

	__declspec(dllexport) PlayerID* ntPlayerIDGetUnassignedPlayerID()
	{
		PlayerID* output = new PlayerID();
		*output = UNASSIGNED_PLAYER_ID;
		return output;
	}

	/* NetworkID */

	__declspec(dllexport) NetworkID* ntNetworkIDCreatePlayerID()
	{
		return new NetworkID();
	}

	/*
	__declspec(dllexport) bool ntNetworkIDGetPeerToPeerMode(NetworkID* nid)
	{
		return NetworkID::peerToPeerMode;
	}

	__declspec(dllexport) void ntNetworkIDSetPeerToPeerMode(NetworkID* nid, bool mode)
	{
		NetworkID::peerToPeerMode = mode;
	}
	*/

	/*
	__declspec(dllexport) PlayerID* ntNetworkIDGetPlayerID(NetworkID* nid)
	{
		PlayerID* output = new PlayerID();
		*output = nid->playerId;
		return output;
	}
	*/

	__declspec(dllexport) PlayerID* ntNetworkIDGetPlayerID(NetworkID* nid)
	{
		return &(nid->playerId);
	}

	__declspec(dllexport) void ntNetworkIDSetPlayerID(NetworkID* nid, PlayerID* pid)
	{
		memcpy(&(nid->playerId), pid, sizeof(PlayerID));
	}

	__declspec(dllexport) unsigned short ntNetworkIDGetLocalSystemId(NetworkID* nid)
	{
		return nid->localSystemId;
	}

	__declspec(dllexport) void ntNetworkIDSetLocalSystemId(NetworkID* nid, unsigned short lsid)
	{
		nid->localSystemId = lsid;
	}

	__declspec(dllexport) int ntNetworkIDCompareTo(NetworkID* nid1, NetworkID* nid2)
	{
		return ntPlayerIDCompareTo(&(nid1->playerId),&(nid2->playerId)) || nid1->localSystemId - nid2->localSystemId;
	}

	/* BitStream */

	__declspec(dllexport) RakNet::BitStream* bsGetBitStream()
	{
		return new RakNet::BitStream();
	}

	__declspec(dllexport) RakNet::BitStream* bsGetBitStream1(int initialBytesToAllocate)
	{
		return new RakNet::BitStream(initialBytesToAllocate);
	}

	__declspec(dllexport) RakNet::BitStream* bsGetBitStream3(unsigned char* _data, unsigned int lengthInBytes, bool _copyData)
	{
		return new RakNet::BitStream(_data, lengthInBytes, _copyData);
	}

	__declspec(dllexport) void bsDestroyBitStream(RakNet::BitStream* bs)
	{
		delete bs;
	}

	__declspec(dllexport) bool bsReadRakNetTime(RakNet::BitStream* bs, RakNetTime rnt)
	{
		return bs->Read(rnt);
	}

	__declspec(dllexport) void bsReset(RakNet::BitStream* bs)
	{
		bs->Reset();
	}

	__declspec(dllexport) bool bsSerialize(RakNet::BitStream* bs, bool writeToBitstream,  char* input, const int numberOfBytes)
	{
		return bs->Serialize(writeToBitstream, input, numberOfBytes);
	}

	__declspec(dllexport) bool bsSerializeBits(RakNet::BitStream* bs, bool writeToBitstream, unsigned char* input,
		int numberOfBitsToSerialize, const bool rightAlignedBits = true)
	{
		return bs->SerializeBits(writeToBitstream, input, numberOfBitsToSerialize, rightAlignedBits);
	}

	__declspec(dllexport) void bsWriteChars(RakNet::BitStream* bs, const char* input, const int numberOfBytes)
	{
		bs->Write(input, numberOfBytes);
	}

	__declspec(dllexport) void bsWriteBSn(RakNet::BitStream* bs, RakNet::BitStream *bitStream, int numberOfBits)
	{
		bs->Write(bitStream, numberOfBits);
	}

	__declspec(dllexport) void bsWriteBS(RakNet::BitStream* bs, RakNet::BitStream *bitStream)
	{
		bs->Write(bitStream);
	}

	__declspec(dllexport) bool bsRead(RakNet::BitStream* bs, char* output, const int numberOfBytes)
	{
		return bs->Read(output, numberOfBytes);
	}

	__declspec(dllexport) void bsResetReadPointer(RakNet::BitStream* bs)
	{
		bs->ResetReadPointer();
	}

	__declspec(dllexport) void bsResetWritePointer(RakNet::BitStream* bs)
	{
		bs->ResetWritePointer();
	}

	__declspec(dllexport) void bsAssertStreamEmpty(RakNet::BitStream* bs)
	{
		bs->AssertStreamEmpty();
	}

	__declspec(dllexport) void bsPrintBits(RakNet::BitStream* bs)
	{
		bs->PrintBits();
	}

	__declspec(dllexport) void bsIgnoreBits(RakNet::BitStream* bs, const int numberOfBits)
	{
		bs->IgnoreBits(numberOfBits);
	}

	__declspec(dllexport) void bsSetWriteOffset(RakNet::BitStream* bs, const int offset)
	{
		bs->SetWriteOffset(offset);
	}

	__declspec(dllexport) int bsGetNumberOfBitsUsed(RakNet::BitStream* bs)
	{
		return bs->GetNumberOfBitsUsed();
	}

	__declspec(dllexport) int bsGetWriteOffset(RakNet::BitStream* bs)
	{
		return bs->GetWriteOffset();
	}

	__declspec(dllexport) int bsGetNumberOfBytesUsed(RakNet::BitStream* bs)
	{
		return bs->GetNumberOfBytesUsed();
	}

	__declspec(dllexport) int bsGetReadOffset(RakNet::BitStream* bs)
	{
		return bs->GetReadOffset();
	}

	__declspec(dllexport) void bsSetReadOffset(RakNet::BitStream* bs, int newReadOffset)
	{
		bs->SetReadOffset(newReadOffset);
	}

	__declspec(dllexport) int bsGetNumberOfUnreadBits(RakNet::BitStream* bs)
	{
		return bs->GetNumberOfUnreadBits();
	}

	__declspec(dllexport) int bsCopyData(RakNet::BitStream* bs, unsigned char** _data)
	{
		return bs->CopyData(_data);
	}

	__declspec(dllexport) void bsSetData(RakNet::BitStream* bs, unsigned char *input)
	{
		bs->SetData(input);
	}

	__declspec(dllexport) unsigned char* bsGetData(RakNet::BitStream* bs)
	{
		return bs->GetData();
	}

	__declspec(dllexport) void bsWriteBits(RakNet::BitStream* bs, const unsigned char* input, int numberOfBitsToWrite,
		const bool rightAlignedBits = true)
	{
		bs->WriteBits(input, numberOfBitsToWrite, rightAlignedBits);
	}

	__declspec(dllexport) void bsWriteAlignedBytes(RakNet::BitStream* bs, const unsigned char *input,	const int numberOfBytesToWrite)
	{
		bs->WriteAlignedBytes(input, numberOfBytesToWrite);
	}

	__declspec(dllexport) bool bsReadAlignedBytes(RakNet::BitStream* bs, unsigned char *output,const int numberOfBytesToRead)
	{
		return bs->ReadAlignedBytes(output, numberOfBytesToRead);
	}

	__declspec(dllexport) void bsAlignWriteToByteBoundary(RakNet::BitStream* bs)
	{
		bs->AlignWriteToByteBoundary();
	}

	__declspec(dllexport) void bsAlignReadToByteBoundary(RakNet::BitStream* bs)
	{
		bs->AlignReadToByteBoundary();
	}

	__declspec(dllexport) bool bsReadBits(RakNet::BitStream* bs, unsigned char *output, int numberOfBitsToRead,
		const bool alignBitsToRight = true)
	{
		return bs->ReadBits(output, numberOfBitsToRead, alignBitsToRight);
	}

	__declspec(dllexport) void bsWrite0(RakNet::BitStream* bs)
	{
		bs->Write0();
	}

	__declspec(dllexport) void bsWrite1(RakNet::BitStream* bs)
	{
		bs->Write1();
	}

	__declspec(dllexport) void bsWriteBool(RakNet::BitStream* bs, const bool input)
	{
		bs->Write(input);
	}

	__declspec(dllexport) void bsWriteUbyte(RakNet::BitStream* bs, const unsigned char input)
	{
		bs->Write(input);
	}

	__declspec(dllexport) void bsWriteChar(RakNet::BitStream* bs, const char input)
	{
		bs->Write(input);
	}

	__declspec(dllexport) void bsWriteUshort(RakNet::BitStream* bs, const unsigned short input)
	{
		bs->Write(input);
	}

	__declspec(dllexport) void bsWriteShort(RakNet::BitStream* bs, const short input)
	{
		bs->Write(input);
	}

	__declspec(dllexport) void bsWriteUint(RakNet::BitStream* bs, const unsigned int input)
	{
		bs->Write(input);
	}

	__declspec(dllexport) void bsWriteInt(RakNet::BitStream* bs, const int input)
	{
		bs->Write(input);
	}

	/*  //  commented out according to issue 1 in bugzilla
	__declspec(dllexport) void bsWriteUlong(RakNet::BitStream* bs, const unsigned long input)
	{
		bs->Write(input);
	}

	__declspec(dllexport) void bsWriteLong(RakNet::BitStream* bs, const long input)
	{
		bs->Write(input);
	}
	*/

	__declspec(dllexport) void bsWriteFloat(RakNet::BitStream* bs, const float input)
	{
		bs->Write(input);
	}

	__declspec(dllexport) void bsWriteDouble(RakNet::BitStream* bs, const double input)
	{
		bs->Write(input);
	}

	__declspec(dllexport) void bsWriteNetworkID(RakNet::BitStream* bs, NetworkID* networkId)
	{
		bs->Write(*networkId);
	}

	__declspec(dllexport) void bsWriteCompressedUbyte(RakNet::BitStream* bs, const unsigned char input)
	{
		bs->WriteCompressed(input);
	}

	__declspec(dllexport) void bsWriteCompressedChar(RakNet::BitStream* bs, const char input)
	{
		bs->WriteCompressed(input);
	}

	__declspec(dllexport) void bsWriteCompressedUshort(RakNet::BitStream* bs, const unsigned short input)
	{
		bs->WriteCompressed(input);
	}

	__declspec(dllexport) void bsWriteCompressedShort(RakNet::BitStream* bs, const short input)
	{
		bs->WriteCompressed(input);
	}

	__declspec(dllexport) void bsWriteCompressedUint(RakNet::BitStream* bs, const unsigned int input)
	{
		bs->WriteCompressed(input);
	}

	__declspec(dllexport) void bsWriteCompressedInt(RakNet::BitStream* bs, const int input)
	{
		bs->WriteCompressed(input);
	}

	/*  //  commented out according to issue 1 in bugzilla
	__declspec(dllexport) void bsWriteCompressedUlong(RakNet::BitStream* bs, const unsigned long input)
	{
		bs->WriteCompressed(input);
	}

	__declspec(dllexport) void bsWriteCompressedLong(RakNet::BitStream* bs, const long input)
	{
		bs->WriteCompressed(input);
	}
	*/

	__declspec(dllexport) void bsWriteCompressedFloat(RakNet::BitStream* bs, const float input)
	{
		bs->WriteCompressed(input);
	}

	__declspec(dllexport) void bsWriteNormVector(RakNet::BitStream* bs, float x, float y, float z)
	{
		bs->WriteNormVector(x, y, z);
	}

	__declspec(dllexport) void bsWriteVector(RakNet::BitStream* bs, float x, float y, float z)
	{
		bs->WriteVector(x, y, z);
	}

	__declspec(dllexport) void bsWriteNormQuat(RakNet::BitStream* bs, float w, float x, float y, float z)
	{
		bs->WriteNormQuat(w, x, y, z);
	}

	__declspec(dllexport) void bsWriteOrthMatrix(RakNet::BitStream* bs,
		float m00, float m01, float m02,
		float m10, float m11, float m12,
		float m20, float m21, float m22)
	{
		bs->WriteOrthMatrix(m00, m01, m02, m10, m11, m12, m20, m21, m22);
	}

	__declspec(dllexport) void bsWriteCompressedDouble(RakNet::BitStream* bs, const double input)
	{
		bs->WriteCompressed(input);
	}

	__declspec(dllexport) bool bsReadBool(RakNet::BitStream* bs, bool *output)
	{
		return bs->Read(*output);
	}

	__declspec(dllexport) bool bsReadUbyte(RakNet::BitStream* bs, unsigned char *output)
	{
		return bs->Read(*output);
	}

	__declspec(dllexport) bool bsReadChar(RakNet::BitStream* bs, char *output)
	{
		return bs->Read(*output);
	}

	__declspec(dllexport) bool bsReadUshort(RakNet::BitStream* bs, unsigned short *output)
	{
		return bs->Read(*output);
	}

	__declspec(dllexport) bool bsReadShort(RakNet::BitStream* bs, short *output)
	{
		return bs->Read(*output);
	}

	__declspec(dllexport) bool bsReadUint(RakNet::BitStream* bs, unsigned int *output)
	{
		return bs->Read(*output);
	}

	__declspec(dllexport) bool bsReadInt(RakNet::BitStream* bs, int *output)
	{
		return bs->Read(*output);
	}

	/*  //  commented out according to issue 1 in bugzilla
	__declspec(dllexport) bool bsReadUlong(RakNet::BitStream* bs, unsigned long *output)
	{
		return bs->Read(*output);
	}

	__declspec(dllexport) bool bsReadLong(RakNet::BitStream* bs, long *output)
	{
		return bs->Read(*output);
	}
	*/

	__declspec(dllexport) bool bsReadFloat(RakNet::BitStream* bs, float *output)
	{
		return bs->Read(*output);
	}

	__declspec(dllexport) bool bsReadDouble(RakNet::BitStream* bs, double *output)
	{
		return bs->Read(*output);
	}

	__declspec(dllexport) bool bsReadNetworkID(RakNet::BitStream* bs, NetworkID* networkId)
	{
		return bs->Read(*networkId);
	}

	__declspec(dllexport) bool bsReadCompressedUbyte(RakNet::BitStream* bs, unsigned char *output)
	{
		return bs->ReadCompressed(*output);
	}

	__declspec(dllexport) bool bsReadCompressedChar(RakNet::BitStream* bs, char *output)
	{
		return bs->ReadCompressed(*output);
	}

	__declspec(dllexport) bool bsReadCompressedUshort(RakNet::BitStream* bs, unsigned short *output)
	{
		return bs->ReadCompressed(*output);
	}

	__declspec(dllexport) bool bsReadCompressedShort(RakNet::BitStream* bs, short *output)
	{
		return bs->ReadCompressed(*output);
	}

	__declspec(dllexport) bool bsReadCompressedUint(RakNet::BitStream* bs, unsigned int *output)
	{
		return bs->ReadCompressed(*output);
	}

	__declspec(dllexport) bool bsReadCompressedInt(RakNet::BitStream* bs, int *output)
	{
		return bs->ReadCompressed(*output);
	}

	/*  //  commented out according to issue 1 in bugzilla
	__declspec(dllexport) bool bsReadCompressedUlong(RakNet::BitStream* bs, unsigned long *output)
	{
		return bs->ReadCompressed(*output);
	}

	__declspec(dllexport) bool bsReadCompressedLong(RakNet::BitStream* bs, long *output)
	{
		return bs->ReadCompressed(*output);
	}
	*/

	__declspec(dllexport) bool bsReadCompressedFloat(RakNet::BitStream* bs, float *output)
	{
		return bs->ReadCompressed(*output);
	}

	__declspec(dllexport) bool bsReadNormVector(RakNet::BitStream* bs, float *x, float *y, float *z)
	{
		return bs->ReadNormVector(*x, *y, *z);
	}

	__declspec(dllexport) bool bsReadVector(RakNet::BitStream* bs, float *x, float *y, float *z)
	{
		return bs->ReadVector(*x, *y, *z);
	}

	__declspec(dllexport) bool bsReadNormQuat(RakNet::BitStream* bs, float *w, float *x, float *y, float *z)
	{
		return bs->ReadNormQuat(*w, *x, *y, *z);
	}

	__declspec(dllexport) bool bsReadOrthMatrix(RakNet::BitStream* bs,
		float *m00, float *m01, float *m02,
		float *m10, float *m11, float *m12,
		float *m20, float *m21, float *m22)
	{
		return bs->ReadOrthMatrix(*m00, *m01, *m02, *m10, *m11, *m12, *m20, *m21, *m22);
	}

	__declspec(dllexport) bool bsReadCompressedDouble(RakNet::BitStream* bs, double *output)
	{
		return bs->ReadCompressed(*output);
	}



	__declspec(dllexport) bool bsReadBit(RakNet::BitStream* bs)
	{
		return bs->ReadBit();
	}

	__declspec(dllexport) void bsAssertCopyData(RakNet::BitStream* bs)
	{
		bs->AssertCopyData();
	}

	__declspec(dllexport) void bsSetNumberOfBitsAllocated(RakNet::BitStream* bs, const unsigned int lengthInBits)
	{
		bs->SetNumberOfBitsAllocated(lengthInBits);
	}

	__declspec(dllexport) void bsAddBitsAndReallocate(RakNet::BitStream* bs, const int numberOfBitsToWrite)
	{
		bs->AddBitsAndReallocate(numberOfBitsToWrite);
	}

	/* GetTime */

	__declspec(dllexport) RakNetTime gtGetTime()
	{
		return RakNet::GetTime();
	}

	__declspec(dllexport) RakNetTimeNS gtGetTimeNS()
	{
		return RakNet::GetTimeNS();
	}
}
