module tools.fixed_socket;

// version(Tango) import std.compat;

version(Tango) {
  public import tango.net.Socket;
  alias NetHost ActuallyThreadsafeInternetHost;
  alias IPv4Address ActuallyWorkingInternetAddress;
} else {
  
  public import std.socket;
  version(Windows) {
    // no problem under win32
    alias InternetHost ActuallyThreadsafeInternetHost;
    alias InternetAddress ActuallyWorkingInternetAddress;
  } else {
    /*version(linux) import std.c.linux.socket, std.c.stdlib: ERANGE;
    else */version(Unix) import std.c.unix.unix, std.c.stdlib: ERANGE;
    else static assert(0); // No socket support yet.
    
    import std.string;
    
    class ActuallyThreadsafeInternetHost {
      string name;
      string[] aliases;
      uint[] addrList;
      void validHostent(hostent* he) {
        if (he.h_addrtype != cast(int)AddressFamily.INET || he.h_length != 4)
          throw new HostException("Address family mismatch");
      }
      void populate(hostent* he) {
        char* p;
        name = std.string.toString(he.h_name).dup;
        int i;
        for(i = 0;; i++) {
          p = he.h_aliases[i];
          if (!p) break;
        }
        if (i) {
          aliases = new string[i];
          foreach (id, ref _alias; aliases) _alias=std.string.toString(he.h_aliases[id]).dup;
        } else aliases = null;
        
        for(i = 0;; i++) {
          p=he.h_addr_list[i];
          if (!p) break;
        }
        if(i) {
          addrList = new uint[i];
          foreach (id, ref entry; addrList) entry = ntohl(*(cast(uint*)he.h_addr_list[id]));
        }
        else addrList = null;
      }
      
      bool getHostByName(string name)
      {
        version (Windows)
          hostent* he = gethostbyname(toStringz(name));
        else {
          auto he = new hostent;
          auto buffer=new char[512];
          int errno;
          getHostByName_retry: // if we had extended do { } while { } this would not be necessary.
          auto res = gethostbyname_r(toStringz(name), he, buffer.ptr, buffer.length, &he, &errno);
          if (res == ERANGE) { buffer.length = buffer.length * 2; if (!he) he=new hostent; goto getHostByName_retry; }
        }
        if(!he) return false;
        validHostent(he);
        populate(he);
        return true;
      }
      
      bool getHostByAddr(uint addr)
      {
        uint x = htonl(addr);
        hostent* he = gethostbyaddr(&x, 4, cast(int)AddressFamily.INET);
        if(!he)
                return false;
        validHostent(he);
        populate(he);
        return true;
      }
      bool getHostByAddr(string addr)
      {
        uint x = inet_addr(std.string.toStringz(addr));
        hostent* he = gethostbyaddr(&x, 4, cast(int)AddressFamily.INET);
        if(!he)
                return false;
        validHostent(he);
        populate(he);
        return true;
      }
    }
    
    class ActuallyWorkingInternetAddress : Address {
      protected:
        sockaddr_in sin;
        // sockaddr*
        typeof(super.name()) name() { return cast(typeof(super.name()))&sin; }
        int nameLen() { return sin.sizeof; } 
        this() { }
      public:
        const uint ADDR_ANY = INADDR_ANY;	/// Any IPv4 address number.
        const uint ADDR_NONE = INADDR_NONE;	/// An invalid IPv4 address number.
        const ushort PORT_ANY = 0;		/// Any IPv4 port number.
        
        AddressFamily addressFamily() { return cast(AddressFamily)AddressFamily.INET; }
        ushort port() { return ntohs(sin.sin_port); }
        uint addr() { return ntohl(sin.sin_addr.s_addr); }
        this(string addr, ushort port) {
          uint uiaddr = parse(addr);
          if(ADDR_NONE == uiaddr)
          {
                  auto ih = new ActuallyThreadsafeInternetHost;
                  if(!ih.getHostByName(addr))
                          //throw new AddressException("Invalid internet address");
                          throw new AddressException("Unable to resolve host '" ~ addr ~ "'");
                  uiaddr = ih.addrList[0];
          }
          sin.sin_addr.s_addr = htonl(uiaddr);
          sin.sin_port = htons(port);
        }
        this(uint addr, ushort port) {
          sin.sin_addr.s_addr = htonl(addr);
          sin.sin_port = htons(port);
        }
        this(ushort port) {
          sin.sin_addr.s_addr = 0; //any, "0.0.0.0"
          sin.sin_port = htons(port);
        }
        string toAddrString() { return std.string.toString(inet_ntoa(sin.sin_addr)).dup; }
        string toPortString() { return std.string.toString(port()); }
        string toString() { return toAddrString() ~ ":" ~ toPortString(); }
        static uint parse(string addr) { return ntohl(inet_addr(std.string.toStringz(addr))); }
    }
  }
}