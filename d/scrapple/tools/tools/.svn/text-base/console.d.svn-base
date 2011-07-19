module tools.console;

import std.string, tools.base, tools.ctfe, tools.ctypes;

// VGA 0..16 colors
mixin(fancyEnum("ubyte Color:
  Black, DarkBlue, DarkGreen, DarkCyan, DarkRed, Purple, DarkYellow, LightGray,
  Gray, LightBlue, LightGreen, LightCyan, LightRed, Pink, LightYellow, White
"));

abstract class Terminal {
  string toString();
  Stuple!(int, int) getTermSize();
  void setForeColor(Color);
  void setBackColor(Color);
  void resetColors();
  void setCursorPos(int col, int row);
  void writeData(string data);
  final void write(T...)(T t) { writeData(Format(t)); }
  final void writeln(T...)(T t) { writeData(Format(t, "\n")); }
}

const string ColorMapping = `
              | Windows | Linux
  ------------+---------+-------
  Black       | 0       | 0
  DarkBlue    | 1       | 4
  DarkGreen   | 2       | 2
  DarkCyan    | 3       | 6
  DarkRed     | 4       | 1
  Purple      | 5       | 5
  DarkYellow  | 6       | 3
  Gray        | 7       | 60
  LightGray   | 8       | 7
  LightBlue   | 9       | 64
  LightGreen  | 10      | 62
  LightCyan   | 11      | 66
  LightRed    | 12      | 61
  Pink        | 13      | 65
  LightYellow | 14      | 63
  White       | 15      | 67
`;

int mapColor(Color c) {
  version(Windows) const string MapIndex = "Windows";
  else const string MapIndex = "Linux";
  mixin(ColorMapping.ctTableUnrollColumn(MapIndex, "if (c == Color.$ROW) return $CELL; "));
  assert(false, Format("No such color: ", c));
}

version(Windows) {
  extern(System) {
    typedef void* HANDLE;
    HANDLE GetStdHandle(int nStdHandle = -11);
    struct COORD { short x, y; }
    struct SMALL_RECT { short Left, Top, Right, Bottom; }
    struct CONSOLE_SCREEN_BUFFER_INFO { // Ballmer sure shouts a lot.
      COORD dwSize, dwCursorPosition;
      short wAttributes;
      SMALL_RECT srWindow; COORD dwMaximumWindowSize;
    }
    bool GetConsoleScreenBufferInfo(HANDLE, CONSOLE_SCREEN_BUFFER_INFO*);
    bool SetConsoleTextAttribute(HANDLE, uint attribs);
    uint GetLastError();
    bool SetConsoleCursorPosition(HANDLE, COORD);
    mixin(constEnum("int:1*2: (FORE,BACK)GROUND_(BLUE, GREEN, RED, INTENSITY)"));
    bool WriteConsoleW(HANDLE, void*, int, void*, void*);
  }
  class WinTerm : Terminal {
    HANDLE hdl;
    short initialAttribs;
    this() { hdl = GetStdHandle(); initialAttribs = getInfo.wAttributes; }
    private CONSOLE_SCREEN_BUFFER_INFO getInfo() {
      CONSOLE_SCREEN_BUFFER_INFO INFOOOOOOOOOOOOOOOOO;
      if (!GetConsoleScreenBufferInfo(hdl, &INFOOOOOOOOOOOOOOOOO))
        throw new Exception(Format("Could not get console info: ", GetLastError()));
      return INFOOOOOOOOOOOOOOOOO;
    }
    override {
      string toString() { return "Windows console"; }
      Stuple!(int, int) getTermSize() {
        return Stuple!(int, int)(getInfo.dwSize.tupleof);
      }
      void setForeColor(Color col) {
        if (!SetConsoleTextAttribute(hdl, (getInfo.wAttributes & ~0xff) | mapColor(col)))
          throw new Exception("Couldn't set foreground color! ");
      }
      void setBackColor(Color col) {
        if (!SetConsoleTextAttribute(hdl, (getInfo.wAttributes & ~0xff00) | (mapColor(col) * 16)))
          throw new Exception("Couldn't set background color! ");
      }
      void resetColors() {
        if (!SetConsoleTextAttribute(hdl, initialAttribs))
          throw new Exception("Couldn't reset colors! ");
      }
      void setCursorPos(int col, int row) {
        if (!SetConsoleCursorPosition(hdl, COORD(col, row)))
          throw new Exception("Couldn't position the cursor! ");
      }
      void writeData(string str) {
        auto data = str.toUTF16();
        WriteConsoleW(hdl, data.ptr, data.length, null, null);
      }
    }
  }
  Terminal openTerminal() { return new WinTerm; }
} else { // presume termios
  import std.stdio: getErrno;
  extern(System) {
    typedef C_int FD;
    FD open(char* pathname, uint flags);
    int isatty(FD);
    char* ttyname(FD);
    int ioctl(FD, C_ulong request, ...);
    struct winsize {
      ushort ws_row, ws_col, ws_xpixel, ws_ypixel;
    }
    char* strerror(C_int errno);
  }
  mixin(constEnum("uint: 1*2: O_(RDONLY = 0, WRONLY = 1, RDWR = 2,
    CREAT = 0x100, EXCL, NOCTTY,
    TRUNC = 0x1000, APPEND, NONBLOCK, NDELAY = O_NONBLOCK,
    SYNC = 0x10000, FSYNC = O_SYNC, ASYNC
  )"));
  // Now tell me this ain't beautiful.
  mixin(constEnum("uint: 0x5401: T(
    C(
      (G,S)ETS, SETS(W, F), (G,S)ETA, SETA(W, F),
      SBRK, XONC, FLSH
    ),
    IOC(
      (E,N)XCL, SCTTY, (G,S)PGRP, OUTQ, STI,
      (G,S)WINSZ, M(GET, BIS, BIC, SET),
      (G,S)SOFTCAR,
      INQ, LINUX, CONS,
      (G,S)SERIAL, PKT, NOTTY=0x5422,
      (S,G)ETD,
      (S=0x5427,C)BRK,
      GSID,
      SER(CONFIG = 0x5453, (G,S)WILD),
      (G,S)LCKTRMIOS,
      SERG(STRUCT, ETLSR),
      SER(G,S)ETMULTI,
      MIWAIT, GICOUNT,
      (G,S)HAYESESP,
    )
  ),
  TCSBRKP = 0x5425,
  FIO(NREAD = TIOCINQ, NBIO = 0x5421, NCLEX = 0x5450, CLEX, ASYNC, QSIZE = 0x5460)
  "));
  class TermiosAnsi : Terminal {
    FD fd;
    this() {
      fd = 0; // stdout
      if (!isatty(fd)) throw new Exception("Not attached to a terminal! ");
    }
    override {
      string toString() {
        return Format("termios: ", std.string.toString(ttyname(fd)));
      }
      Stuple!(int, int) getTermSize() {
        winsize ws;
        if (ioctl(fd, TIOCGWINSZ, &ws) == -1)
          throw new Exception("IOCTL failed: "~std.string.toString(strerror(getErrno)));
        return stuple!(int, int)(ws.ws_row, ws.ws_col);
      }
      void setForeColor(Color c) {
        writef("\033[", 30+mapColor(c), "m");
      }
      void setBackColor(Color c) {
        writef("\033[", 40+mapColor(c), "m");
      }
      void resetColors() {
        writef("\033[0m");
      }
      void setCursorPos(int col, int row) {
        writef("\033[", row, ";", col, "H");
      }
      void writeData(string s) {
        printf("%.*s", s);
      }
    }
  }
  Terminal openTerminal() { return new TermiosAnsi; }
}

void main() {
  auto term = openTerminal();
  writefln(term.toString());
  auto ts = term.getTermSize();
  term.writeln(ts._0, "x", ts._1);
  term.writeln("Test\rt");
  term.write("             ");
  foreach (Color back; Color) {
    string name = Format(back.value);
    while (name.length < 2) name ~= " ";
    term.write(name, " ");
  }
  term.writeln();
  foreach (string forename, Color fore; Color) {
    while (forename.length < 12) forename ~= " ";
    term.resetColors();
    term.write(forename, " ");
    foreach (string backname, Color back; Color) {
      term.setForeColor(fore);
      term.setBackColor(back);
      term.write("###");
    }
    term.writeln();
  }
}
