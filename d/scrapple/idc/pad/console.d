module pad.console;

import pad.mainloop, pad.utils;
import tools.base, std.stdio, tools.log;

void main(string[] args) {
  auto exec = args.take();
  if (args.length != 1) {
    writefln("Syntax: ", exec, " <pastebin url or file>");
    return;
  }
  readMode = ReadFS;
  runLoop(args[0],
    (string s) { logln(s.replace("<br>", "\n")); },
    { writef("> "); return readln().chomp(); },
    ReadFS
  );
}
