module tools.smtp;

import tools.fixed_socket, tools.base;
import std.string: replace, find, toString;
import std.base64: encode64=encode;

// reads and writes lines to socket
Stuple!(string delegate(), void delegate(string), void delegate()) readlines(Socket s) {
  auto readLine = stuple(s, "", false) /apply/ (Socket s, ref string line_buf, ref bool closed) {
    if (closed) return cast(string) null;
    auto buffer = new char[128];
    while (true) {
      auto newline = line_buf.find("\r\n");
      if (newline != -1) {
        auto res = line_buf[0 .. newline];
        line_buf = line_buf[newline+2 .. $];
        logln("< ", res);
        return res;
      }
      auto len = s.receive(buffer);
      if (len) line_buf ~= buffer[0 .. len];
      else { closed = true; return cast(string) null; }
    }
  };
  auto writeLine = s /apply/ (Socket s, string l) {
    if (l.length != 72) logln("> ", l);
    s.send(l);
    s.send("\r\n");
  };
  auto close = s /apply/ (Socket s) { s.close(); };
  return stuple(readLine, writeLine, close);
}

void to_lines(string s, void delegate(string) dg) {
  while (true) {
    auto pos = s.find("\n");
    if (pos == -1) { dg(s); break; }
    if (!pos || s[pos-1] != '\r') dg(s[0 .. pos]);
    else dg(s[0 .. pos-1]);
    s = s[pos+1 .. $];
  }
}

class Session {
  private {
    TcpSocket sock;
    string delegate() readln;
    void delegate(string) writeln;
    void delegate() closeConn;
    void expectResponse(int id) {
      auto ln = readln();
      if (!ln.startsWith(.toString(id)))
        throw new Exception(Format("Unexpected reply: wanted ", id, ", got ", ln));
    }
    void send_expect(string mesg, int id) {
      writeln(mesg);
      NestedException.wrap("While sending "~mesg, expectResponse(id));
    }
    bool open;
  }
  this(string host, ushort port = 25) {
    New(sock, new ActuallyWorkingInternetAddress(host, port));
    open = true;
    ptuple(readln, writeln, closeConn) = readlines(sock);
    expectResponse(220);
    writeln("EHLO");
    while (true) {
      auto line = readln();
      if (!line.length) throw new Exception("Connection closed prematurely");
      if (auto rest = line.startsWith("250-")) {
        // logln("EHLO ", rest);
      } else if (auto rest = line.startsWith("250 ")) {
        // logln("EHLO/done ", rest);
        break;
      } else throw new Exception("Unexpected line during EHLO: "~line);
    }
    // send_expect("HELO", 250); // should not be needed
  }
  ~this() { if (open) throw new Exception("Session not closed!"); }
  static void base64_enc(ubyte a, ubyte b, ubyte c, string dest) {
    int id = (a << 16) | (b << 8) | c;
    const string field = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    dest[0] = field[id >> 18];
    dest[1] = field[(id&0x3f000) >> 12];
    dest[2] = field[(id&0xfc0) >> 6];
    dest[3] = field[id&0x3f];
  }
  static string base64_enc(void[] input) {
    auto data = cast(ubyte[]) input;
    auto full = data.length / 3, rest = data[full * 3 .. $];
    string res;
    res.length = full * 4 + (rest.length?4:0);
    for (int i = 0; i < full; ++i) {
      base64_enc(data[i*3], data[i*3+1], data[i*3+2], res[i*4 .. i*4+4]);
    }
    if (rest.length) {
      assert(rest.length == 1 || rest.length == 2);
      if (rest.length == 1) {
        base64_enc(rest[0], 0, 0, res[full * 4 .. $]);
        return res[0 .. $-2]; // we had enough bits to partially fill two bytes of the result (8 < 12)
      } else {
        base64_enc(rest[0], rest[1], 0, res[full * 4 .. $]);
        return res[0 .. $-1]; // enough for 3 bytes (16 < 18)
      }
    }
    return res;
  }
  void compose(string from, string[] to, string subject, string mesg, string attachname = null, void[] attach = null) {
    logln("COMPOSE: attach ", !!attach);
    string coded_msg = Format("From: ", from, "\n");
    foreach (addr; to) coded_msg ~= Format("To: ", addr, "\n");
    coded_msg ~= Format("Subject: ", subject, "\n");
    if (!attach) {
      coded_msg ~= "\n";
      coded_msg ~= mesg;
    } else {
      const sep = "blarglegarblefwoobarghz";
      logln("Building message");
      coded_msg ~= "Content-Type: multipart/mixed;\n boundary=\""~sep~"\"\n";
      coded_msg ~= "\n\nThis is a multi-part message in MIME format.\n--"~sep~"\n";
      coded_msg ~= "Content-Type: text/plain; charset=utf8\n";
      coded_msg ~= "Content-Transfer-Encoding: 8bit\n\n";
      coded_msg ~= mesg;
      coded_msg ~= "\n\n--"~sep~"\n";
      coded_msg ~= "Content-Type: application/octet;\n name=\""~attachname~"\"\n";
      coded_msg ~= "Content-Transfer-Encoding: base64\n";
      coded_msg ~= "Content-Disposition: attachment;\n filename=\""~attachname~"\"\n\n";
      logln("BASE64 encoding ", attach.length);
      string enc;
      try enc = base64_enc(attach);
      catch (Exception ex) { logln(ex); throw ex; }
      logln("=> ", enc.length);
      while (enc.length >= 72) {
        coded_msg ~= enc[0 .. 72] ~ "\r\n";
        enc = enc[72 .. $];
      }
      if (enc.length) coded_msg ~= enc;
      coded_msg ~= "=\n\n--"~sep~"\n";
    }
    sendMesg(from, to, coded_msg);
  }
  void sendMesg(string from, string[] to, string data) {
    send_expect("MAIL FROM:"~from, 250);
    foreach (recipient; to)
      send_expect("RCPT TO:"~recipient, 250);
    send_expect("DATA", 354);
    data.to_lines(writeln);
    send_expect(".", 250);
  }
  void close() { send_expect("QUIT", 221); open = false; }
  void auth_plain(string user, string pass) {
    auto code = ("\0"~user~"\0"~pass).encode64();
    send_expect("AUTH PLAIN", 334);
    send_expect(code, 235);
  }
}
