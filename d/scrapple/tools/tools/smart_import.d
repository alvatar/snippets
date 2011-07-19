module tools.smart_import;

char[] import_treecon(char[] base, ref char[] i) {
  auto start_i = i;
  char[][] res; bool done;
  char[] buffer;
  while (i.length) {
    if (i[0] == '[') {
      i = i [1 .. $];
      auto sup = import_treecon(base~buffer, i); buffer = "";
      char[] buf;
      while (sup.length) {
        if (sup[0] == ',') {
          res ~= buf;
          buf = "";
        } else buf ~= sup[0];
        sup = sup[1 .. $];
      }
      if (buf.length) res ~= buf;
    } else {
      if (i[0] == ',' || i[0] == ']') {
        if (buffer.length) {
          res ~= base~buffer;
          buffer = "";
        }
      } else if (i[0] == ' ' || i[0] == '\r' || i[0] == '\n') {
        // ignore whitespace
      } else buffer ~= i[0];
      if (i[0] == ']') {
        i = i[1 .. $];
        done = true;
        break;
      }
      i = i[1 .. $];
    }
  }
  if (done) {
    char[] cc_res;
    foreach (k, str; res) {
      if (base.length) {
        if (k) cc_res ~= ", ";
        cc_res ~= str;
      } else {
        cc_res ~= "import "~str~"; ";
      }
    }
    return cc_res;
  }
  else assert(false, "Import tree [] pair unclosed: "~start_i~"!");
}

char[] expandImport(char[] i) { i = i~"]"; return import_treecon("", i); }
