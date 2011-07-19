module tools.ctfe;

version(Tango) import tools.compat;

string ctToString(long l) {
  if (l<0) {
    assert(-l !< 0, "l<0 but -l not >0! Internal fuck-up. ");
    return "-"~ctToString(-l);
  }
  string res;
  do {
    res = "0123456789"[l%10] ~ res;
    l /= 10;
  } while (l);
  return res;
}

int ctAtoi(string s) {
  s = ctStrip(s);
  assert(s.length);
  if (s[0] == '-') return -ctAtoi(s[1 .. $]);
  int res;
  while (s.length) {
    assert(s[0] >= '0' && s[0] <= '9');
    res = res * 10 + (s[0] - '0');
    s = s[1 .. $];
  }
  return res;
}

string ctToLower(string s) {
  string res;
  foreach (ch; s) {
    if (ch >= 'A' && ch <= 'Z') {
      res ~= ch - 'A' + 'a';
    } else
      res ~= ch;
  }
  return res;
}

int ctFind(string text, string match) {
  if (match.length > text.length) return -1;
  for (int i = 0; i <= text.length - match.length; ++i) {
    if (text[i .. i+match.length] == match) return i;
  }
  return -1;
}

int ctFind(string text, char match) {
  for (int i = 0; i < text.length; ++i) {
    if (text[i] == match) return i;
  }
  return -1;
}

int ctRFind(string text, string match) {
  if (match.length > text.length) return -1;
  for (int i = text.length - match.length; i >= 0; --i) {
    if (text[i .. i+match.length] == match) return i;
  }
  return -1;
}

string ctStripL(string str) {
  while (str.length && (str[0] == ' ' || str[0] == '\n' || str[0] == '\t')) str = str[1 .. $];
  return str;
}

string ctStripR(string str) {
  while (str.length && (str[$-1] == ' ' || str[$-1] == '\n' || str[$-1] == '\t')) str = str[0 .. $-1];
  return str;
}

string ctStrip(string str) {
  if (!str.length) return str;
  int a = 0, b = str.length - 1;
  while (a < str.length && (str[a] == ' ' || str[a] == '\r' || str[a] == '\n' || str[a] == '\t')) a++;
  while (b+1 > a        && (str[b] == ' ' || str[b] == '\r' || str[b] == '\n' || str[b] == '\t')) b--;
  return str[a .. b+1];
}

string ctSlice(ref string what, string where, bool cutOff = true) {
  auto loc = what.ctFind(where);
  if (loc == -1) {
    if (cutOff) {
      auto res = what;
      what = null;
      return res;
    } else return null;
  }
  auto res = what[0 .. loc];
  what = what[loc+where.length .. $];
  return res;
}

string ctReplace(string source, string what, string wth) {
  int i = 0;
  string res;
  if (source.length < what.length) return source;
  while (i <= cast(int) source.length - what.length) {
    if (source[i .. i+what.length] == what) {
      res ~= source[0 .. i]; res ~= wth;
      source = source[i + what.length .. $];
      i = 0;
    } else {
      i++;
    }
    if (source.length < what.length) break;
  }
  res ~= source;
  return res;
}

string ctBetween(string text, string from, string to, bool adhere_right = false) {
  int pos1, pos2;
  if (adhere_right) {
    if (to.length) pos2 = text.ctFind(to);
    else pos2 = text.length;
    
    if (pos2 == -1) return null;
    
    if (from.length) pos1 = text[0 .. pos2].ctRFind(from);
    else pos1 = 0;
    if (pos1 == -1) return null;
    
    return text[pos1 + from.length .. pos2];
  } else {
    if (from.length) pos1 = text.ctFind(from);
    if (pos1 == -1) return null;
    
    text = text[pos1 + from.length .. $];
    
    if (to.length) pos2 = text.ctFind(to);
    else pos2 = text.length;
    if (pos2 == -1) return null;
    
    return text[0 .. pos2];
  }
}

string ctReplace(string source, string a, string b, string c, string d) {
  return ctReplace(ctReplace(source, a, b), c, d);
}

string ctReplace(string source, string a, string b, string c, string d, string e, string f) {
  return ctReplace(ctReplace(ctReplace(source, a, b), c, d), e, f);
}

string ctReplace(string source, string a, string b, string c, string d, string e, string f, string g, string h) {
  return ctReplace(ctReplace(ctReplace(ctReplace(source, a, b), c, d), e, f), g, h);
}

string ctReplace(string source, string a, string b, string c, string d, string e, string f, string g, string h, string i, string k) {
  return ctReplace(ctReplace(ctReplace(ctReplace(ctReplace(source, a, b), c, d), e, f), g, h), i, k);
}

/* ==TABLE PARSING== */

bool ctIsRelevant(string line) {
  foreach (ch; line) {
    if (ch != ' ' && ch != '-' && ch != '|' && ch != '+') return true;
  }
  return false;
}

int ctTableHeight(string table) {
  int cur, res;
  foreach (i, ch; table) {
    if (ch == '\n' || i == table.length - 1) {
      if (ctIsRelevant(table[cur .. i])) res++;
      cur = i + 1;
    }
  }
  return res;
}

int ctStops(string line) {
  int res;
  foreach (ch; line) if (ch == '|') res ++;
  return res;
}

string ctGetLine(string table, int line) {
  int cur, res;
  foreach (i, ch; table) {
    if (ch == '\n') {
      if (ctIsRelevant(table[cur .. i]))
        if (!line--) return table[cur .. i].ctStrip();
      cur = i + 1;
    }
  }
  if (!line--) return table[cur .. $].ctStrip();
  assert(false, "Cannot get line "~ctToString(line)~" from "~table);
}

string ctGetCell(string line, int cell) {
  int cur;
  foreach (i, ch; line) {
    if (ch == '|') {
      if (!cell--) return line[cur .. i].ctStrip();
      cur = i + 1;
    }
  }
  if (!cell--) return line[cur .. $].ctStrip();
  assert(false, "Cannot get cell "~ctToString(cell+1)~" from "~line);
}

int ctTableLineWidth(string line) {
  return line.ctStops() + 1;
}

int ctTableWidth(string table) {
  return table.ctGetLine(0).ctTableLineWidth();
}

string ctTableUnrollColMajor(string table, string colWrap, string cell, int selectCol = -1) {
  int width = ctTableWidth(table), height = ctTableHeight(table);
  string res;
  int start = 1, end = width;
  if (selectCol != -1) { start = selectCol + 1; end = start + 1; }
  for (int x = start; x < end; ++x) {
    auto colname = table.ctGetLine(0).ctGetCell(x);
    string col;
    for (int y = 1; y < height; ++y) {
      auto line = table.ctGetLine(y);
      auto rowname = line.ctGetCell(0), content = line.ctGetCell(x);
      col ~= cell.ctReplace("$ROW", rowname).ctReplace("$CELL", content);
    }
    res ~= colWrap.ctReplace("$BODY", col).ctReplace("$COL", colname);
  }
  return res;
}

string ctTableUnrollColMajor(string table, int selectCol, string colWrap, string cell) {
  return table.ctTableUnrollColMajor(colWrap, cell, selectCol);
}

string ctTableUnroll(string table, string rowWrap) {
  int width = ctTableWidth(table), height = ctTableHeight(table);
  string res;
  for (int y = 1; y < height; ++y) {
    string lres = rowWrap;
    string myLine = table.ctGetLine(y);
    for (int x = 0; x < myLine.ctTableLineWidth(); ++x) {
      string colname = table.ctGetLine(0).ctGetCell(x);
      lres = lres.ctReplace("$"~colname, myLine.ctGetCell(x));
    }
    for (int x = myLine.ctTableLineWidth(); x < width; ++x) {
      string colname = table.ctGetLine(0).ctGetCell(x);
      if (lres.ctFind("$"~colname) != -1)
        assert(false, "In line "~ctToString(y)~": "
          "referenced but not defined column "~colname~"!"
        );
    }
    res ~= lres;
  }
  return res;
}

string ctTableUnrollColumn(string table, string column, string cell) {
  int width = ctTableWidth(table);
  int col = -1;
  string firstline = table.ctGetLine(0);
  for (int i = 1; i < width; ++i) {
    if (firstline.ctGetCell(i) == column) col = i - 1; // col parameter is plus one.
  }
  if (col == -1) assert(false, "Cannot find column: "~column~" in table! ");
  return table.ctTableUnrollColMajor("$BODY", cell, col);
}

string ctTableLookup(string table, int row, string col) {
  int col_id = -1;
  int width = ctTableWidth(table);
  for (int i = 1; i < width; ++i)
    if (table.ctGetLine(0).ctGetCell(i) == col) col_id = i;
  assert(col_id != -1, "No such column: "~col);
  return table.ctGetLine(row+1).ctGetCell(col_id);
}

/* End TABLE PARSING */

/* Start Constant-Enum Expander */

int ctExtFind(string s, string t, int fail = -1) {
  int res = s.ctFind(t);
  if (res == -1) res = fail;
  return res;
}

// expand a string of the form "foo, bar(0, 1)whee" into "|foo|bar0whee|bar1whee".
// This function used to be much cleaner, with sub-functions and all.
// Turns out that causes silent failure in some GDC versions.
// Don't blame me.
// PS don't attempt to clean this up. It _just_ works.
string ctExpand(string spec) { return ctExpand("|", spec); }
string ctExpand(string sep, string spec) {
  /*
   * The currently used strings are stack[$-1] and working[$-1].
   * When reaching '(', a new layer is opened.
   * ',' commits the current working string to the stack.
   * ')' crosses the current stack with the working set below.
   */
  string[] stack, working;
  stack ~= "";
  working ~= "";
  while (spec.length) {
    string pre;
    bool isComma, isOpen, isClose;
    int offs;
    while (!isComma && !isOpen && !isClose && offs < spec.length) {
      isComma = spec[offs] == ',';
      isOpen = spec[offs] == '(';
      isClose = spec[offs] == ')';
      offs ++;
    }
    if (!isComma && !isOpen && !isClose) {
      isComma = true;
    }
    pre = spec[0 .. offs - 1].ctStrip();
    spec = spec[offs .. $];
    if (isComma) {
      { /* brk(pre) */
        int eqpos = pre.ctFind("=");
        if (eqpos != -1) {
          string value = pre[eqpos+1 .. $].ctStrip();
          pre = "[" ~ value ~ "]" ~ pre[0 .. eqpos].ctStrip();
        }
        { /* headAppend(pre) */
          string w = working[$-1];
          if (!w.length) {
            working[working.length-1] = sep ~ pre;
          } else {
            string nw;
            if (w.length) {
              w = w[1 .. $];
              do nw ~= sep ~ w.ctSlice(sep) ~ pre;
              while (w.length);
            }
            working[working.length-1] = nw;
          }
        }
        stack[stack.length-1] ~= working[working.length-1];
        working[working.length-1] = "";
      }
      continue;
    }
    if (isOpen) {
      { /* headAppend(pre) */
        string w = working[$-1];
        if (!w.length) {
          working[working.length-1] = sep ~ pre;
        } else {
          string nw;
          if (w.length) {
            w = w[1 .. $];
            do nw ~= sep ~ w.ctSlice(sep) ~ pre;
            while (w.length);
          }
          working[working.length-1] = nw;
        }
      }
      stack ~= ""; working ~= ""; // start new layer
      continue;
    }
    if (isClose) {
      { /* brk(pre)*/
        int eqpos = pre.ctFind("=");
        if (eqpos != -1) {
          string value = pre[eqpos+1 .. $].ctStrip();
          pre = "[" ~ value ~ "]" ~ pre[0 .. eqpos].ctStrip();
        }
        { /* headAppend(pre) */
          string w = working[$-1];
          if (!w.length) {
            working[working.length-1] = sep ~ pre;
          } else {
            string nw;
            if (w.length) {
              w = w[1 .. $];
              do nw ~= sep ~ w.ctSlice(sep) ~ pre;
              while (w.length);
            }
            working[working.length-1] = nw;
          }
        }
        stack[stack.length-1] ~= working[working.length-1];
        working[working.length-1] = "";
      }
      string myData = stack[$-1];
      stack = stack[0 .. $-1];
      working = working[0 .. $-1];
      { /* was : cross(myData) */
        // This is no fun.
        string t = myData;
        string w = working[$-1];
        if (!w.length) {
          working[$-1] = t;
        } else {
          string[] temp;
          if (t.length) t = t[1 .. $];
          while (t.length) {
            temp ~= t.ctSlice(sep);
          }
          string nw;
          if (w.length) {
            w = w[1 .. $]; // remove leading |
            do {
              string curPiece = w.ctSlice(sep);
              int i;
              foreach (tv; temp) nw ~= sep ~ curPiece ~ tv;
            } while (w.length);
          }
          working[working.length-1] = nw;
        }
      }
      continue;
    }
    assert(false);
  }
  assert(stack.length == 1 && working.length == 1);
  return stack[0];
}

/* End Constant-Enum Expander */

string litstring_expand(string str) {
  string result;
  int depth = 0;
  int i;
  for (i = 0; i < str.length; ++i) {
    if (i < str.length - 1) {
      if (str[i] == 'q' && str[i+1] == '{') {
          int count;
          for (int k = 0; k < depth; ++k) count = count * 2 + 1;
          for (int k = 0; k < count; ++k) result ~= "\\";
        result ~= "\"";
        depth ++;
        ++i;
      } else if (str[i .. i+2] == "}p") {
        depth --;
          int count;
          for (int k = 0; k < depth; ++k) count = count * 2 + 1;
          for (int k = 0; k < count; ++k) result ~= "\\";
        result ~= "\"";
        ++i;
      } else result ~= str[i];
    } else result ~= str[i];
  }
  return result;
}

static assert(litstring_expand("q{ foo; q{ bar; }p }p") == "\" foo; \\\" bar; \\\" \"");
static assert(litstring_expand("q{ foo; q{ bar; }p }p ") == "\" foo; \\\" bar; \\\" \" ");
