import std.stdio;
import std.ctype;
import std.demangle;

void writefl(char c) {
  printf("%c", c); fflush(null);
}
void writefl(char[] c) {
  printf("%.*s", c); fflush(null);
}

void mangprint(string line) {
  bool inword=false;
  char[] buffer;
  
  foreach (ch; line) {
    if (inword) {
      if (ch == '_' || isalnum(ch)) buffer ~= ch;
      else {
        inword = false;
        writefl(demangle(buffer)~ch);
      }
    } else {
      if (ch == '_' || isalpha(ch)) {
        inword=true;
        buffer=[ch];
      } else writefl(ch);
    }
  }
  if (inword) writefl(demangle(buffer));
  writefln;
}

int main() {
  char[] line;
  bool immediate=false;
  int c;

  while ((c = fgetc(stdin)) != EOF) {
    char ch=cast(char)c;
    if (immediate) {
      if (c == 10) {
        writefln;
        immediate=false;
      } else writefl(ch);
      continue;
    }
    if (c == 10) { mangprint(line); line=""; continue; }
    line ~= ch;
    if (line == "(gdb) ") {
      immediate=true; 
      writefl(line);
      line="";
      continue;
    }
  }
  return 0;
}
