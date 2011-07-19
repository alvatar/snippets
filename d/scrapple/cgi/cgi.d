/*
This program is distributed without any warranty of any kind!

Author: Benjamin Shropshire (shro8822drop_this at vandals dot uidaho edu)

License: Freeware
Please give credit where credit is due. Please don't remove this notice.

   Revision history:
      31/Mar/2007: Rodney Berriman (rodneyDROP_THIS at optimail com au)
                    Added support for HTTP GET method
   
*/

module cgi;

import std.stdio;
import std.stream;
import std.cstream;
static import str = std.string;
static import clib = std.c.stdlib;

class CGI : Stream
{
	char[][char[]] params;

	void Spill()
	{
		foreach(k,v; params)
			this.writef("[%s: %s]\n",k,v);
	}

	this(char[][] argv)
	{
        char[] buffer, buffer_1, method;
        method = str.toString(clib.getenv(str.toStringz("REQUEST_METHOD")));
        // silently ignore ISINDEX method, now deprecated for security reasons:
        if (method == "GET")
          buffer_1 = str.toString(clib.getenv(str.toStringz("QUERY_STRING")));
        else if (method == "POST")
          buffer_1 = StripStream(din);

		int hex = 0;
		char ret, back;

		while(buffer_1.length > 0)
		{
			int i;
			foreach(ind,c; buffer_1)
			{
				i = ind;
				if(c == '=')
				{
					buffer = buffer_1[0..ind];
					break;
				}
			}
			if(i < buffer_1.length)
			{
				buffer_1 = buffer_1[i+1..$];
				params[buffer] = WalkCGItext(buffer_1);
			}
		}

	}

	char[] Value(char[] type){if(auto ret = type in params) return (*ret).dup; else return ""; }

	char[] WalkCGItext(inout char[] buffer_1)
	{
	//	scope(exit) writef("_%d__<br>\n", buffer_1.length);
		char[] buffer;

		int hex = 0;
		char ret, back;
		loop: foreach(int i, char c; buffer_1)
		{
			if(hex>=1)
			{
				back = c;
				ret <<= 4;
				hex++;
				switch(c)
				{
					case 'A','B','C','D','E','F':
						back = c;
						ret += (c - 'A' + 10);
						break;

					case '0','1','2','3','4','5','6','7','8','9':
						back = c;
						ret += (c - '0');
						break;

					default:
						buffer ~= '%';
						buffer ~= back;
				}
				if(hex >= 3)
				{
					hex = 0;
					buffer ~= ret;
					ret = 0;
				}
			}
			else switch(c)
			{
				case '+':
					buffer~=' ';
					break;

				case '%':
					hex = 1;
					ret = 0;
					break;

				case '&':
					buffer_1 = buffer_1[i+1..$];
					return buffer;

				default:
					buffer~=c;
					break;
			}
		}

		buffer_1.length = 0;
		return buffer;
	}

	static private char[] StripStream(Stream str)
	{
		char[][] set;
		int length = 0;
		int i;
		for(i = 0; !str.eof; i++)
		{
			if(set.length <= i) set.length = i+5;
			set[i].length = 30;
			set[i].length = str.read(cast(ubyte[])set[i]);
			length += set[i].length;
		}
		set.length = i;

		char[] buffer = new char[length];

		int at = 0;
		foreach(s;set)
		{
			buffer[at..at + s.length] = s[0..$];
			at += s.length;
		}
		return buffer;
	}

	size_t readBlock(void* buffer, size_t size)	{ return 0; }
	size_t writeBlock(void* buffer, size_t size)	{ return dout.writeBlock(buffer, size); }
	ulong seek(long offset, SeekPos whence)		{ return dout.seek(offset, whence); }
}


class TextCGI : CGI
{
	this(char[][] argv)
	{
		writef("Content-type: text/plain\n\n\n");
		super(argv);
	}
}


class HTML_CGI : CGI
{
	bool head = false;
	bool bod = false;
	this(char[][] argv)
	{
		this.writef("Content-type: text/html\n\n\n<html>\n");
		super(argv);
	}

	~this()
	{
		this.writef("</html>");
	}

	void Head(void delegate() dg)
	{
		if(head) throw new Error("Only One head is allowed"); else head=true;
		this.writef("<head>\n");
		scope(exit) this.writef("</head>\n");
		dg();
	}
	void Head(char[] data)
	{
		if(head) throw new Error("Only One head is allowed"); else head=true;
		this.writef("<head>\n%s</head>\n", data);
	}

	void Body(void delegate() dg)
	{
		if(bod) throw new Error("Only One body is allowed"); else bod=true;
		this.writef("<body>\n");
		scope(exit) this.writef("</body>\n");
		dg();

	}
	void Body(char[] data)
	{
		if(bod) throw new Error("Only One body is allowed"); else bod=true;
		this.writef("<body>\n%s</body>\n",data);
	}

	void Form(char[] act, char[] method, char[] data)	{this.writef("<form action=%s method=%s>\n%s\n</form>\n", act, method, data);}
	void Form(char[] act, char[] method, void delegate() dg)
	{
		this.writef("<form action=%s method=%s>\n", act, method);
		scope(exit) this.writef("\n</form>\n");
		dg();
	}

	void InputText(char[] name, uint size)
		{this.writef("<input type=text name=%s size=%d>", name, size);}

	void InputText(char[] name, uint size, char[] def)
		{this.writef("<input type=text name=%s value=\"%s\" size=%d>", name, def, size);}

	void InputSubmit(char[] value)
		{this.writef("<input type=submit value=\"%s\">", value);}

	void InputCheckbox(char[] name, bool ck=false)
		{this.writef("<INPUT TYPE=checkbox NAME=\"%s\"%s>",name, ck?" CHECKED":"");}

	void SelectList(char[] name, char[] sel, char[][] options...)
	{
		this.writef("<SELECT NAME=\"%s\">",name);
		scope(exit) this.writef("</SELECT>");
		foreach(char[] op; options)
			this.writef("<option%s>%s</option>", op==sel?" SELECTED":"",op);
	}

	void TableData(char[] data){this.writef("<td>%s</td>", data);}
	void TableData(void delegate() dg)
	{
		this.writef("<td>");
		scope(exit) this.writef("</td>");
		dg();
	}
	void TableData(int span, char[] data){this.writef("<td colspan=%d>%s</td>", span, data);}
	void TableData(int span, void delegate() dg)
	{
		this.writef("<td colspan=%d>", span);
		scope(exit) this.writef("</td>");
		dg();
	}

	void TableRow(char[] data)		{this.writef("<tr>\n%s\n</tr>\n", data);}
	void TableRow(void delegate() dg)
	{
		this.writef("<tr>\n");
		scope(exit) this.writef("\n</tr>\n");
		dg();
	}

	void Table(char[] data)		{this.writef("<table>\n%s\n</table>\n", data);}
	void Table(void delegate() dg)
	{
		this.writef("<table>\n");
		scope(exit) this.writef("\n</table>\n");
		dg();
	}

	void Heading1(char[] data){this.writef("<h1>%s</h1>\n", data);}
	void Heading1(void delegate() dg)
	{
		this.writef("<h1>");
		scope(exit) this.writef("</h1>\n");
		dg();
	}

	void Heading2(char[] data){this.writef("<h2>%s</h2>\n", data);}
	void Heading2(void delegate() dg)
	{
		this.writef("<h2>");
		scope(exit) this.writef("</h2>\n");
		dg();
	}

	void Heading3(char[] data){this.writef("<h3>%s</h3>\n", data);}
	void Heading3(void delegate() dg)
	{
		this.writef("<h3>");
		scope(exit) this.writef("</h3>\n");
		dg();
	}

	void Heading4(char[] data){this.writef("<h4>%s</h4>\n", data);}
	void Heading4(void delegate() dg)
	{
		this.writef("<h4>");
		scope(exit) this.writef("</h4>\n");
		dg();
	}

	void Link(char[] name, char[] text) { this.writef(`<A HREF="%s">%s</A>`, name,text);}
	void Link(char[] name) { this.writef(`<A HREF="%s">%s</A>`, name,name);}

}