module StringSourceSink;

import Interface;

class Si : Sink!(char)
{
	char[] str;
	int at = 0;

	char[] get() { return str[0..at]; }

	this(uint len = 16)
	{
		if (len <= at) len = 1;
		str.length = len;
	}

	void Dump(char c)
	{
		if(at >= str.length) str.length = at * 2;
		str[at++] = c;
	}

	void Dump(char[] c)
	{
		if(at + c.length >= str.length) str.length = at + c.length + 10;
		str[at..at+c.length] = c;
		at += c.length;
	}
}

class So : Source!(char)
{
	this(char[] s){ str = s.dup; }

	char[] str;
	uint at = 0;
	
	bool Peek(char t) { return at < str.length  && str[at] == t; }
	char Peek() { return str[at]; }

	bool Pick(ref char t, bool must)
	{
		if(at < str.length)
		{
			t = str[at];
			at++;
			return true;
		}
		else if(must) throw new Error("\n"~str[at..$]~"\n"~t~"<<");
		else return false;
	}

	bool Pick(char[] ts, bool must)
	{
		if(at + ts.length > str.length || ts != str[at..at+ts.length])
		{
			if(must) throw new Error("\n"~str[at..$]~"\n"~ts);
			return false;
		}
		at += ts.length;
		return true;
	}
}
