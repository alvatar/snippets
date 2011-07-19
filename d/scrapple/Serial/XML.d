module XML;

import Serialize;
import Interface;

/**
 * Assuming not in a tag, read data untill a tag is found
 * Params:
 *     term = the char to terminate on
 *     source = the data source
 * Returns:
 *     the read data with escaping removed.
 */
char[] ReadContentXML(char[] term = "<")(Source!(char) source)
{
	char[] ret = new char[16];
	int i = 0;
	char t;
	while(-1 == std.string.find(term, source.Peek()))
	{
		source.Pick(t);
		if(t == '&')
		{
			if(!source.Pick(t)) throw new Exception("Invalid Source");
			root: switch(t)
			{
			case 'a':
				if(!source.Pick(t)) goto default;
				switch(t)
				{
				case 'm':
					if(!source.Pick("p;")) goto default;
					t = '&';
					break root;

				case 'p':
					if(!source.Pick("os;")) goto default; 
					t = '\''; 
					break root;

				default:
					throw new Exception("Invalid Source");
				}
				break;

			case 'g':
				if(!source.Pick("t;")) goto default;
				t = '>'; 
				break;

			case 'l':
				if(!source.Pick("t;")) goto default;
				t = '<'; 
				break;

			case 'q':
				if(!source.Pick("uot;")) goto default;
				t = '\"'; 
				break;

			default:
				throw new Exception("Invalid Source");
			}
		}
		ret[i++] = t;

		if(ret.length <= i) ret.length = i*2;
	}
	return ret[0..i];
}

void WriteContentXML(Sink!(char) sink, char[] content)
{
	int from = 0;
	foreach(int i, char c; content)
		switch(c)
		{
		case '\"':
			if(from != i) sink.Dump(content[from..i]);
			sink.Dump("&quot;");
			from = i+1;
			break;

		case '&':
			if(from != i) sink.Dump(content[from..i]);
			sink.Dump("&amp;");
			from = i+1;
			break;

		case '\'':
			if(from != i) sink.Dump(content[from..i]);
			sink.Dump("&apos;");
			from = i+1;
			break;

		case '<':
			if(from != i) sink.Dump(content[from..i]);
			sink.Dump("&gt;");
			from = i+1;
			break;

		case '>':
			if(from != i) sink.Dump(content[from..i]);
			sink.Dump("&lt;");
			from = i+1;
			break;

		default:
			break;
		}
	if(from != content.length) sink.Dump(content[from..$]);

	return;
}
