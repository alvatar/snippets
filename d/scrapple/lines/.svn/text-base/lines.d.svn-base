module lines;
import std.conv;
import std.stdio;
import std.stream;
import std.regexp;

enum Act
{
	Print,
	Line,
}

void main(char[][] args)
{
	Act act = Act.Print;
	if(args.length < 4)
	{
		writef("Usage: %s infile trapName trapLine [ -Print | -Line | ]\n",args[0]);
		return;
	}
	foreach(arg;args[4..$])
	{
		switch(arg)
		{
		case "-print": act = Act.Print; break;
		case "-line":  act = Act.Line; break;
		}
	}
	
	char[] trapName = args[2];
	int trapLine = toInt(args[3]);
	
	char[] name = args[1];
	int lineNum = 1;

	char[] realName = args[1];
	int realLine = 1;

	static const char[] prefix = "#line";

	foreach(char[] line; new File(name))
	{
		if(line.length > prefix.length && line[0..prefix.length] == prefix)
		{
			auto rex = search(line, `#line\s+([0-9]+)\s+"([\w.]+)"`);
			if(rex is null)
			{
				writef("Error %s(%d)", args[1],realLine);
				return;
			}
			//writef("wow %s %s\n",rex.match(1), rex.match(2));
			lineNum = toInt(rex.match(1));
			name = rex.match(2).dup;
		}
		else
		{
			//writef("%s / %s   %s / %s\n", name,trapName, lineNum,trapLine);
			if(lineNum == trapLine && name == trapName)
				switch(act)
				{
				case Act.Print: writef("%s\n", line); break;
				case Act.Line: writef("%s(%d)\n",realName,realLine);
				}
			lineNum++;
		}
		realLine++;
	}
}

#line 2 "bob.d"
/*

line 4
line 5
*/