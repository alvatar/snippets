import std.stdio;
import std.string;

import syntax.dparse;

class data : IParser
{
	char[] dat;
	uint i =0;
	uint pos()		{return i;}
	void pos(uint j){ i = j;}

	void mark()
	{
		writef(">>\"%s\"\n", dat[i..($-i)>60? i+60: $]);
	}
}



class GrammarParser
{

	PObject Terminal(char[] str : "NAME")(IParser p)
	{
		debug(dParse_runtime) writef("in %s\n", str);
		auto lex = cast(data) p;
		assert(lex !is null);

		int i = lex.i;

		for({} i < lex.dat.length; i++)
			switch(lex.dat[i])
			{
				case ' ', '\t', '\n', '\r': continue;

				case 'a','b','c','d','e','f','g','h','i','j','k','l','m','n',
				     'o','p','q','r','s','t','u','v','w','x','y','z',
				     'A','B','C','D','E','F','G','H','I','J','K','L','M','N',
				     'O','P','Q','R','S','T','U','V','W','X','Y','Z',
					 '_':
					goto more;

				default:
					debug(dParse_runtime) writef("fail at %s:%d with %x\n", str,__LINE__, lex.dat[i]);
					return new PObjectFail();
			}

		debug(dParse_runtime) writef("fail at %s:%d with EOF\n", str,__LINE__);
		return new PObjectFail();

		more:

		int start = i;

		i++;

		for({} i < lex.dat.length; i++)
			if(!(
				('a' <= lex.dat[i] && lex.dat[i] <= 'z') ||
				('A' <= lex.dat[i] && lex.dat[i] <= 'Z') ||
				('0' <= lex.dat[i] && lex.dat[i] <= '9') ||
				(lex.dat[i] == '_')
				))
			break;

		lex.i = i;

		assert(i <= lex.dat.length);

		return new PObjectBox!(char[])(lex.dat[start..i]);
	}

	PObject Terminal(char[] str : "COLLON")(IParser p)
	{
		debug(dParse_runtime) writef("in %s\n", str);
		auto lex = cast(data) p;
		assert(lex !is null);

		foreach(int i, char c; lex.dat[lex.i..$])
			switch(c)
			{
				case ':':
					lex.i += 1+i;
					return new PObjectPass();

				case ' ', '\t', '\n', '\r':  continue;
				default:  return new PObjectFail();
			}
		return new PObjectFail();
	}

	PObject Terminal(char[] str : "STAR")(IParser p)
	{
		debug(dParse_runtime) writef("in %s\n", str);
		auto lex = cast(data) p;
		assert(lex !is null);

		foreach(int i, char c; lex.dat[lex.i..$])
			switch(c)
			{
				case '*':
					lex.i += 1+i;
					return new PObjectPass();

				case ' ', '\t', '\n', '\r':  continue;
				default:  return new PObjectFail();
			}
		return new PObjectFail();
	}

	PObject Terminal(char[] str : "PLUS")(IParser p)
	{
		debug(dParse_runtime) writef("in %s\n", str);
		auto lex = cast(data) p;
		assert(lex !is null);

		foreach(int i, char c; lex.dat[lex.i..$])
			switch(c)
			{
				case '+':
					lex.i += 1+i;
					return new PObjectPass();

				case ' ', '\t', '\n', '\r':  continue;
				default:  return new PObjectFail();
			}
		return new PObjectFail();
	}

	PObject Terminal(char[] str : "QMARK")(IParser p)
	{
		debug(dParse_runtime) writef("in %s\n", str);
		auto lex = cast(data) p;
		assert(lex !is null);

		foreach(int i, char c; lex.dat[lex.i..$])
			switch(c)
			{
				case '?':
					lex.i += 1+i;
					return new PObjectPass();

				case ' ', '\t', '\n', '\r':  continue;
				default:  return new PObjectFail();
			}
		return new PObjectFail();
	}

	PObject Terminal(char[] str : "PIPE")(IParser p)
	{
		debug(dParse_runtime) writef("in %s\n", str);
		auto lex = cast(data) p;
		assert(lex !is null);

		foreach(int i, char c; lex.dat[lex.i..$])
		{
			switch(c)
			{
				case '|':
					lex.i += 1+i;
					return new PObjectPass();

				case ' ', '\t', '\n', '\r':  continue;
				default:  return new PObjectFail();
			}
		}
		return new PObjectFail();
	}

	PObject Terminal(char[] str : "SEMICOLLON")(IParser p)
	{
		debug(dParse_runtime) writef("in %s\n", str);
		auto lex = cast(data) p;
		assert(lex !is null);

		foreach(int i, char c; lex.dat[lex.i..$])
		{
			switch(c)
			{
				case ';':
					lex.i += 1+i;
					return new PObjectPass();

				case ' ', '\t', '\n', '\r':  continue;
				default:  return new PObjectFail();
			}
		}
		return new PObjectFail();
	}

	PObject Terminal(char[] str : "SLASH")(IParser p)
	{
		debug(dParse_runtime) writef("in %s\n", str);
		auto lex = cast(data) p;
		assert(lex !is null);

		foreach(int i, char c; lex.dat[lex.i..$])
			switch(c)
			{
				case '/':
					lex.i += 1+i;
					return new PObjectPass();

				case ' ', '\t', '\n', '\r':  continue;
				default:  return new PObjectFail();
			}
		return new PObjectFail();
	}

	template Terminal(char[] str ) { PObject Terminal(IParser i); pragma(msg, "<need type=ter>"~str~"</need>"); }
	template Action(char[]str) { PObject Action(PObject[] p); pragma(msg, "<need type=act>"~str~"</need>"); }

	PObject Action(char[]s:"pass1")(PObject[1]p){return p[0];}
	PObject Action(char[]s:"pass2nd")(PObject[2]p){return p[1];}

	PObject Action(char[]s:"formRule")(PObject[5]p) 
	{
		// NAME COLLON Option ElseOption* SEMICOLLON
		auto name = cast(PObjectBox!(char[])) p[0];
		auto first = cast(PObjectBox!(Opt)) p[2];
		auto rest = cast(PObjectSet) p[3];
		assert(name !is null, p[0].BaseName ~" != " ~ typeof(name).stringof);
		assert(first !is null, p[2].BaseName ~" != " ~ typeof(first).stringof);
		assert(rest !is null, p[3].BaseName ~" != " ~ typeof(rest).stringof);

		GRule ret;
		ret.name = name.Get;

		ret.opts.length = 1 + rest.Count();

		ret.opts[0] = first.Get;
		foreach(int i, po; rest.get)
		{
			auto box = cast(PObjectBox!(Opt))po;
			assert(box !is null);
			ret.opts[i+1] = box.Get;
		}

		return new PObjectBox!(GRule)(ret);
	}

	PObject Action(char[]s:"formOpt")(PObject[3]p)
	{
		// NAME SLASH Parts*
		auto name = cast(PObjectBox!(char[])) p[0];
		auto rest = cast(PObjectSet) p[2];

		Opt ret;
		ret.name = name.Get;

		ret.parts.length = rest.Count();
		foreach(int i, po; rest.get)
		{
			auto box = cast(PObjectBox!(Part))po;
			assert(box !is null);
			ret.parts[i] = box.Get;
		}

		return new PObjectBox!(Opt)(ret);
	}

	PObject Action(char[]s:"any")(PObject[2]p)
	{
		auto name = cast(PObjectBox!(char[])) p[0];

		Part ret;

		ret.name = name.Get;
		ret.type = Part.Type.any;

		return new PObjectBox!(Part)(ret);
	}

	PObject Action(char[]s:"many")(PObject[2]p)
	{
		auto name = cast(PObjectBox!(char[])) p[0];

		Part ret;

		ret.name = name.Get;
		ret.type = Part.Type.many;

		return new PObjectBox!(Part)(ret);
	}

	PObject Action(char[]s:"maybe")(PObject[2]p)
	{
		auto name = cast(PObjectBox!(char[])) p[0];

		Part ret;

		ret.name = name.Get;
		ret.type = Part.Type.maybe;

		return new PObjectBox!(Part)(ret);
	}

	PObject Action(char[]s:"one")(PObject[1]p)
	{
		auto name = cast(PObjectBox!(char[])) p[0];

		Part ret;

		ret.name = name.Get;
		ret.type = Part.Type.one;

		return new PObjectBox!(Part)(ret);
	}

	static const char[] gram = 
	"
	Gram : pass1 / Rule+ ;
	Rule : formRule / NAME COLLON Option ElseOption* RuleEnd ;
	RuleEnd : 
		pass1 / PIPE |
		pass1 / SEMICOLLON;
	Option : formOpt / NAME SLASH Parts*;
	ElseOption : pass2nd / PIPE Option ;
	Parts:
		any / NAME STAR |
		many / NAME PLUS |
		maybe / NAME QMARK |
		one / NAME;
	";
	static const char[] mix = MakeMixin!("Gram",ReduceWhite(gram));
	//pragma(msg,mix);
	mixin(mix);
}

struct GRule
{
	char[] name;
	Opt[] opts;

	char[] toString()
	{
		char[] ret = "";

//		writef("%s > %d\n", name, opts.length);

		foreach(Opt p; opts)
			ret ~= p.toString ~ " |\n";

//		writef("--%s==\n\n", ret);

		return  name ~ " : \n" ~ ret[0..$-3] ~ " ;\n\n";
	}
}

struct Opt
{
	char[] name;
	Part[] parts;

	char[] toString()
	{
		char[] ret = "";

		foreach(Part p; parts)
			ret ~= p.toString ~ " ";

		return \t ~ name ~ " / " ~ ret;
	}
}

struct Part
{
	char[] name;
	enum Type { any=0, many=1, maybe=2, one=3 }
	Type type;

	char[] toString() { return name ~ "*+? "[type]; }
}

import std.file;
import std.cstream;

void main(char[][] argv)
{
	bool 
		printOrg = false,
		ConvertRec = false,
		TestRec = false,
		DmpFinal = true;

	char[] filename = "";

	foreach(arg;argv[1..$])
		if(arg.length >= 2)
			switch(arg[0..2])
			{
				case "-o": printOrg   = (arg[2..$] != "-"); break;
				case "-c": ConvertRec = (arg[2..$] != "-"); break;
				case "-t": TestRec    = (arg[2..$] != "-"); break;
				case "-f": DmpFinal   = (arg[2..$] != "-"); break;
				default: filename = arg; break;
			}
		else
			{filename = arg;}

	data d = new data;

	if(filename != "")
		d.dat = ReduceWhite(cast(char[])std.file.read(filename));
	else
	{
		//d.dat = ReduceWhite(GrammarParser.gram.dup);
		//d.dat = ReduceWhite("AttributeSpecifier: dummy / Attribute  opCollin  | dummy / Attribute DeclarationBlock | ;".dup);
		//d.dat = "Foo: A1 / Foo B | A2 / Foo C D | A3 / Foo E | A4 / F | A5 / G H ;".dup;
		assert(false, "no file");
	}

	GrammarParser gp = new GrammarParser;

	auto g = gp.Parser(d);
	assert(g !is null);
	assert(!g.fail);

	GRule[] inp;
	GRule[] outp;


	auto parsed = (cast(PObjectSet)g).get;
	inp.length = parsed.length;

	//writef("Parsing returned: %s\n", g.BaseName);
	foreach(ind, r;parsed)
	{
		//writef("  Object #%d is: %s\n", ind, r.BaseName);
		inp[ind] = (cast(PObjectBox!(GRule ))r).Get;
	}

	if(printOrg)
	{
		foreach(r; inp) writef(">>%s", r.toString);
	}

	bool b;
	if(ConvertRec)
		do
		{
			b = false;
			
			b |= Convert2(inp);

			int index = 0;

			foreach(r; inp) b |= Convert(r, outp, index);

			inp = outp[0..index];
		} while(b)

	if(TestRec)
	{
		GRule*[char[]] rules;
		int[GRule*] state;
		foreach(inout r; inp)
		{
			rules[r.name] = &r;
			state[&r] = 0;
		}

		bool newc = true;

		char[] breaker;

		int Walk(GRule* r)
		{
			int max = state[r];

			if(max != 0) return max;
			
			state[r] = -1;

			foreach(o; r.opts)
			{
				GRule* first;
				//derr.writef("%s\n", o.toString);
				assert(o.parts.length > 0, r.name~":"~o.name~" has no parts");
				if(auto s = o.parts[0].name in rules)
					first = *s;
				else
					continue;

				int step = Walk(rules[first.name]);

				if(step == -1)
				{
					newc = false;
					if(breaker == null)
						breaker = first.name;

					writef("%s<-", first.name);

					if(breaker == r.name)
					{
						breaker= null;
						writef("%s\n", r.name);
						continue;
					}
					return -1;
				}

				max = max > step ? max : step;
			}

			max++;
			state[r] = max;
			return max;
		}

		int max = 0;
		foreach(inout r; inp)
		{
			int step = Walk(&r);
			max = max > step ? max : step;
		}

		if(!newc)
		{
			writef("\nfound cycle\n\n");
			return;
		}
	}

	if(DmpFinal)
	{
		foreach(r; inp) writef("%s", r.toString);
	}
}

bool Convert(inout GRule gr, inout GRule[] outp, inout int index)
{
	foreach(Opt o; gr.opts)
		if(o.parts.length > 0 && o.parts[0].name == gr.name)
			goto patch;

	if(outp.length <= index)
		outp.length = outp.length + 10;

	outp[index] = gr;
	index++;

	return false;

	patch:

	GRule first, rest;
	rest.name = "__LRT_"~gr.name;
	first.name = gr.name;

	Part p;
	p.name = rest.name;
	p.type = Part.Type.any;

	Opt n;

	foreach(Opt o; gr.opts)
	{
		//writef("%s\n", o.name);
		if(o.parts.length > 0 && o.parts[0].name == gr.name)
		{	// recusive
			n.name = format("$(L,%d,%s)",o.parts.length-1,o.name);
			n.parts = o.parts[1..$];
			rest.opts ~= n;
		}
		else
		{	// non recursive
			n.name = format("$(T,%d,%s)",o.parts.length,o.name);
			n.parts = o.parts;
			n.parts ~= p;
			first.opts ~= n;
		}
	}

	if(outp.length <= index+1)
		outp.length = outp.length + 10;

	outp[index] = first;
	index++;

	outp[index] = rest;
	index++;

	return true;
}

bool Convert2(GRule[] outp)
{
	int[char[]] map;
	foreach(int i, GRule g; outp) map[g.name] = i;

	bool ret = false;

	outer: foreach(first; outp)
	{
//		foreach(o; first.opts) if(o.parts[0].name == first.name) continue outer; writef("%s not direclt LR\n", first.name);

		Opt[] add, tmp;

		mid: foreach(inout fo; first.opts)
		{
			if(fo.parts.length != 1) continue mid;

			if(!(fo.parts[0].name in map)) continue mid;

			auto second = &outp[map[fo.parts[0].name]];

			if(second.opts.length == 0) continue mid;


			foreach(Opt so; second.opts)
				if(so.parts[0].name != first.name)
					continue mid;

			fo.name = format("$(N,%d,%s,%s)",second.opts[0].parts.length ,second.opts[0].name, fo.name);
			fo.parts = second.opts[0].parts;

			tmp.length = second.opts.length - 1;
			foreach(i, so; second.opts[1..$])
			{
				tmp[i].name = format("$(N,%d,%s,%s)",so.parts.length ,so.name, fo.name);
				tmp[i].parts = so.parts;
			}

			add ~= tmp;
		}
		if(add.length > 0)
		{
			ret |= true;
			first.opts ~= add;
		}
	}

	return ret;
}