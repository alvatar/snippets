/++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
This is a template meta-program that generates a recursive decent
parser. It is to be mixed into a class or struct. The parameters
for the template are: a string containing almost normal BNF
reduction definitions, and a string defining the start rule.

It extends BNF by allowing "|" for "or" but doesn't implement
optional or repeated terms.

$(TABLE
$(TR $(TD Grammar)  $(TD ::= Rule Grammar | Rule))
$(TR $(TD Rule)     $(TD ::= ID ":" Cases ";"))
$(TR $(TD Cases)    $(TD ::= Case "|" Alt | Case))
$(TR $(TD Case)     $(TD ::= ID "/" Sequence))
$(TR $(TD Sequence) $(TD ::= ID Sequence | ID))
$(TR $(TD Item)     $(TD ::= ID "[?+*]?";))
$(TR $(TD ID)       $(TD ::= "[A-Za-z][A-Za-z0-9]*";))
)

The term between the ':' and '/' is the name of the reduction
action. If a reduction is used but not defined a terminal must
be defined for it.

Example:

-----------------------------------------
struct set
{
/******* action code ********/
static PObject Action(char[] string : "MULTIPLY")(PObject[3] set)
   {...}
static PObject Action(char[] string : "SUM"     )(PObject[3] set)
   {...}
static PObject Action(char[] string : "PASS"    )(PObject[1] set)
   {...}

/******** Terminal code *********/
static PObject Terminal(char[] name : "NUM")(IParser p)
   {...}
static PObject Terminal(char[] name : "PLUS")(IParser p)
   {...}
static PObject Terminal(char[] name : "TIMES")(IParser p)
   {...}

// "ADD" indicates the root rule

mixin Parser!("ADD", ReduceWhite(
    "PRIMARY : PASS/     NUM;
     MUL     : MULTIPLY/ PRIMARY TIMES MUL
             | PASS/     PRIMARY;
     ADD     : SUM/      MUL PLUS ADD
             | PASS/     MUL;
    "));
}

void main()
{
   auto a = new ExpGrammer;
   a.data = "1 + 3 * 4 + 5 * 5 ";

   Value v = cast(Value)set.Parser(a);

   assert(v !is null,"ERR0R");
   writef("%d\n", v.value);
}
----------------

This meta program is distributed with no warranty of any kind.

Author: Benjamin Shropshire (shro8822drop_this at vandals DOT uidaho edu)

Please give credit where credit is due. Don't remove this notice.

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++/
module syntax.dparse;

// Add not grammer, might use non-Action parser
private import std.stdio : writef;
private import std.string : ToString = toString ;

private import glue.templates;


/******************************************************************************
********* PObject Hieracrcy
******************************************************************************/


/** Base class for parsed objects
*/
abstract class PObject
{
	///
	abstract bool fail();

	abstract char[] BaseName();
}


/************************************************
	Common base type for ObjectVector types
*/
class PObjectVectorBase : PObject
{
	char[] BaseName(){return typeof(this).stringof;}
	abstract char[] string();
}

/************************************************
	A template sub class of PObject for basic tree building
*/
class PObjectVector(uint i) : PObjectVectorBase
{
	char[] BaseName(){return typeof(this).stringof;}
	alias Tupleof!(i,PObject) type;
	type data;
	/// constructor taking i PObjects
	this(type s)
	{
		foreach(int j, t; type)
			data[j] = s[j];
	}

	this(PObject[i] d)
	{
		foreach(uint j, v; data)
		{
			data[j] = d[j];
		}
	}

	PObject[] Get()
	{
		PObject[i] ret;
		foreach(uint j, v; data)
		{
			ret[j] = data[j];
		}
		return ret.dup;
	}

	/// Get the j'th item
	PObject Get(uint j)
	{
		switch(j)
		{
			foreach(int k, t; type)
				case k: return data[k];
			default: throw new Error("Out of bound");
		}
	}

	/// Get the j'th item
	PObject GetT(uint j)()
	{
		static if(j<data.length)
			return data[j];
		else
			static assert(false);
	}

	char[] string()
	{
		char[] ret =  "[";

		foreach(i,_;type)
		{
			if(auto v = cast(PObjectVectorBase)data[i])
			{
				ret ~= " " ~ v.string();
			}
			else if(auto v = cast(PObjectBoxBase)data[i])
			{
				ret ~= " \"" ~ v.string()~\";
			}
			else ret ~= " <UNKNOWN>";
		}
		return ret ~ "]";
	}

	bool fail() { return false; }
}

/************************************************
	Common base type for BoxObject types
*/
class PObjectBoxBase : PObject
{
	char[] BaseName(){return typeof(this).stringof;}
	abstract char[] string();
}

/**
*/
class PObjectBox(T,bool str = true) : PObjectBoxBase
{
	char[] BaseName(){return typeof(this).stringof~"!("~T.stringof~")";}
	T t;

	///
	T Get() { return t; }

	///
	this(T ti){t=ti;}

	bool fail() { return false; }

	///
	char[] string()
	{
		static if(str)
		{
			static if(is(T == char[]))
				return t;
			else static if(is(ToString(t)))
				return ToString(t);
			else
				return "<<"~T.stringof~">>";
		}
		else
			return "<<"~T.stringof~">>";
	}
}
//typedef PObjectVector!(5) Five;

/**
*/
class PObjectList(T) : PObject
{
	char[] BaseName(){return typeof(this).stringof;}
	debug(TypeReport) pragma(msg, ">>T-"__FILE__~":"~itoa!(__LINE__)~": "~typeof(this).stringof~"!("~T.stringof~")");

	T[] list;
	int at=0;

	this(){ list = null; }

	this(T ti)
	{
		static if(is(T :Object)) assert(ti !is null);
		list = new T[5];
		list[at] = ti;
		at++;

	}

	/// Add a T
	uint Add(T ti)
	{
		static if(is(T :Object)) assert(ti !is null);
		if(list.length <= at) list.length = at + 5;
		list[at] = ti;
		at++;
		return at-1;
	}

	/// return the number of items stored
	uint Count(){return at;}

	///
	T[] get()
	{
		return list[0..at];
	}

	bool fail() { return false; }
}
unittest
{
	writef("unittest@"__FILE__":"~itoa!(__LINE__)~\n);
	
	auto o = new PObjectList!(int);
	o.Add(1);
	o.Add(2);
	o.Add(3);
	o.Add(4);
	o.Add(5);
	o.Add(6);

	assert(o.get == [1,2,3,4,5,6], "PObjectList failed");
}

/************************************************
	List Object that build to the left
*/
class PObjectListLeft(T) : PObjectList!(T)
{
	char[] BaseName(){return typeof(this).stringof;}
	debug(TypeReport) pragma(msg, ">>T-"__FILE__~":"~itoa!(__LINE__)~": "~typeof(this).stringof~"!("~T.stringof~")");

	this(T t)
	{
		list.length = 5;
		list[$-1] = t;
		at = 1;
	}
	this(){}
	
	/// Add a T
	uint Add(T ti)
	{
		static if(is(T :Object)) assert(ti !is null);

		if(list.length <= at)
		{
			auto t = list[$-at..$].dup;
			list.length = at + 5;
			list[$-at..$] = t;
		}
		list[$-1-at] = ti;
		at++;
		return at-1;
	}

	T[] get()
	{
		return list[$-at..$];
	}
}

unittest
{
	writef("unittest@"__FILE__":"~itoa!(__LINE__)~\n);
	
	auto o = new PObjectListLeft!(int)( 0);
	           o.Add( 1); o.Add( 2); o.Add( 3); o.Add( 4); o.Add( 5); o.Add( 6); o.Add( 7); o.Add( 8); o.Add( 9);
	o.Add(10); o.Add(11); o.Add(12); o.Add(13); o.Add(14); o.Add(15); o.Add(16); o.Add(17); o.Add(18); o.Add(19);
	o.Add(20); o.Add(21); o.Add(22); o.Add(23); o.Add(24); o.Add(25); o.Add(26); o.Add(27); o.Add(28); o.Add(29);

	assert(o.get == [29,28,27,26,25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0], "PObjectListLeft failed");
}


/************************************************
	A sub class of PObject that is used for repeated reductions
*/
class PObjectSet : PObjectList!(PObject)
{
	char[] BaseName(){return typeof(this).stringof;}
	/// Discard some PObjects
	void Back(uint b)
	{
		at = b;
	}
}



/************************************************
	Uniform term to prevent needing to know the
	exact type at compile time.
*/
interface PInterfaceLeftFactor(T) { PObject InsertLeft(T,PObject); }

/************************************************
	PObject that uses T.Action!(str) to process a
	left side term with a Right side term
*/
class PObjectLeftFactorT(Tp, int i, char[] str) : PObjectVector!(i), PInterfaceLeftFactor!(Tp)
{
	this(PObject[i] d){super(d);}

	PObject InsertLeft(Tp that, PObject L)
	{
		PObject[i+1] args;

		args[1..$] = Get;
		args[0] = L;

		return SpecialAction!(Tp,str)(that,args);
	}
}


/************************************************
	default fail object
*/
class PObjectFail : PObject
{
	char[] BaseName(){return typeof(this).stringof;}
	char[][] msg;

	this()
	{
		msg.length = 1;
		msg[0] = "Failed";
	}

	this(char[] m)
	{
		msg.length = 1;
		msg[0] = m.dup;
	}

	void Add(char[] m)
	{
		msg ~= m.dup;
	}

	bool fail() { return true; }
}

/************************************************
	default Pass object
*/
class PObjectPass : PObject
{
	char[] BaseName(){return typeof(this).stringof;}
	this() { }
	bool fail() { return false; }
}

/************************************************
	default Filler object, returns fail state as instructed
*/
class PObjectFill : PObject
{
	char[] BaseName(){return typeof(this).stringof;}
	bool b;
	this(bool _b){b=_b;}
	bool fail(){return b;}
}

/*******************************************************
	interface to handle parser data
*/
interface IParser
{
	uint pos();
	void pos(uint);
	debug(dParse_runtime) void mark();
}


/******************************************************************************
******** genaric Template code
******************************************************************************/


/** generate a tuple of i U's
*/
template Tupleof(uint i, U)
{
	static if(i == 0)
		alias T!() Tupleof;
	else
	static if(i == 1)
		alias T!(U) Tupleof;
	else
		alias T!(Tupleof!(i-1, U), U) Tupleof;
}

/** Tuple literal template
*/
template T(A...){alias A T;}


/******************************************************************************
******** CTFE functions
******************************************************************************/

/******************************
drop leading whitespace
*/
char[] DropWhiteF(char[] str)
{

    foreach(int i, char c; str)
        switch(c)
        {
            case ' ', '\t', '\r', '\n':
                continue;
            default:
                return str[i..$];
        }
        return "";
}
struct UnittetDropWhite{
	static const char[] str = DropWhiteF(" \t\n\rhello");
	static assert(DropWhiteF(" \t\n\rhello") == "hello");
}


/**********************************
  find first instance of t in str,
  return str up through that char
*/
char[] FindChar(char t)(char[] str)
{
	foreach(int i, char c; str)
		if(c == t)
			return str[0..i+1];
	return str;
}


/**********************************
  return an identifier
*/
char[] GetID(char[] instr)
{
	char[] str = DropWhiteF(instr);

	if(str.length == 0) return "";

	if(
		!('a' <= str[0] && str[0] <= 'z') &&
		!('A' <= str[0] && str[0] <= 'Z') &&
		!('_' == str[0])
	) return "";

	foreach(int i, char c; str)
	if(
		!('a' <= c && c <= 'z') &&
		!('A' <= c && c <= 'Z') &&
		!('0' <= c && c <= '9') &&
		!('_' == c)
	) return str[0..i];

	return str;
}
struct UnittetGetID{
	static assert(GetID("hello world") == "hello", GetID("hello world"));
}

/*********************************
  replace all sequences of white
  space with single spaces
*/
public char[] ReduceWhite(char[] str)
{
	bool pass = true;
	int at = 0;
	foreach(char c; str)
		switch(c)
		{
			case ' ', '\n', '\r', '\t':
				if(!pass)
				{
					pass = true;
					str[at++] = ' ';
				}
				break;

			default:
				pass = false;
				str[at++] = c;
				break;
		}
	return str[0..at];;
}
static const char[] test = ReduceWhite("hello");

/*********************************
	Extract # from "$(C,#,NAME)"
*/
int ExtractCount(char[] str)
{
	int ret = int.min;

	if(str.length > 4 && '0' <= str[0] && str[0] <= '9')
	{
		ret = str[0] - '0';
		for(int i = 1; i<str.length; i++)
		{
			if('0' <= str[i] && str[i] <= '9')
			{
				ret *= 10;
				ret += str[i] - '0';
			}
			else
				break;
		}
	}

	return ret;
}

/*********************************
	Extract NAME from "$(C,#,NAME)"
*/
char[] ExtractAct(char[] str)
{
	int c, i;
	for(i=0; i < str.length && c < 2; i++)
		c += (str[i] == ',');

	str = str[i..$];
	i=0;
	int d = 1;
	while(i < str.length && d > 0)
	{
		d += (str[i] == '('); 
		d -= (str[i] == ')'); 
		i++;
	}

	return str[0..i-1];
}

static assert(ExtractAct("$(T,1,$(N,1,abc,def))") == "$(N,1,abc,def)", ExtractAct("$(T,1,$(N,1,abc,def))"));

/******************************************************************************
******* Special Action Code
******************************************************************************/

/// Pack up stuff in a computed type of PObjectLeftFactorT Object
PObject L_Action(Tp, char[] str ) (Tp, PObject[ExtractCount(str[4..$])] i)
{
	static const int c = ExtractCount(str[4..$]);
	static const char[] n = ExtractAct(str);
	static if(false) pragma(msg, str~" == $(L,"~c.stringof~","~n~")");

	return new PObjectLeftFactorT!(Tp, c, n)(i);
}

/// Left Process a Leftmost term and right side terms list.
PObject T_Action(Tp,char[] str) (Tp that, PObject[1+ExtractCount(str[4..$])] i)
{
	static const int num = ExtractCount(str[4..$]);
	static const char[] act = ExtractAct(str);
	static if(false) pragma(msg, str~" == $(T,"~num.stringof~","~act~")");

	PObject[num] args = i[0..num];
	auto ret = SpecialAction!(Tp,act)(that,args);

	auto listO = cast(PObjectList!(PObject)) i[num]; 
	auto list = listO.get;

	foreach(obj; list)
	{
		auto Tobj = cast(PInterfaceLeftFactor!(Tp))obj;
		ret = Tobj.InsertLeft(that,ret);
	}
	return ret;
}


template GetSize(Tp, char[] str)
{
	alias Tp.Action!(str) Fn;
	static if(is(typeof(Fn) Args == function))
		const int size = Args[0].length;
	else
		static assert(false);
}

template GetArgs(Tp, char[] str)
{
	alias Tp.Action!(str) Fn;
	static if(is(typeof(Fn) Args == function))
	{}
	else
		static assert(false);
}

/// Next actions 

template N_parts(char[] str)
{
	const int num = ExtractCount(str[4..$]);
	const char[] firstAct = ExtractAct(str[4..$]);
	const char[] s1 = str[FindChar!(',')(str[4..$]).length + 4..$];
	const char[] secondAct = s1[FindChar!(',')(s1).length..$-1];
}

//alias N_parts!("$(N,3,dummy,dummy)") NP;

PObject N_Action(Tp,char[] str) (Tp that, GetArgs!(Tp,N_parts!(str).firstAct).Args i)
{
	alias N_parts!(str) P;
	static if(false) pragma(msg, str~" == $(N,"~P.num.stringof~","~P.act~")");

//	pragma(msg,">"~str);	pragma(msg,">"~P.firstAct);	pragma(msg,">"~P.secondAct);

	PObject[] ret;
	ret[0] = SpecialAction!(Tp,P.firstAct)(that,i);
	ret[0] = SpecialAction!(Tp,P.secondAct)(that,ret);

	return ret[0];
}




template SpecialAction(Tp,char[] str)
{
//	pragma(msg,str);
	static if(str.length > 3 && str[0..2] == "$(")
	{
		static if(false) pragma(msg, str);
		static if(str[2] == 'T')
			alias T_Action!(Tp,str) SpecialAction;
		else static if(str[2] == 'L')
			alias L_Action!(Tp,str) SpecialAction;
		else static if(str[2] == 'N')
		{
			alias N_Action!(Tp,str) SpecialAction;
		}
		else
			static assert (false, "unknown special action: "~str);
	}
	else
	{
		
		PObject SpecialAction(Tp that, GetArgs!(Tp,str).Args i)
		{
			return that.Action!(str)(i);
		}
		//static assert (false, "unknown special action: "~str);
	}
}

/******************************************************************************
******* Parser
******************************************************************************/


/************************************************
	Parse an Identifier name (ID ::= "[A-Za-z][A-Za-z0-9]*")
*/
template Parse_ID (char[] str)
{
	private const char[] without = DropWhiteF(str);
	private const char[] text = GetID(without);
	const bool Match = (text.length != 0);
	static if(Match)
	{
		const char[] Text = text;
		const char[] Remaining = without[Text.length .. $];
		const char[] Used = str[0 .. $-Remaining.length];
	}
}
struct UnittetParse_ID
{
	// Tests
	alias Parse_ID!(" \thello world") Parse_ID_test1;
	static assert(Parse_ID_test1.without =="hello world","Parse_ID failed: "~Parse_ID_test1.without);
	static assert(Parse_ID_test1.Remaining == " world",  "Parse_ID failed: "~Parse_ID_test1.Remaining);
	static assert(Parse_ID_test1.Text == "hello",        "Parse_ID failed: "~Parse_ID_test1.Text);
	static assert(Parse_ID_test1.Match,                  "Parse_ID failed: ");
	static assert(Parse_ID_test1.Used == " \thello",     "Parse_ID failed: \""~Parse_ID_test1.Used~\");

	alias Parse_ID!("	\t!hello world") Parse_ID_test2;
	static assert(!Parse_ID_test2.Match,"Parse_ID failed: ");
}


char[] MatchPairs(char I, char O)(char[] str)
{
	int i = 1;
	foreach(int j, char c; str)
	{
		if (c == I) i++;
		if (c == O) i--;
		if (i == 0) return str[0..j+1];
	}
	return "";
}


/************************************************
	Parse a Sepcial action name name
	'$([^)]*)' |
	'$[A-Z][A-Za-z0-9_]*' |
	'$'
*/
template Parse_SpecialAct (char[] str)
{
	private const char[] without = DropWhiteF(str);
	//pragma(msg,">>"__FILE__":"~__LINE__.stringof[0..$-1]~": '"~without~\');

	//pragma(msg,__LINE__.stringof~":"~without);

	static if(without.length < 4 || without[0..2] != "$(")
	{
		pragma(msg,">>"__FILE__":"~__LINE__.stringof[0..$-1]~": unknown Special '"~without~"' from '"~str~\');
		const bool Match = false;
	}
	else
	{
		const char[] t = MatchPairs!('(',')')(without[2..$]);

		//pragma(msg,__LINE__.stringof~":"~t);
		const bool Match = (t != "");
		static if(Match)
		{
			const char[] Text = without[0..2+t.length];
			const char[] Remaining = without[Text.length .. $];
			const char[] Used = str[0 .. $-Remaining.length];
		}
	}
	//pragma(msg,__LINE__.stringof~":"~Match.stringof);
}

struct UnittetParse_Parse_SpecialAct{
	// Tests
	static assert(Parse_SpecialAct!("$(Ltree) ").Match);

	//static assert(!Parse_SpecialAct!("$a ").Match);
	//static assert(!Parse_SpecialAct!("a ").Match);
}



/************************************************
	The types of rules
*/
enum ItemType
{
	single,
	star,
	plus,
	optional
}

/************************************************
	Parse a rule part (Item ::= ID "[?+*]?")
*/
struct Parse_Item(char[] str)
{
	private alias Parse_ID!(str) Item;
	static if(Item.Match)
	{
		/// test set if the rule matched
		static const bool Match = true;
		static const char[] Text = Item.Text;

		private static const char[] tmp = DropWhiteF(Item.Remaining);
		static if(tmp.length > 0)
		{
			static if(tmp[0] == '*')
			{
				static const char[] Remaining = tmp[1..$];
				static const ItemType Type = ItemType.star;
			}
			else
			static if(tmp[0] == '+')
			{
				static const char[] Remaining = tmp[1..$];
				static const ItemType Type = ItemType.plus;
			}
			else
			static if(tmp[0] == '?')
			{
				static const char[] Remaining = tmp[1..$];
				static const ItemType Type = ItemType.optional;
			}
			else
			{
				static const char[] Remaining = Item.Remaining;
				static const ItemType Type = ItemType.single;
			}
		}
		else
		{
			static const char[] Remaining = Item.Remaining;
			static const ItemType Type = ItemType.single;
		}
		static const char[] Used = str[0 .. $-Remaining.length];
	}
	else
		static const bool Match = false;
}
struct UnittestParse_Item
{
	// Tests
	alias Parse_Item!(" hello world") Parse_Item_test1;
	static assert(Parse_Item_test1.Match);
	static assert(Parse_Item_test1.Text == "hello");
	static assert(Parse_Item_test1.Used == " hello",       Parse_Item_test1.Used);
	static assert(Parse_Item_test1.Remaining == " world");
	static assert(Parse_Item_test1.Type == ItemType.single);

	alias Parse_Item!(" hello +world") Parse_Item_test2;
	static assert(Parse_Item_test2.Match);
	static assert(Parse_Item_test2.Text == "hello");
	static assert(Parse_Item_test2.Used == " hello +",       Parse_Item_test1.Used);
	static assert(Parse_Item_test2.Remaining == "world");
	static assert(Parse_Item_test2.Type == ItemType.plus);

	alias Parse_Item!(" hello* +world") Parse_Item_test3;
	static assert(Parse_Item_test3.Match);
	static assert(Parse_Item_test3.Text == "hello");
	static assert(Parse_Item_test3.Used == " hello*",       Parse_Item_test1.Used);
	static assert(Parse_Item_test3.Remaining == " +world");
	static assert(Parse_Item_test3.Type == ItemType.star);

	alias Parse_Item!(" hello?*+world") Parse_Item_test4;
	static assert(Parse_Item_test4.Match);
	static assert(Parse_Item_test4.Text == "hello");
	static assert(Parse_Item_test4.Used == " hello?",       Parse_Item_test1.Used);
	static assert(Parse_Item_test4.Remaining == "*+world");
	static assert(Parse_Item_test4.Type == ItemType.optional);

	alias Parse_Item!(" \t?*+world") Parse_Item_test5;
	static assert(!Parse_Item_test5.Match);
}


/************************************************
	Parse a Sequence of rule parts (Sequence ::= ID Sequence | ID)
*/
struct Parse_Sequence (char[] str)
{
	private alias Parse_Item!(str) first;
	static if(first.Match)
	{
		static const bool Match = true;
		alias Parse_Sequence!(first.Remaining) rest;
		static if(rest.Match)
		{
			static alias T!(first, rest.Clauses) Clauses;

			static const char[] Used = str[0 .. first.Used.length + rest.Used.length];
		}
		else
		{
			alias T!(first) Clauses;
			static const char[] Used = first.Used;
		}
		static const char[] Remaining = str[Used.length .. $];
	}
	else
		static const bool Match = false;
}
struct UnittestParse_Sequence
{
	// Tests
	alias Parse_Sequence!("Hello world+this?is* good; by") Parse_Sequence_test1;
	static assert(Parse_Sequence_test1.Match);
	static assert(Parse_Sequence_test1.Used == "Hello world+this?is* good", Parse_Sequence_test1.Used);
	static assert(Parse_Sequence_test1.Remaining == "; by");
	static assert(Parse_Sequence_test1.Clauses[0].Text == "Hello");

	alias Parse_Sequence!("+this?is* good; by") Parse_Sequence_test2;
	static assert(!Parse_Sequence_test2.Match);
}



/************************************************
	Parse a Reduction Case (Case ::= ID "/" Sequence)
*/
struct Parse_Case(char[] str)
{
	static private alias Parse_ID!(str) act_1;
	static if(act_1.Match)
	{
		alias act_1 act;
		const bool Special = false;
		//pragma(msg,__LINE__.stringof~":"~str);
	}
	else
	{
		alias Parse_SpecialAct!(str) act;
		const bool Special = act.Match;
		//pragma(msg,__LINE__.stringof~":"~Special.stringof~":"~str);
	}

	static if(act.Match)
	{
		static const char[] n1 = DropWhiteF(act.Remaining);
		static if(n1.length > 1 && n1[0] == '/')
		{
			static private alias Parse_Sequence!(n1[1..$]) n2;

			static const bool Match = true;
			static const char[] Action = act.Text;
			static const char[] Seq = n2.Used;
			static alias n2.Clauses Clauses;
		}
		else
		{
			//pragma(msg,__LINE__.stringof~":"~n1);
			static const bool Match = false;
		}
	}
	else
	{
		//pragma(msg,__LINE__.stringof~":"~str);
		static const bool Match = false;
	}

}


struct UnittestParse_Case
{
	// Tests
	alias Parse_Case!("Act/foo?bar+baz*sig ") Parse_Case_test1;
	static assert(Parse_Case_test1.Match);
	static assert(Parse_Case_test1.Action == "Act");
	static assert(Parse_Case_test1.Seq == "foo?bar+baz*sig");
	static assert(Parse_Case_test1.Clauses[0].Text == "foo");

	alias Parse_Case!("Act / foo? bar+ baz* sig|") Parse_Case_test2;
	static assert(Parse_Case_test2.Match);
	static assert(Parse_Case_test2.Action == "Act");
	static assert(Parse_Case_test2.Seq == " foo? bar+ baz* sig");
	static assert(Parse_Case_test2.Clauses[0].Text == "foo");

	alias Parse_Case!("Act foo? / bar+ baz* sig|") Parse_Case_test3;
	static assert(!Parse_Case_test3.Match);
}



/************************************************
	Parse Set of Reduction cases (Cases ::= Case "|" Alt | Case)
*/
struct Parse_Cases(char[] str)
{
	const char[] t_p = FindChar!('|')(str);
	const char[] t_s = FindChar!(';')(str);
	static if(t_p[$-1] == '|')
	{
		//pragma(msg,__LINE__.stringof~":"~str);
		const char[] t = t_p[0..$-1];
	}
	else static if(t_s[$-1] == ';')
	{
		//pragma(msg,__LINE__.stringof~":"~str);
		const char[] t = t_s[0..$-1];
	}
	else
	{
		pragma(msg, "Debugging? '"~str~\');
		const char[] t = str;
	}
	

	private alias Parse_Case!(t) c1;
	static if(c1.Match)
	{
		static const bool Match = true;
		private static const char[] n1 = DropWhiteF(str[t.length..$]);
		static if(n1.length > 1 && n1[0] == '|')
		{
			private alias Parse_Cases!(n1[1..$]) c2;
			static if(c2.Match)
			{
				alias T!(c1, c2.Disjuncts) Disjuncts;
				static const char[] Remaining = c2.Remaining;
			}
			else
			{
				alias T!(c1) Disjuncts;
				static const char[] Remaining = str[t.length..$];
			}
		}
		else
		{
			static alias T!(c1) Disjuncts;
			static const char[] Remaining = str[t.length..$];
		}
		static const char[] Used = str[0 .. $-Remaining.length];
	}
	else
	{
		//pragma(msg,__LINE__.stringof~":"~str);
		static const bool Match = false;
	}
}


struct UnittestParse_Cases
{
	// Tests
	alias Parse_Cases!("Act/foo| For/Bar ") Parse_Cases_test1;
	static assert(Parse_Cases_test1.Match);
	static assert(Parse_Cases_test1.Remaining == "");
	static assert(Parse_Cases_test1.Used == "Act/foo| For/Bar ");
	static assert(Parse_Cases_test1.Disjuncts[0].Action == "Act");
	static assert(Parse_Cases_test1.Disjuncts[1].Action == "For");

	alias Parse_Cases!("Act/foo| For Bar ") Parse_Cases_test2;
	static assert(Parse_Cases_test2.Match);
	static assert(Parse_Cases_test2.Remaining == "| For Bar ");
	static assert(Parse_Cases_test2.Used == "Act/foo");
	static assert(Parse_Cases_test2.Disjuncts[0].Action == "Act");
}



/************************************************
	Parse a Rule (Rule ::= ID ":" Cases ";")
*/
struct Parse_Rule (char[] str)
{
		// parse off the rule name
	static private alias Parse_ID!(str) name;
	static if(name.Match)
	{
			// parse off to the ':'
		static private const char[] n1 = DropWhiteF(name.Remaining);
		static if(n1.length > 1 && n1[0] == ':')
		{
				// pares the cases
			static private alias Parse_Cases!(n1[1..$]) cases;
			static if(cases.Match)
			{
				static private const char[] n2 = DropWhiteF(cases.Remaining);
				static if(n2.length >= 1 && n2[0] == ';')
				{
					static const bool Match = true;
					static const char[] Name = name.Text;
					static const char[] Remaining = n2[1..$];
					static const char[] Used = str[0 .. $-Remaining.length];
					static alias cases.Disjuncts Disjuncts;
				}
				else
				{
					//pragma(msg,__LINE__.stringof~":"~n2~\n\n);
					static const bool Match = false;
				}
			}
			else
			{
				//pragma(msg,__LINE__.stringof~":"~str);
				static const bool Match = false;
			}
		}
		else
		{
			//pragma(msg,__LINE__.stringof~":"~str);
			static const bool Match = false;
		}
	}
	else
	{
		//pragma(msg,__LINE__.stringof~":"~str);
		static const bool Match = false;
	}
}
struct UnittestParse_Rule
{
	//tests
	alias Parse_Rule!("Foo:bar/baz | pig/owl*horse ;  ") Parse_Rule_test1;
	static assert(Parse_Rule_test1.Match);
	static assert(Parse_Rule_test1.Used == "Foo:bar/baz | pig/owl*horse ;");
	static assert(Parse_Rule_test1.Remaining == "  ");
	static assert(Parse_Rule_test1.Name == "Foo");
	static assert(Parse_Rule_test1.Disjuncts[0].Action == "bar");
	static assert(Parse_Rule_test1.Disjuncts[0].Clauses[0].Text == "baz");
	static assert(Parse_Rule_test1.Disjuncts[1].Action == "pig");
}

char[] strOf(int v)
{
	if(v==0) return "0";

	char[] ret;

	while(v)
	{
		ret = cast(char)('0' + (v%10)) ~ ret;
		v/=10;
	}
	return ret;
}

char[] GramParts(char[] str)
{
	char[] ret, match;

	int i;

	while(str.length > 0)
	{
		auto tmp = FindChar!(';')(str);
		if(ReduceWhite(tmp) != "")
		{
			ret ~= "Parse_Rule!(\""~ReduceWhite(tmp)~"\"), ";
			match ~= "Reductions["~strOf(i)~"].Match && ";
		}
		str = str[tmp.length..$];
	}

	return 
		"alias T!("~ret[0..$-2]~") Reductions;\n"
		"const bool Match = "~match[0..$-4]~";";
}


/************************************************
	Parse a Grammar (Grammar ::= Rule Grammar | Rule)
*/
template Parse_Grammar(char[] str)
{
	debug(dparse_verbose) pragma(msg,GramParts(str));
	mixin(GramParts(str));
}

struct UnittestParse_Grammar
{
	alias Parse_Grammar!("Foo:bar/baz | pig/owl*horse ;  ") Parse_Grammar_test1;

	static assert(Parse_Grammar_test1.Match);
	static assert(Parse_Grammar_test1.Reductions[0].Name == "Foo");
	static assert(Parse_Grammar_test1.Reductions[0].Disjuncts[0].Action == "bar");
	static assert(Parse_Grammar_test1.Reductions[0].Disjuncts[0].Clauses[0].Text == "baz");
	static assert(Parse_Grammar_test1.Reductions[0].Disjuncts[1].Action == "pig");
	static assert(Parse_Grammar_test1.Reductions[0].Disjuncts[1].Clauses[0].Text == "owl");
}

template MakeMixin(char[] starter, char[] str)
{
	const char[] MakeMixin = 
	MakeMixin_fn(str,starter);
	debug(dparse_verbose) pragma(msg,MakeMixin);
}

char[] MakeMixin_fn(char[] str,char[] starter)
{
	char[] ret;

	while(str.length > 0)
	{
		auto tmp = FindChar!(';')(str);
		if(ReduceWhite(tmp) != "")
		{
			ret ~= 
			("PObject Terminal(char[] name : \""~GetID(FindChar!(':')(tmp)[0..$-1])~"\")(IParser p)"
			"{"
				"return Rule!(typeof(this), Parse_Rule!(\""~ReduceWhite(tmp)~"\"))(this,p);"
			"}\n");
		}

		str = str[tmp.length..$];
	}

	return ret ~"alias Terminal!(\""~starter~"\") Parser;\n";
}



enum Places
{
	Back,
	Revert,
	Start
}

template CaseLable(int i)
{
	const int Back   = i*(Places.max+1) + Places.Back;
	const int Revert = i*(Places.max+1) + Places.Revert;
	const int Start  = i*(Places.max+1) + Places.Start;
//	debug(dparse_verbose) pragma(msg, ">> CaseLabel!("~itoa!(i)~") = { Back : "~itoa!(Back)~", Revert : "~itoa!(Revert)~", Start : "~itoa!(Start)~"}");
}

/****
PARSER	 : RULE PARSER | ;
RULE	 : ID.name ":" CASE ALT ";";
ALT	 : "|" CASE ALT | ;
CASE	 : ID.action "/" SEQUENCE;
SEQUENCE : ID SEQUENCE | ;
*/
debug(dParse_runtime) int counter = 0;

debug(dParse_runtime) int tager = 0;

private struct Frame{uint pos; uint rule; uint count; debug(dParse_runtime) int tag;}

PObject Rule(ParserBase,rule)(ParserBase parserBase, IParser p)
{
	debug(dParse_runtime)
	{
		int ind = counter++;
		writef("Try\t(%d)%s...\n", ind, rule.Name);
	}
	debug(dParse_runtime) scope(success) writef("Done\t(%d)%s\n", ind, rule.Name);
	debug(dParse_runtime) scope(failure) writef("FAILED\t(%d)%s\n", ind, rule.Name);

	static const char[] nameIs = rule.Name;
	//debug(dParse_light) 
	//pragma(msg, "<used>"~nameIs~"</used>");

	Stack!(Frame) backups;
	Frame store;

	debug(dparse_verbose) pragma(msg,"Build: \""~rule.Name~"\"" );
		// record start location
	uint start = p.pos;
		// try case

	caseLoop: foreach(ci,casev;rule.Disjuncts)
	{
		debug(dParse_runtime) writef("*\t(%d)%s:%s...\n", ind, rule.Name,casev.Action);

			// dump all checkpoints
		backups.Empty();

			// return to start location
		static if(ci != 0)
		{
			debug(dParse_runtime) writef("backing\t (%d):%d\n", ind ,start);
			p.pos = start;
		}

			// allocate storage for returns
		const int count = casev.Clauses.length;
		PObject[count] temps;
		static assert(count == temps.length);

		// debug(dparse_verbose) pragma(msg, "ICE from "~rule.name);
		debug(dparse_verbose) pragma(msg, "\tfor \""~rule.Name~"\" doing case #"~itoa!(ci)~" action = \""~casev.Action~"\", length = "~itoa!(count));

		static const int FirstCase = -1;

		int action = FirstCase;
		int back = 0;

		mixin("failBack_"~itoa!(ci)~":;");	// insert uniqe label here

		switch(action)
		{
			case FirstCase:

			foreach(index, cl; casev.Clauses)
			{
				debug(dParse_runtime) p.mark();

				static if(/*ci == 0 &&*/ index == 0 && nameIs == cl.Text)
					pragma(msg, "Directly recursive rule: "~nameIs)
					// static stuff

				debug(dParse_runtime) writef("Attemping clause %s (%d)...\n", cl.Text, index);

				debug(dparse_verbose) pragma(msg, "\t\tgenerating clause: \""~cl.Text~"\" ("~itoa!(index)~")");

				static if(cl.Type != ItemType.single)
				{
					// on [*+?] add a set
					temps[index] = new PObjectSet();
				}

				static if(cl.Type == ItemType.single || cl.Type == ItemType.plus)
				{
					// for cases where empty match is not allowed
						// get one
					debug(dparse_verbose) pragma(msg, "\t\t\t recurse in from \""~rule.Name~"\" on \""~cl.Text~\"\n);
						//alias DotAction!(nameIs, cl.Text) _;
					auto tmpStore1 = parserBase.Terminal!(cl.Text)(p);
					assert(tmpStore1 !is null);
					debug(dParse_runtime) writef("clause %s (%d) returned a %s fail=%s\n", cl.Text, index, tmpStore1.BaseName, tmpStore1.fail);
					debug(dparse_verbose) pragma(msg, "\n\t\t\t recurse out from \""~rule.Name~"\" on \""~cl.Text~\");

						// test it
					if(tmpStore1.fail())
					{
						if(backups.Count != 0) // checkpoints remain
						{
							store = backups.Pop();  // get checkpoint
							debug(dParse_runtime) writef(">>>>>Poped %d\n", store.tag);
							p.pos = store.pos;      // move input steram back
							action = store.rule;    // set point to return to
							back = store.count;     // set backput amount

							// goto uniqe label from here
							mixin("goto failBack_"~itoa!(ci)~";");
						}
						else
						{
						// try another
							continue caseLoop;
						}
					}

						// on good: store
					static if(cl.Type != ItemType.single)
					{
						(cast(PObjectSet)temps[index]).Add(tmpStore1);
					}
					else
					{
						temps[index] = tmpStore1;
					}
				}

				case CaseLable!(index).Back:;

				static if(cl.Type != ItemType.single)
				{
					debug(dParse_runtime)
					{
						writef("ReAttemping clause %s (%d)...\n", cl.Text, index);
						store.tag = tager++;
						writef("=======saved  %d @ ", store.tag);
						p.mark();
					}
					store.pos = p.pos;
					store.rule = CaseLable!(index).Revert;
					store.count = (cast(PObjectSet)temps[index]).Count();

					debug(dParse_runtime) writef("<<<<<< pushed  %d\n", store.tag);
					backups.Push(store);

					debug(dparse_verbose) pragma(msg, "\t\t\trecurse in from \""~rule.Name~"\" on \""~cl.Text~\"\n);
						//alias DotAction!(nameIs, cl.Text) __;
					auto tmpStore2 = parserBase.Terminal!(cl.Text)(p);
					assert(tmpStore2 ! is null);
					debug(dParse_runtime) writef("clause %s (%d) returned a %s fail=%s\n", cl.Text, index, tmpStore2.BaseName, tmpStore2.fail);
					debug(dparse_verbose) pragma(msg, "\n\t\t\trecurse out from \""~rule.Name~"\" on \""~cl.Text~\");

					if(!tmpStore2.fail())
					{
						(cast(PObjectSet)temps[index]).Add(tmpStore2);

						static if(cl.Type == ItemType.star || cl.Type == ItemType.plus)
						{
							goto case CaseLable!(index).Back;
						}
					}

						// label clause
					goto case CaseLable!(index).Start;
					case CaseLable!(index).Revert:;

					(cast(PObjectSet)temps[index]).Back(back);       // back out parsed data
					case CaseLable!(index).Start:;
				}
			}
		}
		debug(dparse_verbose) pragma(msg,"\tdoing Action \""~casev.Action~\");
		debug(dParse_runtime) writef("doing Action \""~casev.Action~\"\n);
		auto ret = SpecialAction!(ParserBase,casev.Action)(parserBase,temps);
		debug(dParse_runtime) writef("Action \""~casev.Action~"\" done\n");
		debug(dParse_runtime) writef("\treturn (%d)Act '%s' fail=%s\n", ind, casev.Action, ret.fail);
		return ret;
	}

	debug(dParse_runtime) writef("Return (%d)failed\n", ind);
	return new PObjectFail("Failed while looking for "~nameIs~\n);//	debug(dparse_verbose) pragma(msg, ">>"__FILE__":"~itoa!(__LINE__)~": is this right?");
	debug(dparse_verbose) pragma(msg, "Done: "~rule.Name);
}

template DotAction(char[] from, char[] to)
{
	pragma(msg, from ~ " -> " ~ to);
}

/************************************************
	A basic stack struct used internally to the parser implementation
*/
struct Stack(T)
{
	debug(TypeReport) pragma(msg, ">>T-"__FILE__~":"~itoa!(__LINE__)~": "~typeof(*this).stringof);
	private T[] data = null;
	private int at = 0;

		/**
			Push an item onto the stack
		*/
	void Push(T din)
	{
		if(data.length <= at) data.length = at + 10;
		data[at] = din;
		at++;
	}

		/**
			Pop an item off the stack

			Throw an "Error" exception on underflow
		*/
	T Pop()
	{
		if(at)
		{
			at--;
			return data[at];
		}
		else
			throw new Error("Stack Underflow");
	}

		/// return the number of items on the stack
	uint Count(){return at;}

		/// Clear the stack
	void Empty(){ at = 0; }

		/// Dump everything
	void Dist(){delete data;}

	unittest // unittest Stack!(T)
	{
		writef("unittest@"__FILE__":"~itoa!(__LINE__)~"!("~T.stringof~")\n");

		Stack!(int) st;
		int i;
		for(i = 0; i < 20; i++)
		{
			assert(st.Count == i);
			st.Push(i);
		}

		i--;

		for(; i >= 0; i--)
		{
			int j = st.Pop;
			assert(j == i);
			assert(st.Count == i);
		}

		assert(st.Count == 0);
	}
}

debug(dparse_unittest)
{
	// Unittest code
	unittest
	{
		writef("unittest@"__FILE__":"~itoa!(__LINE__)~\n);

		P p;
		data d = new data;
		char[][] pass =
			[
			"BB"[],         //root > next baz > (baz) baz)
			"HOB",          //root > next baz > (owl* horse owl) baz > (() horse owl) baz
			"OHOB",         //root > next baz > (owl* horse owl) baz > ((owl) horse owl) baz
			"OOHOB",        //root > next baz > (owl* horse owl) baz > ((owl owl) horse owl) baz
			"BHO",          //root > baz horse owl
			"CHB",          //root > cat horse+ owl* baz > cat (horse) () baz
			"CHOB",         //root > cat horse+ owl* baz > cat (horse) (owl) baz
			"CHHOB",        //root >  cat horse+ owl* baz > cat (horse horse) (owl) baz
			"CCC",          //root > cat* cat cat > (cat) cat cat
			"KQKQKQKQQKQK", // root > twin* car > (twin twin twin) (car) > ((king qween) (king qween) (king qween)) (king qween qween king)
			],
			fail =
			[
			"BHHO"[],
			"COB",
			"CB",
			"CHHOOB",
			];

		foreach(char[] passTest; pass)
		{
			d.dat = passTest;
			d.i=0;
			assert(!p.Parser(d).fail, \"~passTest~"\" failed to parse");
		}

		foreach(char[] failTest; fail)
		{
			d.dat = failTest;
			d.i=0;
			 assert(p.Parser(d).fail, \"~failTest~"\" failed to fail to parse");
		}
	}

	struct P
	{
		PObject Terminal(char[] str : "baz")(IParser i)   {data d = cast(data)i; if(d.i >= d.dat.length || d.dat[d.i] != 'B') return new PObjectFail; d.i++; return new PObjectBox!(char)('B');}
		PObject Terminal(char[] str : "owl")(IParser i)   {data d = cast(data)i; if(d.i >= d.dat.length || d.dat[d.i] != 'O') return new PObjectFail; d.i++; return new PObjectBox!(char)('O');}
		PObject Terminal(char[] str : "horse")(IParser i) {data d = cast(data)i; if(d.i >= d.dat.length || d.dat[d.i] != 'H') return new PObjectFail; d.i++; return new PObjectBox!(char)('H');}
		PObject Terminal(char[] str : "cat")(IParser i)   {data d = cast(data)i; if(d.i >= d.dat.length || d.dat[d.i] != 'C') return new PObjectFail; d.i++; return new PObjectBox!(char)('C');}
		PObject Terminal(char[] str : "king")(IParser i)  {data d = cast(data)i; if(d.i >= d.dat.length || d.dat[d.i] != 'K') return new PObjectFail; d.i++; return new PObjectBox!(char)('K');}
		PObject Terminal(char[] str : "qween")(IParser i) {data d = cast(data)i; if(d.i >= d.dat.length || d.dat[d.i] != 'Q') return new PObjectFail; d.i++; return new PObjectBox!(char)('Q');}

		PObject Action(char[] str : "bar") (PObject[1] i){return new PObjectVector!(1)(i);}
		PObject Action(char[] str : "beer")(PObject[2] i){return new PObjectVector!(2)(i);}
		PObject Action(char[] str : "pig") (PObject[3] i){return new PObjectVector!(3)(i);}
		PObject Action(char[] str : "keg") (PObject[4] i){return new PObjectVector!(4)(i);}

		const char[] gram = "
			next :
				bar / baz |
				pig / owl * horse owl ;
			root :
				beer / next baz |
				pig  / baz horse owl |
				keg  / cat horse+ owl? baz |
				pig  / cat* cat cat |
				beer / twin * car;
			twin :
				beer / king qween* ;
			car :
				keg / king qween qween king;
				";

		const char[] gram2 = import("dmp.g");

		PObject Terminal(char[] str)(IParser i){return null;}
		PObject Action(char[] str)(PObject[] i){return null;}

		mixin(MakeMixin!("root",ReduceWhite(gram)));
//		mixin(MakeMixin!("Module",ReduceWhite(gram2)));
	}

	class data : IParser
	{
		char[] dat;
		uint i =0;
		uint pos()		{return i;}
		void pos(uint j){ i = j;}
	}
	void main(){}
}
