/******
 * SEQUENCE
 *   : ID ( '?' | '*' | '+' )? SEQUENCE
 *   | ; 
 *
 * CASE
 *   : ID.action "/" SEQUENCE ;
 *
 * Options
 *   : CASE ( '|' CASE )?;
 *
 * RULE
 *   : ID.name ":" Options  ";" ;
 *
 * Grammar 
 *   : RULE Grammar
 *   | ;
 */
module dparse;

interface LexicalSource
{
	ulong at();
	void at(ulong);
}

interface IVector(T)
{
	T[] Get();
	uint Count();
	char[] toString();
}

/+private+/ interface ICount // should be private
{
	void Count(uint);
}
class Vector(T) : IVector!(T)
{
	private T[] data;
	static Vector!(T) opCall(V...)(V v)
	{
		auto ret = new Vector!(T);
		int c;
		foreach(int i,_;V)
		{
			static if(is(V[i] == T))
				c += 1;
			else static if(is(V[i] : T[]))
				c += v[i].length;
			else static if(is(V[i] : IVector!(T)))
				c += v[i].Count();
			else static assert(false, "Expect some kinf of "~ T.stringof ~" can't deal with "~V[i].stringof);
		}
		ret.data.length = c;
		c = 0;
		foreach(int i,_;V)
		{
			static if(is(V[i] == T))
			{
				ret.data[c] = v[i];
				c++;
			}
			else static if(is(V[i] == T[]))
			{
				ret.data[c..c+v[i].length] = v[i];
				c += v[i].length;
			}
			else static if(is(V[i] == IVector!(T)))
			{
				ret.data[c..c+v[i].Count()]  = v[i].Get();
				c += v[i].Count();
			}
			else static assert(false, "can't deal with "~V[i].stringof);
		}
		return ret;
	}
	T[] Get() { return data; }
	uint Count() { return data.length; }
	char[] toString()
	{
		return std.format.format("%s",data);
	}
}

debug(dparse_test_parser_CT) debug = dparse_CT_Dump;


/// scan for f and return everything befor it, return null if not found
char[] ScanFor  (char[] str, char f) { foreach(int i, char c; str) if(c==f) return str[0..i];   return ""; }
/// scan for f and return everything befor it, return the input if not found
char[] NotAfter (char[] str, char f) { foreach(int i, char c; str) if(c==f) return str[0..i];   return str; }
/// return everythin after the firt f, null if not found
char[] ScanAfter(char[] str, char f) { foreach(int i, char c; str) if(c==f) return str[i+1..$]; return ""; }
/// return the index of the first f, or zero if not found
 int   ScanForI (char[] str, char f) { foreach(int i, char c; str) if(c==f) return i;           return 0;  }
// return the input with leading or trailing whitespace removed.
char[] Trim     (char[] str)
{
	foreach(int first, char f; str) if(f!=' ' && f!='\t' && f!='\r' && f!='\n')
		foreach_reverse(int last, char l; str) if(l!=' ' && l!='\t' && l!='\r' && l!='\n')
			return str[first..last+1];
	return "";
}

char[] DropID(char[] str)
{
	int i;
	for(i = 0; i<str.length; i++)
	{
		char c = str[i];
		if('0' <= c && c <= '9') continue;
		if('_' == c) continue;
		c = c | ('a'^'A');
		if('a' <= c && c <= 'z') continue;
		break;
	}
	if(i==str.length) return "";
	assert(i != 0);

	again: switch(str[i++])
	{
		case ' ', '\t', '\n', '\r':
			if(i<str.length) goto again;
			else             return str[i..$];

		case '*', '?', '+':
			return str[i..$];

		default:
			return str[i-1..$];
	}
}
char[] ScanID(char[] str)
{
	int i;
	for(i = 0; i<str.length; i++)
	{
		char c = str[i];
		if('0' <= c && c <= '9') continue;
		if('_' == c) continue;
		c = c | ('a'^'A');
		if('a' <= c && c <= 'z') continue;
		break;
	}
	if(i==str.length) return str;
	if(i==0) return "";
	int j = i;
	again: switch(str[i++])
	{
		case ' ', '\t', '\n', '\r':
			if(i<str.length) goto again;
			else             return str[0..j];

		case '*', '?', '+':
			return str[0..j] ~ str[i-1];

		default:
			return str[0..j];
	}
}

static assert("HelloWorld" == ScanID("HelloWorld"));
static assert("HelloWorld" == ScanID("HelloWorld   "));
static assert("HelloWorld" == ScanID("HelloWorld   Bob"));
static assert("Hello*" == ScanID("Hello*World   "));
static assert("HelloWorld+" == ScanID("HelloWorld   +*"));
static assert("HelloWorld*" == ScanID("HelloWorld   *Bob"));
static assert("Hello*" == ScanID("Hello* World   "));
static assert("" == ScanID("*Hello* World   "));
static assert("" == ScanID("$(bob, dillon) "));

template Tuple(T...) { alias T Tuple; }

template TupleSeq(uint i)
{
	static if(i == 0)
		alias Tuple!() TupleSeq;
	else
		alias Tuple!(TupleSeq!(i-1),i-1) TupleSeq;
}

enum ClauseType { One, ZeroOrOne, ZeroOrMore, OneOrMore }

struct Parse_Clause(char[] name)
{
		 static if(name[$-1] == '?') static const ClauseType Type = ClauseType.ZeroOrOne;
	else static if(name[$-1] == '*') static const ClauseType Type = ClauseType.ZeroOrMore;
	else static if(name[$-1] == '+') static const ClauseType Type = ClauseType.OneOrMore;
	else  static const ClauseType Type = ClauseType.One;

	static if(Type == ClauseType.One)
	{
		static const char[] Name = name;
		debug(dparse_CT_Dump)static const char[] StringOf = name ~ '.';
	}
	else
	{
		static const char[] Name = name[0..$-1];
		debug(dparse_CT_Dump)static const char[] StringOf = name;
	}
}

struct Parse_Clauses(char[] file)
{
	static if(file.length == 0)
	{
		debug(dparse_CT_Dump)static const char[] StringOf = "";
		alias Tuple!() Clauses;
	}
	else
	{
		static assert(ScanID(file) != "", "Could not Parse a Clause out of \""~file~\");
		alias Parse_Clause!(ScanID(file)) firstClause;
		alias Parse_Clauses!(Trim(DropID(file))) otherClauses;
		alias Tuple!(firstClause, otherClauses.Clauses) Clauses;
		debug(dparse_CT_Dump)static const char[] StringOf = firstClause.StringOf ~ otherClauses.StringOf;
	}
}

template Parse_Case(char[] file)
{
	const bool Success = ScanForI(file,'/') != 0;

	static if(Success)
	{
		const char[] Action = Trim(ScanFor(file,'/'));
		alias Parse_Clauses!(Trim(ScanAfter(file,'/'))) Clauses;
		debug(dparse_CT_Dump)const char[] StringOf = Action ~ "("~Clauses.StringOf ~")";
	}
	else
	{
		debug(dparse_CT_Dump)const char[] StringOf = "";
	}
}

template Parse_Options(char[] file)
{
	static if(file == null)	const bool Success = false; else{
	static if(ScanForI(file,'|'))
	{
		alias Parse_Case!(NotAfter(file,'|')) firstCase;
		alias Parse_Options!(ScanAfter(file,'|')) otherCases;

		static if(otherCases.Success)
		{
			alias Tuple!(firstCase,otherCases.Cases) Cases;
			alias Tuple!(firstCase.Action,otherCases.ActionNames) ActionNames;
			debug(dparse_CT_Dump)const char[] StringOf = firstCase.StringOf~" | "~otherCases.StringOf;
		}
		else
		{
			static assert(Trim(ScanFor(file,'|')).length == 0 , "Found \""~Trim(ScanAfter(file,'|'))~"\" could not parse as Cases");
			alias Tuple!(firstCase) Cases;
			alias Tuple!(firstCase.Action) ActionNames;
			debug(dparse_CT_Dump)const char[] StringOf = firstCase.StringOf;
		}
		const bool Success = firstCase.Success;
	}
	else
	{
		alias Parse_Case!(file) firstCase;
		alias Tuple!(firstCase) Cases;
		alias Tuple!(firstCase.Action) ActionNames;
		debug(dparse_CT_Dump)const char[] StringOf = firstCase.StringOf;
		const bool Success = firstCase.Success;
	}}

	template hasALeftH(char[] name, T...)
	{
		static if(T.length == 0)							const bool hasALeftH = false;
		else static if(T[0].Clauses.Clauses.length == 0)	const bool hasALeftH = hasALeftH!(name,T[1..$]);
		else static if(T[0].Clauses.Clauses[0].Name != name)const bool hasALeftH = hasALeftH!(name,T[1..$]);
		else												const bool hasALeftH = true;
	}
	static if(Success) template hasALeft(char[] name) { const bool hasALeft = hasALeftH!(name,Cases); }

	debug(dparse_CT_Dump)static if(!is(typeof(StringOf))) const char[] StringOf = "x";
}

template Parse_Rule(char[] file)
{
	static if(file.length == 0 || ScanForI(file,':') == 0)
	{
		const bool Success = false;
		debug(dparse_CT_Dump)const char[] StringOf = null;
	}
	else
	{
		const char[] Name = Trim(ScanFor(file,':'));
		alias Parse_Options!(Trim(ScanAfter(file,':'))) Options;
		const bool Success = Options.Success;
		debug(dparse_CT_Dump)const char[] StringOf = Name ~ "{" ~ Options.StringOf~ "}";

		const bool IsLeftRec = Options.hasALeft!(Name);;
		alias Options.ActionNames ActionNames;
	}
}

template Parse_Grammar(char[] file)
{
	alias Parse_Rule!(Trim(ScanFor(file,';'))) firstRule;

	const bool Success = firstRule.Success;
	static assert(Success || Trim(file).length == 0, "Found \""~file[0..($>20?20:$)]~"\" could not parse as a rule");

	static if(Success)
	{
		alias Parse_Grammar!(Trim(ScanAfter(file,';'))) otherRules;
		static if(otherRules.Success)
		{
			debug(dparse_CT_Dump)const char[] StringOf = "[ " ~ firstRule.StringOf ~ ", " ~ otherRules.StringOf[1..$-2] ~ " ]";
			alias Tuple!(firstRule, otherRules.Rules) Rules;
		}
		else
		{
			debug(dparse_CT_Dump)const char[] StringOf = "[ " ~ firstRule.StringOf ~ " ]";
			alias firstRule Rules;
		}

		template select(char[] str)
		{
			static if(firstRule.Name == str)
				alias firstRule select;
			else static if(otherRules.Success)
				alias otherRules.select!(str) select;
			else
				static assert(false,"Grammar does not define "~str);
		}

		template has(char[] str)
		{
			static if(firstRule.Name == str)
				static const bool has = true;
			else static if(otherRules.Success)
				static const bool has = otherRules.has!(str);
			else
				static const bool has = false;
		}

		template nameset()
		{
			static if(otherRules.Success)
				alias Tuple!(firstRule.Name,  otherRules.nameset!()) nameset;
			else
				alias Tuple!(firstRule.Name) nameset;
		}
	}
	else
	{
		debug(dparse_CT_Dump)const char[] StringOf = "[]";
	}
}

debug(dparse_test_parser_CT)
{
	alias Parse_Grammar!("hello:$(yo) / bob?;world: wow /	man? of+ war| wow/guy*steel; ") parsed;
	pragma(msg, parsed.StringOf);
	pragma(msg, parsed.select!("world").StringOf);
}

/******************************************************************************
 * Extract the first Class type from the input tuple
 */
template ExtractClass(Types...)
{
	static if(Types.length > 0)
	{
		static if(is(Types[0] P == class)) alias P ExtractClass;
		else      alias ExtractClass!(Types[1..$]) ExtractClass;
	}
	else static assert(false);
}

/******************************************************************************
 * Return the base class of The given type
 */
template BaseClass(Type)
{
	static if(is(Type _ == class) && !is(_ == Object) && is(Type BaseT == super))
		alias ExtractClass!(BaseT) BaseClass;
	else static assert(false);
}

/******************************************************************************
 * Return the Closest Common Type of the 2 given types
 */
template CCT2(Type,U)
{
	//pragma(msg, \t~Type.stringof ~" / "~U.stringof);
	static if (is(Type : U))      alias U CCT2;
	else static if (is(U : Type)) alias Type CCT2;
	else static if(is(Type _ == class) && is(U __ == class))
		alias CCT2!(BaseClass!(Type),U) CCT2;
	else
		static assert(false,"Cant find common type for "~Type.stringof~" and "~U.stringof);
}

/******************************************************************************
 * return the closest common type of the Type tuple
 */
template CCT(Types...)
{
	//pragma(msg, ">>"~Types.stringof);
	static if(Types.length > 1)
		alias CCT!(CCT2!(Types[0..2]), Types[2..$]) CCT;
	else static if(Types.length == 1)
		alias Types[0] CCT;
	else static assert(false);
	//pragma(msg, "<<"~CCT.stringof);
}

private struct CCT_UnitTest
{
	interface I {}
	class A {}
	class B : A, I {}
	class C : A {}
	class E : B {}
	class F : B {}

	static assert(is(CCT!(A, B) == A));
	static assert(is(CCT!(B, C) == A));
	static assert(is(CCT!(C, E) == A));
	static assert(is(CCT!(E, F) == B));
}


template TypeOfProduction(alias SubItem,Base, char[] name)
{
	alias SubItem!(Base,name).Parse getter;
	static if(is(typeof(getter) R == return))
	{
		alias R T;
		//pragma(msg,name ~ " returns " ~ R.stringof);
	}
	else
		static assert(false, "failed to resolve type for "~name);
}

/// test what type an action returns
template ActionReturns(alias Action, char[] action)
{
	static if(is(typeof(Action!(action).Act) R == return))
	{
		alias R ActionReturns;
		debug(dparse_trace_CT) pragma(msg,action ~ " returns " ~ R.stringof);
	}
	else
	{
		alias typeof(Action!(action).Act) ActionReturns;
		debug(dparse_trace_CT) pragma(msg,action ~ " is a " ~ ActionReturns.stringof);
	}
	static assert(is(typeof((cast(ActionReturns)null) is null)), NotAfter(Action.stringof,'(')~"!(\""~action~"\").Act resulted in a "~ActionReturns.stringof~" and can't be converted to null");
}

/// derive the return type of a production with cases using the given Actions
template ProductionReturns(alias Action, Actions...)
{
	static assert(Actions.length > 0);
	static if(Actions.length == 1)	alias ActionReturns!(Action,Actions[0]) ProductionReturns;
	else							alias CCT2!(ActionReturns!(Action,Actions[0]), ProductionReturns!(Action,Actions[1..$])) ProductionReturns;
}

class VariableCount(T) : IVector!(T), ICount
{
	private T[] data;	// the data
	private uint at;	// one more than the size of data in data. Zero == fixed

	this()
	{
		data.length = 4;
		at = 1;
	}
		/// add another
	private void Add(T t)
	{
		if(!at) throw new Error("Can't add to fixed Vector");
		if(data.length <= at-1) data.length = at*2;
		data[at-1] = t;
		at++;
	}
		/// 
	void Fix()
	{
		if(at) data.length = at-1;
		at = 0;
	}
	T[] Get()
	{
		Fix();
		return data;
	}
	uint Count()
	{
		return at ? at-1 : data.length;
	}
	void Count(uint a)
	{
		assert(at==0 && a <= data.length); data.length = a;
	}
	char[] toString()
	{
		return std.format.format("%s",data[0..at?at-1:$]);
	}
}


struct Stack(T)
{
	T[] data;
	uint at = 0;

	void Push(T t)
	{
		if(data.length <= at) data.length = (at+8)*2;
		data[at++] = t;
	}
	T Peek() { return data[at]; }
	T Pop() { return data[--at]; }
	bool Empty(){return at==0; }
}
unittest
{
	Stack!(int) st;
	st.Push = 5;
	st.Push = 6;
	assert(st.Pop == 6);
	assert(st.Pop == 5);
}

struct Frame { int item; uint depth; ulong at; }

///
struct dparse(alias Action, alias SubItem, LexSource, char[] Grammar)
{
	/// Parse the grammar
	alias Parse_Grammar!(Grammar) GrammarAST;

	template ActionReturnType(Action)
	{
			static if(Action.Type == ClauseType.One)
			{
				alias TypeOfProduction!(SubItem,typeof(*this),Action.Name).T ActionReturnType;
				debug(dparse_trace_CT) pragma(msg, Action.Name~"= "~ActionReturnType.stringof);
			}
			else
			{
				alias IVector!(TypeOfProduction!(SubItem,typeof(*this),Action.Name).T) ActionReturnType;
				debug(dparse_trace_CT) pragma(msg, Action.Name ~ "?*+ "~ActionReturnType.stringof);
			}
	}

	template ActionReturnTypes(Action...)
	{
		static if(Action.length == 0) alias Tuple!() ActionReturnTypes;
		static if(Action.length == 1) alias Tuple!(ActionReturnType!(Action[0])) ActionReturnTypes;
		static if(Action.length >  1) alias Tuple!(ActionReturnType!(Action[0]), ActionReturnTypes!(Action[1..$])) ActionReturnTypes;
	}

	static TypeOfProduction!(SubItem,typeof(*this),name).T ParseOne       (char[] name)(ref Stack!(Frame) stack, int i,LexSource source)
	{
		debug(dparse_trace_CT) pragma(msg, "\t= "~name);
		return SubItem!(typeof(*this),name).Parse(source);
	}
	static VariableCount!(TypeOfProduction!(SubItem,typeof(*this),name).T) ParseZeroOrOne (char[] name)(ref Stack!(Frame) stack, int i,LexSource source)
	{
		debug(dparse_trace_CT) pragma(msg, "\t? "~name);
		Frame frame;
		frame.item = i;
		TypeOfProduction!(SubItem,typeof(*this),name).T tmp;
		VariableCount!(typeof(tmp)) ret = new VariableCount!(typeof(tmp));

		frame.at = source.at;
		frame.depth = ret.Count;
		tmp = SubItem!(typeof(*this),name).Parse(source);
		if(tmp !is null)
		{
			stack.Push(frame);
			ret.Add(tmp);
		}
		ret.Fix();
		return ret;
	}
	static VariableCount!(TypeOfProduction!(SubItem,typeof(*this),name).T) ParseZeroOrMore(char[] name)(ref Stack!(Frame) stack, int i,LexSource source)
	{
		debug(dparse_trace_CT) pragma(msg, "\t* "~name);
		Frame frame;
		frame.item = i;
		TypeOfProduction!(SubItem,typeof(*this),name).T tmp;
		auto ret = new VariableCount!(typeof(tmp));

		frame.at = source.at;
		frame.depth = ret.Count;
		while(null !is (tmp = SubItem!(typeof(*this),name).Parse(source)))
		{
			stack.Push(frame); // push befor each location

			ret.Add(tmp);
			frame.at = source.at;
			frame.depth = ret.Count;
		}

		ret.Fix();
		return ret;
	}
	static VariableCount!(TypeOfProduction!(SubItem,typeof(*this),name).T) ParseOneOrMore (char[] name)(ref Stack!(Frame) stack, int i,LexSource source)
	{
		debug(dparse_trace_CT) pragma(msg, "\t+ "~name);
		Frame frame;
		frame.item = i;
		TypeOfProduction!(SubItem,typeof(*this),name).T tmp;
		auto ret = new VariableCount!(typeof(tmp));

		while(null !is (tmp = SubItem!(typeof(*this),name).Parse(source)))
		{
			frame.at = source.at;	// push after each location
			frame.depth = ret.Count;
			stack.Push(frame);

			ret.Add(tmp);
		}

		if(tmp is null) return null;

		stack.Pop;  // the last one is at thes position so discard it.
		ret.Fix();
		return ret;
	}

	enum Places { Start = -1, Abort = -2 };

	static ActionReturns!(Action,ActionName) TryParseOption(char[] ActionName, alias Clauses)(LexSource source)
	{
		Stack!(Frame) stack;
		Frame stackFrame;

		stackFrame.item = Places.Abort;
		stack.Push(stackFrame);

		stackFrame.item = Places.Start;
		stackFrame.at = source.at;
		stack.Push(stackFrame);

		//alias ActionReturnTypes!(Clauses.Clauses) StorageTypes;// pragma(msg, "StorageTypes: "~StorageTypes.stringof);
		ActionReturnTypes!(Clauses.Clauses) clauseResults;
		static assert(clauseResults.length == Clauses.Clauses.length);

		again: stackFrame = stack.Pop;
		bool back = true;
		switch(stackFrame.item)
		{
			case Places.Abort:
				debug(dparse_trace_RT)writef("%s:%d %s <failed\n",__FILE__,__LINE__, Clauses.Clauses.stringof);
				return null;

			case Places.Start:

			back=false;
			foreach(int i, clause; Clauses.Clauses)
			{
				//pragma(msg, typeof(clauseResults[i]).stringof ~ " "~clause.Name);		// <- fails
				debug(dparse_trace_CT) pragma(msg, ActionReturnTypes!(Clauses.Clauses)[i].stringof ~ " "~clause.Name);

				static if(clause.Type == ClauseType.One)		if(null is (clauseResults[i] = ParseOne       !(clause.Name)(stack,i,source))) goto again;
				static if(clause.Type == ClauseType.ZeroOrOne)	if(null is (clauseResults[i] = ParseZeroOrOne !(clause.Name)(stack,i,source))) goto again;
				static if(clause.Type == ClauseType.ZeroOrMore)	if(null is (clauseResults[i] = ParseZeroOrMore!(clause.Name)(stack,i,source))) goto again;
				static if(clause.Type == ClauseType.OneOrMore)	if(null is (clauseResults[i] = ParseOneOrMore !(clause.Name)(stack,i,source))) goto again;

				case i:;
				if(back)
				{
					source.at=stackFrame.at;
					static if(clause.Type != ClauseType.One)
					{
						//(cast(VariableCount!(ActionReturnTypes!(Clauses.Clauses)[i].Base))clauseResults[i]).Count = stackFrame.depth;
						(cast(ICount)clauseResults[i]).Count = stackFrame.depth;
					}
				}
			}
		}
		debug(dparse_trace_RT)writef("%s:%d Action %s\n",__FILE__,__LINE__, ActionName);
		static if(is(typeof(Action!(ActionName).Act) == return))
			return Action!(ActionName).Act(clauseResults);
		else
			return Action!(ActionName).Act;
	}

	/// abstracts a function that parses the production named
	template TryParseProduction(char[] name)
	{
		alias GrammarAST.select!(name) Production;
		static if(Production.IsLeftRec)
		{
			static assert(false, name ~ " is directly Left Recursive (try again later)");
		}
		else
		{
			//pragma(msg, name ~ " is not directly Left Recursive");
			//pragma(msg, Production.ActionNames);
			alias ProductionReturns!(Action,Production.ActionNames) R;
			static R Go(LexSource source)
			{
				R ret;
				alias Production.Options.Cases Cases;
				static const int j = Cases.length;
				foreach(i; TupleSeq!(j))			//////// <--- HACK b/c foreach isn't working on the tuple
				{
					alias Tuple!(Cases[i].Clauses) clauses;
					debug(dparse_trace_CT) pragma(msg, name ~ " invloking " ~ clauses.stringof);
					ret = TryParseOption!(Cases[i].Action,Cases[i].Clauses)(source);
					if(null !is ret) break;
				}
				debug(dparse_trace_RT)writef("%s:%d %s %s\n",__FILE__,__LINE__, name, ret !is null);
				return ret;
			}
		}
	}

	static char[] GenMixin()
	{
		static const char[]
			Prefix = "alias TryParseProduction!(\"",
			Infix  = "\").Go\tParse_",
			Suffix = ";\n";

		static const int Overhead = Prefix.length + Infix.length + Suffix.length;
		
		alias GrammarAST.nameset!() set;

		static const int i = set.stringof[5..$-1].length*2 + (Overhead-3)*set.length;
		char[] ret;
		foreach(name;set)
		{
			ret ~= Prefix;
			ret ~= name;
			ret ~= Infix;
			ret ~= name;
			ret ~= Suffix;
		}
		return ret;
	}
	debug(dparse_Mixin) pragma(msg, GenMixin());
	mixin(GenMixin());
}


debug(dparse_test_parser_RT)
{
	const char[] gram = 
	"GRAMMAR"
		":EchoOneR/RULE*"
		";"

	"RULE"
		":MakeRule/ID Collon OPTIONS SemiCollon"
		";"

	"OPTIONS"
		":AcumulateSkip/CASE NEXT*"
		";"

	"NEXT"
		":EchoTwo/Pipe CASE"
		";"

	"CASE"
		":MakeCase/ID Slash IDP*"
		";"

	"IDP"
		":MakeID/ID SUFFIX?"
		";"

	"SUFFIX"
		":EchoOneC/QMark"
		"|EchoOneC/Star"
		"|EchoOneC/Plus"
		";"
	;

template Foo(char[] str)
{
		 static if(str[1] == '0')	{Ret function() Act; static this(){ Act =  function Ret(){return new Ret();}; } }
	else static if(str[1] == '1')	{Ret function(Ret) Act; static this(){ Act =  function Ret(Ret){return new Ret();}; } }
	else static if(str[1] == '2')	{Ret function(Ret,Ret) Act; static this(){ Act =  function Ret(Ret,Ret){return new Ret();}; } }
	else static if(str == "x3")		{Ret function(Ret,Ret,Ret) Act; static this(){ Act =  function Ret(Ret,Ret,Ret){return new Ret();}; } }
	else static if(str == "y3")		{Ret function(Ret,IVector!(Ret),Ret) Act; static this(){ Act =  function Ret(Ret,IVector!(Ret),Ret){return new Ret();}; } }
	else static if(str[1] == '4')	{Ret function(Ret,Ret,Ret,Ret) Act; static this(){ Act =  function Ret(Ret,Ret,Ret,Ret){return new Ret();}; } }
	else
	static assert(false);
}

class Rule
{
	char[] name; Case[] cases;
	this(char[]n,IVector!(Case)c) {name=n;cases=c.Get;}
	char[] toString(){return std.format.format("%s:%s;",name,cases);}
}
class ID
{
	char[] name; char type;
	this(char[]n,IVector!(char[])t){name=n; type=t.Count?t.Get[0][0]:'.';}
	char[] toString(){return name~type;}
}
class Case
{
	char[] act; ID[] clauses;
	this(char[]a,IVector!(ID)c){act=a;clauses=c.Get;}
	char[] toString(){return std.format.format("%s/%s", act,clauses);}
}
template DPaprseGrammarAction(char[] str)
{
	//pragma(msg, str);
	static if(false){}
	else static if(str == "AcumulateSkip")	IVector!(Case) Act(Case r,IVector!(Case) R)
		{debug(dparse_trace_RT)writef(\t~str~\n);return Vector!(Case)(r,R);}

	else static if(str == "MakeRule")		Rule Act(char[] n,char[],IVector!(Case) c,char[])
		{debug(dparse_trace_RT)writef(\t~str~\n);return new Rule(n,c);}
	else static if(str == "MakeID")			ID   Act(char[] n, IVector!(char[]) t)
		{debug(dparse_trace_RT)writef(\t~str~\n);return new ID(n,t);}
	else static if(str == "MakeCase")		Case Act(char[] a,char[],IVector!(ID) c)
		{debug(dparse_trace_RT)writef(\t~str~\n);return new Case(a,c);}

	else static if(str == "EchoTwo")		Case Act(char[],Case t)
		{debug(dparse_trace_RT)writef(\t~str~\n);return t;}
	else static if(str == "EchoOneC")		char[] Act(char[] t)
		{debug(dparse_trace_RT)writef(\t~str~\n);return t;}
	else static if(str == "EchoOneR")		IVector!(Rule) Act(IVector!(Rule) t)
		{debug(dparse_trace_RT)writef(\t~str~\n);return t;}

	else static assert(false, "No rule for "~str);
}

class DPaprseLexicalSource
{
	char[] data;
	ulong at;
	void X(){ while(at<data.length && data[at] == ' ')at++; }
	char It(){return at<data.length ? data[at] : '-'; }
	char[] Lex_Collon()     { X; debug(dparse_trace_RT)writef("Lex.Collon %s\n",It);     if(data.length > at && data[at] == ':') return data[at++..$][0..1]; debug(dparse_trace_RT)writef("failed\n"); return null; }
	char[] Lex_SemiCollon() { X; debug(dparse_trace_RT)writef("Lex.SemiCollon %s\n",It); if(data.length > at && data[at] == ';') return data[at++..$][0..1]; debug(dparse_trace_RT)writef("failed\n"); return null; }
	char[] Lex_Pipe()       { X; debug(dparse_trace_RT)writef("Lex.Pipe %s\n",It);       if(data.length > at && data[at] == '|') return data[at++..$][0..1]; debug(dparse_trace_RT)writef("failed\n"); return null; }
	char[] Lex_Slash()      { X; debug(dparse_trace_RT)writef("Lex.Slash %s\n",It);      if(data.length > at && data[at] == '/') return data[at++..$][0..1]; debug(dparse_trace_RT)writef("failed\n"); return null; }
	char[] Lex_QMark()      { X; debug(dparse_trace_RT)writef("Lex.QMark %s\n",It);      if(data.length > at && data[at] == '?') return data[at++..$][0..1]; debug(dparse_trace_RT)writef("failed\n"); return null; }
	char[] Lex_Star()       { X; debug(dparse_trace_RT)writef("Lex.Star %s\n",It);       if(data.length > at && data[at] == '*') return data[at++..$][0..1]; debug(dparse_trace_RT)writef("failed\n"); return null; }
	char[] Lex_Plus()       { X; debug(dparse_trace_RT)writef("Lex.Plus %s\n",It);       if(data.length > at && data[at] == '+') return data[at++..$][0..1]; debug(dparse_trace_RT)writef("failed\n"); return null; }
	char[] Lex_ID()
	{
		X;
		debug(dparse_trace_RT)writef("Lex.ID\n");
		foreach(int i, char c; data[at..$])
			switch(c)
			{
				default:
					if(i == 0) return null;
					auto ret = data[at..at+i];
					at += i;
					debug(dparse_trace_RT)writef("\"%s\"\n", ret);
					return ret;
				case '0','1','2','3','4','5','6','7','8','9','q','w','e','r',
					 't','y','u','i','o','p','l','k','j','h','g','f','d','s',
					 'a','z','x','c','v','b','n','m','Q','W','E','R','T','Y',
					 'U','I','O','P','L','K','J','H','G','F','D','S','A','Z',
					 'X','C','V','B','N','M','_':
					 continue;
			}
		debug(dparse_trace_RT)writef("%s\n", data.length != 0);
		return data;
	}
}

template DPaprseRuleCall(Base, char[] name)
{
	//pragma(msg,"Doing Name");
	static if(name == "") static assert(false);
	else static if(name == "ID")         char[] Parse(DPaprseLexicalSource source) {debug(dparse_trace_RT)writef("Sub:%s\n",name);return source.Lex_ID;}
	else static if(name == "Collon")     char[] Parse(DPaprseLexicalSource source) {debug(dparse_trace_RT)writef("Sub:%s\n",name);return source.Lex_Collon;}
	else static if(name == "SemiCollon") char[] Parse(DPaprseLexicalSource source) {debug(dparse_trace_RT)writef("Sub:%s\n",name);return source.Lex_SemiCollon;}
	else static if(name == "Pipe")       char[] Parse(DPaprseLexicalSource source) {debug(dparse_trace_RT)writef("Sub:%s\n",name);return source.Lex_Pipe;}
	else static if(name == "Slash")      char[] Parse(DPaprseLexicalSource source) {debug(dparse_trace_RT)writef("Sub:%s\n",name);return source.Lex_Slash;}
	else static if(name == "QMark")      char[] Parse(DPaprseLexicalSource source) {debug(dparse_trace_RT)writef("Sub:%s\n",name);return source.Lex_QMark;}
	else static if(name == "Star")       char[] Parse(DPaprseLexicalSource source) {debug(dparse_trace_RT)writef("Sub:%s\n",name);return source.Lex_Star;}
	else static if(name == "Plus")       char[] Parse(DPaprseLexicalSource source) {debug(dparse_trace_RT)writef("Sub:%s\n",name);return source.Lex_Plus;}
	else
	{
		debug(dparse_trace_RT)
		{
			typeof(mixin("Base.Parse_"~name))* Parse;
			static this()
			{
				Parse = function(DPaprseLexicalSource source)
				{
					writef("Call:%s\n",name);
					auto ret = mixin("Base.Parse_"~name~"(source)");
					writef("Call:%s <%s\n",name,ret !is null);
					return ret;
				};
			}
		}
		else
			mixin("alias Base.Parse_"~name~" Parse;");
	}
}

//	alias dparse!(Foo,Sub,LexicalSource,gram) parserA;
	alias dparse!(DPaprseGrammarAction,DPaprseRuleCall,DPaprseLexicalSource,gram) parserB;

	import std.stdio;
	void main()
	{
		auto source = new DPaprseLexicalSource;
		source.data = gram.dup;

		writef("%s\n",source.data);
		auto a = parserB.Parse_GRAMMAR(source);
		if(a !is null)
/+			foreach(int i, Rule r; a)
			{
				if(r is null)
					writef("%d is null\n", i);
				else
					writef("%s\n",r.toString());
			}+/
			writef("%s\n",a.toString());
		else
			writef("null\n");
	}
}