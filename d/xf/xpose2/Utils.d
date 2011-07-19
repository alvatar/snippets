module xf.xpose2.Utils;



version (Tango) {
	/**
		Templates from std.traits, by Walter Bright.
		Reformatted to fit here better.
	*/
	template ReturnType(alias dg) {
		alias ReturnType!(typeof(dg)) ReturnType;
	}
 
	template ReturnType(dg) {
		static if (is(dg R == return)) {
			alias R ReturnType;
		} else {
			static assert(0, "argument has no return type");
		}
	}
 
	template ParameterTypeTuple(alias dg) {
		alias ParameterTypeTuple!(typeof(dg)) ParameterTypeTuple;
	}
 
	template ParameterTypeTuple(dg) {
		static if (is(dg P == function)) {
			alias P ParameterTypeTuple;
		} else static if (is(dg P == delegate)) {
			alias ParameterTypeTuple!(P) ParameterTypeTuple;
		} else static if (is(dg P == P*)) {
			alias ParameterTypeTuple!(P) ParameterTypeTuple;
		} else {
			static assert(0, "argument has no parameters");
		}
	}


	// stuff stolen from Phobos
	enum
	{
		_SPC =	8,
		_CTL =	0x20,
		_BLK =	0x40,
		_HEX =	0x80,
		_UC  =	1,
		_LC  =	2,
		_PNC =	0x10,
		_DIG =	4,
		_ALP =	_UC|_LC,
	}

	ubyte _ctype[128] =
	[
		_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,
		_CTL,_CTL|_SPC,_CTL|_SPC,_CTL|_SPC,_CTL|_SPC,_CTL|_SPC,_CTL,_CTL,
		_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,
		_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,_CTL,
		_SPC|_BLK,_PNC,_PNC,_PNC,_PNC,_PNC,_PNC,_PNC,
		_PNC,_PNC,_PNC,_PNC,_PNC,_PNC,_PNC,_PNC,
		_DIG|_HEX,_DIG|_HEX,_DIG|_HEX,_DIG|_HEX,_DIG|_HEX,
		_DIG|_HEX,_DIG|_HEX,_DIG|_HEX,_DIG|_HEX,_DIG|_HEX,
		_PNC,_PNC,_PNC,_PNC,_PNC,_PNC,
		_PNC,_UC|_HEX,_UC|_HEX,_UC|_HEX,_UC|_HEX,_UC|_HEX,_UC|_HEX,_UC,
		_UC,_UC,_UC,_UC,_UC,_UC,_UC,_UC,
		_UC,_UC,_UC,_UC,_UC,_UC,_UC,_UC,
		_UC,_UC,_UC,_PNC,_PNC,_PNC,_PNC,_PNC,
		_PNC,_LC|_HEX,_LC|_HEX,_LC|_HEX,_LC|_HEX,_LC|_HEX,_LC|_HEX,_LC,
		_LC,_LC,_LC,_LC,_LC,_LC,_LC,_LC,
		_LC,_LC,_LC,_LC,_LC,_LC,_LC,_LC,
		_LC,_LC,_LC,_PNC,_PNC,_PNC,_PNC,_CTL
	];

	int isalpha(dchar c)  { return (c <= 0x7F) ? _ctype[c] & (_ALP)      : 0; }
	int isdigit(dchar c)  { return (c <= 0x7F) ? _ctype[c] & (_DIG)      : 0; }

	int rfind(char[] str, char[] foo) {
		assert (foo.length == 1);
		foreach_reverse(i, c; str) {
			if (c == foo[0]) return i;
		}
		return -1;
	}

	template ToString(int i) {
		static if (i < 10) const char[] ToString = "" ~ cast(char)(i + '0');
		else const char[] ToString = ToString!(i / 10) ~ ToString!(i % 10);
	}

	template ToString(bool B) {
		const char[] ToString = B ? "true" : "false";
	}
} else {
	public import std.traits : ReturnType, ParameterTypeTuple;
	public import std.ctype : isalpha, isdigit;
	public import std.string : rfind;
	public import std.metastrings : ToString;
}



char[] shortName(char[] classname) {
	int dot = rfind(classname, `.`);
	return -1 == dot ? classname : classname[dot+1..$];
}


char[] shortNameOf(T)() {
	return shortName(typeid(T).toString);
}


char[] capitalize(char[] name) {
	assert (name.length > 0);
	
	if (name[0] >= 'a' && name[0] <= 'z') {
		return cast(char)(name[0] + 'A' - 'a') ~ name[1..$];
	}
	else return name;
}


// ---- this stuff was inside HandlerStructMix before
template firstUnusedHandlerIndex(A, int i = 0) {
	static if (is(typeof(A.handler!(i)))) {
		const int firstUnusedHandlerIndex = firstUnusedHandlerIndex!(A, i+1);
	} else {
		const int firstUnusedHandlerIndex = i;
	}
}


static private char[] intToString__(uint i) {
	if (i < 10) return ""~"0123456789"[i];
	return intToString__(i/10) ~ ("0123456789"[i%10]);
}
static private char[] generateExposeHandlers(int n, int offset, char[] context) {
	char[] res;
	for (int i = 0; i < n; ++i) {
		res ~= `template handler(int i : `~intToString__(i+offset)~`) { alias `~context~`.handler!(`~intToString__(i)~`) handler; }`;
	}
	return res;
}


struct CombinedHandlerType(A, B) {
	const int firstUnusedInA = firstUnusedHandlerIndex!(A);
	const int firstUnusedInB = firstUnusedHandlerIndex!(B);
	mixin(generateExposeHandlers(firstUnusedInA, 0, `A`));
	mixin(generateExposeHandlers(firstUnusedInB, firstUnusedInA, `B`));
}
// ----


template HandlerStructMix() {
	static CombinedHandlerType!(typeof(*this), RHS) opAnd(RHS)(RHS rhs) {
		return CombinedHandlerType!(typeof(*this), RHS)();
	}
}


template Combine(T ...) {
	static if (T.length > 1) {
		alias typeof(T[0]() & Combine!(T[1..$])()) Combine;
	} else {
		alias T[0] Combine;
	}
}


// Thanks, downs!
char[] ctReplace(char[] source, char[][] pairs) {
	assert((pairs.length%2) == 0, "Parameters to ctReplace must come in pairs. ");
	if (!pairs.length) return source;
	else {
		char[] what = pairs[0], wth = pairs[1];
		int i = 0;
		while (i <= source.length - what.length) {
			if (source[i .. i+what.length] == what) {
				source = source[0 .. i] ~ wth ~ source[i+what.length .. $];
				i += wth.length;
			} else {
				i++;
			}
		}
		return ctReplace(source, pairs[2 .. $]);;
	}
}



char[] intToStringCT(int i) {
	char[] res = "";
	do
	{
		res ~= "0123456789"[i%10];
		i /= 10;
	} while (i > 0);
	
	for (int j = 0; j < res.length/2; ++j) {
		char c = res[j];
		res[j] = res[res.length-j-1];
		res[res.length-j-1] = c;
	}
	return res;
}

char[] rangeCodegen(int i) {
	char[] res = `alias RangeTuple!(`;
	if (i > 0) {
		res ~= "0";
		for (int j = 1; j < i; ++j) {
			res ~= "," ~ intToStringCT(j);
		}
	}
	return res ~ ") Range;";
}

template RangeTuple(T ...) {
	alias T RangeTuple;
}

template Range(int i) {
	mixin(rangeCodegen(i));
}



// ---- struct/class field name iteration ----

char[][] allNamesInAlias(alias target)() {
	char[][] names = [];
	const int len = target.tupleof.length;
	int prefix = target.stringof.length + 3;		// "(Type)."
	foreach (i; Range!(len)) {
		names ~= target.tupleof[i].stringof[prefix..$];
	}
	return names;
}


private bool matchesClassCT(char c, char[] cls) {
	if (char.init == c) return false;
	assert (cls.length > 0);

	if ("." == cls) return true;
	if (1 == cls.length && c == cls[0]) return true;

	if ('[' == cls[0]) {
		assert (']' == cls[$-1]);
		cls = cls[1..$-1];
		bool res = true;
		int from = 0;
		if ('^' == cls[0]) {
			res = false;
			from = 1;
		}
		for (int i = from; i < cls.length; ++i) {
			if ('\\' == cls[i]) {
				if (cls[i+1] == c) return res;
				else ++i;
			} else if (i+1 < cls.length && '-' == cls[i+1]) {
				assert (i+2 < cls.length);
				if (c >= cls[i] && c <= cls[i+2]) return res;
				else i += 2;
			} else if (cls[i] == c) return res;
		}
		return !res;
	}
	assert (1 == cls.length);
	return cls[0] == c;
}

private int cutBRExprCT(char[] str, out char[] cls) {
	for (int i = 0; i < str.length; ++i) {
		if ('\\' == str[i]) ++i;
		else if (']' == str[i]) {
			cls = str[0..i+1];
			return i;
		}
	}
	assert (false);
	return int.max;
}


/**
	A very greedy pattern matching function.
	
	.				matches any character
	[abc]		matches a, b and c
	[a-z]			matches a through z
	[abcA-Z]	matches a, b, c, and A through Z, etc.
	[^stuff]		matches the inverse of [stuff]
	?				matches the preceding element zero or one time
	*				matches the preceding element zero or more times
	
	It's not a regex engine, it's much less powerful in order to be lightweight.
	The greediness means that "f.*r" will not match "foobar", but "f.*" will.
*/
private bool matchesPatternCT(char[] str, char[] pattern) {
	char[]	cls;
	bool		prevMatched = true;
	bool		fail = true;
	int		stri = -1;
	
	strIter: while (pattern.length > 0) {
		switch (pattern[0]) {
			case '*': {
				if (!prevMatched) --stri;
				while (prevMatched && matchesClassCT(stri+1 < str.length ? str[stri+1] : char.init, cls)) {
					++stri;
				}
				pattern = pattern[1..$];
				prevMatched = true;
			} break;
			
			case '?': {
				if (!prevMatched) --stri;
				pattern = pattern[1..$];
				prevMatched = true;
			} break;
			
			default: {
				// see if the previous class matched, return false if it didn't
				if (!prevMatched) {
					return false;
				} else {
					++stri;
				}
				
				// find the class
				if ('[' == pattern[0]) {
					pattern = pattern[cutBRExprCT(pattern, cls)+1..$];
				} else {
					cls = pattern[0..1];
					pattern = pattern[1..$];
				}
				prevMatched = matchesClassCT(stri < str.length ? str[stri] : char.init, cls);
			}
		}
	}
	
	return prevMatched && stri+1 >= str.length;
}


/**
	Matches the str to a pattern expression formed by patterns supported by matchesPatternCT
	and operators '+' and '-'
*/
private bool matchesComplexPatternCT(char[] str, char[] pattern) {
	bool res = false;
	char prevFunc = '+';
	int i = 0;
	for (; i < pattern.length; ++i) {
		if ('-' == pattern[i] || '+' == pattern[i]) {
			bool match = matchesPatternCT(str, pattern[0..i]);
			if ('+' == prevFunc) {
				res |= match;
			} else {
				res &= !match;
			}
			prevFunc = pattern[i];
			pattern = pattern[i+1..$];
			i = -1;
		}
	}

	if (i > 0) {
		bool match = matchesPatternCT(str, pattern[0..i]);
		if ('+' == prevFunc) {
			res |= match;
		} else {
			res &= !match;
		}
	}
	
	return res;
}


char[][] matchedNamesCT(alias target)(char[] pattern) {
	char[][] res;
	foreach (name; allNamesInAlias!(target)) {
		if (matchesComplexPatternCT(name, pattern)) {
			res ~= name;
		}
	}
	return res;
}
