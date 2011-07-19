module utill;

template ArrayDepth(T)
{
	static if(is(T B == B[]))
		const int ArrayDepth = 1 + ArrayDepth!(B);
	else
		const int ArrayDepth = 0;
}

static assert(0 == ArrayDepth!(int));
static assert(1 == ArrayDepth!(int[]));
static assert(2 == ArrayDepth!(int[][]));



template NameFromType(T, char[] suffix)
{
	static if(is(T B == B[])) const char[] NameFromType = NameFromType!(B, "A" ~ suffix);
	else static if(is(T B == B*)) const char[] NameFromType = NameFromType!(B, "P" ~ suffix);
	else const char[] NameFromType = suffix == "" ? T.stringof : T.stringof ~ "-" ~ suffix;
}

template NameFromType(T)
{
	const char[] NameFromType = NameFromType!(T,"");
}

pragma(msg, NameFromType!(int));
pragma(msg, NameFromType!(int[]));
pragma(msg, NameFromType!(int[][]));
pragma(msg, NameFromType!(int*[]));

template Usefull(T)
{
	static if(is(T== struct))
		alias T BaseType;
	else static if(is(T U == U*)) 
	{
		static if(is(U == struct))
			alias U BaseType;
	}
	else static if(is(T == class))
		alias T BaseType;
}