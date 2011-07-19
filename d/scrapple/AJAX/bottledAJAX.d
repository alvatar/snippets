import cgi.cgi;
import std.conv;

/**********************************
 handal calls to an object of type 
 T to any of methouds X...
**********************************/
void Process(T,X...)(CGI c, T f)
{
	if(c.Value("classname") != T.mangleof)
	{
		c.writef("ERROR=bad_class");
		return;
	}
	try
	{
		switch(c.Value("methname"))
		{
				// handal the use cases
			foreach(i,str; X)
				{
					case X[i]:
					{
						auto r = Call!(T, cast(char[])X[i]).It(f,c);
						c.writef("ret=%s\n", r);
						return;
					}
				}

				// handal Errors
			case "": c.writef("ERROR=no_method"); return;
			default: c.writef("ERROR=no_method_%s", c.Value("methname")); return;
		}
	}	catch (Object o) { c.writef("ERROR=%s", o.toString); }
}


/**********************************
 generate a JavaScript object 
 decleration named "'Base'_Proxy"
 to Proxy for Base
**********************************/
template ClientCode(Base, char[] target, fns...)
{
	debug	pragma(msg, "ClientCode("~Base.stringof~", \""~target~"\", "~fns.stringof~")");

	const char[] ClientCode = 
		"var "~Base.stringof~"_Proxy = {"~\n~
		methouds!(Base,target,fns).methouds~
		"	}";
}


/**********************************
 Generate the method definitions 
 for a proxy object.
**********************************/
template methouds(Base, char[] target, names...)
{
	static if(names.length > 0)
	{
		const char[] name = names[0];
		mixin("alias typeof(Base."~name~") FullName;");

		// extract the return type and args types of FullName
		static if( is(FullName R == return) && is(FullName argsT == function) )
		{
			const char[] methouds = 
				methoud!(Base, target, names[0], argsT.length) ~\n~
				methouds!(Base, target, names[1..$]).methouds;
		}
		else
			static assert(false,"woops, Can't find: "~Base.stringof~"."~names[0]~" from "~FullName.stringof);
	}
	else
	{
		alias void Null;
		const char[] methouds = "";
	}
}


/**********************************
 build the whole method
**********************************/
template methoud(T,char[] target, char[] name, uint count)
{
	const char[] methoud = 
		\t~ name ~": function("~buildArgs!(Names!(count))~")"
			" {return Http.waitfor({"
				"url:'" ~ target ~ "',"
				"method:Http.Method.Post,"
				"body:'classname="~T.mangleof~"&methname="~name~buildBody!(Names!(count))[0..$-2]~
			"}).responseText;},";
}


/**********************************
 build an argument list
**********************************/
template buildArgs(A...)
{
	static if(A.length > 1)       const char[] buildArgs = A[0] ~ "," ~ buildArgs!(A[1..$]);
	else static if(A.length == 1) const char[] buildArgs = A[0];
	else                          const char[] buildArgs = "";
}


/**********************************
 build a argument string
 construction expression
**********************************/
template buildBody(A...)
{
	static if(A.length > 0) const char[] buildBody = "&"~A[0]~"='+"~A[0]~"+'" ~ buildBody!(A[1..$]);
	else                    const char[] buildBody = "";
}


/**********************************
 handal a call to "name" by 
 extracting "args" from a HTTP
 request and converting them to 
 the type of the args of Base."name"
**********************************/
template Call(Base, char[] name)
{
	// get the type of Base."name"
	mixin("typedef typeof(&Base."~name~") A;");
	alias typeof(*A) FullName;


	// extract the return type and args types of FullName
	static if( is(FullName R == return) && is(FullName argsT == function) )
	{
		// the actual function
		R It(Base b, CGI cgi)
		{
			alias Names!(argsT.length) args;

				// make shure stuff matches (old)
			//static assert(args.length == argsT.length);

			//pragma(msg, "in "~name~" copy " ~ args.stringof ~ " to " ~ argsT.stringof);

				// the arrgs
			argsT ag;

				// decode
			foreach(int i, A; ag)
				ag[i] = Extract!(typeof(A))(cast(char[])args[i], cgi);

				// call and return
			return mixin("b."~name~"(ag)");
		}
	}
	else	/// what to do on error
		static assert(false,"Can't work with "~FullName.stringof~" from "~Base.stringof~"."~name);

		// translate a named parameter from cgi tp type T
	T Extract(T)(char[] name, CGI cgi)
	{
			 static if(is(T == char[])) {debug cgi.writef("char[]: [%s]: %s -> %s\n", name, cgi.Value(name),         cgi.Value(name));  return          cgi.Value(name);}
		else static if(is(T == int))    {debug cgi.writef("int   : [%s]: %s -> %s\n", name, cgi.Value(name),   toInt(cgi.Value(name))); return    toInt(cgi.Value(name));}
		else static if(is(T == uint))   {debug cgi.writef("uint  : [%s]: %s -> %s\n", name, cgi.Value(name),  toUint(cgi.Value(name))); return   toUint(cgi.Value(name));}
		else static if(is(T == long))   {debug cgi.writef("long  : [%s]: %s -> %s\n", name, cgi.Value(name),  toLong(cgi.Value(name))); return   toLong(cgi.Value(name));}
		else static if(is(T == ulong))  {debug cgi.writef("ulong : [%s]: %s -> %s\n", name, cgi.Value(name), toUlong(cgi.Value(name))); return  toUlong(cgi.Value(name));}
		else static if(is(T == short))  {debug cgi.writef("short : [%s]: %s -> %s\n", name, cgi.Value(name), toShort(cgi.Value(name))); return  toShort(cgi.Value(name));}
		else static if(is(T == ushort)) {debug cgi.writef("ushort: [%s]: %s -> %s\n", name, cgi.Value(name),toUshort(cgi.Value(name))); return toUshort(cgi.Value(name));}
		else static if(is(T == byte))   {debug cgi.writef("byte  : [%s]: %s -> %s\n", name, cgi.Value(name),  toByte(cgi.Value(name))); return   toByte(cgi.Value(name));}
		else static if(is(T == ubyte))  {debug cgi.writef("ubyte : [%s]: %s -> %s\n", name, cgi.Value(name), toUbyte(cgi.Value(name))); return  toUbyte(cgi.Value(name));}
		else static if(is(T == float))  {debug cgi.writef("float : [%s]: %s -> %s\n", name, cgi.Value(name), toFloat(cgi.Value(name))); return  toFloat(cgi.Value(name));}
		else static if(is(T == double)) {debug cgi.writef("double: [%s]: %s -> %s\n", name, cgi.Value(name),toDouble(cgi.Value(name))); return toDouble(cgi.Value(name));}
		else static if(is(T == real))   {debug cgi.writef("real  : [%s]: %s -> %s\n", name, cgi.Value(name),  toReal(cgi.Value(name))); return   toReal(cgi.Value(name));}
		else static assert(false, "Can't work with type "~T.stringof);
	}
}


/**********************************
 Generate a Tuple of names for use.
 Each name in the tuple is uniqie
 but the same names are returned
 each time
**********************************/
template Names(uint i)
{
	static if(i>0) alias T!(Names!(i-1), itoName!(i-1)) Names;
	else           alias T!() Names;
}


/**********************************
	based off Don Clugston's itoa.
	see http://trac.dsource.org/projects/ddl/browser/trunk/meta/conv.d
**********************************/
template CharDigit(int n) { const char[] CharDigit = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[n..n+1];}
template itoName(ulong n)
{
	static if (n < 26) const char[] itoName = CharDigit!(n);
	else               const char[] itoName = itoName!(n/10L) ~ CharDigit!(n%26L);
}


// helper Tuple template
template T(A...){alias A T;}

