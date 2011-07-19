template Tpl(Tp...){alias Tp Tpl;}

template ArrayOf(T...)
{
	static assert(T.length > 0);
	static assert(is(T[0]));

	static if (T.length == 1) alias T[0][] ArrayOf;
	else alias Tpl!(T[0][], ArrayOf!(T[1..$])) ArrayOf;
}

char[] AfterLast(char[] s, char c)
{
	foreach_reverse(int i, char d; s) if(c==d) return s[i+1..$];
	return s;
}

template BuildArrayAccess(U,uint j )
{
	static if(U.tupleof.length <= j) const char[] BuildArrayAccess = "";
	else
		const char[] BuildArrayAccess = 
			typeof(U.tupleof)[j].stringof ~ "[] " ~ AfterLast(U.tupleof[j].stringof,'.') ~ "(){return members["~j.stringof~"];}\n" ~ BuildArrayAccess!(U,j+1);
}

template BuildItemAccess(U,uint j )
{
	static if(U.tupleof.length <= j) const char[] BuildItemAccess = "";
	else
		const char[] BuildItemAccess = 
			typeof(U.tupleof)[j].stringof ~ " " ~ AfterLast(U.tupleof[j].stringof,'.') ~ "(){return __base.members["~j.stringof~"][__i];}\n" ~
			typeof(U.tupleof)[j].stringof ~ " " ~ AfterLast(U.tupleof[j].stringof,'.') ~ "("~typeof(U.tupleof)[j].stringof~" v){return __base.members["~j.stringof~"][__i] = v;}\n" ~
			BuildItemAccess!(U,j+1);
}

struct Columns(T)
{

	static assert(is(T == struct));
	//pragma(msg, typeof(T.tupleof).stringof);
	//pragma(msg, ArrayOf!(typeof(T.tupleof)).stringof);

	private ArrayOf!(typeof(T.tupleof)) members;


	struct At
	{
		Columns!(T)* __base;
		int __i;

		T FullItem()
		{
			T ret;
			foreach(i,_;typeof(T.tupleof)) ret.tupleof[i] = __base.members[i][__i];
			return ret;
		}

		//pragma(msg,">"~BuildItemAccess!(T,0)~"<");
		mixin(BuildItemAccess!(T,0));
	}

	//pragma(msg,">"~BuildArrayAccess!(T,0)~"<");
	mixin(BuildArrayAccess!(T,0));
	At opIndex(uint i) { return At(this,i); }

	T opIndexAssign(T inp,int i)
	{
		foreach(j,_;typeof(T.tupleof)) members[j][i] = inp.tupleof[j];
		return inp;
	}


	int length(){ return members[0].length; }
	void length(int L) { foreach(i,_;members) members[i].length = L; }
}




import std.stdio;
unittest
{
	struct Foo
	{
		float k;
		int i;
		uint j;
		void print(){writef("%s, %s, %s\n", k, i, j);}
	}

	Foo foo;
	Columns!(Foo) FooCol;


	FooCol.length = 10;
	for(int i = 0; i < 10; i++)
	{
		foo.k = 3.14 * i;
		foo.i = -3 - i;
		foo.j = 5 + i;
		FooCol[i] = foo;
	}
	foreach(float f; FooCol.k) writef("%s ", f); writef(\n);
	foreach(int   i; FooCol.i) writef("%s ", i); writef(\n);
	foreach(uint  u; FooCol.j) writef("%s ", u); writef(\n);
	FooCol[3].FullItem.print();

}