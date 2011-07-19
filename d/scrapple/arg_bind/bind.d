template T(t...){alias t T;}

template For(alias fn)   // the function to replace the arges with
{
			// make shure fn is a function and get it's args list
	static if(
		is(typeof(fn) arg == function) &&
		is(typeof(fn) R   == return)
		)
	{
		template Args(A...)       // the 0 based indices of the args to replace
		{
				// generate an args list without the given args removed
			alias Remove!(A).From!(arg) pArgs;

				// V is the value to replace the args with
			R Become(V...)(pArgs p)
			{
					// the new function

				static assert(A.length == V.length);
					// genate an index list of the args that remain
				static const uint vl = arg.length;
				alias Remove!(A).From!(range!(0,vl-1)) map;

				arg set;
					// set constant args
				foreach(uint i, uint v; A)
				{
					set[A[i]] = V[i];
				}

					// copy args from this function's args
				foreach(uint i, uint v; map)
				{
					set[map[i]] = p[i];
				}

					// call and return
				return fn(set);
			}
		}
	}
	else
		static assert(false);
}

template With(T)   // the function to replace the arges with
{
			// make shure fn is a function and get it's args list
	static if(
		is(T arg == function) &&
		is(T R   == return)
		)
	{
		template Args(A...)       // the 0 based indices of the args to replace
		{
				// generate an args list without the given args removed
			alias Select!(A).From!(arg) cArgs;
			alias Remove!(A).From!(arg) rArgs;

				// V is the value to replace the args with
			R delegate(rArgs) Become(T* fn, cArgs c)
			{
				struct Ret
				{
					cArgs ca;
					T* fn;

					R ret(rArgs v)
					{
						arg set;

							// set constant args
						foreach(uint i, uint _; A)
						{
							set[A[i]] = ca[i];
						}

							// copy args from this function's args

							// genate an index list of the args that remain
						static const uint vl = arg.length;
						alias Remove!(A).From!(range!(0,vl-1)) map;
						foreach(uint i, uint _; map)
						{
							set[map[i]] = v[i];
						}

							// call and return
						return fn(set);
					}
				}
					// the new function
				Ret* ret = new Ret;
					// set constant args
				foreach(uint i, uint _; A)
				{
					ret.ca[i] = c[i];
				}
				ret.fn = fn;
				return &ret.ret;
			}
		}
	}
	else
		static assert(false);
}

// return true if A[0] is in A[1..$], return false if A.length == 1 or otherwise
template isIn(A...)
{
	static assert(A.length != 0);

	static if(A.length == 1)
		const bool isIn = false;
	else
	{
		static if(A[0] == A[$-1])
			const bool isIn = true;
		else
			const bool isIn = isIn!(A[0], A[1..$-1]);
	}
}
static assert(!isIn!(1,2,3,4,5,6));
static assert(isIn!(2,2,3,4,5,6));
static assert(isIn!(4,2,3,4,5,6));
static assert(isIn!(6,2,3,4,5,6));
static assert(!isIn!(6));


template Remove(R...)      // 0 based indices of the Tuple parts to remove
{
	template From(A...)    // the tuple to remove stuff from
	{
		static if(A.length != 0)
		{
			static if(isIn!(A.length-1, R))
			{
				alias From!(A[0..$-1]) From;
			}
			else
			{
				alias T!(From!(A[0..$-1]), A[$-1]) From;
			}
		}
		else
		{
			alias T!() From;
		}
	}
}
alias Remove!(1,3,5).From!(ubyte,byte,ushort,short,uint,int) remove;
static assert(remove.length == 3, "lenght is: "~cast(char)('0' + remove.length));
static assert(is(remove[0] == ubyte));
static assert(is(remove[1] == ushort));
static assert(is(remove[2] == uint));

template Select(R...)      // 0 based indes the the Tuple parts to remove
{
	template From(A...)    // the tuple to remove stuff from
	{
		static if(A.length != 0)
		{
			static if(isIn!(A.length-1, R))
			{
				alias T!(From!(A[0..$-1]), A[$-1]) From;
			}
			else
			{
				alias From!(A[0..$-1]) From;
			}
		}
		else
		{
			alias T!() From;
		}
	}
}
alias Select!(1,3,5).From!(ubyte,byte,ushort,short,uint,int) select;
static assert(select.length == 3, "lenght is: "~cast(char)('0' + select.length));
static assert(is(select[0] == byte));
static assert(is(select[1] == short));
static assert(is(select[2] == int));

// return a tuple with ints from start to stop
template range(int start, int stop, A...)
{
	static if(start >= stop) 
		alias T!(A,start) range;
	else
		alias range!(start+1, stop, A, start) range;
}
alias range!(0,5) r;
static assert(r.length == 6);
static assert(r[0] == 0);
static assert(r[$-1] == 5);


//void main(){}

private const bool output = false;
static if(output) { import std.stdio; }
unittest
{
		// set up some locals
	const float fc = 3.1415;
	static int that_s = 2;
	static char has_s = '\0';
	static bool lots_s = false;
	static int[] of_s = [9,8,7,6,5];
	static float args_s = fc;

		// this function sets the locals from the args
	static int fnc(int that, char has, bool lots, int[] of, float args)
	{
		static if(output) writef("%s,\t%s,\t%s,\t%s,\t%s\n",that_s,has_s,lots_s,of_s,args_s);

		that_s = that;
		has_s = has;
		lots_s = lots;
		of_s = of;
		args_s = args;

		static if(output) writef("%s,\t%s,\t%s,\t%s,\t%s\n",that_s,has_s,lots_s,of_s,args_s);
		return 0;
	}

		// bind some args
	alias For!(fnc).
	      Args!  (0,2,   4).
	      Become!(1,true,1e7) fn2;

		// test that stuff is what is expected
	assert(that_s == 2,         "error");
	assert(has_s == '\0',       "error");
	assert(lots_s == false,     "error");
	assert(of_s == [9,8,7,6,5], "error");
	assert(args_s == fc,    "error");

		// try the function
	fn2('c', [1,2,3]);

		// test that stuff is changed
	assert(that_s == 1,       "error");
	assert(has_s  == 'c',     "error");
	assert(lots_s == true,    "error");
	assert(of_s[] == [1,2,3], "error");
	assert(args_s == 1e7,     "error");

	that_s = 2;
	has_s = '\0';
	lots_s = false;
	of_s = [9,8,7,6,5];
	args_s = 3.1415;

	auto rt = With!(typeof(fnc)).
	      Args!      (0,2,   4).
	      Become(&fnc,1,true,1e7);

		// test that stuff is what is expected
	assert(that_s == 2,         "error");
	assert(has_s == '\0',       "error");
	assert(lots_s == false,     "error");
	assert(of_s == [9,8,7,6,5], "error");
	assert(args_s == fc,    "error");

		// try the function
	rt('c', [1,2,3]);

		// test that stuff is changed
	assert(that_s == 1,       "error");
	assert(has_s  == 'c',     "error");
	assert(lots_s == true,    "error");
	assert(of_s[] == [1,2,3], "error");
	assert(args_s == 1e7,     "error");


}