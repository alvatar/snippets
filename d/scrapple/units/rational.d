module rational;

template Tpl(T...){alias T Tpl;}

///
uint gcd(int n, uint d)
{
	if(n < 0) n = -n;
	while(d)
	{
		int t = d;
		d = n%d;
		n = t;
	}
	return n;
}
static assert(7 == gcd(14,21));

///
template Reduce(int n, uint d)
{
	static assert(d != 0);
	static if(n != 0)
	{
		//pragma(msg,n.stringof~"/"~d.stringof);
		const int N =  n / cast(int)gcd(n,d);
		const int D = cast(uint)(d / gcd(n,d));
		const bool Reduced = (gcd(n,d) == 1);
	}
	else
	{
		const int N =  0;
		const int D = 1;
		const bool Reduced = (D == 1);
	}
	alias Tpl!(N,D) V;
}

static assert(!Reduce!(-14,21).Reduced);
static assert( Reduce!(-16,21).Reduced);
