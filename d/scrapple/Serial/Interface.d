module Interface;

interface Sink(T)
{
	static assert(is(T == char) || is(T == byte));
	void Dump(T);
	void Dump(T[]);
}

interface Source(T)
{
	static assert(is(T == char) || is(T == byte));
	bool Peek(T t);
	bool Peek(T[] ts);
	T Peek();
	bool Pick(ref T t, bool must = false);
	bool Pick(T[] ts, bool must = true);
}

