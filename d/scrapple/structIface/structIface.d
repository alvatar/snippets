module structIface;


// ---- evil implementation
char[] structImplementInterface(char[] strname, char[] iface, char[][] funcs) {
	char[] res = "private {";
	res ~= \n"alias typeof(*this) _iface_"~iface~"_ThisType;";

	foreach (func; funcs) {
		res ~= \n"static assert (is(typeof(&"~iface~".init."~func~")),
			`The interface "~iface~" doesn't declare a '"~func~"' function, `
			`thus struct `~_iface_"~iface~"_ThisType.stringof~` cannot implement it`);";
		res ~= \n"static void _iface_" ~ iface ~ "_func_" ~ func ~ "() {";
		res ~= \n\t"asm { naked; ";
		version (GNU) {
			res ~= "push EAX; mov EAX, dword ptr 8[ESP]; ";
		}
		res ~= "sub EAX, _iface_" ~ iface ~ "_vtbl.offsetof; mov EAX, [EAX]; ";
		version (GNU) {
			res ~= "mov dword ptr 8[ESP], EAX; pop EAX; ";
		}
		res ~= "jmp " ~ strname ~ "." ~ func ~ "; }";
		res ~= \n"}";
	}
	res ~= \n ~ iface ~ " as" ~ iface ~ "() {";
	res ~= \n\t"return cast("~iface~")cast(void*)&_iface_" ~ iface ~ "_vtbl;";
	res ~= \n"}";
   
	res ~= \n"void** _iface_" ~ iface ~ "_vtbl = cast(void**)([";
	res ~= \n\t"null";		// for classinfo
	foreach (func; funcs) {
		res ~= ",\n\tcast(void*)&_iface_" ~ iface ~ "_func_" ~ func;
	}
	res ~= \n"]).ptr;";
	res ~= "}";

	return res;
}
// ---- end of the evil implementation


char[][] splitFuncs(char[] str) {
	char[][] res;
	while (str.length > 0) {
		while (str.length > 0 && (' ' == str[0] || ',' == str[0])) {
			str = str[1..$];
		}
		int to = 0;
		for (; to < str.length && str[to] != ' ' && str[to] != ','; ++to) {}
		if (to > 0) {
			res ~= str[0..to];
			str = str[to..$];
		}
	}
	return res;
}


struct Impl_(Intf, Struct, char[] funcs) {
	Struct* thisptr;
	mixin(structImplementInterface(Struct.stringof, Intf.stringof, splitFuncs(funcs)));

	static Impl_ opCall(ref Struct s) {
		Impl_ res;
		res.thisptr = &s;
		return res;
	}

	Intf opCast() {
		mixin("return as"~Intf.stringof~"();");
	}
}


template Impl(Intf, char[] funcs) {
	Impl_!(Intf, Struct, funcs) Impl(Struct)(ref Struct s) {
		return Impl_!(Intf, Struct, funcs)(s);
	}
}



/+ // Sample


interface IFoo {
	void func1();
	void func2();
}

interface IBar {
	void func2();
	void func3(int, char[]);
}


extern (C) int printf(char*, ...);


struct Foo {
	float val1;
	char[] val2;

	void func1() {
		printf("Foo.func1 called ; val1 = %f, val2 = %.*s"\n, val1, val2);
	}

	void func2() {
		printf("Foo.func2 called ; val1 = %f, val2 = %.*s"\n, val1, val2);
	}

	void func3(int p1, char[] p2) {
		printf("Foo.func3 called ; val1 = %f, val2 = %.*s ; p1 = %d, p2 = %.*s"\n, val1, val2, p1, p2);
	}
}



// ---- Steven's example
interface I
{
	int x();
}

int foo(I i)
{
	return i.x;
}

struct S
{
	int x() { return 123456; }
}

void bar()
{
	S s;
	auto si = Impl!(I, "x")(s);
	int res = foo(cast(I)si);
	printf("foo(si) returned %d"\n, res);
}
// ----


void main() {
	printf("Entering main"\n);

	Foo f;
	f.val1 = 3.14159f;
	f.val2 = "Hello, world!";

	auto sfi = Impl!(IFoo, "func1, func2")(f);
	auto fi = cast(IFoo)sfi;

	fi.func1();
	fi.func2();

	auto sbi = Impl!(IBar, "func2, func3")(f);
	auto bi = cast(IBar)sbi;

	bi.func2();
	bi.func3(123, "some parameter");

	bar();

	printf("Leaving main"\n);
}

+/