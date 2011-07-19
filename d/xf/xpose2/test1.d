module xf.xpose2.test1;

private {
	import xf.xpose2.Expose;
	import xf.xpose2.Serialization;

	import tango.io.Stdout;
	import tango.text.convert.Format : format = Format;
}



char[][] xposeAttribParser_attr0(char[] foo) {
	return [
		"val1", foo,
		"val2", foo
	];
}


struct Foobar {
	char[] NAME;
	
	int func(float, real) {
		return 0;
	}
	
	int func(float) {
		return 0;
	}
	
	float aFloat;
	
	mixin(xpose2(`
		NAME	attr0 666 ; attr1 12 ; serial { skip }
		func		attr1 3.14159
		func		overload int function(float)
		aFloat	net { compress ; server ; foo { zomg 12345 } }
	`));
}




// -------------------

interface IFoo {
}

class Foo : IFoo {
	int		a;
	float		b;
	char[]	c;
	char[]	c2;
	double	d;
	Foo		e;
	IFoo[]	ie;
	Bar[]	f;
	int[10]	stat;
	float		meh;
	int		blah;
	
	mixin(xpose2(`.*-meh-blah`));
	mixin xposeSerialization;
	
	char[] toString() {
		return format(`Foo this={} a={} b={} c={} c2={} d={} e={} ie={}; stat={}`, cast(void*)this, a, b, c, c2, d, cast(void*)e, cast(void*)ie, stat);
	}
}


class Bar {
	double				a;
	char[]				b;
	char[][char[]]	c;
	
	mixin(xpose2(`a|b serial { rename "dupa" }|c`));
	mixin xposeSerialization;
	
	this() {}	
	this(double a, char[] b) {
		this.a = a;
		this.b = b;
	}
	
	char[] toString() {
		return format(`Bar this={} a={} b={} c={}`, cast(void*)this, a, b, c);
	}
}


struct Baz {
	float		a;
	int		dupa;
	Baz*		c;
	char[]	d;
	
	char[] toString() {
		return format(`Baz a={} dupa={} c={} d={}`, a, dupa, c is null ? "(null)" : `(`~c.toString~`)`, d);
	}
}

struct BazExporter {
	static void rdupa(Baz* b, Unserializer s) {
		Stdout.formatln("BazExporter.rdupa called");
		s(b.dupa);
	}
	static void wdupa(Baz* b, Serializer s) {
		Stdout.formatln("BazExporter.wdupa called");
		s(b.dupa);
	}
	mixin(xpose2(`Baz`, `a serial { skip }|dupa serial { read "rdupa"; write "wdupa" } |c|d`));
	mixin xposeSerialization!("Baz");
}


class A {
	char[]	str;
}
class AExport {
	mixin(xpose2(`A`, `.*`));
	mixin xposeSerialization!("A");
}


class B : A {
	char[]	foo;
	
	char[] toString() {
		return format(`B, str='{}' foo='{}'`, str, foo);
	}
}
class BExport : AExport {
	mixin(xpose2(`B`, `.*`));
	mixin xposeSerialization!("B");
}


class C(T) : A {
	T	t;

	char[] toString() {
		return format(`C, str='{}' t='{}'`, str, t);
	}
}
class CExport(T) : AExport {
	const char[] Ctypename = `C!(`~T.stringof~`)`;
	mixin(xpose2(Ctypename, `.*`));
	mixin xposeSerialization!(Ctypename);
}

alias CExport!(float) CExport_float;
alias CExport!(char[]) CExport_string;


class Spam {
	Spam spam;
	mixin(xpose2(`spam`));
	mixin xposeSerialization!("Spam");
}




version (Tango) {
	void writefln(T)(T o) {
		static if (is(T == class)) {
			if (o is null) Stdout("(null)").newline;
			else Stdout(o).newline;
		} else {
			Stdout(o).newline;
		}
	}
}


struct OMG {
	struct Heh {
		int foo;
		int fooooo;
		int bar;
		int fooA;
		int barA;
		int fxx;
		int fxx134;
	}
}


void main() {
	{
		dumpXposeData!(Foobar);
		dumpXposeData!(Foo);
		dumpXposeData!(Bar);
		dumpXposeData!(BazExporter);
		dumpXposeData!(BExport);
		dumpXposeData!(CExport!(float));
		dumpXposeData!(CExport!(char[]));
		
		pragma (msg, "--------------------------------------------------------------------------------");

		foreach (field; Foobar.xposeFields) {
			pragma (msg, field.name ~ " {");
			static if (field.isFunction) {
				pragma (msg, "num params: " ~ field.paramTypes.length.stringof);
			} else {
				pragma (msg, "type: " ~ field.type.stringof);
			}
			static if (is(field.attribs.attr1)) {
				pragma (msg, "attr1 value: " ~ field.attribs.attr1.value.stringof);
			}
			foreach (attr; field.attribs.tupleof) {
				pragma (msg, "attrib: " ~ attr.name);
			}
			static if (is(field.attribs.overload)) {
				pragma (msg, "overload: " ~ field.attribs.overload.typeName);
			}
			pragma (msg, "}");
		}

		pragma (msg, "--------------------------------------------------------------------------------");
	}

	{
		auto f = new Foo;
		f.c = f.c2 = `some test string`;
		f.a = 666;
		f.b = 3.14159;
		f.stat[] = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512];
		
		auto f2 = new Foo;
		f.e = f2;
		f.ie ~= f2;
		f2.e = f;
		
		f2.c = f.c;
		f2.a = 0x1337;
		
		f2.f ~= new Bar(1.2345, `blah`);
		f2.f ~= new Bar(2.3456, `z0mg!`);
		with ((f2.f ~= new Bar(3.4567, `all your base are belong to us`))[$-1]) {
			c[`1`] = `a`;
			c[`2`] = `b`;
		}
		
		auto bar = f2.f[$-1];
		
		Baz baz;
		baz.a = 2.87654f;
		baz.dupa = 8888;
		
		Baz baz2;
		baz.c = &baz2;
		baz.d = baz2.d = `a string in a struct`;
		
		A objA = new B;
		objA.str = `string in base class, A`;
		(cast(B)objA).foo = `string in derived class, B`;
		
		auto tmpl1 = new C!(float);
		auto tmpl2 = new C!(char[]);
		tmpl1.t = 123.456f;
		tmpl2.t = `class template member`;
		tmpl1.str = tmpl2.str = `class template base member`;
		
		// serialize ----------------------------------------------------------------------------------------------------------------------------------------------
		(new Serializer(`foobar.dat`))(f)(bar)(baz)(objA)(tmpl1)(tmpl2)(cast(Spam)null)(new Spam).close;
	}
	
	{
		// unserialize -------------------------------------------------------------------------------------------------------------------------------------------
		scope auto u = new Unserializer(`foobar.dat`);
		auto o = u.get!(Foo);
		auto bar = u.get!(Bar);
		auto o2 = u.get!(Baz);
		auto objA = u.get!(A);
		auto tmpl1 = u.get!(A);
		auto tmpl2 = u.get!(A);
		auto spam = u.get!(Spam);
		auto spam2 = u.get!(Spam);
		
		writefln(o);
		writefln(o.ie[0]);
		writefln(bar);
		writefln(o.e);
		writefln(o2.toString);
		writefln(objA);
		writefln(tmpl1);
		writefln(tmpl2);
		writefln(spam);
		writefln(spam2);
	}
	
	{
		auto f = new Foo;
		f.c = f.c2 = `some test string`;
		f.a = 666;
		f.b = 3.14159;
		
		auto f2 = new Foo;
		f.e = f2;
		f2.e = f;
		
		f2.c = f.c;
		f2.a = 0x1337;
		
		for (int i = 0; i < 1_000; ++i) {
			with ((f2.f ~= new Bar(3.4567, `all your base are belong to us`))[$-1]) {
				c[`1`] = `a`;
				c[`2`] = `b`;
			}
		}

		auto serial = new Serializer(`large.dat`);
		serial(f);
		serial.close;
	}
	
	{
		scope auto u = new Unserializer(`large.dat`);
		auto o = u.get!(Foo);
		writefln(o.e.f.length);
	}
	
	writefln("Terminating test");
}
