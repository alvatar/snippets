module xf.xpose.test2;

private {
	import xf.xpose.Serialization;

	version (Tango) {
		import tango.io.Stdout;
		import tango.text.convert.Format : format = Format;
	} else {
		import std.stdio;
		import std.string : format;
	}
}



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
	
	mixin(expose!(SerializationExpose)(`a|b|c|c2|d|e|ie|f`));
	
	version (Tango) char[] toString() {
		return format(`Foo this={} a={} b={} c={} c2={} d={} e={} ie={}`, cast(void*)this, a, b, c, c2, d, cast(void*)e, cast(void*)ie);
	}
	else char[] toString() {
		return format(`Foo this=%0.8x a=%s b=%s c=%s c2=%s d=%s e=%0.8x ie=%0.8x f=%s`, cast(void*)this, a, b, c, c2, d, cast(void*)e,cast(void*)ie,  f);
	}
}


class Bar {
	double				a;
	char[]				b;
	char[][char[]]	c;
	
	mixin(expose!(SerializationExpose)(`a|b|c`));
	
	this() {}	
	this(double a, char[] b) {
		this.a = a;
		this.b = b;
	}
	
	version (Tango) char[] toString() {
		return format(`Bar this={} a={} b={} c={}`, cast(void*)this, a, b, c);
	} else char[] toString() {
		return format(`Bar this=%0.8x a=%s b=%s c=%s`, cast(void*)this, a, b, c);
	}
}


struct Baz {
	float		a;
	int		dupa;
	Baz*		c;
	char[]	d;
	
	version (Tango) char[] toString() {
		return format(`Baz a={} dupa={} c={} d={}`, a, dupa, c is null ? "(null)" : `(`~c.toString~`)`, d);
	}
	else char[] toString() {
		return format(`Baz a=%s dupa=%s c=%s d=%s`, a, dupa, c is null ? "(null)" : `(`~c.toString~`)`, d);
	}
}

struct BazExporter {
	static void rdupa(Baz* b, Unserializer s) {
		s(b.dupa);
	}
	static void wdupa(Baz* b, Serializer s) {
		s(b.dupa);
	}
	mixin(expose!(SerializationExpose)(`Baz`, `a no-serial|dupa read=rdupa;write=wdupa|c|d`));
}


class A {
	char[]	str;
}
class AExport {
	mixin(expose!(SerializationExpose)(`A`, `str`));	
}


class B : A {
	char[]	foo;
	
	version(Tango) char[] toString() {
		return format(`B, str='{}' foo='{}'`, str, foo);
	} else char[] toString() {
		return format(`B, str='%s' foo='%s'`, str, foo);
	}
}
class BExport : AExport {
	mixin(expose!(SerializationExpose)(`B`, `foo`));
}


class C(T) : A {
	T	t;

	version(Tango) char[] toString() {
		return format(`C, str='{}' t='{}'`, str, t);
	} else char[] toString() {
		return format(`C, str='%s' t='%s'`, str, t);
	}
}
class CExport(T) : AExport {
	mixin(expose!(SerializationExpose)(`C!(`~T.stringof~`)`, `t`));
}

alias CExport!(float) CExport_float;
alias CExport!(char[]) CExport_string;


class Spam {
	Spam spam;
	mixin(expose!(SerializationExpose)(`spam`));
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


void main() {
	{
		auto f = new Foo;
		f.c = f.c2 = `some test string`;
		f.a = 666;
		f.b = 3.14159;
		
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
		version(Tango) writefln(o2.toString);
		else writefln(o2);
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
