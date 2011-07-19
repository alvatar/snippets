module xf.xpose.teste;

private {
	import xf.xpose.Serialization;

	import tango.io.Stdout;
	import tango.text.convert.Sprint;

	Sprint!(char) format;
	static this() {
		format = new Sprint!(char);
	}
}


class Base {
}

class Derived : Base {
    float a;
    int b;
    char[] c;

    //pragma(msg, expose!(SerializationExpose)(`a|b|c`));
    mixin(expose!(SerializationExpose)(`a|b|c`));

    char[] toString() {
        return format("Derived: a={} b={} c={}",a,b,c);
    }
}

interface IBase {
}

class DerivedI : IBase {
    float a;
    int b;
    char[] c;

    //pragma(msg, expose!(SerializationExpose)(`a|b|c`));
    mixin(expose!(SerializationExpose)(`a|b|c`));

    char[] toString() {
        return format("DerivedI: a={} b={} c={}",a,b,c);
    }
}


//pragma(msg, expose_ext!(IShape,SerializationExpose)(`IShape.area`));
//mixin(expose_ext!(IShape,SerializationExpose)(`IShape.area`));




void main() {
	{
        auto der = new Derived;
        der.a = 1.1;
        der.b = 42;
        der.c = "hi";
        Base base = der;
		
		// serialize -----------------------------------------------------------------------
		(new Serializer(`der.dat`))(base).close;
	}
	
	{
		// unserialize ---------------------------------------------------------------------
		scope auto u = new Unserializer(`der.dat`);
        auto der = u.get!(Base);
		Stdout.formatln("{}", der);
        assert(der.toString() == "Derived: a=1.10 b=42 c=hi");
	}
	

	{
        auto der = new DerivedI;
        der.a = 1.1;
        der.b = 42;
        der.c = "hi";
        IBase base = der;
		
		// serialize -----------------------------------------------------------------------
		(new Serializer(`ider.dat`))(base).close;
	}
	
	{
		// unserialize ---------------------------------------------------------------------
		scope auto u = new Unserializer(`ider.dat`);
        auto der = u.get!(IBase);
		Stdout.formatln("{}", (cast(DerivedI)der).toString);
        assert((cast(DerivedI)der).toString() == "DerivedI: a=1.10 b=42 c=hi");
	}
	
    Stdout.formatln("Derived.TypeOfSuper == {}", Derived.TypeOfSuper.stringof);

	Stdout.formatln("Terminating test");
}
