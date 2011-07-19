//==========================================================================
// test_ext.d
//    Written in the D Programming Language (http://www.digitalmars.com/d)
/***************************************************************************
 * <TODO: module summary>
 *
 * <TODO: module description>
 *
 * Authors:  William V. Baxter III, OLM Digital, Inc.
 * Created: 01 Jul 2008
 * Copyright: (C) 2008  William Baxter, OLM Digital, Inc.
 */
//===========================================================================

module xf.xpose.test_ext;

private {
	import xf.xpose.Serialization;

	import tango.io.Stdout;
	import tango.text.convert.Sprint;

	Sprint!(char) format;
	static this() {
		format = new Sprint!(char);
	}
}

/** Class exposed intrusively */
class Class {
    float york;
    int b;
    char[] c;

    char[] toString() {
        return format("Class: york={} b={} c={}",york,b,c);
    }
    mixin(expose!(SerializationExpose)(`york|b|c`));
}


class Klass {
    float zork;
    int b;
    char[] c;

    char[] toString() {
        return format("Klass: zork={} b={} c={}",zork,b,c);
    }
}

struct Struct {
    float zork;
    int b;
    char[] c;

    char[] toString() {
        return format("Struct: zork={} b={} c={}",zork,b,c);
    }
}

class Base {
    float bork;
}

class BasedKlass : Base {
    float zork;
    int b;
    char[] c;

    char[] toString() {
        return format("BaseKlass: bork={} zork={} b={} c={}",bork,zork,b,c);
    }
}


class KlassXpose {
    mixin(expose!(SerializationExpose)(`Klass`, `zork|b|c`));
    
}
class StructXpose {
    mixin(expose!(SerializationExpose)(`Struct`, `zork|b|c`));
}

class BaseXpose {
    mixin(expose!(SerializationExpose)(`Base`, `bork`));
}

class BasedKlassXpose : BaseXpose {
    mixin(expose!(SerializationExpose)(`BasedKlass`, `zork|b|c`));
}


void main() {
    char[] stringified;

    //--- Simple test of basic intrusive exposure --------------
	{
        auto klass = new Class;
        klass.york = 1.1;
        klass.b = 42;
        klass.c = "hi";
        stringified = klass.toString;

		Stdout.formatln("Original     :  {}", klass);

		// serialize ---------------------------------
		(new Serializer(`klass.dat`))(klass).close;
	}

	{
		// unserialize ---------------------------------
		scope auto u = new Unserializer(`klass.dat`);
        auto klass = u.get!(Class);
		Stdout.formatln("Reconstructed:  {}", klass);
        assert(klass.toString() == stringified);
        u.close();
	}


    //--- Test of non-intrusive class exposure --------------
	{
        auto klass = new Klass;
        klass.zork = 1.1;
        klass.b = 42;
        klass.c = "hi";
        stringified = klass.toString;

		Stdout.formatln("Original     :  {}", klass);

		// serialize ---------------------------------
		(new Serializer(`klass.dat`))(klass).close;
	}
	
	{
		// unserialize ---------------------------------
		scope auto u = new Unserializer(`klass.dat`);
        auto klass = u.get!(Klass);
		Stdout.formatln("Reconstructed:  {}", klass);
        assert(klass.toString() == stringified);
        u.close();
	}


    //--- Test of non-intrusive struct exposure --------------
	{
        Struct strukt;
        strukt.zork = 1.1;
        strukt.b = 42;
        strukt.c = "hi";
        Struct strukt2 = strukt;
        strukt2.zork = -2.2;
        strukt2.b = 64;
        strukt2.c = "there";
        stringified = strukt.toString ~ strukt2.toString;

		Stdout.formatln("Original     :  {}", strukt);
		Stdout.formatln("             :  {}", strukt2);

		// serialize ---------------------------------
		(new Serializer(`strukt.dat`))(strukt)(strukt2).close;
        Stdout.formatln("serialized.");
	}
	
	{
		// unserialize ---------------------------------
        Stdout.formatln("unserializing...");
		scope auto u = new Unserializer(`strukt.dat`);
        auto strukt = u.get!(Struct);
        auto strukt2 = u.get!(Struct);
		Stdout.formatln("Reconstructed:  {}", strukt);
		Stdout.formatln("             :  {}", strukt2);
        assert(strukt.toString~strukt2.toString == stringified);
        u.close();
	}


    //--- Test of non-intrusive inherited class exposure --------------
    // This one doesn't work right now...
	{
        auto klass = new BasedKlass;
        klass.bork = 2.25;
        klass.zork = 1.1;
        klass.b = 42;
        klass.c = "hi";
        stringified = klass.toString;
        Base base = klass;

		Stdout.formatln("Original     :  {}", klass);

		// serialize ---------------------------------
		(new Serializer(`klass.dat`))(base).close;
	}
	
	{
		// unserialize ---------------------------------
		scope auto u = new Unserializer(`klass.dat`);
        auto obj = u.get!(BasedKlass)();
		Stdout.formatln("Reconstructed:  {}", obj);
        assert(obj.toString() == stringified);
        u.close();
	}


	Stdout.formatln("Terminating test");
}


//--- Emacs setup ---
// Local Variables:
// c-basic-offset: 4
// indent-tabs-mode: nil
// End:

