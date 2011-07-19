module xf.utils.Tricks;


/**
	---
	class Foo {
		int myIter_impl(int delegate(inout int, inout float) dg) {
			int a; float b;
			a = 0; b = 53245.f;	dg(a, b);
			a = 1; b = 345.3f;		dg(a, b);
			return 0;
		}
		mixin opHackedApply!(myIter_impl) myIter_mix;
		alias myIter_mix.apply myIter;
	}
	
	
	import std.string : split;
	
	struct Bar {
		int myIter_impl(char[] arg, int delegate(inout char[]) dg) {
			foreach (char[] a; split(arg)) {
				dg(a);
			}
			return 0;
		}
		mixin opHackedApply!(myIter_impl) myIter_mix;
		alias myIter_mix.apply myIter;
	}
	
	
	import std.stdio : writefln;
	
	void main() {
		Foo f = new Foo;
		foreach (int a, float b; f.myIter) {
			writefln("a: %s   b: %s", a, b);
		}
		
		Bar b;
		foreach (char[] str; b.myIter("foo bar baz")) {
			writefln(str);
		}
	}
	---
*/

template opHackedApply(alias impl) {
	import tango.core.Traits : ParameterTupleOf;
	
	alias typeof(&impl)							implType;
	alias ParameterTupleOf!(implType)	implMeta;
	
	static if (implMeta.length == 1) {
		alias implMeta[0]				dgType;
		alias int delegate(dgType)	implDgType;
		
		struct Iter {
			implDgType implDg;
	
			int opApply(dgType dg) {
				return implDg(dg);
			}
		}
		
		Iter apply() {
			Iter it;
			it.implDg = &impl;
			return it;
		}
	} else static if (implMeta.length == 2) {
		alias implMeta[0]	argType;
		alias implMeta[1]	dgType;
		alias int delegate(argType, dgType)	implDgType;
		
		struct Iter {
			implDgType	implDg;
			argType		arg;
	
			int opApply(dgType dg) {
				return implDg(arg, dg);
			}
		}
		
		Iter apply(argType arg) {
			Iter it;
			it.implDg = &impl;
			it.arg = arg;
			return it;
		}
	} else {
		static assert (false);		// TODO
	}
}
