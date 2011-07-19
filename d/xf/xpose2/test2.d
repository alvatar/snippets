module xf.xpose2.test2;

private {
	import xf.xpose2.Expose;
	import xf.xpose2.MiniD;
	import tango.text.convert.Format;
}


struct Dupaadfasdf {
	mixin (xpose2(""));
}


struct vec3 {
	float x, y, z;
	
	char[] toString() {
		return Format("x={} y={} z={}", x, y, z);
	}
	
	mixin(xpose2(`
		.*
		toString
		_ctor overload vec3 function(float, float, float)
	`));
	mixin xposeMiniDNoSubclass;
}


class Poop {
	int x = 0;
	
	this() {
		this.x = 123456;
	}
	
	this(int x) {
		this.x = x;
	}
	
	this (char[] zomg) {
		this.x = zomg.length;
	}
	
	int max(int a, int b) {
		return a > b ? a : b;
	}
	
	int dmax(int a, int b) {
		return max(max(a, b), x);
	}
	
	int dmax(int a, int b, int c) {
		return dmax(a, dmax(b, c));
	}
	
	vec3 getVec3(int x, int y, int z) {
		return vec3(x, y, z);
	}
}

class PoopWrap : Poop {
	mixin(xpose2(`Poop`,`
		dmax
		dmax overload int function(int, int, int) ; md { rename "dmax2" }
		getVec3
		.*
		_ctor
		_ctor overload Poop function(int)
		_ctor overload Poop function(char[])
	`));

	mixin xposeMiniD!(`Poop`);
}

/**
	TODO: use Jarrett's MinArgs for default values
*/


void main() {
	MDVM vm;
	auto t = openVM(&vm);
	loadStdlibs(t);
	
	xposeMiniD_initAll(t);
	
	superPush(t, new Poop(10));
	newGlobal(t, "poop");

	runString(t, `
		//global poop = Poop(5)
		
		local blah = vec3(1, 2, 30000)
		writeln("created a vec3: ", blah)
	
		writeln("Hello world")
		poop.x = 123
		writeln("poop.dmax2(1, 20, 150) == ", poop.dmax2(1, 20, 150))
		writeln("poop.dmax(1, 20) == ", poop.dmax(1, 20))
		
		writeln("getting a vec3...")
		local v3 = poop.getVec3(3, 6, 9)
		writeln("x = ", v3.x, " y = ", v3.y, " z = ", v3.z)
		
		class Zomg : Poop {
			/+this() {
				super(1, 2, 3)
			}+/
			
			function dmax(a : int, b : int) {
				return 1337
			}
		}
		
		poop = Zomg()
	`);
	
	lookup(t, "poop");
	auto poop = superGet!(Poop)(t, -1);
	
	Stdout.formatln("poop.dmax(1, 2) == {}", poop.dmax(1, 2));
}
