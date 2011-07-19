module xf.xpose.test1;

private {
	import xf.xpose.Expose;
	
	version (Serialization) import xf.xpose.Serialization;
	version (PrintoutReflection) import xf.xpose.ReflectionExpose;	
	
	version (Tango) {
		import tango.io.Stdout;
		import tango.stdc.string : strlen;
	} else {
		import std.stdio;
		import std.c.string : strlen;
	}
	
	version (IoReflection) {
		import xf.xpose.IoExpose;
		import dio.dio;
	}
	
	version (LuaReflection) import xf.xpose.LuaExpose;
}


struct StdExpose {
	private const int exposeHId0 = 0;

	version (PrintoutReflection) {
		struct handler(int i : exposeHId0) {		// reflection printout
			mixin PrintoutExpose_mix;
		}

		private const int exposeHId1 = exposeHId0 + 1;
	} else {
		private const int exposeHId1 = exposeHId0;
	}


	version (IoReflection) {
		struct handler(int i : exposeHId1) {		// Dio high-level bindings
			mixin IoExposeHighLevel_mix;
		}


		struct handler(int i : exposeHId1+1) {		// Dio low-level bindings
			mixin IoExposeLowLevel_mix;
		}
		private const int exposeHId2 = exposeHId1 + 2;
	} else {
		private const int exposeHId2 = exposeHId1;
	}
	
	
	version (Serialization) {
		struct handler(int i : exposeHId2) {
			mixin SerializationExpose_mix0;
		}

		struct handler(int i : exposeHId2+1) {
			mixin SerializationExpose_mix1;
		}
		
		private const int exposeHId3 = exposeHId2 + 2;
	} else {
		private const int exposeHId3 = exposeHId2;
	}
	
	
	version (LuaReflection) {
		struct handler(int i : exposeHId3) {
			mixin LuaExposeHighLevel_mix;
		}


		struct handler(int i : exposeHId3+1) {
			mixin LuaExposeLowLevel_mix;
		}
	}
}


// example ----------------------------------------------------------------------------------------------------------------------------------------------------



struct vec3 {
	float	x, y, z;

	mixin(expose!(StdExpose)(`
		x	name ecks
		y	name whyy
		z	name zombo
	`));
}


//pragma (msg, StdExpose.handler!(0).begin());


class Foo {
	int				foo;
	float				bar;
	char[]			baz;
	char[char[]]	spam;
	vec3				ham;
	
	this() {
		version (Tango) Stdout(`new Foo`).newline;
		else writefln(`new Foo`);
	}
	
	void func1(int a, int b, char[] c) {
		version (Tango) Stdout.formatln(`func1 called with {} {} {} and foo == {}`, a, b, c, foo);
		else writefln(`func1 called with %s %s %s and foo == %s`, a, b, c, foo);
	}
	
	int func2(float a) { return 2; }
	
	static void func3() {
		version (Tango) Stdout.formatln(`func3 called`);
		else writefln(`func3 called`);
	}
	
	void func4(int a) {
		version (Tango) Stdout.formatln(`z0mg teh {}`, a);
		else writefln(`z0mg teh `, a);
	}
	
	int prop() {
		version (Tango) Stdout.formatln(`** getter called! returning {}`, foo);
		else writefln(`** getter called! returning `, foo);
		return foo;
	}
	void prop(int a) {
		version (Tango) Stdout.formatln(`** setter called! setting`, a);
		else writefln(`** setter called! setting`, a);
		foo=a;
	}
	
	mixin(expose!(StdExpose)(`
		foo		range(10, 100];	no-net
		bar		range[0, 1];		no-serial
		baz		ascii; length[0, 8]; readOnly
		spam
		ham		normal; name tehHam
		
		func1
		func2
		func3
		func4	name tehFunc4
		
		prop		overload int();		name prop_get
		prop		overload void(int);	name prop_set
	`));
}


class Bar {
	int	val;
	int	val2;
	
	
	void func(int a) {
		version (Tango) Stdout.formatln(`Bar.func({})`, a);
		else writefln(`Bar.func(%s)`, a);
	}
	
	void func(char[] a) {
		version (Tango) Stdout.formatln(`Bar.func({})`, a);
		else writefln(`Bar.func(%s)`, a);
	}


	this() {
		val2 = 1234567;
		version (Tango) Stdout.formatln(`new Bar`);
		else writefln(`new Bar`);
	}
	
	void printStuff() {
		version (Tango) Stdout.formatln(`val is set to {}`, val);
		else writefln(`val is set to `, val);
	}
	
	mixin(expose!(StdExpose)(`
		val
		val2	readOnly
		func	overload void(int); name func_i
		func	overload void(char[]); name func_s
		printStuff
	`));
}



void main() {
	version (PrintoutReflection) {
		version (Tango) Stdout.formatln(`reflection info for Foo:`);
		else writefln(`reflection info for Foo:`);
		(new Foo).initReflection();

		version (Tango) Stdout.newline().formatln(`reflection info for vec3:`);
		else {
			writefln;
			writefln(`reflection info for vec3:`);
		}
		vec3.init.initReflection();
	}
	
	version (IoReflection) {
		version (Tango) Stdout.newline().formatln(`Io test -----------------------------------------------------------------------`).newline;
		else writefln(\n`Io test -----------------------------------------------------------------------`\n);
		dynDioInit(`./`);

		Dio dio = new Dio;
		registerDClassesWithIo(dio);

		dio.doString(`
			foo := Foo clone
			foo setFoo(66667)
			foo func1(4, 7, "FOOBAR")
			foo func2(0) println
			foo func3
			foo tehFunc4(31337)

			"" println

			bar := Bar clone
			bar setVal(1337)
			bar printStuff
			bar getVal2 println
			bar func_i(123)
			bar func_s("omg this is teh pwn")
		`);
	}


	version (LuaReflection) {
		version (Tango) Stdout.newline().formatln(`Lua test ----------------------------------------------------------------------`).newline;
		else writefln(\n`Lua test ----------------------------------------------------------------------`\n);

		lua_State* L = luaL_newstate();
		luaL_openlibs(L);
		registerDClassesWithLua(L);

		int res = luaL_dostring(L, `
			foo = Foo.new()
			foo:setFoo(66667)
			foo:func1(4, 7, "FOOBAR")
			io.write(foo:func2(0), "\n")
			foo:func3()
			foo:tehFunc4(31337)

			io.write("\n")

			bar = Bar.new()
			bar:setVal(1337)
			bar:printStuff()
			io.write(bar:getVal2(), "\n")
			bar:func_i(123)
			bar:func_s("omg this is teh pwn")
		`);
		if (res) {
			version (Tango) Stdout.formatln(`error code: `, res);
			else writefln(`error code: `, res);
			
			char* str = lua_tostring (L, -1);
			
			if (str) {
				version (Tango) Stdout.formatln(`error: `, str[0..strlen(str)]);
				else writefln(`error: %s`, str[0..strlen(str)]);
			}
		}
		lua_close(L);
	}


	version (Serialization) {
		version (Tango) Stdout.newline().formatln(`Serialization test ------------------------------------------------------------`).newline;
		else writefln(\n`Serialization test ------------------------------------------------------------`\n);
		(new Serializer(`foobar.dat`))(new Foo)(new Bar)(vec3.init).close;
		
		version (Tango) Stdout.newline().formatln(`unserializing...`);
		else writefln(\n`unserializing...`);
		
		scope auto u = new Unserializer(`foobar.dat`);
		u.get!(Foo);
		u.get!(Bar);
		u.get!(vec3);
	}
}
