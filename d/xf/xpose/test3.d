module xf.xpose.test3;

import xf.xpose.Expose;
import xf.xpose.LuaExpose;
import xf.xpose.IoExpose;
import xf.xpose.Serialization;
import xf.xpose.ReflectionExpose;


alias Combine!(IoExpose, LuaExpose, SerializationExpose, PrintoutExpose) StdExpose;


class Foo {
	void doFoo() {
		printf("Foo.doFoo called!"\n);
	}
	
	float doBar() {
		printf("Foo.doBar called!"\n);
		return 3.14159;
	}
	
	mixin(expose!(StdExpose)(`doFoo|doBar`));
}


class Bar {
	int	spam;
	float	ham;
	
	mixin(expose!(StdExpose)(`spam|ham`));
}


void main() {
	{
		dynDioInit(`./`);
		Dio dio = new Dio;
		registerDClassesWithIo(dio);

		dio.doString(`
			foo := Foo clone
			foo doFoo()
			foo doBar() println
			
			bar := Bar clone
			bar setSpam(20)
			bar setHam(foo doBar)
			bar getSpam println
			bar getHam println
		`);
	}
	
	printf(\n);
	
	{
		lua_State* L = luaL_newstate();
		luaL_openlibs(L);
		registerDClassesWithLua(L);

		if (0 != luaL_dostring(L, `
			foo = Foo.new()
			foo:doFoo()
			io.write(foo:doBar(), "\n")
			
			bar = Bar.new()
			bar:setSpam(20)
			bar:setHam(foo:doBar())
			io.write(bar:getSpam(), "\n")
			io.write(bar:getHam(), "\n")
		`))
		{
			if (auto str = lua_tostring (L, -1)) printf(`error: %s`\n, str);
		}
		
		lua_close(L);
	}

	printf(\n);
	
	{
		(new Serializer(`foobar.dat`))(new Foo).close;
		printf("%.*s"\n, (new Unserializer(`foobar.dat`)).get!(Foo).toString);
	}

	printf(\n);
	
	{
		(new Foo).initReflection();
		(new Bar).initReflection();
	}
}
