/**
	Struct and Class access unification
	
	Examples:
	---
		struct SomeStruct {
			int a;
			float b;
			char[] c;
		}
		
		
		class SomeClass {
			char[] foo;
			int bar;
			char[] baz;
		
			this (int x) {
				writefln("ctor called with ", x);
			}
		
			this () {
				writefln("ctor called with no args");
			}
		}
		
		
		void main() {
			auto s = create!(SomeStruct, `a=5; b=3.14159; c="dupa jasia"`)();
		
			writefln("s.a = ", s.a);
			writefln("s.b = ", s.b);
			writefln("s.c = ", s.c);
			
			
			auto c = create!(SomeClass, `baz = "yo!"; foo = "hello"; bar = 6667`)(10);
			
			writefln("c.foo = ", c.foo);
			writefln("c.bar = ", c.bar);
			writefln("c.baz = ", c.baz);
		
			create!(SomeClass)();
		}
	---
*/
module xf.utils.StructClass;



template create(Type, char[] namedInit = "") {
Type create(Params ...)(Params params) {
	static if (is(Type : Object)) {
		Type create_res_ = new Type(params);
	} else {
		static if (Params.length > 0) {
			Type create_res_ = Type(params);
		} else {
			static if (is(typeof(Params.opCall()))) {
				Type create_res_ = Type();
			} else {
				Type create_res_;
			}
		}
	}

	with (create_res_) {
		mixin(namedInit ~ `;`);
	}

	return create_res_;
}}


template reference(Type) {
	static if (is(Type : Object)) {
		alias Type reference;
	} else {
		alias Type* reference;
	}
}








/**
	Mixin for a simple ctor - one that takes params as in the struct's tupleof
	Warning: due to a bug in dmd 0.175 .. 1.010, let this mixin be after all fields in the struct
*/
template simpleStructCtor() {
	private alias typeof(*this) ThisType;
	private alias typeof(ThisType.init.tupleof) StructFieldTypes;

	static ThisType opCall(StructFieldTypes t) {
		ThisType res;
		foreach (i, x; t) {
			res.tupleof[i] = x;
		}
		return res;
	}
}
alias simpleStructCtor MSimpleStructCtor;


/+/**
	Mixin for a simple toString - a comma-separated list of all fields
	Warning: due to a bug in dmd 0.175 .. 1.010, let this mixin be after all fields in the struct
*/
template simpleStructToString() {
	private import std.string : stdStringFormat = format;

	char[] toString() {
		char[] res;
		foreach (i, x; this.tupleof) {
			if (0 != i) res ~= ", ";
			res ~= stdStringFormat(x);
		}
		return res;
	}
}
+/



char[] makeTypedUnion(char[] name, char[][] origTypes, char[][] typeNames) {
	char[] result = "struct " ~ name ~ " {"\n;

	result ~= \t"enum Type : ubyte {"\n;
	foreach (n; typeNames) {
		result ~= \t\t ~ n ~ ","\n;
	}
	result ~= \t"}"\n\n;
	
	result ~= \t"union {"\n;
	foreach (i, n; typeNames) {
		result ~= \t\t ~ origTypes[i] ~ "\t" ~ n ~ ";"\n;
	}
	result ~= \t"}"\n\n;
	result ~= \t"Type type;"\n\n;

	result ~= \t"template TypeT(T) {"\n;
	foreach (i, n; typeNames) {
		result ~= \t\t;
		if (i > 0) result ~= "else ";
		
		result ~= "static if (is(T == " ~ origTypes[i] ~ ")) const Type TypeT = Type." ~ n ~ ";"\t;
	}
	result ~= \t\t"else static assert (false, `No union member for type ` ~ T.stringof);"\n;
	result ~= \t"}"\n\n;
	
	result ~= \t"static " ~ name ~ " opCall(Type type) {"\n;
	result ~= \t ~ name ~ " res; res.type = type; return res;";
	result ~= \t"}"\n\n;
	
	foreach (i, n; typeNames) {
		result ~= \t"static " ~ name ~ " opCall(" ~ origTypes[i] ~ " v) {"\n;
		result ~= \t\t ~ name ~ " result = void;"\n;
		result ~= \t\t"result.type = Type." ~ n ~ ";"\n;
		result ~= \t\t"result." ~ n ~ " = v;"\n;
		result ~= \t\t"return result;"\n;
		result ~= \t"}"\n\n;
	}

	result ~= "}"\n;
	
	return result;
}
