module xf.utils.BitStream;

private {
	import xf.omg.core.LinearAlgebra;
}



// just a fancy way to create reading/writing function declarations for various types
private static {
	char[] w_integral(char[] type) {
		return `typeof(this) `~`opCall(`~type~` i, `~type~` min = `~type~`.min, `~type~` max = `~type~`.max);`;
	}

	char[] w_floatingPoint(char[] type) {
		return `typeof(this) `~`opCall(`~type~` f, `~type~` min = -`~type~`.max, `~type~` max = `~type~`.max);`;
	}

	char[] w_string(char[] type) {
		return `typeof(this) `~`opCall(`~type~` f, uint maxLen = 255);`;
	}

	char[] r_integral(char[] type) {
		return `typeof(this) `~`opCall(`~type~`* i, `~type~` min = `~type~`.min, `~type~` max = `~type~`.max);`;
		//return type~` `~`opCall(`~type~` min = `~type~`.min, `~type~` max = `~type~`.max);`;
	}

	char[] r_floatingPoint(char[] type) {
		return `typeof(this) `~`opCall(`~type~`* f, `~type~` min = -`~type~`.max, `~type~` max = `~type~`.max);`;
		//return type~` `~`opCall(`~type~` min = -`~type~`.max, `~type~` max = `~type~`.max);`;
	}

	char[] r_string(char[] type) {
		return `typeof(this) `~`opCall(`~type~`* f, uint maxLen = 255);`;
	}
}


interface BitStreamWriter {
	typeof(this) opCall(bool f);
	typeof(this) raw(void[]);

	mixin(w_integral(`byte`));
	mixin(w_integral(`ubyte`));
	mixin(w_integral(`short`));
	mixin(w_integral(`ushort`));
	mixin(w_integral(`int`));
	mixin(w_integral(`uint`));
	mixin(w_integral(`long`));
	mixin(w_integral(`ulong`));
	
	mixin(w_floatingPoint(`float`));
	
	mixin(w_string(`char[]`));
}



interface BitStreamReader {
	typeof(this) opCall(bool* f);
	typeof(this) raw(void[]);

	mixin(r_integral(`byte`));
	mixin(r_integral(`ubyte`));
	mixin(r_integral(`short`));
	mixin(r_integral(`ushort`));
	mixin(r_integral(`int`));
	mixin(r_integral(`uint`));
	mixin(r_integral(`long`));
	mixin(r_integral(`ulong`));
	
	mixin(r_floatingPoint(`float`));
	
	mixin(r_string(`char[]`));
	
	bool moreBytes();
}


void bsWrite(T)(BitStreamWriter bs, ref T t) {
	static if (is(T == vec2)) {
		bs(t.x)(t.y);
	}
	else static if (is(T == vec3)) {
		bs(t.x)(t.y)(t.z);
	}
	else static if (is(T == vec4)) {
		bs(t.x)(t.y)(t.z)(t.w);
	}
	else {
		bs(t);
	}
}


void bsRead(T)(BitStreamReader bs, T t) {
	static if (is(T == vec2*)) {
		bs(&t.x)(&t.y);
	}
	else static if (is(T == vec3*)) {
		bs(&t.x)(&t.y)(&t.z);
	}
	else static if (is(T == vec4*)) {
		bs(&t.x)(&t.y)(&t.z)(&t.w);
	}
	else {
		bs(t);
	}
}
