module xf.game.StateUtils;

private {
	import tango.core.Traits;
	import xf.utils.BitStream;
	version (OldMath) {
		import xf.maths.Vec;
		import xf.maths.Quat;
	} else {
		import xf.omg.core.LinearAlgebra : vec3, vec2, quat;
	}
}



void writeState(State)(ref State st, BitStreamWriter bs) {
	static if (is(typeof(st.write(bs)))) {
		st.write(bs);
	} else {
		foreach (i, _dummy; st.tupleof) {
			static if (is(typeof(writeField(&st.tupleof[i], bs)))) {
				writeField(&st.tupleof[i], bs);
			} else {
				writeState(st.tupleof[i], bs);
			}
		}
	}
}


void writeField(T)(T* field, BitStreamWriter bs) {
	static if (isStaticArrayType!(T)) {
		for (int i = 0; i < T.length; ++i) {
			writeField(&(*field)[i], bs);
		}
	}
	else static if (is(T == vec3)) {
		writeField(&field.x, bs);
		writeField(&field.y, bs);
		writeField(&field.z, bs);
	}
	else static if (is(T == vec2)) {
		writeField(&field.x, bs);
		writeField(&field.y, bs);
	}
	else static if (is(T == quat)) {
		writeField(&field.x, bs);
		writeField(&field.y, bs);
		writeField(&field.z, bs);
		writeField(&field.w, bs);
	} else {
		bs(*field);
	}
}


// ----


void readState(State)(ref State st, BitStreamReader bs) {
	static if (is(typeof(st.read(bs)))) {
		st.read(bs);
	} else {
		foreach (i, dummy_; st.tupleof) {
			static if (is(typeof(readField(&st.tupleof[i], bs)))) {
				readField(&st.tupleof[i], bs);
			} else {
				readState(st.tupleof[i], bs);
			}
		}
	}
}


void readField(T)(T* field, BitStreamReader bs) {
	static if (isStaticArrayType!(T)) {
		for (int i = 0; i < T.length; ++i) {
			readField(&(*field)[i], bs);
		}
	}
	else static if (is(T == vec3)) {
		readField(&field.x, bs);
		readField(&field.y, bs);
		readField(&field.z, bs);
	}
	else static if (is(T == vec2)) {
		readField(&field.x, bs);
		readField(&field.y, bs);
	}
	else static if (is(T == quat)) {
		readField(&field.x, bs);
		readField(&field.y, bs);
		readField(&field.z, bs);
		readField(&field.w, bs);
	} else {
		bs(field);
	}
}


// ----


float compareStates(State)(ref State st1, ref State st2) {
	static if (is(typeof(st1.compare(st2)))) {
		auto res = st1.compare(st2);
		static assert (is(typeof(res) == float));
		return res;
	} else {
		pragma (msg, `Warning: no float compare(` ~ State.stringof ~ `) function for ` ~ State.stringof ~ `. Return value: [0 - no difference .. 1 - high difference]`);
		return 0.f;
	}
}
