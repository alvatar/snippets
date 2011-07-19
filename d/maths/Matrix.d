module xf.maths.Matrix;

private {
	import xf.maths.Vec;
	import xf.maths.Misc;
}




/+   OpenGL style matrix

|	0	4	8	 	12	|
|	1	5	9		13	|
|	2	6	10	14	|
|	3	7	11	15	|

+/
struct mat4T(flt)
{
	private alias vec!(flt, 3, 0, flt.sizeof, flt.sizeof*2, flt.sizeof*3) vec3;


	flt data[16];
	
	
	flt rc(uint row, uint col) {
		return data[col * 4 + row];
	}


	const static mat4T identity = { data: [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1] };


	mat4T dup() {
		mat4T res;
		res.data[] = data[];
		return res;
	}
	
	
	void makeIdentity() {
		*this = identity;
	}
	
	
	flt opIndex(int i) {
		return data[i];
	}
	
	
	flt opIndexAssign(flt x, int i) {
		return data[i] = x;
	}
	
	
	void setRow(uint i, vec4 r) {
		data[i] = r.cell[0];
		i += 4;
		data[i] = r.cell[1];
		i += 4;
		data[i] = r.cell[2];
		i += 4;
		data[i] = r.cell[3];
	}
	
	
	void transpose() {
		flt tmp;
		
		void chng(int a, int b) {
			tmp = data[a];
			data[a] = data[b];
			data[b] = tmp;
		}
		
		chng(1, 4);
		chng(2, 8);
		chng(3, 12);
		chng(6, 9);
		chng(7, 13);
		chng(11, 14);
	}
	
	
	flt determinant() {
		return		(data[0] * data[5] - data[1] * data[4]) * (data[10] * data[15] - data[11] * data[14])
					- 	(data[0] * data[6] - data[2] * data[4]) * (data[9]  * data[15] - data[11] * data[13])
					+	(data[0] * data[7] - data[3] * data[4]) * (data[9]  * data[14] - data[10] * data[13])
					+	(data[1] * data[6] - data[2] * data[5]) * (data[8]  * data[15] - data[11] * data[12])
					-	(data[1] * data[7] - data[3] * data[5]) * (data[8]  * data[14] - data[10] * data[12])
					+	(data[2] * data[7] - data[3] * data[6]) * (data[8]  * data[13] - data[9]  * data[12]);
	}
	
	mat4T inverse() {
		flt A = data[10] * data[15] - data[11] * data[14];
		flt B = data[9]  * data[15] - data[11] * data[13];
		flt C = data[9]  * data[14] - data[10] * data[13];
		flt D = data[2]  * data[7]  - data[3]  * data[6];
		flt E = data[1]  * data[7]  - data[3]  * data[5];
		flt F = data[1]  * data[6]  - data[2]  * data[5];
		flt G = data[8]  * data[15] - data[11] * data[12];
		flt H = data[8]  * data[14] - data[10] * data[12];
		flt I = data[0]  * data[7]  - data[3]  * data[4];
		flt J = data[0]  * data[6]  - data[2]  * data[4];
		flt K = data[8]  * data[13] - data[9]  * data[12];
		flt L = data[0]  * data[5]  - data[1]  * data[4];
	
		// Determinant
		flt	det = L * A - J * B + I * C + F * G - E * H + D * K;
	
		if (det == 0) return *this;
		
		det = (cast(flt)1) / det;
		
		mat4T result;
		
		result.data[0]  = det * (data[5]   * A - data[6]  * B + data[7]  * C);
		result.data[2]  = det * (data[13]  * D - data[14] * E + data[15] * F);
		result.data[4]  = det * (data[6]   * G - data[7]  * H - data[4]  * A);
		result.data[6]  = det * (data[14]  * I - data[15] * J - data[12] * D);
		result.data[8]  = det * (data[7]   * K + data[4]  * B - data[5]  * G);
		result.data[10] = det * (data[15]  * L + data[12] * E - data[13] * I);
		result.data[12] = det * (-data[4]  * C + data[5]  * H - data[6]  * K);
		result.data[14] = det * (-data[12] * F + data[13] * J - data[14] * L);
		
		A = data[2] * data[15] - data[3] * data[14];
		B = data[3] * data[13] - data[1] * data[15];
		C = data[1] * data[14] - data[2] * data[13];
		D = data[7] * data[10] - data[6] * data[11];
		E = data[5] * data[11] - data[7] * data[9];
		F = data[6] * data[9]  - data[5] * data[10];
		G = data[0] * data[15] - data[3] * data[12];
		H = data[2] * data[12] - data[0] * data[14];
		I = data[7] * data[8]  - data[4] * data[11];
		J = data[4] * data[10] - data[6] * data[8];
		K = data[0] * data[13] - data[1] * data[12];
		L = data[5] * data[8]  - data[4] * data[9];
		
		result.data[1]  = det * (data[9]  * A + data[10] * B + data[11] * C);
		result.data[3]  = det * (data[1]  * D + data[2]  * E + data[3]  * F);
		result.data[5]  = det * (data[10] * G + data[11] * H - data[8]  * A);
		result.data[7]  = det * (data[2]  * I + data[3]  * J - data[0]  * D);
		result.data[9]  = det * (data[11] * K - data[8]  * B - data[9]  * G);
		result.data[11] = det * (data[3]  * L - data[0]  * E - data[1]  * I);
		result.data[13] = det * (-data[8] * C - data[9]  * H - data[10] * K);
		result.data[15] = det * (-data[0] * F - data[1]  * J - data[2]  * L);
		
		return result;
	}
	
	void getData(float[] result) {
		// may the compiler unroll this loop :>
		for (int i = 0; i < 16; ++i)
			result[i] = data[i];
	}
	
	mat4T rotationPart() {
		mat4T res;
		res.data[0] = data[0];
		res.data[1] = data[1];
		res.data[2] = data[2];
		res.data[4] = data[4];
		res.data[5] = data[5];
		res.data[6] = data[6];
		res.data[8] = data[8];
		res.data[9] = data[9];
		res.data[10] = data[10];
		res.data[3] = res.data[7] = res.data[11] = res.data[12] = res.data[13] = res.data[14] = 0;
		res.data[15] = 1;
		return res;
	}
	
	static mat4T xRot(real angle) {
		mat4T res;
		
		flt	Sin = cast(flt)sin(angle * deg2rad);
		flt Cos = cast(flt)cos(angle * deg2rad);
		
		res[5] = Cos;
		res[10] = Cos;
		res[6] = Sin;
		res[9] = -Sin;
		res[0] = res[15] = 1;
		res[1] = res[2] = res[3] = res[4] = res[7] = res[8] = res[11] = res[12] = res[13] = res[14] = 0;
		
		return res;
	}
	
	static mat4T yRot(real angle) {
		mat4T res;
		
		flt Sin = cast(flt)sin(angle * deg2rad);
		flt Cos = cast(flt)cos(angle * deg2rad);
		
		res[0] = Cos;
		res[10] = Cos;
		res[2] = -Sin;
		res[8] = Sin;
		res[5] = res[15] = 1;
		res[1] = res[3] = res[4] = res[6] = res[7] = res[9] = res[11] = res[12] = res[13] = res[14] = 0;
		
		return res;
	}
	
	static mat4T zRot(real angle) {
		mat4T res;
		
		flt Sin = cast(flt)sin(angle * deg2rad);
		flt Cos = cast(flt)cos(angle * deg2rad);
		
		res[0] = Cos;
		res[5] = Cos;
		res[1]  = Sin;
		res[4]  = -Sin;
		res[10] = res[15] = 1;
		res[2] = res[3] = res[6] = res[7] = res[8] = res[9] = res[11] = res[12] = res[13] = res[14] = 0;
		
		return res;
	}
	
	static mat4T eulerRotation(real x, real y, real z) {
		mat4T res;
		
		flt A = cast(flt)cos(x * deg2rad);
		flt B = cast(flt)sin(x * deg2rad);
		flt C = cast(flt)cos(y * deg2rad);
		flt D = cast(flt)sin(y * deg2rad);
		flt E = cast(flt)cos(z * deg2rad);
		flt F = cast(flt)sin(z * deg2rad);
		
		flt AD = A * D;
		flt BD = B * D;
		
		res[0] =  C  * E;
		res[1] =  BD * E + A * F;
		res[2] = -AD * E + B * F;
		res[4] = -C  * F;
		res[5] = -BD * F + A * E;
		res[6] =  AD * F + B * E;
		res[8] =  D;
		res[9] = -B  * C;
		res[10] =  A  * C;
		
		res[3] = res[7] = res[11] = res[12] = res[13] = res[14] = 0;
		res[15] = 1;
		
		return res;
	}
		
	static mat4T translation(vec3 v) {
		mat4T res;
		
		res[0] = res[5] = res[10] = res[15] = 1;
		res[1] = res[2] = res[3] = res[4] = res[6] = res[7] = res[8] = res[9] = res[11] = 0;
		res[12] = v.x;
		res[13] = v.y;
		res[14] = v.z;
		
		return res;
	}
	
	static mat4T scaling(vec3 v) {
		mat4T res;
		
		res[0] = v.x;
		res[5] = v.y;
		res[10] = v.z;
		res[15] = 1;
		res[1] = res[2] = res[3] = res[4] = res[6] = res[7] = res[8] = res[9] = res[11] = res[12] = res[13] = res[14] = 0;
		
		return res;
	}
	
	
	// not optimized. uses the obvious school course - based derivation
	static mat4T reflection(vec3 normal) {
		vec3 project(vec3 v) {
			float dst = normal.dot(v);
			return v - normal * dst * cast(flt)2.0;
		}
		
		mat4T res;
		vec3 i	= vec3(1, 0, 0);
		vec3 j	= vec3(0, 1, 0);
		vec3 k	= vec3(0, 0, 1);
		i = project(i);
		j = project(j);
		k = project(k);
		res.data[0 .. 3] = i.cell[0 .. 3];
		res.data[4 .. 7] = j.cell[0 .. 3];
		res.data[8 .. 11] = k.cell[0 .. 3];
		res[3] = res[7] = res[11] = res[12] = res[13] = res[14] = 0;
		res[15] = 1;
		return res;
	}	
	
	
	/+char[] toString() {
		char[] row(int i)	{
			return format("[%3.3f,\t%3.3f,\t%3.3f,\t%3.3f]", cast(float)data[i], cast(float)data[i+4], cast(float)data[i+8], cast(float)data[i+12]);
		}
		
		return row(0) ~ "\n" ~ row(1) ~ "\n" ~ row(2) ~ "\n" ~ row(3);
	}+/
	
	mat4T opMul(mat4T rhs) {
		mat4T res;
		
		void calc(int x, int y)
		{
			res[x*4+y] =
				data[y]			* rhs.data[x * 4]
			+	data[y + 4]		* rhs.data[x * 4 + 1]
			+	data[y + 8]		* rhs.data[x * 4 + 2]
			+	data[y + 12]	* rhs.data[x * 4 + 3];
		}
		
		for (int i = 0; i < 4; ++i)
			for (int j = 0; j < 4; ++j)
				calc(i, j);
		
		return res;
	}


	mat4T opAdd(mat4T rhs) {
		mat4T res;
		
		for (uint i = 0; i < data.length; ++i) {
			res.data[i] = data[i] + rhs.data[i];
		}
		
		return res;
	}


	mat4T opMul(float x) {
		mat4T res;
		
		for (uint i = 0; i < data.length; ++i) {
			res.data[i] = cast(flt)(data[i] * x);
		}
		
		return res;
	}


	vec3 rotate(vec3 rhs) {
		return vec3(	data[0]  * rhs.x	+ data[4] * rhs.y +
							data[8]  * rhs.z,
							
							data[1]  * rhs.x	+ data[5] * rhs.y +
							data[9]  * rhs.z,
							
							data[2]  * rhs.x	+ data[6] * rhs.y +
							data[10] * rhs.z);
	}


	vec3 invRotate(vec3 rhs) {
		return vec3(	data[0]  * rhs.x	+ data[1] * rhs.y +
							data[2]  * rhs.z,
							
							data[4]  * rhs.x	+ data[5] * rhs.y +
							data[6]  * rhs.z,
							
							data[8]  * rhs.x	+ data[9] * rhs.y +
							data[10] * rhs.z);
	}


	vec3 xform(vec3 rhs) {
		return vec3(	data[0]  * rhs.x	+ data[4] * rhs.y +
							data[8]  * rhs.z	+ data[12],
							
							data[1]  * rhs.x	+ data[5] * rhs.y +
							data[9]  * rhs.z	+ data[13],
							
							data[2]  * rhs.x	+ data[6] * rhs.y +
							data[10] * rhs.z	+ data[14]);
	}
	
	
	vec4 xform(vec4 rhs) {
		return vec4(	data[0]  * rhs.x	+ data[4] * rhs.y +
							data[8]  * rhs.z	+ data[12] * rhs.w,
							
							data[1]  * rhs.x	+ data[5] * rhs.y +
							data[9]  * rhs.z	+ data[13] * rhs.w,
							
							data[2]  * rhs.x	+ data[6] * rhs.y +
							data[10] * rhs.z	+ data[14] * rhs.w,
							
							data[3]  * rhs.x	+ data[7] * rhs.y +
							data[11] * rhs.z	+ data[15] * rhs.w);
	}
	
	
	vec3 getTranslation() {
		return vec3(data[12], data[13], data[14]);
	}


	void setTranslation(vec3 t) {
		data[12] = t.x;
		data[13] = t.y;
		data[14] = t.z;
	}


	bool isSameAs(mat4T v, float epsilon) {
		foreach (uint i, flt c; data) {
			if (abs(c - v.data[i]) > epsilon) {
				return false;
			}		}
		
		return true;
	}
}


alias mat4T!(float)		mat4;
alias mat4T!(double)	mat4d;
alias mat4T!(real)		mat4r;

