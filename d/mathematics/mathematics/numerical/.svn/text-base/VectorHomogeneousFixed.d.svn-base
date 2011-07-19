/*
Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

module mathematics.numerical.VectorHomogeneousFixed;


version (Tango)
{
	private import tango.math.Math;
	private import tango.io.Stdout;
}
else	//Phobos
{
	private import std.math;
	private import std.stdio;
}

//	TODO: Do I really need this type?


struct VectorHomogeneousFixed(T, int VECTORLENGTH)
{
private:
	T	data[VECTORLENGTH];
public:


	VectorFixed opAdd(VectorFixed right)
	{
		T l, r;
		l = data[data.length-1];
		r = right.data[right.data.length-1];
		VectorFixed result;
		for (int x = 0; x < data.length-2; x++)
			result.data[x] = r * data[x] + l * right.data[x];
		result.data[ data.length-1 ] = l*r;
		return result;

	}
	VectorFixed opAddAssign(VectorFixed right)
	{
		T l, r;
		l = data[data.length-1];
		r = right.data[right.data.length-1];
		for (int x = 0; x < data.length-1; x++)
			data[x] = r * data[x] + l * right.data[x];
		data[data.length-1] = r*l;
		return *this;
	}

	VectorFixed opSub(VectorFixed right)
	{
		T l, r;
		l = data[data.length-1];
		r = right.data[right.data.length-1];

		VectorFixed result;
		for (int x = 0; x < data.length-1; x++)
			result.data[x] = r*data[x] - l* right.data[x];
		result.data[ VECTORLENGTH-1 ] = l*r;
		return result;
	}

	VectorFixed opSubAssign(VectorFixed right)
	{
		T l, r;
		l = data[data.length-1];
		r = right.data[right.data.length-1];


		for (int x = 0; x < data.length-1; x++)
			data[x] = r * data[x] + l * right.data[x];
		data[VECTORLENGTH-1] = l*r;
		return *this;
	}

	

	VectorFixed opMul(T scale)
	{
		VectorFixed result;
		for (int x = 0; x < VECTORLENGTH-1; x++)
			result.data[x]*=scale;
		return result;
	}

	VectorFixed opMulAssign(T scale )
	{
		for (int x = 0; x < data.length-1; x++)
			data[x] *= scale;
		return *this;
	}

	VectorFixed opMul_r(T scale)
	{
		VectorFixed result;
		for (int x = 0; x < VECTORLENGTH-1; x++)
			result.data[x]*=scale;
		return result;
	}


	T norm()
	{
		T result = 0;
		for (int x = 0; x < data.length-1; x++)
			result += data[x] * data[x];
		return std.math.sqrt(result)/ data[VECTORLENGTH-1];
	}

	/*
	VectorFixed normal()
	{
		VectorFixed result;
		T normValue = norm;
		//writefln("Norm is %f: ", normValue);	//bug you can not do this
		result*=normValue;
		return result;		
	}
	*/

	VectorFixed opNeg()
	{
		VectorFixed result;
		for (int x = 0; x < data.length-1; x++)
			result.data[x] = -result.data[x];
		return result;
	}

	T opIndex(int i)
	{
		assert(i>=0 && i < data.length);
		return data[i];
	}

	T opIndexAssign( T value, int i)
	{
		assert(i >=0 && i < data.length);
		data[i] = value;
		return value;
	}

	void lastToOne()
	{
		T last = data[data.length-1];
		for (int x = 0; x < data.length-1; x++)
			data[x] /= last;
		data[data.length-1]=1;
	}

	int opEquals(VectorFixed right)
	{
		int result = 0;

		VectorFixed a = *this;
		VectorFixed b = right;

		a.lastToOne;
		b.lastToOne;

		for (int x = 0; x < data.length; x++)
			if (a.data[x] != b.data[x])
			{
				return false;
			}
		return 0;

	}	

	int opCmp(VectorFixed right )
	{
		T norm1 = norm();
		T norm2 = norm();
		if (norm1 < norm2)
			return -1;
		if (norm1 > norm2)
			return 1;
		return 0;
	}

	char [] toString()
	{
		char[] result = "";		
		if (data.length > 0) {

			result ~= "( ";
			for (int x = 0; x < data.length-1; x++)
				result ~= std.string.toString(data[x]) ~ ", ";
			result ~= std.string.toString(data[data.length-1]);
			return result~=" )";
		}
		return result;
	}

	static if (VECTORLENGTH==3)
	{
		VectorFixed vectorMul(VectorFixed right)
		{
			VectorFixed result;
			result.data[0] = data[1] * right.data[2] - data[2] * right.data[1];
			result.data[1] = data[2] * right.data[0] - data[0] * right.data[2];
			result.data[2] = data[0] * right.data[1] - data[1] * right.data[0];
			return result;	
		}


		T scalarMul(VectorFixed right)
		{
			assert(data.length == right.data.length);
			T result;
			for (int x = 0; x < data.length; x++)
			{
				result += data[x] * right.data[x];
			}
			return result;
		}
		

		static VectorFixed opCall( T x, T y, T z)
		{
			VectorFixed result;
			result.data[0] = x;
			result.data[1] = y;
			result.data[2] = z;
			return result;
		}

	}

	static if (VECTORLENGTH==2)
	{
		static VectorFixed opCall( T x, T y)
		{
			VectorFixed result;
			result.data[0] = x;
			result.data[1] = y;
			return result;
		}		
	}
}


