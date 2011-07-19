	/**
 	* Vectors with fixed dimensions.
	* 
	* Copyright: Copyright (C) 2006-2007 Peter Van Isacker
	*
 	* Authors: Peter Van Isacker (Sclytrack)
	*
	* Date:	June 20, 2006
	*
	* License:
	*
	*      Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
	*
	*      The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
	*
	*      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
	*/

module mathematics.numerical.VectorFixed;


version (Tango)
{
	private import tango.math.Math, tango.io.Stdout;
	private import Floating = tango.text.convert.Float;
} else
{
	private import std.math;
	private import std.stdio;
}


//TODO: Passing by "const ref"


char [] assertIterRange(char [] iterator, char [] list)
{
	return "assert(" ~ iterator ~ ">=&" ~ list ~ "[0] && "
	 ~ iterator ~ "<=&" ~ list ~ "[$-1], \"Iterator out of range\");";
}






/**
*	Structure to represent a vector.
*/

struct VectorFixed(T, uint VECTORLENGTH)
{
private:
	T	data[VECTORLENGTH];
public:
	/**
		Adds two homogeneous vectors together.
	*/
	VectorFixed homAdd(ref VectorFixed right)
	{
		T l, r;
		l = data[data.length-1];
		r = right.data[right.data.length-1];
		VectorFixed result;
		for (int x = 0; x < data.length-1; x++)
			result.data[x] = r * data[x] + l * right.data[x];
		result.data[ data.length-1 ] = l*r;
		return result;
	}
	
	/**
	*	Adds two homogeneous vectors together.
	*/

	VectorFixed homAddAssign(ref VectorFixed right)
	{
		T l, r;
		l = data[data.length-1];
		r = right.data[right.data.length-1];
		for (int x = 0; x < data.length-1; x++)
			data[x] = r * data[x] + l * right.data[x];
		data[data.length-1] = r*l;
		return *this;
	}
	
	/**
	* Substracts two homogeneous vectors.
	*/
	
	VectorFixed homSub(ref VectorFixed right)
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
	
	/**
	* Substraction of homogeneous vector.
	*/
	

	VectorFixed homSubAssign(ref VectorFixed right)
	{
		T l, r;
		l = data[data.length-1];
		r = right.data[right.data.length-1];


		for (int x = 0; x < data.length-1; x++)
			data[x] = r * data[x] + l * right.data[x];
		data[VECTORLENGTH-1] = l*r;
		return *this;
	}
	
	/**
	* Multiplication of homogeneous vector.
	*/


	VectorFixed homMul(T scale)
	{
		VectorFixed result;
		for (int x = 0; x < VECTORLENGTH-1; x++)
			result.data[x]*=scale;
		return result;
	}
	
	/**
	* Multiplication of homogeneous vector.
	*/


	VectorFixed homMulAssign(T scale )
	{
		for (int x = 0; x < data.length-1; x++)
			data[x] *= scale;
		return *this;
	}
	
	/**
	* Scaling of vector.
	*/


	VectorFixed homMul_r(T scale)
	{
		VectorFixed result;
		for (int x = 0; x < VECTORLENGTH-1; x++)
			result.data[x]*=scale;
		return result;
	}

	//	TODO: Figure out what I need this fore and how should I define this.

	/**
	*	Calculates the homegeneous norm of a vector.
	*	RETURNS:
	*		the norm of a homogeneous vector, as real
	*/
	
	real homNorm()
	{
		real result = 0;
		for (int x = 0; x < data.length-1; x++)
			result += data[x] * data[x];
		version (Tango)
		{
			return sqrt(result)/ data[VECTORLENGTH-1];
		} else	//Phobos
		{
			return std.math.sqrt(result)/ data[VECTORLENGTH-1];
		}
	}
	
	/**
	* Returns a negative of a homegeneous vector.
	*/


	VectorFixed homNeg()
	{
		VectorFixed result;
		for (int x = 0; x < data.length-1; x++)
			result.data[x] = -result.data[x];
		return result;
	}
	
	/**
	* Devide everything by the last element.
	*/

	void lastToOne()
	{
		T last = data[data.length-1];
		for (int x = 0; x < data.length-1; x++)
			data[x] /= last;
		data[data.length-1]=1;
	}

	//Start normal routines



	//TODO: VectorFixed opAdd( ref const VectorFixed right )
	
	VectorFixed opAdd( ref VectorFixed right )
	{
		VectorFixed result;		
		T * resultStop = &result.data[$-1];
		for (T * resultIter = &result.data[0], rightIter = &right.data[0], dataIter =  &data[0]; resultIter <= resultStop; resultIter++, rightIter++, dataIter++)
		{
			mixin(assertIterRange("resultIter", "result.data"));
			mixin(assertIterRange("rightIter", "right.data"));
			mixin(assertIterRange("dataIter", "data"));
			*resultIter =  *dataIter + *rightIter;
		}
		return result;
	}


	
	/**
	*	Addition of vectors
	*/


	/*
	VectorFixed opAdd(ref VectorFixed right)
	{
		VectorFixed result;
		for (int x = 0; x < data.length; x++)
			result.data[x] = data[x] + right.data[x];
		return result;
	}
	*/
	
	/**
	*	Addition of vectors
	*/

	VectorFixed opAddAssign(ref VectorFixed right)
	{
		for (int x = 0; x < data.length; x++)
			data[x] += right.data[x];
		return *this;
	}
	
	/**
	*	Substraction of vectors
	*/


	VectorFixed opSub(ref VectorFixed right)
	{
		VectorFixed result;
		for (int x = 0; x < data.length; x++)
			result.data[x] = data[x] - right.data[x];
		return result;
	}
	
	/**
	*	Substraction of vectors
	*/


	VectorFixed opSubAssign(ref VectorFixed right)
	{
		for (int x = 0; x < data.length; x++)
			data[x] -= right.data[x];
		return *this;
	}

	/*
	*	Multiplies this vector with another vector and returns the resulting vector.
	*
	* c[i] = a[i]*b[i]	
	*
	* See_Also:
	*		vectorMul, scalarMul
	*/

	VectorFixed opMul(ref VectorFixed right)
	{
		VectorFixed result;
		for(int x = 0; x < data.length; x++)
		{
			result.data[x] = data[x] * right.data[x];
		}
		return result;
	}

	/*
	*	Multiplies this vector with another vector and returns the resulting vector.
	*
	* c[i] = a[i]*b[i]	
	*
	* See_Also:
	*		vectorMul, scalarMul
	*/

	VectorFixed opMulAssign(ref VectorFixed right)
	{
		for (int x = 0; x < data.length; x++)
			data[x]*=right.data[x];
		return *this;		
	}
	
	/**
	*	Multiply each element with the value.
	*/


	VectorFixed opMul(T scale)
	{
		VectorFixed result;
		for (int x = 0; x < data.length; x++)
			result.data[x] = data[x] * scale;
		return result;
	}
	
	/**
	*	Multiply each element with the value.
	*/


	VectorFixed opMulAssign(T scale )
	{
		for (int x = 0; x < data.length; x++)
			data[x] *= scale;
		return *this;
	}

	/**
	*	Multiply each element with the value.
	*/


	VectorFixed opMul_r(T scale)
	{
		VectorFixed result;
		for (int x = 0; x < VECTORLENGTH; x++)
			result.data[x] = data[x] * scale;
		return result;
	}


	/*
	*	Returns the d-1 metric
	*/

	real sumOfElements()
	{
		real result = 0;
		for (int x = 0; x < data.length; x++)
		{
			result += data[x];
		}
		return result;
	}
	
	/**
	* Sum of the square of the elements
	*/


	real squaredSumOfElements()
	{
		real result = 0;
		for (int x = 0; x < data.length; x++)
		{
			result += data[x] * data[x];
		}
		return result;
	}

	/*
	*	Returns the d-2 metric
	*/


	real norm()
	{
		real result = 0;
		for (int x = 0; x < data.length; x++)
			result += cast(real) data[x] * data[x];
		return sqrt(result);
	}
	
	/**
	* Returns a normalized vector.
	*/

	VectorFixed normal()
	{
		VectorFixed result;
		real normValue = this.norm();
		assert( normValue > double.min);
		real scale = cast(real) 1.0 / normValue;
		for (int x = 0; x < data.length; x++)
			result.data[x] = cast(T)(data[x] * scale);
		return result;		
	}

	/**
	* Normalize the current vector.
	*/
	
	void normalize()
	{
		real normValue = 1.0/norm();
		for (int i = 0; i < VECTORLENGTH; i++)
			data[i] *= normValue;
	}
	

	VectorFixed opNeg()
	{
		VectorFixed result;
		for (int x = 0; x < data.length; x++)
			result.data[x] = -data[x];
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

	int opEquals(ref VectorFixed right)
	{
		int result = 0;
		for (int x = 0; x < data.length; x++)
			if (data[x] != right.data[x])
			{
				return false;
			}
		return true;
	}	

	int opCmp(ref VectorFixed right )
	{
		real norm1 = norm();
		real norm2 = norm();
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
				version (Tango)
				{
					result ~= tango.text.convert.Float.toString(data[x]) ~ ", ";
				} else
				{
					result ~= std.string.toString(data[x]) ~ ", ";
				}
			version (Tango)
			{
				result ~= tango.text.convert.Float.toString( data[data.length-1]  );
			} else //version Phobos
			{
				result ~= std.string.toString(data[data.length-1]);
			}
			return result~=" )";
		}
		return result;
	}
	
	/**
	* Perform the scalar multiplication. <a,b>
	*
	* Returns a scalar.
	*/
	

	T scalarMul(ref VectorFixed right)
	{
		assert(data.length == right.data.length);
		T result = 0;
		for (int x = 0; x < data.length; x++)
		{
			result += data[x] * right.data[x];
		}
		return result;
	}

	static if (VECTORLENGTH==4)
	{
		static VectorFixed opCall(T x, T y, T z, T s)
		{
			VectorFixed result;
			result.data[0] = x;
			result.data[1] = y;
			result.data[2] = z;
			result.data[3] = s;
			return result;
		}
	}

	static if (VECTORLENGTH==3)
	{
		static VectorFixed opCall( T x, T y, T z)
		{
			VectorFixed result;
			result.data[0] = x;
			result.data[1] = y;
			result.data[2] = z;
			return result;
		}
		
		/**
		* Perform the vector multiplication. (dot product I think)
		*
		* Returns a vector.
		*
		* See_Also:
		*   scalerMul, opMul
		*/

		VectorFixed vectorMul(ref VectorFixed right)
		{
			VectorFixed result;
			result.data[0] = data[1] * right.data[2] - data[2] * right.data[1];
			result.data[1] = data[2] * right.data[0] - data[0] * right.data[2];
			result.data[2] = data[0] * right.data[1] - data[1] * right.data[0];
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
		
		/**
		* Perform the vectorial multiplication.
		*/
		
		T vectorMul(ref VectorFixed right)
		{
			return data[0] * right.data[1] - data[1] * right.data[0];
		}
	}
}


/**
*	Point2
*/

alias VectorFixed!(int, cast(uint) 2) Point2;

/**
*	Point3
*/

alias VectorFixed!(int, cast(uint) 3) Point3;


unittest {
	Point2 a = Point2( 1 , 2);
	Point2 b = Point2( 3 , 4);
	Point2 c = a + b;
	assert( c[0] == 4 && c[1] == 6, "Sum is incorrect");
	c = a - b;
	assert( c[0] == -2 && c[1] == -2, "Substraction wrong");
	//TODO: add more test code.
}






