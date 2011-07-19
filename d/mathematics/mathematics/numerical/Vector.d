	/**
 	* Vector class.
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
	
	

module mathematics.numerical.Vector;

version (Tango)
{
	private {
		import tango.math.Math;
		import tango.io.Stdout;
		import Float = tango.text.convert.Float;
	}
} else	//Phobos
{
	private
	{
		import std.math;
		import std.stdio;
	}
}
	import mathematics.numerical.Matrix;


//This is for floating point values only.
//Integer values check out Point(T) or PointFixed(T)

/**
*	A class for representing a vector. This provides a means to inherit from the class.
*/



class Vector(T)
{
private:
	T [] _data;
	uint _stepSize;		//The step to the next element
	uint _startIndex;	//The index this vector starts with.	//startindex can be removed.
	uint _count;		//Number of elements in this vector.
public:


	/**
	* Constructor with length (amount of elements) of the vector.
	*/

	this (int count)
	{
		this(count, count);
	}
	
	/**
	* Constructor plus the length (amount of elements) and the total amount of elements.
	*
	* The "allocated count" is always greater of equal than the "count".
	*/

	this(int count, int allocatedCount)
	{
		assert( count > 0 && allocatedCount >= count);
		_data = new T [allocatedCount];
		_count = count;
		_startIndex = 0;
		_stepSize = 1;
	}
	
	/**
	* Point the current data to external data.
	*/

	this (T [] data)
	{
		this(data, 0, 1, data.length);
	}
	
	/**
	* Constructor with a view on external data.
	*/
	

	this (T [] data, uint startIndex, uint stepSize, uint count)
	{
		assert( startIndex >= 0 && startIndex < data.length, "class Vector: startIndex out of bound");
		assert( startIndex + stepSize * count <= data.length, "class Vector: startIndex + stepSize * count");
		_data = data;
		_startIndex = startIndex;
		_stepSize = stepSize;
		_count = count;
	}

	/**
		Returns the pointer to the first element of the vector.
	*/

	final T * iterStart()
	{
		return &_data[_startIndex];
	}

	/**
		Returns the pointer to the last element of the vector.
	*/

	final T * iterStop()
	{
		return &_data[_startIndex + _count - 1];
	}

	final Vector opAdd(Vector right)
	{
		assert(_count == right._count);
		Vector result = new Vector(_count);
		T * resultStop = result.iterStop;
		for (T * resultIter = result.iterStart, dataIter = this.iterStart, rightIter = right.iterStart; resultIter <= resultStop; resultIter++,  dataIter++, rightIter++)
			*resultIter = *dataIter + *rightIter;
		return result;
	}

	/*
	final Vector opAdd(Vector right)
	{
		assert(_count == right._count);
		Vector result = new Vector(_count);
		for (int x = 0; x < _count; x++)
			result[x] = this[x] + right[x];
		return result;
	}
	*/


	final Vector opAddAssign(Vector right)
	{
		assert(_count == right._count );
		for (int x = 0; x < _count; x++)
			this[x] = this[x] + right[x];
		return this;
	}


	final Vector opSub(Vector right)
	{
		assert(_count == right._count);
		Vector result = new Vector(_count);
		for (int x = 0; x < _count; x++)
			result[x]= this[x] - right[x];
		return result;
	}

	
	final Vector opSubAssign(Vector right)
	{
		assert(_count == right._count);
		for (int x = 0; x < _count; x++)
			this[x] = this[x] + right[x];
		return this;
	}
	
	/**
	* Performs a multiplication between to vectors.
	*
	* c[i] = a[i]*b[i]
	*/
	
	
	final Vector opMul( Vector right )
	{
		assert(getCount == right.getCount );
		Vector result = new Vector(getCount());
		for (int x = 0; x < _count; x++)
		{
				result[x] = this[x] * right[x];
		}
		return result;
	}
	
	
	/**
	* Performs a multiplication between to vectors.
	*
	* a[i] = a[i]*b[i]
	*/

	
	
	final Vector opMulAssign( Vector right)
	{
		assert( getCount == right.getCount );
		for (int x = 0; x < _count; x++)
		{
			this[x] = this[x] * right[x];
		}
		return this;
	}

	final Vector opMul(real scale)
	{
		Vector result = this.clone();
		result*=scale;
		return result;
	}

	final  Vector opMulAssign( real scale )
	{
		for (int x = 0; x < _count; x++)
			this[x] = this[x] * scale;
		return this;
	}

	final Vector opMul_r(real scale)
	{
		Vector result = this.clone();
		result*=scale;
		return result;
	}
	
	/**
	* Returns the norm.
	*/

	final real norm()
	{
		real result = 0;
		for (int x = 0; x < _count; x++)
			result += _data[x] * _data[x];
		//return std.math.sqrt(result);
		return sqrt(result);
	}
	
	/**
	* Returns the normalized vector.
	*/

	final Vector normal()
	{
		Vector result = this.clone();
		real normValue = cast(real) 1.0/norm;
		result*=normValue;
		return result;		
	}
	
	/**
	* Normalize the current vector.
	*/
	
	final void normalize()
	{
		real normValue = cast(real)1.0/norm;
		for (int i = 0; i < getCount(); i++)
			this[i] = this[i]*normValue;
	}

	final Vector opNeg()
	{
		Vector result = this.clone();
		for (int x = 0; x < _count; x++)
			//result._data[x] = -result._data[x];
			this[x] = -this[x];
		return result;
	}

	final T opIndex(int i)
	{
		assert(0 <= i && i < _count, "class Vector: Index out of bound");
		assert(_stepSize > 0, "class Vector: _stepsize is not larger than zero");
		return _data[_startIndex + i * _stepSize];
	}

	final T opIndexAssign( T value, int i)
	{
		assert(0 <= i && i < _count, "class Vector: Index out of bound");
		assert(_stepSize > 0, "class Vector: _stepsize is not larger than zero");
		_data[_startIndex + i * _stepSize] = value;
		return value;
	}

	final int opEquals(Vector right)
	{
		assert(_count == right._count);
		int result = 0;
		for (int x = 0; x < _count; x++)
			if (this[x] != right[x])
				return false;
		return 0;
	}	

	final int opCmp(Vector right )
	{
		assert(_count == right._count, "class Vector: _count are not the same");
		real norm1 = norm();
		real norm2 = norm();
		if (norm1 < norm2)
			return -1;
		if (norm1 > norm2)
			return 1;
		return 0;
	}
	
	/**
	* Get the total amount of relevent elements.
	*/

	final int getCount()
	{
		return _count;
	}

	final char [] toString()
	{
		char[] result = "";

		if (_count > 0)		
			 {
			result ~= "( ";
			for (int x = 0; x < _count-1; x++)
				result ~= tango.text.convert.Float.toString(this[x]) ~ ", ";
			result ~= tango.text.convert.Float.toString(this[_count-1]);
			return result~=" )";
		}
		return result;
	}
	
	/**
	* Performs the scalar multiplication.
	*/

	final T scalarMul( Vector right )
	{
		assert( _count == right._count);
		T result = 0;
		for (int x = 0; x < this._count; x++)
			result += this[x] * right[x];
		return result; 
	}
	
	/**
	* Performs the vector multiplication. The vector must have 3 elements.
	*/
	
	final Vector vectorMul( Vector right)
	{
		assert(this._count == 3 && right._count == 3);
		Vector result = new Vector(3);

		result[0] = this[1] * right[2] - this[2] * right[1];
		result[1] = this[2] * right[0] - this[0] * right[2];
		result[2] = this[0] * right[1] - this[1] * right[0];

		return result;	
	}
	
	/**
	* Clone the vector only up to count elements.
	*/
	

	final Vector clone()		//not an exact clone no more
	{	
		Vector result = new Vector(_count);
		result._count = this._count;
		for (int x = 0; x < _count; x++)
			result[x] = this[x];
		return result;
	}
	
	
	/**
	* Multiply a matrix with another vector and assign it to the current vector.
	*
	
	* Example:
	* ---
	*			v2.mulAssign(matrix, v1);
	* ---
	*/


	final void mulAssign( MatrixBase!(T) matrix, Vector vec)
	{
		//	MxN Nx1 = Mx1
		assert( matrix.columnCount() == vec.getCount(), "matrix size and right vector size do not match");
		assert(_data.length >= matrix.rowCount(), "length of the vector data is not high enough");
		_count = matrix.rowCount();
		T intermediate;
		for (int r = 0; r < matrix.rowCount(); r++)
		{
			intermediate = 0;
			for (int c = 0; c < matrix.columnCount(); c++)
				intermediate += matrix[r,c] * vec[c];
			this[r] = intermediate;
		}
	}

	//TODO: 

	final void mulAssign(Matrix!(T) matrix, Vector vec)
	{
		
	}
}



unittest {

	Vector!(float) a = new Vector!(float)(3);
	a[0] = 1;
	a[1] = 2;
	a[2] = 3;	

	Vector!(float) b = new Vector!(float)(3);
	b[0] = 1;
	b[1] = 2;
	b[2] = 3;	
	Vector!(float) c = a + b;

	assert( c[0] == 2 && c[1] ==  4 && c[3] == 6, "Wrong sum for Vector");

	Vector!(float) d = new Vector!(float) (3,4);
	d += b;
}

