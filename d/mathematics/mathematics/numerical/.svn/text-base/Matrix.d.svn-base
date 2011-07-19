	/**
 	* Matrix Classes
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
 


module mathematics.numerical.Matrix;

public import mathematics.numerical.VectorFixed;


version (Tango)
{
	private import tango.math.Math;
	private import tango.io.Stdout;
}
else
{
	private import std.math;
	private import std.string;
	private import std.stdio;
}


/*******************************************************************************

	Gives access to the data of the matrix, used for adding,
	substracting and other operations. It doesn't instantiate
	anything.

	You can not instantiate this class.

*******************************************************************************/


class MatrixBase(T)
{
private:
	uint _rowCount, _columnCount;
protected:
	
	/**
	 * Is the constructor, it accepts two parameters.
	 * Params:
	 * 	rowCount 	= 	The amount of rows of the matrix.
	 * 	columnCount 	= 	The amount of columns of the matrix.
	 */
	this (uint rowCount, uint columnCount)
	{
		assert(_rowCount >= 0, "Matrix: rowCount failed");
		assert(_columnCount >= 0, "Matrix: columnCount failed");
		_rowCount = rowCount;
		_columnCount = columnCount;
	}

public:
        /***********************************************************************

		Abstract index to give access to an element of the matrix.

        ***********************************************************************/

	abstract T opIndex(uint row, uint column);

        /***********************************************************************

		Abstract index assign to assign a value to an element of the
		matrix.

        ***********************************************************************/

	abstract T opIndexAssign(T value, uint row, uint column);

        /***********************************************************************

		Retrieve the amount of rows.

        ***********************************************************************/

	final uint rowCount()
	{
		return _rowCount;
	}

        /***********************************************************************

		Retrieves the amount of columns.

        ***********************************************************************/
	
	final uint columnCount()
	{
		return _columnCount;
	}
	
        /***********************************************************************

		Returns the sum of the squared of the elements in the matrix.
		(Frobenius Norm)

        ***********************************************************************/

	T squaredSumOfElements()
	{
		T result = 0;
		for (uint r = 0; r < rowCount; r++)
			for (uint c = 0; c < columnCount; c++)
				result += this[r,c] * this[r,c];
		return result;
	}

        /***********************************************************************

		Returns the sum of all the elements in the matrix.

        ***********************************************************************/

	T sumOfElements()
	{
		T result = 0;
		for (uint r = 0; r < rowCount(); r++)
			for (uint c = 0; c < columnCount(); c++)
				result += this[r,c];
		return result;
	}

        /***********************************************************************

		Returns a string suitable for printing in the console.

        ***********************************************************************/

	override char [] toString()
	{
		char [] result = "\n";
		for (uint r = 0; r < rowCount(); r++)
		{
			result ~= "(";
			for (uint c = 0; c < columnCount(); c++)
				version (Tango)
				{
					result ~= "  "~ tango.text.convert.Float.toString( this.opIndex(r,c) ) ~ "  ";
				} else //Phobos
				{
					result~= "  "~std.string.toString(this.opIndex(r, c))~ "  ";
				}
			result~=")\n";
		}
		return result;
	}

        /***********************************************************************

		Checks if the content of the two matrices are the same.

        ***********************************************************************/

	bool opEquals(MatrixBase!(T) right)
	{
		bool result = this.rowCount() == right.rowCount() && this.columnCount() == right.columnCount();
		if (!result) return result;
		for (uint r = 0; r < rowCount(); r++)
			for (uint c = 0; c < columnCount(); c++)
				if (this[r,c] != right[r,c]) return false;
		return result;
	}



        /***********************************************************************

		Returns a new transpose matrix.

        ***********************************************************************/


	Matrix!(T) newTransposeMatrix()
	{
		Matrix!(T) result = new Matrix!(T)(columnCount(), rowCount());
		for (uint r = 0; r < this.rowCount; r++)
			for (uint c = 0; c < columnCount; c++)
				result[c,r] = this[r,c];
		return result;
	}

        /***********************************************************************

		Returns a new matrix that is the sum of two other matrices.

		Example:
		matrix3 = matrix1 + matrix2;

        ***********************************************************************/

	Matrix!(T) opAdd( MatrixBase!(T) right)
	{

		assert( this.rowCount() == right.rowCount() , " RowCount error");
		assert( this.columnCount() == right.columnCount() , "ColumnCount doesn't match");
		Matrix!(T) result = new Matrix!(T)(this.rowCount(), this.columnCount);
			for (uint r = 0; r < rowCount(); r++)
				for (uint c = 0; c < columnCount(); c++)
					result[r,c] = this[r,c] + right[r,c];
		return result;
	}


        /***********************************************************************

		Returns a new matrix that is the substraction of two other
		matrices.

		Example:
		---
		matrix3 = matrix1 - matrix2;
		---

        ***********************************************************************/


	Matrix!(T) opSub(MatrixBase!(T) right)
	{
		assert( this.rowCount() == right.rowCount() , " RowCount doesn't match");
		assert( this.columnCount() == right.columnCount() , "ColumnCount doesn't match");
		Matrix!(T) result = new Matrix!(T)(this.rowCount(), this.columnCount);
			for (uint r = 0; r < rowCount(); r++)
				for (uint c = 0; c < columnCount(); c++)
					result[r,c] = this[r,c] - right[r,c];
		return result;
	}


        /***********************************************************************

		Returns a new matrix multiplied matrix of type
		MatrixRectangular

		Example:
		---
		matrix3 = matrix1 * matrix2;
		---

        ***********************************************************************/


	Matrix!(T) opMul(MatrixBase!(T) right)
	{
		assert( this.columnCount() == right.rowCount(), "Incorrect matrix dimensions for multiplication");
		Matrix!(T) result = new Matrix!(T)(rowCount(), columnCount());
		T intermediate;
		for (uint r = 0; r < this.rowCount(); r++)
			for (uint c = 0; c < right.columnCount; c++)
			{
				intermediate = 0;
				for (uint i = 0; i < this.columnCount(); i++)
					intermediate += this.opIndex(r,i) * right[i,c];
				result[r,c] = intermediate;
			}		
		return result;
	}


        /***********************************************************************

		Returns a new matrix, from the current matrix where every
		element is multiplied by a factor.

		Example:
		---
		matrix2 = matrix1*2;
		---

        ***********************************************************************/

	Matrix!(T) opMul(real scalar)
	{
		Matrix!(T) result = new Matrix!(T)(this.rowCount(), this.columnCount());
		for (uint r = 0; r < rowCount(); r++)
			for (uint c = 0; c < columnCount(); c++)
				result[r,c] = this.opIndex(r, c) * scalar;
		return result;
	}
	
        /***********************************************************************

		Returns a new matrix, from the current matrix where every
		element is multiplied by a factor.

		Example:
		---
		matrix2 = 2*matrix1;
		---

        ***********************************************************************/


	Matrix!(T) opMul_r(real scalar)
	{
		Matrix!(T) result = new Matrix!(T)(this.rowCount(), this.columnCount());
		for (uint r = 0; r < rowCount(); r++)
			for (uint c = 0; c < columnCount(); c++)
				result[r,c] = this.opIndex(r, c) * scalar;
		return result;
	}

        /***********************************************************************

		Returns a new matrix that is the where all its values are
		negated.

        ***********************************************************************/

	Matrix!(T) opNeg()
	{
		Matrix!(T) result = new Matrix!(T) ( rowCount(), columnCount());
		for (uint r = 0; r < rowCount(); r++)
			for (uint c = 0; c < columnCount; c++)
				result[r,c] = -this[r,c];
		return result;
	}


        /***********************************************************************

		Creates a new array and fills it up with a column of the matrix.

        ***********************************************************************/


	T [] getColumn(uint column)
	{
		assert( column >= 0 && column < columnCount(), "column index out of bound");
		T[] result = new T[rowCount()];
		for (uint r = 0; r < rowCount; r++)
			result[r] = this[r, column];
		return result;
	}

        /***********************************************************************

		Creates a new array and fills it up with a row of the matrix.

        ***********************************************************************/


	T [] getRow(uint row)
	{
		T [] result = new T[columnCount()];
		for (uint c = 0; c < _columnCount; c++)
			result[c] = this.opIndex(row, c);
		return result;
	}

        /***********************************************************************

		Returns a new matrix with the exact same content.

        ***********************************************************************/

	Matrix!(T) newClone()
	{
		Matrix!(T) result = new Matrix!(T)(rowCount(), columnCount());
		for (uint r = 0; r < rowCount(); r++)
			for (uint c = 0; c < columnCount(); c++)
				result[r,c] = this[r,c];
		return result;
	}


	//TODO: const ref

        /***********************************************************************

		Performs a 2x2 matrix multiplication with a vector of size 2.

        ***********************************************************************/

	VectorFixed!(T, cast(uint) 2) mul2x2(ref VectorFixed!(T, cast(uint) 2) right)
	{

		assert(this.rowCount() == 2 && this.columnCount() == 2, "not a 2x2 matrix");
		VectorFixed!(T, cast(uint) 2) result;
		T intermediate;
		for (uint r = 0; r < this.rowCount(); r++)
		{
			intermediate = 0;
			for (uint i =  0; i < this.columnCount(); i++)
				intermediate += this.opIndex(r,i) * right[i];
			result[r] = intermediate;
		}
		return result;
	}

        /***********************************************************************

		Performs a 3x3 matrix multiplication with a vector of size 3.

        ***********************************************************************/

	VectorFixed!(T, cast(uint) 3) mul3x3(ref VectorFixed!(T, cast(uint) 3) right)
	{
		assert(this.rowCount() == 3 && this.columnCount() == 3, "not a 3x3 matrix");
		VectorFixed!(T, cast(uint) 3) result;
		T intermediate;
		for (uint r = 0; r < this.rowCount(); r++)
		{
			intermediate = 0;
			for (uint i =  0; i < this.columnCount(); i++)
				intermediate += this.opIndex(r,i) * right[i];
			result[r] = intermediate;
		}
		return result;
	}


        /***********************************************************************

		Performs a 4x4 matrix multiplication with a vector of size 4.

        ***********************************************************************/

	VectorFixed!(T, cast(uint) 4) mul4x4(ref VectorFixed!(T, cast(uint) 4) right)
	{
		assert(this.rowCount() == 4 && this.columnCount() == 4, "not a 4x4 matrix");
		VectorFixed!(T, cast(uint) 4) result;
		T intermediate;
		for (uint r = 0; r < this.rowCount(); r++)
		{
			intermediate = 0;
			for (uint i =  0; i < this.columnCount(); i++)
				intermediate += this.opIndex(r,i) * right[i];
			result[r] = intermediate;
		}
		return result;
	}

        /***********************************************************************

		Performs a 3x4 matrix multiplication with a vector of size 4.

        ***********************************************************************/

	VectorFixed!(T, cast(uint) 3) mul3x4(ref VectorFixed!(T, cast(uint) 4) right)
	{
		assert(this.rowCount() == 3 && this.columnCount() == 4, "not a 4x4 matrix");
		VectorFixed!(T, cast(uint) 3) result;
		T intermediate;
		for (uint r = 0; r < this.rowCount(); r++)
		{
			intermediate = 0;
			for (uint i =  0; i < this.columnCount(); i++)
				intermediate += this.opIndex(r,i) * right[i];
			result[r] = intermediate;
		}
		return result;
	}
}




/*

	..........
	.****.....
	.****.....
	.****.....
	..........

	5x10 	Matrix
	3x4	View

	rowStep is 10
	columnStep is 1

	rowCount = 10
	columnCount = 20

*/



	
/**
 * It is a rectangular array that stores the values in a single array. Can represent sparse data.
 *
 * Params:
 * 	T = Must be a floating point type. (float, double, real)
 * 	LAPACKMODE = is a boolean must be true, or false		
 *
 * Example:
 * ---
 *	Matrix!(float) matrix = new Matrix!(float)(2,2);
 * ---
 */



class Matrix(T):MatrixBase!(T)
{
private:
	static Matrix freelist;
	Matrix freelistNext;
	static uint freelistCount = 0;
protected:
	T [] _data;
	uint _allocatedRowCount;
	uint _allocatedColumnCount;
	uint _rowStep;
	uint _columnStep;
	uint _indexOffset;
	bool _isColumnMajor;	//Column-Major
public:


        /***********************************************************************

	Returns a view to an existing matrix, this is part of the freelist
	algorithm.
		
        ***********************************************************************/
	
	static  Matrix allocateView(uint row, uint column, uint rowCount, uint columnCount, Matrix matrix)
	{
		Matrix!(T) result;
		if (freelist)
		{
			result = freelist;
			freelist = result.freelistNext;
			result.setView( row, column, rowCount, columnCount, matrix);
			freelistCount--;
		} else
			result = new Matrix!(T) (row, column, rowCount, columnCount, matrix);
		return result;
	}

        /***********************************************************************
	
	Puts the matrix onto a freelist for reuse.
		
        ***********************************************************************/

	static void deallocate(Matrix m)
	{
		if (freelistCount < 1000)	//Freelist Buffer size.
		{
			m.freelistNext = freelist;
			freelist = m;
			freelistCount++;
		}
	}

        /***********************************************************************

	Constructor for the Matrix

	For normal use the array should have the dimensions
	allocatedRowCount * allocatedColumnCount.

	The rowCount and columnCount of the matrix may not go over the
	allocatedRowCount and allocatedColumnCount.

	Params:
	rowCount		=	Amount of rows of the matrix.
	columnCount		=	Amount of columns of the matrix.
	allocatedRowCount	=	Amount of rows actually allocated in the array.
	allocatedColumnCount	=	Amount of columns actually allocated in the array.
	columnMajor		=	Store it in column major format?
	indexOffset		=	offset added to the index.

        ***********************************************************************/
	

	this (uint rowCount, uint columnCount, uint allocatedRowCount, uint allocatedColumnCount, bool columnMajor, T [] data, uint indexOffset = 0)
	{
		assert( rowCount > 0, 	"rowCount > 0");
		assert( columnCount > 0,"columnCount > 0");
		assert( allocatedRowCount > 0, "allocatedRowCount > 0");
		assert( allocatedColumnCount > 0, "allocatedColumnCount > 0");
		assert( rowCount <= allocatedRowCount, "rowCount <= allocatedRowCount");
		assert( columnCount <= allocatedColumnCount, "rowCount <= allocatedRowCount");
		assert( data!=null, "data!=null");

		super(rowCount, columnCount);
		_allocatedRowCount = allocatedRowCount;
		_allocatedColumnCount = allocatedColumnCount;
		_indexOffset = indexOffset;
		_isColumnMajor = columnMajor;		//Just discovered that lapackMode == column-major.
		_data = data;
		if (_isColumnMajor)
		{
			_columnStep = _allocatedRowCount;
			_rowStep = 1;			
		}
		else
		{
			_columnStep = 1;
			_rowStep = _allocatedColumnCount;
		}		
		assert(  data.length > (rowCount-1) * _rowStep + (columnCount-1) * _columnStep + _indexOffset ); 
	}


        /***********************************************************************

		Creates a Matrix that is a view of another Matrix

		Params:
		row	=	The row offset
		column	= 	The column offset
		rowCount=	Amount of rows of the view
		columnCount=	Amount of columns of the view
		matrix	=	The other Matrix
		
		Example:
		---
		auto matrix = new MatrixSized!(float, 4, 4) (
			[1,5,9,13,
			2,6,10,14,
			3,7,11,15,
			4,8,12,16]);

		auto view1 = new Matrix!(float) (1,1, 3,3, matrix);
		Stdout( view1 ).newline;

		auto view2 = new Matrix!(float) (0,0,2,2, view1);
		Stdout( view2 ).newline;
		---

		Output:
		---
		
		(  6.00    7.00    8.00  )
		(  10.00    11.00    12.00  )
		(  14.00    15.00    16.00  )


		(  6.00    7.00  )
		(  10.00    11.00  )

		---

        ***********************************************************************/




	this (uint row, uint column, uint rowCount, uint columnCount, Matrix matrix)
	{
		assert( row >= 0 && row < matrix.rowCount() && column >= 0 && column < matrix.columnCount(), "index out of bound");
		uint offset = row * matrix._rowStep + column * matrix._columnStep + matrix._indexOffset;
		this( rowCount, columnCount, matrix.allocatedRowCount, matrix.allocatedColumnCount, matrix.isColumnMajor, matrix.getData, offset);
	}
			



        /***********************************************************************

		Constructor that stores the data in column-major format.

		1  3
		
		2  4
		
		Example:
		---
		auto matrix = new Matrix!(double) (2,2,[1,2,3,4]);
		---

        ***********************************************************************/


	this(uint rowCount, uint columnCount, T [] data)
	{
		assert( rowCount > 0, 	"rowCount > 0");
		assert( columnCount > 0,"columnCount > 0");
		assert( data.length == rowCount * columnCount);
		this(rowCount, columnCount, rowCount, columnCount, true, data);
	}


        /***********************************************************************

	Returns whether or not the matrix is in column major format.
	
        ***********************************************************************/



	final bool isColumnMajor()
	{
		return _isColumnMajor;
	}


        /***********************************************************************
	
	You can set the matrix to be in column major format or not.	
	
        ***********************************************************************/

	final void isColumnMajor(bool columnMajor)
	{
		_isColumnMajor = columnMajor;
		if (_isColumnMajor)
		{
			_columnStep = _allocatedRowCount;
			_rowStep = 1;			
		}
		else
		{
			_columnStep = 1;
			_rowStep = _allocatedColumnCount;
		}		
		assert(  _data.length > (rowCount-1) * _rowStep + (columnCount-1) * _columnStep + _indexOffset ); 
	}
	

        /***********************************************************************
	
	Returns the offset of the first index (upper left element of the matrix)
	in the data array.
	
        ***********************************************************************/


	final uint indexOffset()
	{
		return _indexOffset;
	}

	/**
	* Is the constructor, it accepts two parameters.
	*
	* Params:
	* 	rowCount = The amount of rows of the matrix.
	* 	columnCount = The amount of columns of the matrix.
	*/
	

	this(uint rowCount, uint columnCount)
	{
		assert( rowCount > 0, "rowCount > 0");
		assert( columnCount >0,"columnCount > 0");
		T [] data = new T[rowCount * columnCount];
		this(rowCount, columnCount, rowCount, columnCount, true, data);
	}



        /***********************************************************************
	
	Sets a view of Matrix!(T).

	Params:
		row		=	top left corner
		column		=	top left corner
		rowCount	=	rowCount of the view
		columnCount	=	columnCount of the view
		matrix		=	another matrix to view.
	
        ***********************************************************************/



	final void setView( uint row, uint column, uint rowCount, uint columnCount, Matrix matrix)
	{
		assert( row >= 0 && row < matrix.rowCount() && column >= 0 && column < matrix.columnCount(), "index out of bound");
		_rowCount = rowCount;
		_columnCount = columnCount;
		_allocatedRowCount = matrix.allocatedRowCount;
		_allocatedColumnCount = matrix.allocatedColumnCount;
		_indexOffset = row * matrix._rowStep + column * matrix._columnStep + matrix._indexOffset;
		_isColumnMajor = matrix.isColumnMajor;
		_data = matrix.getData;
		if (_isColumnMajor)
		{
			_columnStep = _allocatedRowCount;
			_rowStep = 1;			
		}
		else
		{
			_columnStep = 1;
			_rowStep = _allocatedColumnCount;
		}
		assert(  _data.length > (rowCount-1) * _rowStep + (columnCount-1) * _columnStep + _indexOffset );
	}


        /***********************************************************************
	
	Returns the allocated amount of rows.
	
        ***********************************************************************/

	final uint allocatedRowCount()
	{
		return _allocatedRowCount;
	}

        /***********************************************************************
	
	Returns the allocated amount of columns.
	
        ***********************************************************************/


	final uint allocatedColumnCount()
	{
		return _allocatedColumnCount;
	}


	/**
	* Returns the array where all the matrix elements are stored. All the data in the matrix is stored in the single
	* array. If you retrieve the address of this array have the pointer to the actual data.
	*/


	final T [] getData()
	{
		return _data;
	}


	/**
	*	Returns a matrix that is the sum
	*	Example:
	*	---
	*	matrix1 += matrix2;
	*	---
	*/


	final Matrix opAddAssign( MatrixBase!(T) right)
	{
		assert( this.rowCount()==right.rowCount() , "RowCount doesn't match");
		assert( this.columnCount() == right.columnCount() , "ColumnCount doesn't match");
		for (uint r = 0; r < rowCount(); r++)
			for (uint c = 0; c < columnCount(); c++)
				this[r,c] = this[r,c] + right[r,c];
		return this;
	}




	/**
	*	Returns a matrix that is the substraction
	*	Example:
	*	---
	*	matrix1 -= matrix2;
	*	---
	*/


	final Matrix opSubAssign(MatrixBase!(T) right)
	{
		assert( this.rowCount() == right.rowCount() , " RowCount doesn't match");
		assert( this.columnCount() == right.columnCount() , "ColumnCount doesn't match");
		for (uint r = 0; r < rowCount(); r++)
			for (uint c = 0; c < columnCount(); c++)
				this[r,c] =  this[r,c] - right[r,c];
		return this;
	}

	
	/*
	*	Multiplies every element of the current matrix with a scalar.
	*/


	final Matrix opMulAssign(real scalar)
	{
		for (uint r = 0; r < rowCount(); r++)
			for (uint c = 0; c < columnCount; c++)
				this[r,c] = this[r,c] * scalar;
		return this;
	}


	
	/*
	*	Provides access to an element of the matrix.
	*
	*	Example:
	*	float test = matrix[row, column];
	*/
	
	final T opIndex(uint row, uint column)
	{
		assert( row >= 0 && row < this.rowCount() && column >= 0 && column < this.columnCount(), "index out of bound");
		return this._data[row*_rowStep + column*_columnStep + _indexOffset];
	}
	
	/**
	*	Assign a value to row and column.
	*
	*	Example:
	*	matrix[row, column] = 10;
	*/

	final T opIndexAssign( T value, uint row, uint column)
	{
		assert( row >= 0 && row < this.rowCount() && column >= 0 && column < this.columnCount(), "index out of bound");
		this._data[row*_rowStep + column*_columnStep+ _indexOffset] = value;
		return value;
	}


	/**
	*	Copies a portion from the right matrix into the current matrix.
	*	The right matrix must always be larger than the current matrix.
	*/

	final void copyFrom(MatrixBase!(T) right)
	{
		assert( right.rowCount() >= this.rowCount(),  "The size of the current matrix is not large enough");
		assert( right.columnCount() >= this.columnCount(), "The size of the current matrix is not large enough");

		for (uint r = 0; r < this.rowCount(); r++)
		{
			for (uint c = 0; c < this.columnCount(); c++)
				this[r,c] = right[r,c];
		}
	}


	final void setIdentity()
	{
		for (uint r=0; r < rowCount; r++)
			for (uint c= 0; c < columnCount; c++)
				this[r,c] = (r==c?1:0);	
	}
		
	/**
	*	Sets the scaling matrix.
	* The rest of the matrix is not touched.
	*/
	
	final void setScalingMatrix2x2(T xScale, T yScale)
	{
		assert( columnCount >= 2, "Your matrix is not big enough");
		assert( rowCount >= 2,"Your matrix is not big enough");
		
		this[0,0] = xScale; this[0,1]=0;
		this[1,0] = 0; this[1,1]=yScale;
	}	
	
	
	/**
	*	Sets only the 2x2 subset of the matrix to the counter-clockwise rotation.
	* The rest of the matrix is not touched.
	*/
	
	final void setRotationMatrix2x2(T angle)
	{
		assert( columnCount >= 2, "Your matrix is not big enough");
		assert( rowCount >= 2,"Your matrix is not big enough");
		
		version (Tango)
		{
			T s = sin(angle);
			T c = cos(angle);
		} else
		{
			T s = std.math.sin(angle);
			T c = std.math.cos(angle);
		}
		this[0,0] = c; this[0,1]=-s;
		this[1,0] = s; this[1,1]=c;
	}


	final void setLookAtMatrix4x4( ref VectorFixed!(T, cast(uint) 3) center, ref VectorFixed!(T, cast(uint) 3) z,ref VectorFixed!(T, cast(uint) 3) y )
	{
		z = z.normal();	
		VectorFixed!(T, cast(uint) 3) x = z.vectorMul(y);
		x = x.normal();
		y=z.vectorMul(x);	//flipping of the y axis
		y = y.normal();

		assert( rowCount >= 4);
		assert( columnCount >= 4);

		auto matrix = new Matrix!(T)(4,4);

		matrix[0,0] = x[0]; matrix[0,1] =  x[1]; matrix[0,2] =  x[2]; matrix[0,3] = 0;
		matrix[1,0] = y[0]; matrix[1,1] =  y[1]; matrix[1,2] =  y[2]; matrix[1,3] = 0;
		matrix[2,0] = z[0]; matrix[2,1] =  z[1]; matrix[2,2] =  z[2]; matrix[2,3] = 0;
		matrix[3,0] = 0; matrix[3,1] = 0; matrix[3,2] = 0;  matrix[3,3] = 1;

		auto translationMatrix = new Matrix!(T) (4,4);
		translationMatrix.setTranslationMatrix4x4(-center[0], -center[1], -center[2]);
		matrix = matrix * translationMatrix;
		this.copyFrom(matrix);
	}


	
		/*
		http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/rotate.html
		
		glRotate produces a rotation of angle	degrees	around the
	  vector (x,y,z).  The current matrix (see glMatrixMode) is
	  multiplied by	a rotation matrix with the product replacing
	  the current matrix, as if glMultMatrix were called with the
	  following matrix as its argument:

		  ( xx(1-c)+c	xy(1-c)-zs  xz(1-c)+ys	 0  )
		  |					    |
		  | yx(1-c)+zs	yy(1-c)+c   yz(1-c)-xs	 0  |
		  | xz(1-c)-ys	yz(1-c)+xs  zz(1-c)+c	 0  |
		  |					    |
		  (	 0	     0		 0	 1  )
	  Where	c = cos(angle),	s = sine(angle), and ||( x,y,z )|| = 1
	  (if not, the GL will normalize this vector).

	  If the matrix	mode is	either GL_MODELVIEW or GL_PROJECTION,
	  all objects drawn after glRotate is called are rotated.  Use
	  glPushMatrix and glPopMatrix to save and restore the
	  unrotated coordinate system.
		*/

	/**
	* Sets only the 3x3 subset of the matrix.
	* The rest of the matrix is not touched.
	*/
	
	final void setRotationMatrix3x3( T angle, VectorFixed!(T, 3) normal)
	{
		assert( columnCount >= 3, "Your matrix is not big enough columnwise");
		assert( rowCount >= 3,"Your matrix is not big enough rowwise");
		version (Tango)
		{
			T s = sin(angle);
			T c = cos(angle);
		}
		else	//Phobos
		{
			T s = std.math.sin(angle);
			T c = std.math.cos(angle);
		}
		//Debug.Assert(Vector3Double.Norm(normal)==1f,"Parameter normal is not normalized");
		T xx = normal[0] * normal[0];
		T yy = normal[1] * normal[1];
		T zz = normal[2] * normal[2];
		T xy = normal[0] * normal[1];
		T yz = normal[1] * normal[2];
		T xz = normal[0]* normal[2];
		T xs = normal[0] * s;
		T ys = normal[1] * s;
		T zs = normal[2] * s;
		T oneminusc = 1.0 - c;
		
		this[0,0] = (xx*oneminusc) + c; this[0,1] = (oneminusc * xy) - zs;  this[0,2] = (oneminusc * xz) + ys;
		this[1,0] = (xy*oneminusc) + zs;this[1,1] = (oneminusc * yy) + c;   this[1,2] = (oneminusc * yz) - xs;
		this[2,0] = (xz*oneminusc) - ys;this[2,1] = (oneminusc * yz) + xs;  this[2,2] = (oneminusc * zz) + c;
	}
	
	final void setRotationMatrix4x4( T angle, VectorFixed!(T, 3) normal)
	{
		assert( columnCount >= 4, "Your matrix is not big enough columnwise");
		assert( rowCount >= 4,"Your matrix is not big enough rowwise");
		setRotationMatrix3x3(angle, normal);
                                              this[0,3] = 0;	//translation X
                                              this[1,3] = 0;	//Translation Y
                                              this[2,3] = 0;  //Translation z;
		this[3,0] = 0;this[3,1] = 0;this[3,2] = 0;this[3,3] = 1;
	}
	
	
	/*
	
	http://www.opengl.org/documentation/specs/man_pages/hardcopy/GL/html/gl/frustum.html
			 (					   )
		   |  __________			   |
		   |  right-left       0       A       0   |
		   |					   |
		   |	  0	  __________   B       0   |
		   |		  top-bottom		   |
		   |	  0	       0       C       D   |
		   |					   |
		   |	  0	       0       -1      0   |
		   (					   )

				     __________
				 A = right-left
				     __________
				 B = top-bottom
				     __________
				C = -zFar-zNear
				    ____________
			       D = - zFar-zNear
	*/
	
	//	C and D are the same???????
	
	/**
	* Sets the frustrum Matrix in the 4x4 top left area of the matrix.
	*
	* Params:
	*  left               = left coordinate. X axis points to the right.
	*  right              = right coordinate. X axis points to the right.
	*  bottom             = bottom coordinate. Y axis points upwards.
	*  top                = top coordinate. Y axis points upwards.
	*  near               = near plane. make it negative. Z axis pokes your eye.
	*  far                = far plane. make it negative. Z axis pokes your eye.
	*/ 
	
	final void setFrustumMatrix4x4( T left, T right, T bottom, T top, T near, T far)
	{		
		assert( columnCount >= 4, "Your matrix is not big enough columnwise");
		assert( rowCount >= 4,"Your matrix is not big enough rowwise");
			
		this[0,0] =1.0/(right-left);this[0,1] =0;this[0,2] = 1.0/(right-left);this[0,3] =0;
		this[1,0] =0;this[1,1] =1.0/(top-bottom);this[1,2] = 1.0/(top-bottom);this[1,3] =0;
		this[2,0] =0;this[2,1] =0;this[2,2] =1.0/(-far-near);this[2,3] =1.0/(-far-near);
		this[3,0] =0;this[3,1] =0;this[3,2] =-1.0;this[3,3] =0;
	}
	
	
	/*
		this[0,0] =0;this[0,1] =;this[0,2] =;this[0,3] =;
		this[1,0] =0;this[1,1] =;this[1,2] =;this[1,3] =;
		this[2,0] =0;this[2,1] =;this[2,2] =;this[2,3] =;
		this[3,0] =0;this[3,1] =;this[3,2] =;this[3,3] =;
	*/
	
/*
	
   (					   )
		   |__________	    0		0	t  |
		   |right-left				 x |
		   |		__________		   |
		   |	0	top-bottom	0	ty |
		   |					   |
		   |	0	    0	    __________	t  |
		   |			    zFar-zNear	 z |
		   |	0	    0		0	1  |
		   (					   )

	  where
				t  = -__________
				 x    right-left
				t  = -__________
				 y    top-bottom
				t  = -__________
				 z    zFar-zNear
				 
 */
 
	/**
	* Sets the orthographic projection matrix. Only the 4x4 area.
	*
	* Params:
	*  left               = left coordinate. X axis points to the right.
	*  right              = right coordinate. X axis points to the right.
	*  bottom             = bottom coordinate. Y axis points upwards.
	*  top                = top coordinate. Y axis points upwards.
	*  near               = near plane. Make it positive. Z axis pokes your eye.
	*  far                = far plane. Make it negative. Z axis pokes your eye.
	*/ 
	
	final void setOrthoMatrix4x4(T left, T right, T bottom, T top, T near, T far)
	{
		assert( columnCount >= 4, "Your matrix is not big enough columnwise");
		assert( rowCount >= 4,"Your matrix is not big enough rowwise");
		
		T width = right - left;
		T height = top - bottom;
		T distance = far-near;

		this[0,0] =1.0/(width);this[0,1] =0;  this[0,2] =0;     this[0,3] = -1.0/width ;
		this[1,0] =0;this[1,1] =1.0/height;   this[1,2] =0;     this[1,3] = -1.0/height;
		this[2,0] =0;this[2,1] =0.0;this[2,2] =1.0/(distance);  this[2,3] = -1.0/(distance);
		this[3,0] =0;this[3,1] =0.0;this[3,2] =0.0;this[3,3] =1.0;			
	}
	
	
	/**
	*	Set the rotation around the X axis
	* Does not touch the rest of the matrix.
	*/
	
	final void setRotationXMatrix3x3(T angle)
	{
		assert( columnCount >= 3, "Your matrix is not big enough columnwise");
		assert( rowCount >= 3,"Your matrix is not big enough rowwise");
		T s = sin(angle);
		T c = cos(angle);
		
		this[0,0]=1;		this[0,1]=0;		this[0,2]=0;
		this[1,0]=0;		this[1,1]=c;		this[1,2]=-s;
		this[2,0]=0;		this[2,1]=s;		this[2,2]=c;		
	}
	
	/**
	*	Set the rotation around the Y axis
	* Does not touch the rest of the matrix.
	*/
	
	final void setRotationYMatrix3x3(T angle)
	{
		assert( columnCount >= 3, "Your matrix is not big enough columnwise");
		assert( rowCount >= 3,"Your matrix is not big enough rowwise");
		T s = sin(angle);
		T c = cos(angle);
		
		this[0,0]=c;		this[0,1]=0;		this[0,2]=s;
		this[1,0]=0;		this[1,1]=1;		this[1,2]=0;
		this[2,0]=-s;		this[2,1]=0;		this[2,2]=c;		
	}
	
	/**
	*	Set the rotation around the Z axis
	* Does not touch the rest of the matrix.
	*/

	final void setRotationZMatrix3x3(T angle)
	{
		assert( columnCount >= 3, "Your matrix is not big enough columnwise");
		assert( rowCount >= 3,"Your matrix is not big enough rowwise");
		T s = sin(angle);
		T c = cos(angle);	
		this[0,0]=c;		this[0,1]=-s;		this[0,2]=0;
		this[1,0]=s;		this[1,1]=c;		this[1,2]=0;
		this[2,0]=0;		this[2,1]=0;		this[2,2]=1;
	}
	

	
	/**
	*	Sets the translation matrix.
	* Does not touch the rest of the matrix.
	*/
	
	final void setTranslationMatrix3x3(T x, T y)
	{
		assert( columnCount >= 3, "Your matrix is not big enough columnwise");
		assert( rowCount >= 3,"Your matrix is not big enough rowwise");
		this[0,0]=1;		this[0,1]=0;		this[0,2]=x;
		this[1,0]=0;		this[1,1]=1;		this[1,2]=y;
		this[2,0]=0;		this[2,1]=0;		this[2,2]=1;
	}
	

	final void setTranslationMatrix4x4(T x, T y, T z)
	{
		assert( columnCount >= 4);
		assert( rowCount >= 4);
		this[0,0]=1;	this[0,1]=0;		this[0,2]=0;	this[0,3]= x;
		this[1,0]=0;	this[1,1]=1;		this[1,2]=0;	this[1,3]= y;
		this[2,0]=0;	this[2,1]=0;		this[2,2]=1;	this[2,3]= z;
		this[3,0]=0;	this[3,1]=0;		this[3,2]=1;	this[3,3]=1;
	}	
	
	/**
	*	Sets the scaling matrix.
	* Does not touch the rest of the matrix.
	*/
	
	final void setScalingMatrix3x3(T xScale, T yScale, T zScale)
	{
		assert( columnCount >= 3, "Your matrix is not big enough columnwise");
		assert( rowCount >= 3,"Your matrix is not big enough rowwise");
		this[0,0]=xScale;		this[0,1]=0;		this[0,2]=0;
		this[1,0]=0;		this[1,1]=yScale;		this[1,2]=0;
		this[2,0]=0;		this[2,1]=0;		this[2,2]=zScale;
	}	
}




/*******************************************************************************

	Provides a column-major rectangular matrix.

*******************************************************************************/


/*

class Matrix(T):Matrix!(T)
{
public:
	this(uint rowCount, uint columnCount, T [] data)
	{
		assert(rowCount * columnCount == data.length);
		super(rowCount, columnCount, data);
	}

	this(uint rowCount, uint columnCount)
	{
		super(rowCount, columnCount);
	}
}

*/


/*******************************************************************************

	Provides a column-major rectangular matrix with a fixed size.

*******************************************************************************/


class MatrixSized(T, uint ROWCOUNT, uint COLUMNCOUNT):Matrix!(T)
{
public:
	this(T [] data)
	{
		super(ROWCOUNT, COLUMNCOUNT, data);
	}

	this()
	{
		super(ROWCOUNT, COLUMNCOUNT);
	}
	
        /***********************************************************************

	Multiplication of a matrix with a vector, returns a vector.
	
        ***********************************************************************/

	final VectorFixed!(T, ROWCOUNT) opMul( VectorFixed!(T, COLUMNCOUNT) right)
	{
		VectorFixed!(T, ROWCOUNT) result;
		T intermediate;
		for (uint r = 0; r < this.rowCount(); r++)
		{
			intermediate = 0;
			for (uint c = 0; c < this.columnCount(); c++)
				intermediate +=  this[r,c] * right[c];
			result[r] = intermediate;
		}
		return result;
		
	}

        /***********************************************************************

	Returns a new Transposed Matrix.
	
        ***********************************************************************/

	final MatrixSized!(T, COLUMNCOUNT, ROWCOUNT) newTransposeMatrix()
	{
		MatrixSized!(T, COLUMNCOUNT, ROWCOUNT) result = new MatrixSized!(T, COLUMNCOUNT, ROWCOUNT);
		for (uint r = 0; r < this.rowCount; r++)
			for (uint c = 0; c < columnCount; c++)
				result[c,r] = this[r,c];
		return result;
	}
	

	static if (ROWCOUNT == COLUMNCOUNT)
	{


        /***********************************************************************

	Multiplication of two squared matrices of the same dimensions, where
	rowCount == columnCount. 
	
        ***********************************************************************/

		final MatrixSized opMul(MatrixSized right)
		{
			assert( this.columnCount() == right.rowCount(), "Incorrect matrix dimensions for multiplication");
			MatrixSized result = new MatrixSized;
			T intermediate;
			for (uint r = 0; r < this.rowCount(); r++)
				for (uint c = 0; c < right.columnCount; c++)
				{
					intermediate = 0;
					for (uint i = 0; i < this.columnCount(); i++)
						intermediate += this.opIndex(r,i) * right[i,c];
					result[r,c] = intermediate;
				}		
			return result;
		}		
	}
}


//-----------------------------------------
//	MatrixDiagonal (square matrix)
//-----------------------------------------


	/**
	* Matrix where only the diagonal elements are stored.
	*/


class MatrixDiagonal(T): MatrixBase!(T)
{
protected:
	T [] _data;
public:

	this (uint rowCount, uint columnCount, T [] data)
	{
		uint minimum = rowCount < columnCount? rowCount: columnCount;		
		assert( data.length == minimum );
		super(rowCount, columnCount);
		_data = data;
	}


	/**
	* This is the constructor of the matrix. It does not have to be a square matrix.
	*/
	
	this (uint rowCount, uint columnCount)
	{
		super(rowCount, columnCount);
		uint minimum = rowCount < columnCount? rowCount: columnCount;
		_data = new T[minimum];
	}
	
	/**
	* Access to the internal data.
	*/

	T [] getData()
	{
		return _data;
	}

	override Matrix!(T) opAdd( MatrixBase!(T) right)
	{
		return super.opAdd(right);
	}

	
	MatrixDiagonal opAdd(MatrixDiagonal right)
	{

		assert(this.rowCount == right.rowCount() && this.columnCount() == right.columnCount(),
			 "dimensions do not match");
		MatrixDiagonal result = new MatrixDiagonal(rowCount, columnCount());
		
		for (int i = 0; i < this.getData().length; i++)
			result._data[i] = this._data[i] + right._data[i];
		return result;
	}

	override T opIndex(uint row, uint column)
	{
		assert( row >= 0 && row < rowCount() && column >= 0 && column < columnCount() , "index out of bound");
		if (row == column) return _data[row];
		return 0;
	}

	override T opIndexAssign( T value, uint row, uint column)
	{
		assert( row >= 0 && row < rowCount() && column >= 0 && column < columnCount() , "index out of bound");
		_data[row] = value;
		return value;
	}


	/**
	* 	Get the smallest dimension of row or column
	*/

	uint getMinimumDimension()
	{
		return rowCount() < columnCount()? rowCount(): columnCount();
	}

}




/*******************************************************************************

	A Matrix class that stores the upper right triangular data in a
	culumn-major packed array. This is a square matrix.

	0 1 3

	. 2 4

	. . 5

	References:
	$(LINK http://netlib.org/lapack/lug/node123.html)

*******************************************************************************/



class MatrixUpperTriangular(T):MatrixBase!(T)
{
protected:
	T [] _data;
public:
        /***********************************************************************

		Provides a constructor for a packed storage array triangular
		matrix.

		Example:
		---
		auto triang1 = new MatrixUpperTriangular!(float)(3, [1,2,3,4,5,6]);
		---

        ***********************************************************************/

	this(uint size, T [] data)
	{
		assert( size > 0);
		super(size, size);
		assert( size*(size+1)/2 == data.length);
		_data = data;
	}

	this(uint size)
	{
		assert( size > 0);
		super(size, size);
		_data = new T[size*(size+1)/2];
	}

        /***********************************************************************

		Returns the values elements of the triangular matrix.

        ***********************************************************************/


	override T opIndex(uint row, uint column)
	{
		assert(row >= 0, "row");
		assert(column >= 0, "column");
		assert(row < rowCount, "row");
		assert(column < columnCount, "column");
		if (row <= column)
			return _data[row + column*(column+cast(uint)1)/cast(uint)2];
		else
			return 0;
	}

        /*

		normalDist(x) = 1/$(SQRT) &pi; $(INTEGRAL -$(INFINITY), x) exp( - $(POWER t, 2)/2) dt
 	    		= 0.5 + 0.5 * erf(x/sqrt(2))
		 	    = 0.5 * erfc(- x/sqrt(2))
        ***********************************************************************/


	override T opIndexAssign(T value, uint row, uint column)
	{
		assert(row >= 0, "row");
		assert(column >= 0, "column");
		assert(row < rowCount, "row");
		assert(column < columnCount, "column");

		assert(row <= column, "row > column");
		_data[row + column*(column+ cast(uint)1)/ cast(uint)2 ] = value;
		return value;
	}
}



/*******************************************************************************

	A Matrix class that stores the lower left triangular data in a
	culumn-major packed array. This is a square matrix.

	0 . .

	1 3 .

	2 4 5

	References:
	$(LINK http://netlib.org/lapack/lug/node123.html)

*******************************************************************************/


class MatrixLowerTriangular(T):MatrixBase!(T)
{
protected:
	T [] _data;
public:

        /***********************************************************************

		Provides a constructor for a packed storage array triangular
		matrix.

		Example:
		---
		auto triang1 = new MatrixLowerTriangular!(float)(3, [1,2,3,4,5,6]);
		---

        ***********************************************************************/

	this(uint size, T [] data)
	{
		assert(size > 0);
		super(size, size);
		assert( size * (size + 1) / 2 == data.length);
		_data = data;
	}

        /***********************************************************************
        ***********************************************************************/

	this(uint size)
	{
		assert(size > 0);
		super(size, size);
		_data = new T[size * (size + 1) / 2];
	}

        /***********************************************************************

		Returns the values elements of the triangular matrix.

        ***********************************************************************/


	override T opIndex(uint row, uint column)
	{
		assert(row >= 0, "row");
		assert(column >= 0, "column");
		assert(row < rowCount, "row");
		assert(column < columnCount, "column");
		if (row >= column)
			return _data[row + rowCount * column - column * (column + 1) / 2];
		else
			return 0;
	}

        /***********************************************************************

		Assign values at a certain index. 

		row >= column must be valid

        ***********************************************************************/
	
	override T opIndexAssign(T value, uint row, uint column)
	{
		assert(row >= 0, "row");
		assert(column >= 0, "column");
		assert(row < rowCount, "row");
		assert(column < columnCount, "column");

		assert(row >= column, "column > row");
		//return _data[row + (2*size-column)*(column -1) /2 ] = value;
		_data[ row + rowCount  * column - column*(column + 1) / 2] = value;
		return value;
	}
}



//	http://www.netlib.org/lapack/lug/node121.html  "matrix storage schemes. Lapack"


/*
class MatrixUnsymmetricTriDiagonal(T):MatrixBase!(T)
{	
}
*/

/*
class MatrixSymmetricTriDiagonal(T):MatrixBase!(T)
{
}
*/

/*
class MatrixBand:MatrixBase!(T)
{
}
*/

/*
	Sparse matrices use an index table of the location of the elements.

class MatrixSparse(T):MatrixBase!(T)
{
}
*/


alias Matrix!(double) MatrixD;
alias Matrix!(float) MatrixF;


unittest {

	//	Sum Addition, Substraction, Multiplication, Saling tests
	Matrix!(float) mat1 = new Matrix!(float)(2,2,[1,2,3,4]);
	Matrix!(float) mat2 = new Matrix!(float)(2,2,[1,2,3,4]);
	Matrix!(float) mat3 = mat1 + mat2;
	assert(mat3[0,0] == 2 && mat3[1,0] ==4 && mat3[0,1] == 6 && mat3[1,1] == 8, "sum");
	mat3 = mat1 - mat2;
	assert(mat3[0,0] == 0 && mat3[1,0] ==0 && mat3[0,1] == 0 && mat3[1,1] == 0);
	mat3 = mat1 * mat2;
	assert(mat3[0,0] == 7 && mat3[1,0] ==10 && mat3[0,1] == 15 && mat3[1,1] == 22);

	auto mat1_2x2 = new MatrixSized!(float, cast(uint) 2, cast(uint) 2)([1,2,3,4]);
	auto mat2_2x2 = new MatrixSized!(float, cast(uint) 2, cast(uint) 2)([1,2,3,4]);
	auto mat3_2x2 = mat1_2x2 + mat2_2x2;

	assert(mat3_2x2[0,0] == 2 && mat3_2x2[1,0] ==4 && mat3_2x2[0,1] == 6 && mat3_2x2[1,1] == 8);
	mat3_2x2 = mat1_2x2 - mat2_2x2;
	assert(mat3_2x2[0,0] == 0 && mat3_2x2[1,0] ==0 && mat3_2x2[0,1] == 0 && mat3_2x2[1,1] == 0);
	mat3_2x2 = mat1_2x2 * mat2_2x2;	
	assert(mat3_2x2[0,0] == 7 && mat3_2x2[1,0] ==10 && mat3_2x2[0,1] == 15 && mat3_2x2[1,1] == 22);

	assert( mat3 == mat3_2x2 );
	mat3[0,1] = 1;
	assert( mat3 != mat3_2x2 );

	auto triangu1 = new MatrixUpperTriangular!(float)(2,[1,2,3]);
	auto triangu2 = new MatrixUpperTriangular!(float)(2,[1,2,3]);
	auto trianguSum = triangu1 + triangu2;
	assert( trianguSum == new Matrix!(float) (2,2,[1,0,2,3]) + new Matrix!(float) (2,2,[1,0,2,3]));
	auto trianguMul1 = triangu1 * triangu2;
	assert( trianguMul1 == new Matrix!(float) (2,2,[1,0,2,3]) * new Matrix!(float) (2,2,[1,0,2,3]));

	auto triangl1 = new MatrixLowerTriangular!(float)(2,[1,2,3]);
	auto triangl2 = new MatrixLowerTriangular!(float)(2,[1,2,3]);
	auto trianglSum = triangl1 + triangl2;
	assert( trianglSum == new Matrix!(float) (2,2,[1,2,0,3]) + new Matrix!(float) (2,2,[1,2,0,3]));
	auto trianglMul1 = triangl1 * triangl2;
	assert( trianglMul1 == new Matrix!(float) (2,2,[1,2,0,3]) * new Matrix!(float) (2,2,[1,2,0,3]));

	auto matView1 = new Matrix!(float)(2,2);
	matView1.copyFrom( mat1 );
	assert( matView1 == mat1 );
	mat2.copyFrom(mat1);
	assert( mat1 == mat2 );

	auto trans1 = triangu1.newTransposeMatrix();
	assert( trans1 == triangl1 );

	//	OpenGL routines tests.

	mat1.setIdentity();
	auto a = VectorFixed!(float, cast(uint) 2)(1,2);
	auto b = mat1.mul2x2(a);

	Stdout(mat1).newline;
	Stdout(a).newline;
	Stdout(b).newline;
	assert( b[0] == 1 && b[1] == 2);
	assert( b == a);
	
	mat1.setScalingMatrix2x2(2, 3);
	b = mat1.mul2x2(a);
	assert( b[0] == 2 && b[1] == 6);



/*
void setIdentity()
void setScalingMatrix2x2(T xScale, T yScale)
void setRotationMatrix2x2(T angle)
void setRotationMatrix3x3( T angle, VectorFixed!(T, 3) normal)
void setRotationMatrix4x4( T angle, VectorFixed!(T, 3) normal)
void setFrustumMatrix4x4( T left, T right, T bottom, T top, T near, T far)
void setOrthoMatrix4x4(T left, T right, T bottom, T top, T near, T far)
void setRotationXMatrix3x3(T angle)
void setRotationYMatrix3x3(T angle)
void setRotationZMatrix3x3(T angle)
void setTranslationMatrix3x3(T x, T y)
void setScalingMatrix3x3(T xScale, T yScale, T zScale)
*/

		

}








