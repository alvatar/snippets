/*
Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/



module mathematics.numerical.MatrixFixed;

private import mathematics.numerical.VectorFixed;


version (Tango)
{
	private import tango.math.Math;
	private import tango.io.Stdout;
	private import Integer = tango.text.convert.Integer;
	private import Floating = tango.text.convert.Float;
}
else	//Phobos
{
	private import std.math;
	private import std.stdio;
}


//	TODO: speed things up by passing by reference.
//	OpenGL matrices are column major. OpenGL spec pg 43


//	I don't know why but MatrixFixed!(T, uint ROWCOUNT, uint COLUMNCOUNT) doesn't work too well.

//I'm thinking of depricating this (any complaints?)

//TODO: Passing by "const ref"



struct MatrixFixed(T, uint ROWCOUNT, uint COLUMNCOUNT)
{
private:
	T [COLUMNCOUNT*ROWCOUNT] _data;
public:

	MatrixFixed opAdd(ref MatrixFixed right)
	{
		MatrixFixed result;
		for (uint x = 0; x < COLUMNCOUNT*ROWCOUNT; x++)
			result._data[x] = _data[x] + right._data[x];
		return result;
	}

	MatrixFixed opAddAssign(ref MatrixFixed right)
	{
		for (uint x = 0; x < COLUMNCOUNT*ROWCOUNT; x++)
			_data[x]+=right._data[x];
		return *this;
	}

	MatrixFixed opSub(ref MatrixFixed right)
	{
		MatrixFixed result;
		for (uint x = 0; x < COLUMNCOUNT*ROWCOUNT; x++)
			result._data[x] = _data[x] - right._data[x];
		return result;
	}

	MatrixFixed opSubAssign(ref MatrixFixed right)
	{
		for (uint x = 0; x < COLUMNCOUNT*ROWCOUNT; x++)
			_data[x]-= right._data[x];
		return *this;
	}

	MatrixFixed scale( T value )
	{
		MatrixFixed result;
		for (uint x = 0; x < COLUMNCOUNT*ROWCOUNT; x++)
			result._data[x] = _data[x] * value;
		return result;
	}

	MatrixFixed scaleAssign( T value )
	{
		for (uint x = 0; x < COLUMNCOUNT*ROWCOUNT; x++)
			_data[x] *= value;
		return *this;
	}	

	MatrixFixed opMul( T scale )
	{
		MatrixFixed result;
		for (uint x = 0; x < COLUMNCOUNT*ROWCOUNT; x++)
			result._data[x] = _data[x] * scale;
		return result;
	}

	MatrixFixed opMul_r( T scale )
	{
		MatrixFixed result;
		for (uint x = 0; x < COLUMNCOUNT*ROWCOUNT; x++)
			result._data[x] = _data[x] * scale;
		return result;
	}

	
	MatrixFixed opMulAssign( T scale )
	{
		for (uint x = 0; x < COLUMNCOUNT*ROWCOUNT; x++)
			_data[x]*=scale;
		return *this;
	}

	MatrixFixed opNeg()
	{
		MatrixFixed result;
		for (uint x = 0; x < COLUMNCOUNT*ROWCOUNT; x++)
			result._data[x] = - _data[x];
		return result;		
	}

	T opIndex(uint r, uint c)
	{
		assert( r >= 0 && r < ROWCOUNT, "row index out of bounds");
		assert( c >= 0 && c < COLUMNCOUNT, "column index out of bounds");
		return _data[c*ROWCOUNT+r];
	}

	void opIndexAssign( T value, uint r, uint c)
	{
		assert( r >= 0 && r < ROWCOUNT, "row index out of bounds");
		assert( c >= 0 && c < COLUMNCOUNT, "column index out of bounds");
		_data[c*ROWCOUNT+r] = value;
	}

	int opEquals( ref MatrixFixed right )
	{
		for (uint x = 0; x < COLUMNCOUNT*ROWCOUNT; x++)
			if (_data[x] != right._data[x] ) return false;		
		return true;
	}


	//This code below is correct, I know the compiler goes completely nuts sometimes



	VectorFixed!(T, ROWCOUNT) opMul( VectorFixed!(T, COLUMNCOUNT) right )
	{
		VectorFixed!(T, ROWCOUNT) result;
		T sum;
		for (uint r = 0; r < ROWCOUNT; r++)
		{
			sum = 0;
			for (uint c = 0; c < COLUMNCOUNT; c++)
				sum += opIndex(r,c)*right[c];
			result[r] = sum;
		}
		return result;
	}

	static if (ROWCOUNT==COLUMNCOUNT)
	{
		static MatrixFixed transposeMatrix(ref MatrixFixed m)
		{
			MatrixFixed result;
			for (uint r = 0; r < ROWCOUNT; r++)
				for (uint c = 0; c < COLUMNCOUNT; c++)
					result[r,c] = m[c,r];
			return result;
		}


		MatrixFixed opMul(ref MatrixFixed right )
		{
			MatrixFixed result;
			for (uint r = 0; r < ROWCOUNT; r++)
				for (uint c = 0; c < COLUMNCOUNT; c++)
				{
					T sum = 0;
					for (uint i = 0; i < ROWCOUNT; i++)
						sum += opIndex(r,i) * right[i,c];
					result[r,c] = sum;
				}
			return result;		
		}
	
		MatrixFixed opMulAssign(ref MatrixFixed right )
		{
			MatrixFixed result;
			for (uint r = 0; r < ROWCOUNT; r++)
				for (uint c = 0; c < COLUMNCOUNT; c++)
				{
					T sum = 0;
					for (uint i = 0; i < ROWCOUNT; i++)
						sum += opIndex(r,i) * right[i,c];
					result[r,c] = sum;
				}
			*this = result;	
			return *this;
		}



	}	//static if( ROWCOUNT==COLUMNCOUNT)

	static if (ROWCOUNT == 2 && COLUMNCOUNT == 2)
	{
		static MatrixFixed opCall(
				T m00, T m01,
				T m10, T m11
				)
	
		{
			MatrixFixed result;
			result[0,0] = m00;result[0,1] = m01;
			result[1,0] = m10;result[1,1] = m11;
			return result;
		}

		static MatrixFixed identityMatrix()
		{
			return MatrixFixed(
				1,0,
				0,1
			);
		}

		static MatrixFixed rotationMatrix(T angle)
		{
			version (Tango)
			{
			T s = sin(angle);
			T c = cos(angle);
			} else
			{
			T s = std.math.sin(angle);
			T c = std.math.cos(angle);
			}
			return  MatrixFixed(
				c,-s,
				s,c);
		}

		static MatrixFixed scalingMatrix(VectorFixed!(T, cast(uint) 2) scale)
		{
			return MatrixFixed(
				scale[0],0,
				0,scale[1]);
		}
	}

	static if( ROWCOUNT == 3 && COLUMNCOUNT == 3 )
	{
		static MatrixFixed opCall(
				T m00, T m01, T m02, 
				T m10, T m11, T m12, 
				T m20, T m21, T m22
				)
	
		{
			MatrixFixed result;
			result[0,0] = m00;result[0,1] = m01;result[0,2]=m02;	
			result[1,0] = m10;result[1,1] = m11;result[1,2]=m12;	
			result[2,0] = m20;result[2,1] = m21;result[2,2]=m22;	
			return result;
		}

		static MatrixFixed identityMatrix()
		{
			return MatrixFixed(
				1,0,0,
				0,1,0,
				0,0,1
			);
		}

		static MatrixFixed rotationXMatrix( T angle )
		{
			T s = sin(angle);
			T c = cos(angle);
			return MatrixFixed(
				1,0,0,
				0,c,-s,
				0,s,c
				);
		}
	
		static MatrixFixed rotationYMatrix(T angle)
		{
			T s = sin(angle);
			T c = cos(angle);
			return MatrixFixed(
				c,0,s,
				0,1,0,
				-s,0,c
				);
		}

		static MatrixFixed rotationZMatrix(T angle)
		{
			T s = sin(angle);
			T c = cos(angle);
			return  MatrixFixed(
				c,-s,0,
				s,c,0,
				0,0,1);
		}
	
		//	Mesa3D

		static MatrixFixed rotationMatrix(T angle,ref VectorFixed!(T, cast(uint) 3) normal)
		{
			T s = sin(angle);
			T c = cos(angle);
	
			//Debug.Assert(Vector3Double.Norm(normal)==1f,"Parameter normal is not normalized");
	
			T xx = normal[0] * normal[0];
			T yy = normal[1] * normal[1];
			T zz = normal[2] * normal[2];
			T xy = normal[0] * normal[1];
			T yz = normal[1] * normal[2];
			T zx = normal[2] * normal[0];
			T xs = normal[0] * s;
			T ys = normal[1] * s;
			T zs = normal[2] * s;
			T one_c = 1.0 - c;
	
			return MatrixFixed(
				(one_c * xx) + c, (one_c * xy) - zs, (one_c * zx) + ys,
				(one_c * xy) + zs,(one_c * yy) + c,(one_c * yz) - xs,
				(one_c * zx) - ys, (one_c * yz) + xs,(one_c * zz) + c);
		}

		static MatrixFixed translationMatrix(T x, T y)
		{
			return MatrixFixed(
					1,0,x,
					0,1,y,
					0,0,1);
		}

		static MatrixFixed scalingMatrix(VectorFixed!(T,cast(uint) 3) scale)
		{
			return MatrixFixed(
				scale[0],0,0,
				0,scale[1],0,
				0,0,scale[2]);			
		}
	}


	static if (ROWCOUNT==4 && COLUMNCOUNT==4)		//ROWCOUNT==4 COLUMNCOUNT=4
	{

		static MatrixFixed opCall(
				T m00, T m01, T m02, T m03,  
				T m10, T m11, T m12, T m13,  
				T m20, T m21, T m22, T m23,
				T m30, T m31, T m32, T m33
				)
	
		{
			MatrixFixed result;
			result[0,0] = m00;result[0,1] = m01;result[0,2]=m02;result[0,3]=m03;
			result[1,0] = m10;result[1,1] = m11;result[1,2]=m12;result[1,3]=m13;
			result[2,0] = m20;result[2,1] = m21;result[2,2]=m22;result[2,3]=m23;
			result[3,0] = m30;result[3,1] = m31;result[3,2]=m32;result[3,3]=m33;
			return result;
		}


		/**
		*	Warning, this uses only the 3x4 portion of the matrix to do the multiplication.
		*	Performs a multiplication on a (x,y,z,1)^T vector.
		*/

		VectorFixed!(T, ROWCOUNT-1) opMul(ref VectorFixed!(T, COLUMNCOUNT-1) right )
		{
			VectorFixed!(T, ROWCOUNT-1) result;
			T sum;
			for (uint r = 0; r < ROWCOUNT-1; r++)
			{
				sum = 0;
				for (uint c = 0; c < COLUMNCOUNT-1; c++)
					sum += opIndex(r,c)* right[c];
				sum+=opIndex(r, COLUMNCOUNT-1);
				result[r] = sum;
			}
			return result;
		}



		static MatrixFixed identityMatrix()
		{
			return MatrixFixed(
				1.0,0.0,0.0,0.0,
				0.0,1.0,0.0,0.0,
				0.0,0.0,1.0,0.0,
				0.0,0.0,0.0,1.0
			);
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

		static MatrixFixed rotationMatrix( T angle,ref VectorFixed!(T, cast(uint) 3) normal)
		{
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

			return MatrixFixed(
		                (xx*oneminusc) + c, (oneminusc * xy) - zs, (oneminusc * xz) + ys,	0.0,
		                (xy*oneminusc) + zs,(oneminusc * yy) + c,(oneminusc * yz) - xs,0.0,
		                (xz*oneminusc) - ys, (oneminusc * yz) + xs,(oneminusc * zz) + c,0.0,
				0.0,0.0,0.0,1.0);	
		}

		static MatrixFixed translationMatrix(ref VectorFixed!(T,cast(uint) 3) translation)
		{
			return MatrixFixed(
				1.0,0.0,0.0,translation[0],
				0.0,1.0,0.0,translation[1],
				0.0,0.0,1.0,translation[2],
				0.0,0.0,0.0,1.0
			);
		}

		static MatrixFixed scaleMatrix( VectorFixed!(T, cast(uint) 3) scale)
		{
			return MatrixFixed(
				scale[0],0.0,0.0,0.0,
				0.0,scale[1],0.0,0.0,
				0.0,0.0,scale[2],0.0,
				0.0,0.0,0.0,1.0
			);
	
		}

		//TODO: Test these routines (not certain)

		static MatrixFixed frustumMatrix(T l, T r, T t, T b, T n, T f)
		{
			T FmiN = f-n;
			T TmiB = t-b;
			T RmiL = r-l;
			T Nx2  = 2*n;
			return MatrixFixed(
				(Nx2)/RmiL, 0.0, (r+l)/RmiL, 0.0,
				0.0,(Nx2)/TmiB,(t+b)/TmiB,0.0,
				0.0,0.0,-(f+n)/(FmiN),(-Nx2*f)/(FmiN),
				0.0,0.0,-1.0,0.0);		
		}
		
		//TODO: Test these routines (not certain)

		static MatrixFixed inverseFrustum(T l, T r, T t, T b, T n, T f)
		{
			return MatrixFixed(
				(r-l)/(2*n),0.0,0.0,(r+l)/(2*n),
				0.0,(t-b)/(2*n),0.0,(t+b)/(2*n),
				0.0,0.0,0f,-1.0,
				0.0,0.0,-(f-n)/(2*f*n),(f+n) /(2*f*n));
		}

		//TODO: Test these routines (not certain)
		
		static MatrixFixed orthoMatrix(T l, T r, T t, T b, T n, T f)
		{
			return MatrixFixed(
				2.0/(r-l),0.0,0.0,(r+l)/(r-l),
				0.0,2.0/(t-b),0.0,(t+b)/(t-b),
				0.0,0.0, -2.0/(f-n),(f+n)/(f-n),
				0.0,0.0,0.0,1.0
				);
		}
		
		//TODO: Test these routines (not certain)
			
		static MatrixFixed inverseOrthoMatrix(T l, T r, T t, T b, T n, T f)
		{
			return MatrixFixed(
				(r-l)/2.0,0.0,0.0,(r+l)/2.0,
				0.0,(t-b)/2.0,0.0,(t+b)/2.0,
				0.0,0.0,(f-n)/-2.0,(n+f)/2.0,
				0.0,0.0,0.0,1.0);
		}

		static MatrixFixed lookAt(ref VectorFixed!(T,cast(uint)3) center, ref VectorFixed!(T,cast(uint)3) z,ref VectorFixed!(T,cast(uint)3) y)
		{
			z = z.normal();	
			VectorFixed!(T, cast(uint)3) x = z.vectorMul(y);
			x = x.normal();
			y=z.vectorMul(x);	//flipping of the y axis
			y = y.normal();
			MatrixFixed matrix = MatrixFixed(
				x[0], x[1], x[2], 0,
				y[0], y[1], y[2], 0,
				z[0], z[1], z[2], 0,
				0, 0, 0, 1);
			return matrix * translationMatrix(-center);
		}


	} //END   static if (ROWCOUNT == 4 && COLUMNCOUNT == 4)



	char [] toString()
	{
		char [] result = "Matrix: \n";
		for (uint r = 0; r < ROWCOUNT; r++)
		{
			for (uint c = 0; c < COLUMNCOUNT; c++)
				version (Tango)
				{
					result ~= tango.text.convert.Float.toString(opIndex(r,c)) ~ ", ";
				} else //Phobos
				{
					result ~= std.string.toString(opIndex(r,c)) ~ ", ";
				}
			result ~= "\n";
		}
		return result;
	}

	/*
	*	Returns the sum of the squared of the elements in the matrix.
	*	(Frobenius Norm)
	*/

	T squaredSumOfElements()
	{
		T result = 0;
		for (uint r = 0; r < ROWCOUNT; r++)
			for (uint c = 0; c < COLUMNCOUNT; c++)
				result += opIndex(r,c) * opIndex(r,c);
		return result;
	}

	/**
	*	Returns the sum of all the elements in the matrix.
	*/

	T sumOfElements()
	{
		T result = 0;
		for (uint r = 0; r < ROWCOUNT; r++)
			for (uint c = 0; c < COLUMNCOUNT; c++)
				result += opIndex(r,c);
		return result;
	}
		
}	//End struct MatrixFixed(T, int ROWCOUNT, int COLUMNCOUNT)




unittest {
	MatrixFixed!(double, cast(uint) 4, cast(uint) 4) a;
	MatrixFixed!(double, cast(uint) 4, cast(uint) 4) b;

}








