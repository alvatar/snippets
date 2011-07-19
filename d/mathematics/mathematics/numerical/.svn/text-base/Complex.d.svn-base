	/**
 	* Complex structure.
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


//Note D supports complex numbers as cre.

module mathematics.numerical.Complex;


version (Tango)
{
	private import tango.math.Math, tango.io.Stdout;

}
else {	//Phobos
	private import std.stdio;
	private import std.math;
}

//This is passed on to lapack, real is a type in D.


	/**
	* Complex structure.
	*/

struct Complex(T)
{
public:
	/**
	* The real portion.
	*/
	
	T re;
	
	/**
	* The imaginary portion.
	*/
	
	T im;
	
	/**
	* Easy way to construct.
	*/

	static Complex opCall( T re, T im)
	{
		Complex result;
		result.re = re;
		result.im = im;
		return result;
	}

	Complex opAdd( Complex right)
	{
		Complex result;
		result.re = this.re + right.re;
		result.im = this.im + right.im;
		return result;
	}

	Complex opAddAssign( Complex right)
	{
		this.re += right.re;
		this.im += right.im;
		return *this;		
	}

	Complex opSub( Complex right)
	{
		Complex result;
		result.re = this.re - right.re;
		result.im = this.im - right.im;
		return result;
	}

	Complex opSubAssign( Complex right)
	{
		this.re -= right.re;
		this.im -= right.im;
		return *this;		
	}

	Complex opMul(T scale)
	{
		Complex result;
		result.re = this.re * scale;
		result.im = this.im * scale;
		return result;
	}

	Complex opMul_r(T scale)
	{
		Complex result;
		result.re = this.re * scale;
		result.im = this.im * scale;
		return result;
	}

	Complex opMul(Complex * right)
	{
		Complex result;
		result.re= (this.re*right.re) - (this.im*right.im);
		result.im= (this.re*right.im) + (this.im*right.re);
		return result;
	}
	
	/**
	* Returns the norm of the complex number.
	*/

	T norm()
	{
		return sqrt( this.re * this.re + this.im * this.im );
	}
}

