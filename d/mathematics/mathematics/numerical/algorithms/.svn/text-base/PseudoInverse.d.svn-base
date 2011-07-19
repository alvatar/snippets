/*
Copyright (c) 2006 Peter Van Isacker (sclytrack@pi.be)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



	Not tested not working.
*/


module mathematics.numerical.algorithms.PseudoInverse;

//private import std.stdio;
//private import std.math;
private import mathematics.numerical.Vector;
private import mathematics.numerical.Matrix;
private import mathematics.numerical.algorithms.SingularValueDecomposition;




//	http://en.wikipedia.org/wiki/Singular_value_decomposition#Pseudoinverse

template TMatrixRectangular(T)
{
	//A^"+" = V S^(-1) U^"*"
	
	/**
	*	Finds the pseudoinverse of a matrix. Warning this routines change the values of the input matrix.
	*/
	MatrixRectangular!(T) pseudoInverse( MatrixRectangular!(T) m )
	{
		static assert(is(T==float) || is(T==double), "Only float or double supported in pseudoinverse");
		SingularValueDecomposition!(T) svd = new SingularValueDecomposition!(T)(m);
		svd.decompose();
		DiagonalMatrix sMatrix = svd.sMatrix();
		for (int x = 0; x < sMatrix.rowCount; x++)
			sMatrix[x,x] = 1 / sMatrix[x,x];
		return svd.vTMatrix.TransposeMatrix() * sMatrix * svd.uMatrix.TransposeMatrix();
	}
}
