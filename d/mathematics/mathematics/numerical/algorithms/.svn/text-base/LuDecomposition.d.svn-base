	/**
 	* LU Decomposition.
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
	*
	*   
	*/

module mathematics.numerical.algorithms.LuDecomposition;



private
{
	import mathematics.numerical.Matrix;
	import mathematics.numerical.algorithms.lapack;
}

version (Tango)
{
	private import Integer	= tango.text.convert.Integer;
	private import tango.io.Stdout;
} else
{
	private import std.string;
}


	//NxN matrix Nx
	/**
	* Solves the equations A X = B via LuDecomposition.
	*
	* Params:
	*	aMatrix	= The A matrix. 
	*	bMatrix = The B matrix. Returns the solution in here.
	*	ipivot	= The representation of the pivot matrix
	*/



	int luDecomposition(T)(Matrix!(T) aMatrix, Matrix!(T) bMatrix, out int [] ipivot )
	{
		assert( aMatrix.rowCount() == aMatrix.columnCount(), "not a square matrix");
		assert( aMatrix.rowCount() == bMatrix.rowCount(), "first matrix must be nxn second nxnhrs");
		//Check if matrix is symmetric.

		int n = aMatrix.rowCount();
		int nrhs = bMatrix.columnCount();
		T [] a = aMatrix.getData();			//NxN matrix
		int lda = aMatrix.getAllocatedRowCount();
		T [] b = bMatrix.getData();						//NxNRHS
		int ldb = bMatrix.getAllocatedRowCount(); 
		ipivot = new int[n];
		int info = 0;

		static if (is(T==double))
		{
			dgesv_(&n, &nrhs, &a[0], &lda, &ipivot[0], &b[0], &ldb, &info);
		} else static if(is(T==float))
		{
			sgesv_(&n, &nrhs, &a[0], &lda, &ipivot[0], &b[0], &ldb, &info);
		} else
		{
			static assert(false, "float double supported");
		}

		version (Tango)
		{
			if (info < 0) throw new Exception("Error in parameter: " ~ Integer.toString(info));
		}
		else
		{
			if (info < 0) throw new Exception("Error in parameter: " ~ toString(info));
		}
		return info;
	}
	



unittest {
	Stdout("LU Decomposition").newline;
	Matrix!(double) aMatrix = new Matrix!(double)(2,2,[1,2,3,4]);
	Matrix!(double) bMatrix = new Matrix!(double)(2,2,[1,0,0,1]);
	Stdout("Input:").newline;
	Stdout( aMatrix ).newline;
	Stdout( bMatrix ).newline;
	Stdout("Output:").newline;
	int [] rowPivot;	
	int result = luDecomposition!(double) (aMatrix, bMatrix, rowPivot);
	if (result != 0) Stdout("An error has occured").newline;
	Stdout("resultCode:")(result).newline;
	Stdout( aMatrix ).newline;
	Stdout( bMatrix ).newline;	
	Stdout( rowPivot).newline;
}

/*

     sgesv - compute the solution to  a  real  system  of  linear
     equations  A * X = B,


SYNOPSIS

     SUBROUTINE SGESV(N, NRHS, A, LDA, IPIVOT, B, LDB, INFO)

     INTEGER N, NRHS, LDA, LDB, INFO
     INTEGER IPIVOT(*)
     REAL A(LDA,*), B(LDB,*)

     SUBROUTINE SGESV_64(N, NRHS, A, LDA, IPIVOT, B, LDB, INFO)	

     INTEGER*8 N, NRHS, LDA, LDB, INFO
     INTEGER*8 IPIVOT(*)
     REAL A(LDA,*), B(LDB,*)


PURPOSE

     sgesv computes the solution to a real system of linear equa-
     tions
        A * X = B, where A is an N-by-N matrix and X  and  B  are
     N-by-NRHS matrices.

     The  LU  decomposition  with  partial   pivoting   and   row
     interchanges is used to factor A as
        A = P * L * U,
     where P is a permutation matrix, L is unit lower triangular,
     and  U  is upper triangular.  The factored form of A is then
     used to solve the system of equations A * X = B.


ARGUMENTS

     N (input) The number of linear equations, i.e., the order of
               the matrix A.  N >= 0.

     NRHS (input)
               The number of right hand sides, i.e.,  the  number
               of columns of the matrix B.  NRHS >= 0.

     A (input/output)
               On entry, the N-by-N  coefficient  matrix  A.   On
               exit, the factors L and U from the factorization A
               = P*L*U; the unit diagonal elements of L  are  not
               stored.

     LDA (input)
               The leading dimension of  the  array  A.   LDA  >=
               max(1,N).

     IPIVOT (output)
               The pivot  indices  that  define  the  permutation
               matrix  P;  row  i  of the matrix was interchanged
               with row IPIVOT(i).

     B (input/output)
               On entry, the N-by-NRHS matrix of right hand  side
               matrix  B.   On  exit,  if INFO = 0, the N-by-NRHS
               solution matrix X.

     LDB (input)
               The leading dimension of  the  array  B.   LDB  >=
               max(1,N).

     INFO (output)
               = 0:  successful exit
               < 0:  if INFO = -i, the i-th argument had an ille-
               gal value
               > 0:  if INFO = i, U(i,i) is  exactly  zero.   The
               factorization has been completed, but the factor U
               is exactly singular, so the solution could not  be
               computed.
*/



