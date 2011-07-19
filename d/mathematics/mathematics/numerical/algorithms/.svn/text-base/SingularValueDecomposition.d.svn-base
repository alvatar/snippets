	/**
 	* Singular Value Decomposition.
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
	* See_Also:
	*	
	*   Numerical Recipes in C, 2.6 SVD
	*   
	*/
	
//Cisst netlib uses Lapack3e



module mathematics.numerical.algorithms.SingularValueDecomposition;

//private	import std.stdio;
//private	import std.math;
private	import mathematics.numerical.Matrix;
private	import mathematics.numerical.algorithms.lapack;

version (Tango)
{
} else	//phobos
{
	private import std.string;
}



 	/**
	* Finds the Singular Value Decomposition of a given matrix.
 	* It will override the existing matrix sinds it will use it as a buffer.
	* The class can be reused by changing the values but not its the dimensions.
	*
	* The resulting matrices are automatically created. (Maybe bad design, user input needed)
	*
	* PARAMS:
	*     matrix           = The input matrix.
	*     uMatrix          = Returns a new matrix. A basis for the kernel.
	*     sMatrix          = Returns a new matrix. Diagonal elements are the squareroot of the eigenvalues.
	*     vTMatrix         = Returns a new matrix. The basis that forms the impact.
	*/


//    New in Lapack3 : a faster singular value decomposition (SVD), computed by divide-and-conquer (xGESDD)



void singularValueDecomposition(T)(Matrix!(T) matrix,out Matrix!(T) uMatrix,out MatrixDiagonal!(T) sMatrix, out Matrix!(T) vTMatrix)
{
		uMatrix = new Matrix!(T) (matrix.rowCount(), matrix.rowCount());
		sMatrix = new MatrixDiagonal!(T) (matrix.rowCount(), matrix.columnCount());
		vTMatrix = new Matrix!(T) (matrix.columnCount(), matrix.columnCount());

		T bestLWork;
		char jobu='A';
		char jobvt='A';
		int m = matrix.rowCount();
		int n = matrix.columnCount();
		T [] a = matrix.getData();			//is this the correct address?
		int lda = matrix.getAllocatedRowCount();	//LDA needs to figure out more about this.
		T [] s= sMatrix.getData();			//min(m,n)
		T [] u = uMatrix.getData();			//LDU, M for 'A'
		int ldu = uMatrix.rowCount();		//leading dimensions of u
		T [] vt = vTMatrix.getData();			//N x N for 'A'
		int ldvt = vTMatrix.rowCount();		//N for 'A'
		int lwork = -1; //dimensions of array work	//Two Calls Find Optimal LWORK
		int info;  //0 success, <0 -i argument had illegal value

		static if (is(T==float))
		{
			sgesvd_(&jobu, &jobvt, &m, &n, &a[0], &lda, &s[0], &u[0], &ldu, &vt[0], &ldvt, &bestLWork, &lwork, &info);
		} else static if(is(T == double))
		{
			dgesvd_(&jobu, &jobvt, &m, &n, &a[0], &lda, &s[0], &u[0], &ldu, &vt[0], &ldvt, &bestLWork, &lwork, &info);
		} else
		{
			static assert(false, "SingularValueDecomposition(T), only float or double");
		}
		version(Tango)
		{
			if (info!=0) throw new Exception("xGESVD in parameter: "~tango.text.convert.Integer.toString(info));
		} else	//Phobos
		{
			if (info!=0) throw new Exception("xGESVD in parameter: "~toString(info));
		}


		lwork = cast(int) bestLWork;	//float to int
		T [] workSpace = new T[lwork];
		info = 0;
		
		static if (is(T == float))
		{
			sgesvd_(&jobu, &jobvt, &m, &n, &a[0], &lda, &s[0], &u[0], &ldu, &vt[0], &ldvt, &workSpace[0], &lwork, &info);
		} else static if(is(T==double))
		{
			dgesvd_(&jobu, &jobvt, &m, &n, &a[0], &lda, &s[0], &u[0], &ldu, &vt[0], &ldvt, &workSpace[0], &lwork, &info);
		}
		version(Tango)
		{
			if (info!=0) throw new Exception("xGESVD in parameter: "~ tango.text.convert.Integer.toString(info));
		} else	//Phobos
		{
			if (info!=0) throw new Exception("xGESVD in parameter: "~toString(info));
		}

}


unittest {
	Matrix!(double) matrix = new Matrix!(double)(4,4);
	Matrix!(double) uMatrix;
	MatrixDiagonal!(double) sMatrix;
	Matrix!(double) vTMatrix;
	singularValueDecomposition!(double) (matrix, uMatrix, sMatrix, vTMatrix);	
}






//alias SingularValueDecomposition!(double) svndtest;


/*
      SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBU, JOBVT
      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGESVD computes the singular value decomposition (SVD) of a real
*  M-by-N matrix A, optionally computing the left and/or right singular
*  vectors. The SVD is written
*
*       A = U * SIGMA * transpose(V)
*
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
*  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*
*  Note that the routine returns V**T, not V.
*
*  Arguments
*  =========
*
*  JOBU    (input) CHARACTER*1
*          Specifies options for computing all or part of the matrix U:
*          = 'A':  all M columns of U are returned in array U:
*          = 'S':  the first min(m,n) columns of U (the left singular
*                  vectors) are returned in the array U;
*          = 'O':  the first min(m,n) columns of U (the left singular
*                  vectors) are overwritten on the array A;
*          = 'N':  no columns of U (no left singular vectors) are
*                  computed.
*
*  JOBVT   (input) CHARACTER*1
*          Specifies options for computing all or part of the matrix
*          V**T:
*          = 'A':  all N rows of V**T are returned in the array VT;
*          = 'S':  the first min(m,n) rows of V**T (the right singular
*                  vectors) are returned in the array VT;
*          = 'O':  the first min(m,n) rows of V**T (the right singular
*                  vectors) are overwritten on the array A;
*          = 'N':  no rows of V**T (no right singular vectors) are
*                  computed.
*
*          JOBVT and JOBU cannot both be 'O'.
*
*  M       (input) INTEGER
*          The number of rows of the input matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the input matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit,
*          if JOBU = 'O',  A is overwritten with the first min(m,n)
*                          columns of U (the left singular vectors,
*                          stored columnwise);
*          if JOBVT = 'O', A is overwritten with the first min(m,n)
*                          rows of V**T (the right singular vectors,
*                          stored rowwise);
*          if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
*                          are destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The singular values of A, sorted so that S(i) >= S(i+1).
*
*  U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
*          (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
*          If JOBU = 'A', U contains the M-by-M orthogonal matrix U;
*          if JOBU = 'S', U contains the first min(m,n) columns of U
*          (the left singular vectors, stored columnwise);
*          if JOBU = 'N' or 'O', U is not referenced.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= 1; if
*          JOBU = 'S' or 'A', LDU >= M.
*
*  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
*          If JOBVT = 'A', VT contains the N-by-N orthogonal matrix
*          V**T;
*          if JOBVT = 'S', VT contains the first min(m,n) rows of
*          V**T (the right singular vectors, stored rowwise);
*          if JOBVT = 'N' or 'O', VT is not referenced.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.  LDVT >= 1; if
*          JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
*          if INFO > 0, WORK(2:MIN(M,N)) contains the unconverged
*          superdiagonal elements of an upper bidiagonal matrix B
*          whose diagonal is in S (not necessarily sorted). B
*          satisfies A = U * B * VT, so it has the same singular values
*          as A, and singular vectors related by U and VT.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          LWORK >= MAX(1,3*MIN(M,N)+MAX(M,N),5*MIN(M,N)).
*          For good performance, LWORK should generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if DBDSQR did not converge, INFO specifies how many
*                superdiagonals of an intermediate bidiagonal form B
*                did not converge to zero. See the description of WORK
*                above for details.
*/




