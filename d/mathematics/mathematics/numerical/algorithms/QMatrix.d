	/**
 	* QMatrix reconstruction.
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
	* Bugs:
	*		Not tested not working.
	*/



module mathematics.algorithms.QMmatrix;


private
{
	import mathematics.numerical.Matrix;
	import mathematics.numerical.algorithms.lapack;
}


	/**
	* Recreates the Q matrix.
	*/


template QMatrix(T)
{
	void recompose(inout Matrix!(T) matrix, T [] tau)
	{
		assert( matrix.getRowCount() >= 0, "not enough rows");
		assert(0 <= matrix.getColumnCount() && matrix.getColumnCount() <= matrix.getRowCount(), "colums out of range");
		assert(0 <= tau.length && tau.length <= matrix.getColumnCount(), "tau length out of range");

		int m = matrix.getRowCount();
		int n = matrix.getColumnCount();
		int k =	tau.length;
		T [] a = matrix.getData();
		int lda = matrix.getAllocatedRowCount();
		T [] work = new T[1]; //storage to get workspaceSize
		int lwork = -1;
		int info;		

		static if (is(T==double))
		{
			dorgqr_(&m, &n, &k, &a[0], &lda, &tau[0], &work[0], &lwork, &info);
		} else static if (is(T == float))
		{
			sorgqr_(&m, &n, &k, &a[0], &lda, &tau[0], &work[0], &lwork, &info);
		} else
		{
			static assert(false,"only float and double supported");
		}
		if (info!=0) throw new Exception("xDORGR in parameter: "~toString(info));
		
		lwork = cast(int) work[0];
		work = new T[lwork];
		info = 0;

		static if (is(T==double))
		{
			dorgqr_(&m, &n, &k, &a[0], &lda, &tau[0], &work[0], &lwork, &info);
		} else static if (is(T==float))
		{
			sorgqr_(&m, &n, &k, &a[0], &lda, &tau[0], &work[0], &lwork, &info);
		}
		if (info!=0) throw new Exception("xDORGR in parameter: "~toString(info));
	}	
}


//alias QMatrix!(double) qmusic;


/*

      SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DORGQR generates an M-by-N real matrix Q with orthonormal columns,
*  which is defined as the first N columns of a product of K elementary
*  reflectors of order M
*
*        Q  =  H(1) H(2) . . . H(k)
*
*  as returned by DGEQRF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. M >= N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the i-th column must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by DGEQRF in the first k columns of its array
*          argument A.
*          On exit, the M-by-N matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by DGEQRF.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,N).
*          For optimum performance LWORK >= N*NB, where NB is the
*          optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument has an illegal value
*/



