/*
http://en.wikipedia.org/wiki/QR_decomposition#Connection_to_a_determinant_or_a_product_of_eigenvalues
http://netlib.org/lapack/lug/node42.html
*/



	/**
 	* QR Decomposition.
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




module mathematics.numerical.algorithms.QrDecomposition;


private
{
	import mathematics.numerical.Matrix;
	import mathematics.numerical.algorithms.lapack;
	import Integer = tango.text.convert.Integer;

}


	/**
	*	Performs the QR decomposition.
	*
	*	Params:
	*   matrix                      = The input matrix.
	*   qmatrix                     = The output matrix. (must be allocated in advance)
	*   rmatrix                     = The output matrix. (must be allocated in advance)
	*/


void qrDecomposition(T)(Matrix!(T) matrix, Matrix!(T) qmatrix, Matrix!(T) rmatrix)
{
		assert( matrix.getRowCount() >= 0, "not enough rows");
		assert( matrix.getColumnCount() >= 0, "not enough columns");

		int m = matrix.getRowCount();
		int n = matrix.getColumnCount();
		T [] a = matrix.getData();			//input and returns part of Q and R
		int lda = matrix.getAllocatedRowCount();	//TODO: Try and better understand what LDA is.
		int [] lpvt = new int [n];
		int min_mn = m < n ? m:n;
		T [] tau = new T[min_mn];			//returns part of Q
		T [] work = new T[1];
		int lwork;
		int info = -1;	//query	worksize

		static if (is(T==double))
		{
			dgeqp3_(&m, &n, &a[0], &lda, &lpvt[0], &tau[0], &work[0], &lwork, &info);
		} else static if (is(T==float))
		{
			sgeqp3_(&m, &n, &a[0], &lda, &lpvt[0], &tau[0], &work[0], &lwork, &info);
		} else
		{
			static assert(false, "only float or double supported");
		}

		version (Tango)
		{
			if (info != 0) throw new Exception("xGEQP3 error in parameter: " ~ Integer.toUtf8(info));
		} else //Phobos
		{
			if (info != 0) throw new Exception("xGEQP3 error in parameter: " ~ toString(info));
		}


		lwork = cast(int) work[0];
		work = new T[lwork];

		info = 0;
		static if (is(T==double))
		{
			dgeqp3_(&m, &n, &a[0], &lda, &lpvt[0], &tau[0], &work[0], &lwork, &info);
		} else static if (is(T==float))
		{
			sgeqp3_(&m, &n, &a[0], &lda, &lpvt[0], &tau[0], &work[0], &lwork, &info);
		}

		version (Tango)
		{
			if (info != 0) throw new Exception("xGEQP3 error in parameter: " ~ Integer.toUtf8(info));
		} else
		{
			if (info != 0) throw new Exception("xGEQP3 error in parameter: " ~ toString(info));
		}
}





/*

    The matrix Q is represented as a product of elementary reflectors
 
       Q = H(1) H(2) . . . H(k), where k = min(m,n).   
 
    Each H(i) has the form   
 
       H(i) = I - tau * v * v'   
 
    where tau is a real scalar, and v is a real vector with   
    v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),   
    and tau in TAU(i).




     SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEQP3 computes a QR factorization with column pivoting of a
*  matrix A:  A*P = Q*R  using Level 3 BLAS.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the upper triangle of the array contains the
*          min(M,N)-by-N upper trapezoidal matrix R; the elements below
*          the diagonal, together with the array TAU, represent the
*          orthogonal matrix Q as a product of min(M,N) elementary
*          reflectors.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,M).
*
*  JPVT    (input/output) INTEGER array, dimension (N)
*          On entry, if JPVT(J).ne.0, the J-th column of A is permuted
*          to the front of A*P (a leading column); if JPVT(J)=0,
*          the J-th column of A is a free column.
*          On exit, if JPVT(J)=K, then the J-th column of A*P was the
*          the K-th column of A.
*
*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO=0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= 3*N+1.
*          For optimal performance LWORK >= 2*N+( N+1 )*NB, where NB
*          is the optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0: successful exit.
*          < 0: if INFO = -i, the i-th argument had an illegal value.
*

	Complex!(float)
	---------------

  void cgeqp3_(int *m, int *n, Complex!(float) *a, int *lda,
	 int *jpvt, Complex!(float) *tau, Complex!(float) *work, int *lwork, float *
	rwork, int *info);

	double
	------

  void dgeqp3_(int *m, int *n, double *a, int *
	lda, int *jpvt, double *tau, double *work, int *lwork,
	 int *info);

	float
	-----

  void sgeqp3_(int *m, int *n, float *a, int *lda, 
	int *jpvt, float *tau, float *work, int *lwork, int *info);

	Complex!(double)
	----------------

  void zgeqp3_(int *m, int *n, Complex!(double) *a, 
	int *lda, int *jpvt, Complex!(double) *tau, Complex!(double) *work, 
	int *lwork, double *rwork, int *info);
*/





