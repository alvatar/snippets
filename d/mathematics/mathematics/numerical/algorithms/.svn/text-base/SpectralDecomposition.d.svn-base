	/**
 	* Spectral Decomposition.
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
	*/


//	http://en.wikipedia.org/wiki/Spectral_decomposition



module mathematics.numerical.algorithms.SpectralDecomposition;

//private	import std.stdio;
//private	import std.math;
private	import mathematics.numerical.Matrix;
private	import mathematics.numerical.algorithms.lapack;
//private import std.string;


/*


  void sgeev_(char *jobvl, char *jobvr, int *n, float *a, 
	int *lda, float *wr, float *wi, float *vl, int *ldvl, float *vr, 
	int *ldvr, float *work, int *lwork, int *info);

  void dgeev_(char *jobvl, char *jobvr, int *n, double *
	a, int *lda, double *wr, double *wi, double *vl, 
	int *ldvl, double *vr, int *ldvr, double *work, 
	int *lwork, int *info);
*/

	//	Si une matrice est normale, elle est aussi diagonalisable.	Spektralzerlegung, shur zerlegung


//TODO: Adapter les algorithmes pour les processeurs à 64 bit.


	/**
	* Performs the eigenvalue decomposition.
	*
	* Params:
	*
	*	  aMatrix	=	This value gets overwritten, it must be an NxN matrix, doesn't have to be symmetric.
	*		lMatrix	=	This is the bases of eigenvectors created when using the left eigenvectors.
	*		rMatrix	=	This is the basis of eigenvectors created when using the right eigenvectors.
	*/


void spectralDecomposition(T)(Matrix!(T) aMatrix, Matrix!(T) lMatrix, Matrix!(T) rMatrix)
{
		assert( aMatrix.getRowCount() == aMatrix.getColumnCount(), "must input a square matrix");
		assert( lMatrix.getRowCount() == lMatrix.getColumnCount(), "must input a square matrix");
		assert( rMatrix.getRowCount() == rMatrix.getColumnCount(), "must input a square matrix");
		assert( aMatrix.getRowCount() >= 1, "We only accept not empty matrices");


		char jobvl	='V';
		char jobvr	='V';

		int n 		= aMatrix.getRowCount();


		T [] a		= aMatrix.getData();
		int lda 	= aMatrix.getAllocatedRowCount();
		T [] wr = new T[n];		//real part of the eigenvalues			(are these for the left or right)
		T [] wi = new T[n];		//imaginary part of the eigenvalues

		Matrix!(T) lMatrix = new Matrix!(T)(n,n);
		Matrix!(T) rMatrix = new Matrix!(T)(n,n);

		T [] vl	= lMatrix.getData();
		int ldvl = lMatrix.getAllocatedRowCount();
		T [] vr = rMatrix.getData();
		int ldvr = rMatrix.getAllocatedRowCount();
		T [] work = new T[1];
		int lwork=-1;			//	-1 demander combien de mémoire nous devons réservé.
		int info = 0;

		//	Avec fortran tout est par référence, c'est pourqoui on passe des pointeurs.

		static if (is(T==float))
		{
			sgeev_(&jobvl, &jobvr, &n, &a[0],
				&lda, &wr[0], &wi[0], &vl[0], &ldvl, &vr[0],
				&ldvr, &work[0], &lwork, &info);	
		} else if (is(T==double))
		{
			dgeev_(&jobvl, &jobvr, &n, &a[0],
				&lda, &wr[0], &wi[0], &vl[0], &ldvl, &vr[0],
				&ldvr, &work[0], &lwork, &info);	

		} else
		{
			 static assert(false, "Seulement float et double ont une implémentation.");
		}

		assert(info == 0, "On n'a pas trouver combien de mémoire nous avons besoins.");

		//On fait une allocation de la mémoire, on alloue lwork.

		lwork = work[0];
		work = new T[lwork];
		info = 0;

		static if (is(T==float))
		{
			sgeev_(&jobvl, &jobvr, &n, &a[0],
				&lda, &wr[0], &wi[0], &vl[0], &ldvl, &vr[0],
				&ldvr, &work[0], &lwork, &info);	
		} else if (is(T==double))
		{
			dgeev_(&jobvl, &jobvr, &n, &a[0],
				&lda, &wr[0], &wi[0], &vl[0], &ldvl, &vr[0],
				&ldvr, &work[0], &lwork, &info);	

		} else
		{
			 static assert(false, "Seulement float et double sont supportés.");
		}
}

		

