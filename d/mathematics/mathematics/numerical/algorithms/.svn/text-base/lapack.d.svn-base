	/**
	*	Authors: The Clapack guys, Peter Van Isacker (Sclytrack)
	*
	* See_Also:
	*    http://www.netlib.org/lapack/
	*
	* Bugs:
	*		Possible bug, links to "lapack-3" and "blas-3" (Debian Etch)
	*   Some operating system name them slightly differently.
	*/

module mathematics.numerical.algorithms.lapack;

private import mathematics.numerical.Complex;


//TODO: Stripe away the version.

version(build)
{
	pragma(link, "lapack-3");
	pragma(link, "blas-3");
}


extern (C) {

//	Routines taken from clapack
//	Note: I did the conversions for 32 bit operating system. I didn't think about 64.

  alias void * L_fp;
  alias int logical;
  alias short ftnlen;


  void cbdsqr_(char *uplo, int *n, int *ncvt, int *
	nru, int *ncc, float *d__, float *e, Complex!(float) *vt, int *ldvt, 
	Complex!(float) *u, int *ldu, Complex!(float) *c__, int *ldc, float *rwork, 
	int *info);
 

  void cgbbrd_(char *vect, int *m, int *n, int *ncc,
	 int *kl, int *ku, Complex!(float) *ab, int *ldab, float *d__, 
	float *e, Complex!(float) *q, int *ldq, Complex!(float) *pt, int *ldpt, 
	Complex!(float) *c__, int *ldc, Complex!(float) *work, float *rwork, int *info);
 

  void cgbcon_(char *norm, int *n, int *kl, int *ku,
	 Complex!(float) *ab, int *ldab, int *ipiv, float *anorm, float *rcond, 
	Complex!(float) *work, float *rwork, int *info);
 

  void cgbequ_(int *m, int *n, int *kl, int *ku,
	 Complex!(float) *ab, int *ldab, float *r__, float *c__, float *rowcnd, float 
	*colcnd, float *amax, int *info);
 

  void cgbrfs_(char *trans, int *n, int *kl, int *
	ku, int *nrhs, Complex!(float) *ab, int *ldab, Complex!(float) *afb, int *
	ldafb, int *ipiv, Complex!(float) *b, int *ldb, Complex!(float) *x, int *
	ldx, float *ferr, float *berr, Complex!(float) *work, float *rwork, int *
	info);
 

  void cgbsv_(int *n, int *kl, int *ku, int *
	nrhs, Complex!(float) *ab, int *ldab, int *ipiv, Complex!(float) *b, int *
	ldb, int *info);
 

  void cgbsvx_(char *fact, char *trans, int *n, int *kl,
	 int *ku, int *nrhs, Complex!(float) *ab, int *ldab, Complex!(float) *afb,
	 int *ldafb, int *ipiv, char *equed, float *r__, float *c__, 
	Complex!(float) *b, int *ldb, Complex!(float) *x, int *ldx, float *rcond, float 
	*ferr, float *berr, Complex!(float) *work, float *rwork, int *info);
 

  void cgbtf2_(int *m, int *n, int *kl, int *ku,
	 Complex!(float) *ab, int *ldab, int *ipiv, int *info);
 

  void cgbtrf_(int *m, int *n, int *kl, int *ku,
	 Complex!(float) *ab, int *ldab, int *ipiv, int *info);
 

  void cgbtrs_(char *trans, int *n, int *kl, int *
	ku, int *nrhs, Complex!(float) *ab, int *ldab, int *ipiv, Complex!(float) 
	*b, int *ldb, int *info);
 

  void cgebak_(char *job, char *side, int *n, int *ilo, 
	int *ihi, float *scale, int *m, Complex!(float) *v, int *ldv, 
	int *info);
 

  void cgebal_(char *job, int *n, Complex!(float) *a, int *lda, 
	int *ilo, int *ihi, float *scale, int *info);
 

  void cgebd2_(int *m, int *n, Complex!(float) *a, int *lda,
	 float *d__, float *e, Complex!(float) *tauq, Complex!(float) *taup, Complex!(float) *work, 
	int *info);
 

  void cgebrd_(int *m, int *n, Complex!(float) *a, int *lda,
	 float *d__, float *e, Complex!(float) *tauq, Complex!(float) *taup, Complex!(float) *work, 
	int *lwork, int *info);
 

  void cgecon_(char *norm, int *n, Complex!(float) *a, int *lda,
	 float *anorm, float *rcond, Complex!(float) *work, float *rwork, int *info);
 

  void cgeequ_(int *m, int *n, Complex!(float) *a, int *lda,
	 float *r__, float *c__, float *rowcnd, float *colcnd, float *amax, 
	int *info);
 

  void cgees_(char *jobvs, char *sort, L_fp select, int *n, 
	Complex!(float) *a, int *lda, int *sdim, Complex!(float) *w, Complex!(float) *vs, 
	int *ldvs, Complex!(float) *work, int *lwork, float *rwork, logical *
	bwork, int *info);
 

  void cgeesx_(char *jobvs, char *sort, L_fp select, char *
	sense, int *n, Complex!(float) *a, int *lda, int *sdim, Complex!(float) *
	w, Complex!(float) *vs, int *ldvs, float *rconde, float *rcondv, Complex!(float) *
	work, int *lwork, float *rwork, logical *bwork, int *info);
 

  void cgeev_(char *jobvl, char *jobvr, int *n, Complex!(float) *a, 
	int *lda, Complex!(float) *w, Complex!(float) *vl, int *ldvl, Complex!(float) *vr, 
	int *ldvr, Complex!(float) *work, int *lwork, float *rwork, int *
	info);
 

  void cgeevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, int *n, Complex!(float) *a, int *lda, Complex!(float) *w, Complex!(float) *vl, 
	int *ldvl, Complex!(float) *vr, int *ldvr, int *ilo, int *ihi,
	 float *scale, float *abnrm, float *rconde, float *rcondv, Complex!(float) *work, 
	int *lwork, float *rwork, int *info);
 

  void cgegs_(char *jobvsl, char *jobvsr, int *n, Complex!(float) *
	a, int *lda, Complex!(float) *b, int *ldb, Complex!(float) *alpha, Complex!(float) *
	beta, Complex!(float) *vsl, int *ldvsl, Complex!(float) *vsr, int *ldvsr, 
	Complex!(float) *work, int *lwork, float *rwork, int *info);
 

  void cgegv_(char *jobvl, char *jobvr, int *n, Complex!(float) *a, 
	int *lda, Complex!(float) *b, int *ldb, Complex!(float) *alpha, Complex!(float) *beta,
	 Complex!(float) *vl, int *ldvl, Complex!(float) *vr, int *ldvr, Complex!(float) *
	work, int *lwork, float *rwork, int *info);
 

  void cgehd2_(int *n, int *ilo, int *ihi, Complex!(float) *
	a, int *lda, Complex!(float) *tau, Complex!(float) *work, int *info);
 

  void cgehrd_(int *n, int *ilo, int *ihi, Complex!(float) *
	a, int *lda, Complex!(float) *tau, Complex!(float) *work, int *lwork, int 
	*info);
 

  void cgelq2_(int *m, int *n, Complex!(float) *a, int *lda,
	 Complex!(float) *tau, Complex!(float) *work, int *info);
 
  void cgelqf_(int *m, int *n, Complex!(float) *a, int *lda,
	 Complex!(float) *tau, Complex!(float) *work, int *lwork, int *info);
 

  void cgels_(char *trans, int *m, int *n, int *
	nrhs, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, Complex!(float) *
	work, int *lwork, int *info);
 

  void cgelsx_(int *m, int *n, int *nrhs, Complex!(float) *
	a, int *lda, Complex!(float) *b, int *ldb, int *jpvt, float *rcond,
	 int *rank, Complex!(float) *work, float *rwork, int *info);
 

  void cgelsy_(int *m, int *n, int *nrhs, Complex!(float) *
	a, int *lda, Complex!(float) *b, int *ldb, int *jpvt, float *rcond,
	 int *rank, Complex!(float) *work, int *lwork, float *rwork, int *
	info);
 

  void cgeql2_(int *m, int *n, Complex!(float) *a, int *lda,
	 Complex!(float) *tau, Complex!(float) *work, int *info);
 

  void cgeqlf_(int *m, int *n, Complex!(float) *a, int *lda,
	 Complex!(float) *tau, Complex!(float) *work, int *lwork, int *info);
 

  void cgeqp3_(int *m, int *n, Complex!(float) *a, int *lda,
	 int *jpvt, Complex!(float) *tau, Complex!(float) *work, int *lwork, float *
	rwork, int *info);
 

  void cgeqpf_(int *m, int *n, Complex!(float) *a, int *lda,
	 int *jpvt, Complex!(float) *tau, Complex!(float) *work, float *rwork, int *
	info);
 

  void cgeqr2_(int *m, int *n, Complex!(float) *a, int *lda,
	 Complex!(float) *tau, Complex!(float) *work, int *info);
 

  void cgeqrf_(int *m, int *n, Complex!(float) *a, int *lda,
	 Complex!(float) *tau, Complex!(float) *work, int *lwork, int *info);
 

  void cgerfs_(char *trans, int *n, int *nrhs, Complex!(float) *
	a, int *lda, Complex!(float) *af, int *ldaf, int *ipiv, Complex!(float) *
	b, int *ldb, Complex!(float) *x, int *ldx, float *ferr, float *berr, 
	Complex!(float) *work, float *rwork, int *info);
 

  void cgerq2_(int *m, int *n, Complex!(float) *a, int *lda,
	 Complex!(float) *tau, Complex!(float) *work, int *info);
 

  void cgerqf_(int *m, int *n, Complex!(float) *a, int *lda,
	 Complex!(float) *tau, Complex!(float) *work, int *lwork, int *info);
 

  void cgesc2_(int *n, Complex!(float) *a, int *lda, Complex!(float) *
	rhs, int *ipiv, int *jpiv, float *scale);
 

  void cgesv_(int *n, int *nrhs, Complex!(float) *a, int *
	lda, int *ipiv, Complex!(float) *b, int *ldb, int *info);
 

  void cgesvx_(char *fact, char *trans, int *n, int *
	nrhs, Complex!(float) *a, int *lda, Complex!(float) *af, int *ldaf, int *
	ipiv, char *equed, float *r__, float *c__, Complex!(float) *b, int *ldb, 
	Complex!(float) *x, int *ldx, float *rcond, float *ferr, float *berr, 
	Complex!(float) *work, float *rwork, int *info);
 

  void cgetc2_(int *n, Complex!(float) *a, int *lda, int *
	ipiv, int *jpiv, int *info);
 

  void cgetf2_(int *m, int *n, Complex!(float) *a, int *lda,
	 int *ipiv, int *info);
 
  void cgetrf_(int *m, int *n, Complex!(float) *a, int *lda,
	 int *ipiv, int *info);
 

  void cgetri_(int *n, Complex!(float) *a, int *lda, int *
	ipiv, Complex!(float) *work, int *lwork, int *info);
 

  void cgetrs_(char *trans, int *n, int *nrhs, Complex!(float) *
	a, int *lda, int *ipiv, Complex!(float) *b, int *ldb, int *
	info);
 

  void cggbak_(char *job, char *side, int *n, int *ilo, 
	int *ihi, float *lscale, float *rscale, int *m, Complex!(float) *v, 
	int *ldv, int *info);
 

  void cggbal_(char *job, int *n, Complex!(float) *a, int *lda, 
	Complex!(float) *b, int *ldb, int *ilo, int *ihi, float *lscale, 
	float *rscale, float *work, int *info);
 

  void cgges_(char *jobvsl, char *jobvsr, char *sort, L_fp 
	selctg, int *n, Complex!(float) *a, int *lda, Complex!(float) *b, int *
	ldb, int *sdim, Complex!(float) *alpha, Complex!(float) *beta, Complex!(float) *vsl, 
	int *ldvsl, Complex!(float) *vsr, int *ldvsr, Complex!(float) *work, int *
	lwork, float *rwork, logical *bwork, int *info);
 

  void cggesx_(char *jobvsl, char *jobvsr, char *sort, L_fp 
	selctg, char *sense, int *n, Complex!(float) *a, int *lda, Complex!(float) *b,
	 int *ldb, int *sdim, Complex!(float) *alpha, Complex!(float) *beta, Complex!(float) *
	vsl, int *ldvsl, Complex!(float) *vsr, int *ldvsr, float *rconde, float 
	*rcondv, Complex!(float) *work, int *lwork, float *rwork, int *iwork, 
	int *liwork, logical *bwork, int *info);
 

  void cggev_(char *jobvl, char *jobvr, int *n, Complex!(float) *a, 
	int *lda, Complex!(float) *b, int *ldb, Complex!(float) *alpha, Complex!(float) *beta,
	 Complex!(float) *vl, int *ldvl, Complex!(float) *vr, int *ldvr, Complex!(float) *
	work, int *lwork, float *rwork, int *info);
 

  void cggevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, int *n, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb,
	 Complex!(float) *alpha, Complex!(float) *beta, Complex!(float) *vl, int *ldvl, Complex!(float) *
	vr, int *ldvr, int *ilo, int *ihi, float *lscale, float *
	rscale, float *abnrm, float *bbnrm, float *rconde, float *rcondv, Complex!(float) 
	*work, int *lwork, float *rwork, int *iwork, logical *bwork, 
	int *info);
 

  void cggglm_(int *n, int *m, int *p, Complex!(float) *a, 
	int *lda, Complex!(float) *b, int *ldb, Complex!(float) *d__, Complex!(float) *x, 
	Complex!(float) *y, Complex!(float) *work, int *lwork, int *info);
 

  void cgghrd_(char *compq, char *compz, int *n, int *
	ilo, int *ihi, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb,
	 Complex!(float) *q, int *ldq, Complex!(float) *z__, int *ldz, int *info);
 

  void cgglse_(int *m, int *n, int *p, Complex!(float) *a, 
	int *lda, Complex!(float) *b, int *ldb, Complex!(float) *c__, Complex!(float) *d__, 
	Complex!(float) *x, Complex!(float) *work, int *lwork, int *info);
 

  void cggqrf_(int *n, int *m, int *p, Complex!(float) *a, 
	int *lda, Complex!(float) *taua, Complex!(float) *b, int *ldb, Complex!(float) *taub, 
	Complex!(float) *work, int *lwork, int *info);
 

  void cggrqf_(int *m, int *p, int *n, Complex!(float) *a, 
	int *lda, Complex!(float) *taua, Complex!(float) *b, int *ldb, Complex!(float) *taub, 
	Complex!(float) *work, int *lwork, int *info);
 

  void cggsvd_(char *jobu, char *jobv, char *jobq, int *m, 
	int *n, int *p, int *k, int *l, Complex!(float) *a, int *
	lda, Complex!(float) *b, int *ldb, float *alpha, float *beta, Complex!(float) *u, 
	int *ldu, Complex!(float) *v, int *ldv, Complex!(float) *q, int *ldq, 
	Complex!(float) *work, float *rwork, int *iwork, int *info);
 

  void cggsvp_(char *jobu, char *jobv, char *jobq, int *m, 
	int *p, int *n, Complex!(float) *a, int *lda, Complex!(float) *b, int 
	*ldb, float *tola, float *tolb, int *k, int *l, Complex!(float) *u, 
	int *ldu, Complex!(float) *v, int *ldv, Complex!(float) *q, int *ldq, 
	int *iwork, float *rwork, Complex!(float) *tau, Complex!(float) *work, int *
	info);
 

  void cgtcon_(char *norm, int *n, Complex!(float) *dl, Complex!(float) *
	d__, Complex!(float) *du, Complex!(float) *du2, int *ipiv, float *anorm, float *
	rcond, Complex!(float) *work, int *info);
 

  void cgtrfs_(char *trans, int *n, int *nrhs, Complex!(float) *
	dl, Complex!(float) *d__, Complex!(float) *du, Complex!(float) *dlf, Complex!(float) *df, Complex!(float) *
	duf, Complex!(float) *du2, int *ipiv, Complex!(float) *b, int *ldb, Complex!(float) *
	x, int *ldx, float *ferr, float *berr, Complex!(float) *work, float *rwork, 
	int *info);
 

  void cgtsv_(int *n, int *nrhs, Complex!(float) *dl, Complex!(float) *
	d__, Complex!(float) *du, Complex!(float) *b, int *ldb, int *info);
 

  void cgtsvx_(char *fact, char *trans, int *n, int *
	nrhs, Complex!(float) *dl, Complex!(float) *d__, Complex!(float) *du, Complex!(float) *dlf, Complex!(float) *
	df, Complex!(float) *duf, Complex!(float) *du2, int *ipiv, Complex!(float) *b, int *
	ldb, Complex!(float) *x, int *ldx, float *rcond, float *ferr, float *berr, 
	Complex!(float) *work, float *rwork, int *info);
 

  void cgttrf_(int *n, Complex!(float) *dl, Complex!(float) *d__, Complex!(float) *
	du, Complex!(float) *du2, int *ipiv, int *info);
 

  void cgttrs_(char *trans, int *n, int *nrhs, Complex!(float) *
	dl, Complex!(float) *d__, Complex!(float) *du, Complex!(float) *du2, int *ipiv, Complex!(float) *
	b, int *ldb, int *info);
 

  void cgtts2_(int *itrans, int *n, int *nrhs, 
	Complex!(float) *dl, Complex!(float) *d__, Complex!(float) *du, Complex!(float) *du2, int *ipiv, 
	Complex!(float) *b, int *ldb);
 

  void chbev_(char *jobz, char *uplo, int *n, int *kd, 
	Complex!(float) *ab, int *ldab, float *w, Complex!(float) *z__, int *ldz, 
	Complex!(float) *work, float *rwork, int *info);
 

  void chbevd_(char *jobz, char *uplo, int *n, int *kd, 
	Complex!(float) *ab, int *ldab, float *w, Complex!(float) *z__, int *ldz, 
	Complex!(float) *work, int *lwork, float *rwork, int *lrwork, int *
	iwork, int *liwork, int *info);
 

  void chbevx_(char *jobz, char *range, char *uplo, int *n, 
	int *kd, Complex!(float) *ab, int *ldab, Complex!(float) *q, int *ldq, 
	float *vl, float *vu, int *il, int *iu, float *abstol, int *
	m, float *w, Complex!(float) *z__, int *ldz, Complex!(float) *work, float *rwork, 
	int *iwork, int *ifail, int *info);
 

  void chbgst_(char *vect, char *uplo, int *n, int *ka, 
	int *kb, Complex!(float) *ab, int *ldab, Complex!(float) *bb, int *ldbb, 
	Complex!(float) *x, int *ldx, Complex!(float) *work, float *rwork, int *info);
 

  void chbgv_(char *jobz, char *uplo, int *n, int *ka, 
	int *kb, Complex!(float) *ab, int *ldab, Complex!(float) *bb, int *ldbb, 
	float *w, Complex!(float) *z__, int *ldz, Complex!(float) *work, float *rwork, 
	int *info);
 

  void chbgvx_(char *jobz, char *range, char *uplo, int *n, 
	int *ka, int *kb, Complex!(float) *ab, int *ldab, Complex!(float) *bb, 
	int *ldbb, Complex!(float) *q, int *ldq, float *vl, float *vu, int *
	il, int *iu, float *abstol, int *m, float *w, Complex!(float) *z__, 
	int *ldz, Complex!(float) *work, float *rwork, int *iwork, int *
	ifail, int *info);
 

  void chbtrd_(char *vect, char *uplo, int *n, int *kd, 
	Complex!(float) *ab, int *ldab, float *d__, float *e, Complex!(float) *q, int *
	ldq, Complex!(float) *work, int *info);
 

  void checon_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *ipiv, float *anorm, float *rcond, Complex!(float) *work, int *
	info);
 

  void cheev_(char *jobz, char *uplo, int *n, Complex!(float) *a, 
	int *lda, float *w, Complex!(float) *work, int *lwork, float *rwork, 
	int *info);
 

  void cheevd_(char *jobz, char *uplo, int *n, Complex!(float) *a, 
	int *lda, float *w, Complex!(float) *work, int *lwork, float *rwork, 
	int *lrwork, int *iwork, int *liwork, int *info);
 

  void cheevr_(char *jobz, char *range, char *uplo, int *n, 
	Complex!(float) *a, int *lda, float *vl, float *vu, int *il, int *
	iu, float *abstol, int *m, float *w, Complex!(float) *z__, int *ldz, 
	int *isuppz, Complex!(float) *work, int *lwork, float *rwork, int *
	lrwork, int *iwork, int *liwork, int *info);
 

  void cheevx_(char *jobz, char *range, char *uplo, int *n, 
	Complex!(float) *a, int *lda, float *vl, float *vu, int *il, int *
	iu, float *abstol, int *m, float *w, Complex!(float) *z__, int *ldz, 
	Complex!(float) *work, int *lwork, float *rwork, int *iwork, int *
	ifail, int *info);
 

  void chegs2_(int *itype, char *uplo, int *n, Complex!(float) *
	a, int *lda, Complex!(float) *b, int *ldb, int *info);
 
  void chegst_(int *itype, char *uplo, int *n, Complex!(float) *
	a, int *lda, Complex!(float) *b, int *ldb, int *info);
 
  void chegv_(int *itype, char *jobz, char *uplo, int *
	n, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, float *w, 
	Complex!(float) *work, int *lwork, float *rwork, int *info);
 
  void chegvd_(int *itype, char *jobz, char *uplo, int *
	n, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, float *w, 
	Complex!(float) *work, int *lwork, float *rwork, int *lrwork, int *
	iwork, int *liwork, int *info);
 
  void chegvx_(int *itype, char *jobz, char *range, char *
	uplo, int *n, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, 
	float *vl, float *vu, int *il, int *iu, float *abstol, int *
	m, float *w, Complex!(float) *z__, int *ldz, Complex!(float) *work, int *lwork,
	 float *rwork, int *iwork, int *ifail, int *info);
 
  void cherfs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	a, int *lda, Complex!(float) *af, int *ldaf, int *ipiv, Complex!(float) *
	b, int *ldb, Complex!(float) *x, int *ldx, float *ferr, float *berr, 
	Complex!(float) *work, float *rwork, int *info);
 
  void chesv_(char *uplo, int *n, int *nrhs, Complex!(float) *a,
	 int *lda, int *ipiv, Complex!(float) *b, int *ldb, Complex!(float) *work,
	 int *lwork, int *info);
 
  void chesvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(float) *a, int *lda, Complex!(float) *af, int *ldaf, int *
	ipiv, Complex!(float) *b, int *ldb, Complex!(float) *x, int *ldx, float *rcond,
	 float *ferr, float *berr, Complex!(float) *work, int *lwork, float *rwork, 
	int *info);
 
  void chetf2_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *ipiv, int *info);
 
  void chetrd_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 float *d__, float *e, Complex!(float) *tau, Complex!(float) *work, int *lwork, 
	int *info);
 
  void chetrf_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *ipiv, Complex!(float) *work, int *lwork, int *info);
 
  void chetri_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *ipiv, Complex!(float) *work, int *info);
 
  void chetrs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	a, int *lda, int *ipiv, Complex!(float) *b, int *ldb, int *
	info);
 
  void chgeqz_(char *job, char *compq, char *compz, int *n, 
	int *ilo, int *ihi, Complex!(float) *a, int *lda, Complex!(float) *b, 
	int *ldb, Complex!(float) *alpha, Complex!(float) *beta, Complex!(float) *q, int *ldq,
	 Complex!(float) *z__, int *ldz, Complex!(float) *work, int *lwork, float *
	rwork, int *info);
 
  void chpcon_(char *uplo, int *n, Complex!(float) *ap, int *
	ipiv, float *anorm, float *rcond, Complex!(float) *work, int *info);
 
  void chpev_(char *jobz, char *uplo, int *n, Complex!(float) *ap, 
	float *w, Complex!(float) *z__, int *ldz, Complex!(float) *work, float *rwork, 
	int *info);
 
  void chpevd_(char *jobz, char *uplo, int *n, Complex!(float) *ap, 
	float *w, Complex!(float) *z__, int *ldz, Complex!(float) *work, int *lwork, 
	float *rwork, int *lrwork, int *iwork, int *liwork, 
	int *info);
 
  void chpevx_(char *jobz, char *range, char *uplo, int *n, 
	Complex!(float) *ap, float *vl, float *vu, int *il, int *iu, float *
	abstol, int *m, float *w, Complex!(float) *z__, int *ldz, Complex!(float) *
	work, float *rwork, int *iwork, int *ifail, int *info);
 
  void chpgst_(int *itype, char *uplo, int *n, Complex!(float) *
	ap, Complex!(float) *bp, int *info);
 
  void chpgv_(int *itype, char *jobz, char *uplo, int *
	n, Complex!(float) *ap, Complex!(float) *bp, float *w, Complex!(float) *z__, int *ldz, 
	Complex!(float) *work, float *rwork, int *info);
 
  void chpgvd_(int *itype, char *jobz, char *uplo, int *
	n, Complex!(float) *ap, Complex!(float) *bp, float *w, Complex!(float) *z__, int *ldz, 
	Complex!(float) *work, int *lwork, float *rwork, int *lrwork, int *
	iwork, int *liwork, int *info);
 
  void chpgvx_(int *itype, char *jobz, char *range, char *
	uplo, int *n, Complex!(float) *ap, Complex!(float) *bp, float *vl, float *vu, 
	int *il, int *iu, float *abstol, int *m, float *w, Complex!(float) *
	z__, int *ldz, Complex!(float) *work, float *rwork, int *iwork, 
	int *ifail, int *info);
 
  void chprfs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	ap, Complex!(float) *afp, int *ipiv, Complex!(float) *b, int *ldb, Complex!(float) *x,
	 int *ldx, float *ferr, float *berr, Complex!(float) *work, float *rwork, 
	int *info);
 
  void chpsv_(char *uplo, int *n, int *nrhs, Complex!(float) *
	ap, int *ipiv, Complex!(float) *b, int *ldb, int *info);
 
  void chpsvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(float) *ap, Complex!(float) *afp, int *ipiv, Complex!(float) *b, int *
	ldb, Complex!(float) *x, int *ldx, float *rcond, float *ferr, float *berr, 
	Complex!(float) *work, float *rwork, int *info);
 
  void chptrd_(char *uplo, int *n, Complex!(float) *ap, float *d__, 
	float *e, Complex!(float) *tau, int *info);
 
  void chptrf_(char *uplo, int *n, Complex!(float) *ap, int *
	ipiv, int *info);
 
  void chptri_(char *uplo, int *n, Complex!(float) *ap, int *
	ipiv, Complex!(float) *work, int *info);
 
  void chptrs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	ap, int *ipiv, Complex!(float) *b, int *ldb, int *info);
 
  void chsein_(char *side, char *eigsrc, char *initv, logical *
	select, int *n, Complex!(float) *h__, int *ldh, Complex!(float) *w, Complex!(float) *
	vl, int *ldvl, Complex!(float) *vr, int *ldvr, int *mm, int *
	m, Complex!(float) *work, float *rwork, int *ifaill, int *ifailr, 
	int *info);
 
  void chseqr_(char *job, char *compz, int *n, int *ilo,
	 int *ihi, Complex!(float) *h__, int *ldh, Complex!(float) *w, Complex!(float) *z__, 
	int *ldz, Complex!(float) *work, int *lwork, int *info);
 
  void clabrd_(int *m, int *n, int *nb, Complex!(float) *a, 
	int *lda, float *d__, float *e, Complex!(float) *tauq, Complex!(float) *taup, 
	Complex!(float) *x, int *ldx, Complex!(float) *y, int *ldy);
 
  void clacgv_(int *n, Complex!(float) *x, int *incx);
 
  void clacon_(int *n, Complex!(float) *v, Complex!(float) *x, float *est, 
	int *kase);
 
  void clacp2_(char *uplo, int *m, int *n, float *a, 
	int *lda, Complex!(float) *b, int *ldb);
 
  void clacpy_(char *uplo, int *m, int *n, Complex!(float) *a, 
	int *lda, Complex!(float) *b, int *ldb);
 
  void clacrm_(int *m, int *n, Complex!(float) *a, int *lda,
	 float *b, int *ldb, Complex!(float) *c__, int *ldc, float *rwork);
 
  void clacrt_(int *n, Complex!(float) *cx, int *incx, Complex!(float) *
	cy, int *incy, Complex!(float) *c__, Complex!(float) *s);
 
  void claed0_(int *qsiz, int *n, float *d__, float *e, 
	Complex!(float) *q, int *ldq, Complex!(float) *qstore, int *ldqs, float *rwork,
	 int *iwork, int *info);
 
  void claed7_(int *n, int *cutpnt, int *qsiz, 
	int *tlvls, int *curlvl, int *curpbm, float *d__, Complex!(float) *
	q, int *ldq, float *rho, int *indxq, float *qstore, int *
	qptr, int *prmptr, int *perm, int *givptr, int *
	givcol, float *givnum, Complex!(float) *work, float *rwork, int *iwork, 
	int *info);
 
  void claed8_(int *k, int *n, int *qsiz, Complex!(float) *
	q, int *ldq, float *d__, float *rho, int *cutpnt, float *z__, 
	float *dlamda, Complex!(float) *q2, int *ldq2, float *w, int *indxp, 
	int *indx, int *indxq, int *perm, int *givptr, 
	int *givcol, float *givnum, int *info);
 
  void claein_(logical *rightv, logical *noinit, int *n, 
	Complex!(float) *h__, int *ldh, Complex!(float) *w, Complex!(float) *v, Complex!(float) *b, 
	int *ldb, float *rwork, float *eps3, float *smlnum, int *info);
 
  void claesy_(Complex!(float) *a, Complex!(float) *b, Complex!(float) *c__, Complex!(float) *
	rt1, Complex!(float) *rt2, Complex!(float) *evscal, Complex!(float) *cs1, Complex!(float) *sn1);
 
  void claev2_(Complex!(float) *a, Complex!(float) *b, Complex!(float) *c__, float *rt1, 
	float *rt2, float *cs1, Complex!(float) *sn1);
 
  void clags2_(logical *upper, float *a1, Complex!(float) *a2, float *a3, 
	float *b1, Complex!(float) *b2, float *b3, float *csu, Complex!(float) *snu, float *csv, 
	Complex!(float) *snv, float *csq, Complex!(float) *snq);
 
  void clagtm_(char *trans, int *n, int *nrhs, float *
	alpha, Complex!(float) *dl, Complex!(float) *d__, Complex!(float) *du, Complex!(float) *x, int *
	ldx, float *beta, Complex!(float) *b, int *ldb);
 
  void clahef_(char *uplo, int *n, int *nb, int *kb,
	 Complex!(float) *a, int *lda, int *ipiv, Complex!(float) *w, int *ldw, 
	int *info);
 
  void clahqr_(logical *wantt, logical *wantz, int *n, 
	int *ilo, int *ihi, Complex!(float) *h__, int *ldh, Complex!(float) *w, 
	int *iloz, int *ihiz, Complex!(float) *z__, int *ldz, int *
	info);
 
  void clahrd_(int *n, int *k, int *nb, Complex!(float) *a, 
	int *lda, Complex!(float) *tau, Complex!(float) *t, int *ldt, Complex!(float) *y, 
	int *ldy);
 
  void claic1_(int *job, int *j, Complex!(float) *x, float *sest,
	 Complex!(float) *w, Complex!(float) *gamma, float *sestpr, Complex!(float) *s, Complex!(float) *c__);
 
  void clals0_(int *icompq, int *nl, int *nr, 
	int *sqre, int *nrhs, Complex!(float) *b, int *ldb, Complex!(float) *bx, 
	int *ldbx, int *perm, int *givptr, int *givcol, 
	int *ldgcol, float *givnum, int *ldgnum, float *poles, float *
	difl, float *difr, float *z__, int *k, float *c__, float *s, float *
	rwork, int *info);
 
  void clalsa_(int *icompq, int *smlsiz, int *n, 
	int *nrhs, Complex!(float) *b, int *ldb, Complex!(float) *bx, int *ldbx, 
	float *u, int *ldu, float *vt, int *k, float *difl, float *difr, 
	float *z__, float *poles, int *givptr, int *givcol, int *
	ldgcol, int *perm, float *givnum, float *c__, float *s, float *rwork, 
	int *iwork, int *info);
 
  void clapll_(int *n, Complex!(float) *x, int *incx, Complex!(float) *
	y, int *incy, float *ssmin);
 
  void clapmt_(logical *forwrd, int *m, int *n, Complex!(float) 
	*x, int *ldx, int *k);
 
  void claqgb_(int *m, int *n, int *kl, int *ku,
	 Complex!(float) *ab, int *ldab, float *r__, float *c__, float *rowcnd, float 
	*colcnd, float *amax, char *equed);
 
  void claqge_(int *m, int *n, Complex!(float) *a, int *lda,
	 float *r__, float *c__, float *rowcnd, float *colcnd, float *amax, char *
	equed);
 
  void claqhb_(char *uplo, int *n, int *kd, Complex!(float) *ab,
	 int *ldab, float *s, float *scond, float *amax, char *equed);
 
  void claqhe_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 float *s, float *scond, float *amax, char *equed);
 
  void claqhp_(char *uplo, int *n, Complex!(float) *ap, float *s, 
	float *scond, float *amax, char *equed);
 
  void claqp2_(int *m, int *n, int *offset, Complex!(float) 
	*a, int *lda, int *jpvt, Complex!(float) *tau, float *vn1, float *vn2, 
	Complex!(float) *work);
 
  void claqps_(int *m, int *n, int *offset, int 
	*nb, int *kb, Complex!(float) *a, int *lda, int *jpvt, Complex!(float) *
	tau, float *vn1, float *vn2, Complex!(float) *auxv, Complex!(float) *f, int *ldf);
 
  void claqsb_(char *uplo, int *n, int *kd, Complex!(float) *ab,
	 int *ldab, float *s, float *scond, float *amax, char *equed);
 
  void claqsp_(char *uplo, int *n, Complex!(float) *ap, float *s, 
	float *scond, float *amax, char *equed);
 
  void claqsy_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 float *s, float *scond, float *amax, char *equed);
 
  void clar1v_(int *n, int *b1, int *bn, float *
	sigma, float *d__, float *l, float *ld, float *lld, float *gersch, Complex!(float) 
	*z__, float *ztz, float *mingma, int *r__, int *isuppz, float *
	work);
 
  void clar2v_(int *n, Complex!(float) *x, Complex!(float) *y, Complex!(float) *z__,
	 int *incx, float *c__, Complex!(float) *s, int *incc);
 
  void clarcm_(int *m, int *n, float *a, int *lda, 
	Complex!(float) *b, int *ldb, Complex!(float) *c__, int *ldc, float *rwork);
 
  void clarf_(char *side, int *m, int *n, Complex!(float) *v, 
	int *incv, Complex!(float) *tau, Complex!(float) *c__, int *ldc, Complex!(float) *
	work);
 
  void clarfb_(char *side, char *trans, char *direct, char *
	storev, int *m, int *n, int *k, Complex!(float) *v, int *ldv, 
	Complex!(float) *t, int *ldt, Complex!(float) *c__, int *ldc, Complex!(float) *work, 
	int *ldwork);
 
  void clarfg_(int *n, Complex!(float) *alpha, Complex!(float) *x, int *
	incx, Complex!(float) *tau);
 
  void clarft_(char *direct, char *storev, int *n, int *
	k, Complex!(float) *v, int *ldv, Complex!(float) *tau, Complex!(float) *t, int *ldt);
 
  void clarfx_(char *side, int *m, int *n, Complex!(float) *v, 
	Complex!(float) *tau, Complex!(float) *c__, int *ldc, Complex!(float) *work);
 
  void clargv_(int *n, Complex!(float) *x, int *incx, Complex!(float) *
	y, int *incy, float *c__, int *incc);
 
  void clarnv_(int *idist, int *iseed, int *n, 
	Complex!(float) *x);
 
  void clarrv_(int *n, float *d__, float *l, int *isplit, 
	int *m, float *w, int *iblock, float *gersch, float *tol, 
	Complex!(float) *z__, int *ldz, int *isuppz, float *work, int *
	iwork, int *info);
 
  void clartg_(Complex!(float) *f, Complex!(float) *g, float *cs, Complex!(float) *sn, 
	Complex!(float) *r__);
 
  void clartv_(int *n, Complex!(float) *x, int *incx, Complex!(float) *
	y, int *incy, float *c__, Complex!(float) *s, int *incc);
 
  void clarz_(char *side, int *m, int *n, int *l, 
	Complex!(float) *v, int *incv, Complex!(float) *tau, Complex!(float) *c__, int *ldc, 
	Complex!(float) *work);
 
  void clarzb_(char *side, char *trans, char *direct, char *
	storev, int *m, int *n, int *k, int *l, Complex!(float) *v, 
	int *ldv, Complex!(float) *t, int *ldt, Complex!(float) *c__, int *ldc, 
	Complex!(float) *work, int *ldwork);
 
  void clarzt_(char *direct, char *storev, int *n, int *
	k, Complex!(float) *v, int *ldv, Complex!(float) *tau, Complex!(float) *t, int *ldt);
 
  void clascl_(char *type__, int *kl, int *ku, float *
	cfrom, float *cto, int *m, int *n, Complex!(float) *a, int *lda, 
	int *info);
 
  void claset_(char *uplo, int *m, int *n, Complex!(float) *
	alpha, Complex!(float) *beta, Complex!(float) *a, int *lda);
 
  void clasr_(char *side, char *pivot, char *direct, int *m,
	 int *n, float *c__, float *s, Complex!(float) *a, int *lda);
 
  void classq_(int *n, Complex!(float) *x, int *incx, float *
	scale, float *sumsq);
 
  void claswp_(int *n, Complex!(float) *a, int *lda, int *
	k1, int *k2, int *ipiv, int *incx);
 
  void clasyf_(char *uplo, int *n, int *nb, int *kb,
	 Complex!(float) *a, int *lda, int *ipiv, Complex!(float) *w, int *ldw, 
	int *info);
 
  void clatbs_(char *uplo, char *trans, char *diag, char *
	normin, int *n, int *kd, Complex!(float) *ab, int *ldab, Complex!(float) *
	x, float *scale, float *cnorm, int *info);
 
  void clatdf_(int *ijob, int *n, Complex!(float) *z__, int 
	*ldz, Complex!(float) *rhs, float *rdsum, float *rdscal, int *ipiv, int 
	*jpiv);
 
  void clatps_(char *uplo, char *trans, char *diag, char *
	normin, int *n, Complex!(float) *ap, Complex!(float) *x, float *scale, float *cnorm,
	 int *info);
 
  void clatrd_(char *uplo, int *n, int *nb, Complex!(float) *a, 
	int *lda, float *e, Complex!(float) *tau, Complex!(float) *w, int *ldw);
 
  void clatrs_(char *uplo, char *trans, char *diag, char *
	normin, int *n, Complex!(float) *a, int *lda, Complex!(float) *x, float *scale,
	 float *cnorm, int *info);
 
  void clatrz_(int *m, int *n, int *l, Complex!(float) *a, 
	int *lda, Complex!(float) *tau, Complex!(float) *work);
 
  void clatzm_(char *side, int *m, int *n, Complex!(float) *v, 
	int *incv, Complex!(float) *tau, Complex!(float) *c1, Complex!(float) *c2, int *ldc, 
	Complex!(float) *work);
 
  void clauu2_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *info);
 
  void clauum_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *info);
 
  void cpbcon_(char *uplo, int *n, int *kd, Complex!(float) *ab,
	 int *ldab, float *anorm, float *rcond, Complex!(float) *work, float *rwork, 
	int *info);
 
  void cpbequ_(char *uplo, int *n, int *kd, Complex!(float) *ab,
	 int *ldab, float *s, float *scond, float *amax, int *info);
 
  void cpbrfs_(char *uplo, int *n, int *kd, int *
	nrhs, Complex!(float) *ab, int *ldab, Complex!(float) *afb, int *ldafb, 
	Complex!(float) *b, int *ldb, Complex!(float) *x, int *ldx, float *ferr, float *
	berr, Complex!(float) *work, float *rwork, int *info);
 
  void cpbstf_(char *uplo, int *n, int *kd, Complex!(float) *ab,
	 int *ldab, int *info);
 
  void cpbsv_(char *uplo, int *n, int *kd, int *
	nrhs, Complex!(float) *ab, int *ldab, Complex!(float) *b, int *ldb, int *
	info);
 
  void cpbsvx_(char *fact, char *uplo, int *n, int *kd, 
	int *nrhs, Complex!(float) *ab, int *ldab, Complex!(float) *afb, int *
	ldafb, char *equed, float *s, Complex!(float) *b, int *ldb, Complex!(float) *x, 
	int *ldx, float *rcond, float *ferr, float *berr, Complex!(float) *work, 
	float *rwork, int *info);
 
  void cpbtf2_(char *uplo, int *n, int *kd, Complex!(float) *ab,
	 int *ldab, int *info);
 
  void cpbtrf_(char *uplo, int *n, int *kd, Complex!(float) *ab,
	 int *ldab, int *info);
 
  void cpbtrs_(char *uplo, int *n, int *kd, int *
	nrhs, Complex!(float) *ab, int *ldab, Complex!(float) *b, int *ldb, int *
	info);
 
  void cpocon_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 float *anorm, float *rcond, Complex!(float) *work, float *rwork, int *info);
 
  void cpoequ_(int *n, Complex!(float) *a, int *lda, float *s, 
	float *scond, float *amax, int *info);
 
  void cporfs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	a, int *lda, Complex!(float) *af, int *ldaf, Complex!(float) *b, int *ldb,
	 Complex!(float) *x, int *ldx, float *ferr, float *berr, Complex!(float) *work, 
	float *rwork, int *info);
 
  void cposv_(char *uplo, int *n, int *nrhs, Complex!(float) *a,
	 int *lda, Complex!(float) *b, int *ldb, int *info);
 
  void cposvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(float) *a, int *lda, Complex!(float) *af, int *ldaf, char *
	equed, float *s, Complex!(float) *b, int *ldb, Complex!(float) *x, int *ldx, 
	float *rcond, float *ferr, float *berr, Complex!(float) *work, float *rwork, 
	int *info);
 
  void cpotf2_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *info);
 
  void cpotrf_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *info);
 
  void cpotri_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *info);
 
  void cpotrs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	a, int *lda, Complex!(float) *b, int *ldb, int *info);
 
  void cppcon_(char *uplo, int *n, Complex!(float) *ap, float *anorm,
	 float *rcond, Complex!(float) *work, float *rwork, int *info);
 
  void cppequ_(char *uplo, int *n, Complex!(float) *ap, float *s, 
	float *scond, float *amax, int *info);
 
  void cpprfs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	ap, Complex!(float) *afp, Complex!(float) *b, int *ldb, Complex!(float) *x, int *ldx, 
	float *ferr, float *berr, Complex!(float) *work, float *rwork, int *info);
 
  void cppsv_(char *uplo, int *n, int *nrhs, Complex!(float) *
	ap, Complex!(float) *b, int *ldb, int *info);
 
  void cppsvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(float) *ap, Complex!(float) *afp, char *equed, float *s, Complex!(float) *b, 
	int *ldb, Complex!(float) *x, int *ldx, float *rcond, float *ferr, float 
	*berr, Complex!(float) *work, float *rwork, int *info);
 
  void cpptrf_(char *uplo, int *n, Complex!(float) *ap, int *
	info);
 
  void cpptri_(char *uplo, int *n, Complex!(float) *ap, int *
	info);
 
  void cpptrs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	ap, Complex!(float) *b, int *ldb, int *info);
 
  void cptcon_(int *n, float *d__, Complex!(float) *e, float *anorm, 
	float *rcond, float *rwork, int *info);
 
  void cptrfs_(char *uplo, int *n, int *nrhs, float *d__,
	 Complex!(float) *e, float *df, Complex!(float) *ef, Complex!(float) *b, int *ldb, Complex!(float) 
	*x, int *ldx, float *ferr, float *berr, Complex!(float) *work, float *rwork, 
	int *info);
 
  void cptsv_(int *n, int *nrhs, float *d__, Complex!(float) *e, 
	Complex!(float) *b, int *ldb, int *info);
 
  void cptsvx_(char *fact, int *n, int *nrhs, float *d__,
	 Complex!(float) *e, float *df, Complex!(float) *ef, Complex!(float) *b, int *ldb, Complex!(float) 
	*x, int *ldx, float *rcond, float *ferr, float *berr, Complex!(float) *work, 
	float *rwork, int *info);
 
  void cpttrf_(int *n, float *d__, Complex!(float) *e, int *info);
 
  void cpttrs_(char *uplo, int *n, int *nrhs, float *d__,
	 Complex!(float) *e, Complex!(float) *b, int *ldb, int *info);
 
  void cptts2_(int *iuplo, int *n, int *nrhs, float *
	d__, Complex!(float) *e, Complex!(float) *b, int *ldb);
 
  void crot_(int *n, Complex!(float) *cx, int *incx, Complex!(float) *
	cy, int *incy, float *c__, Complex!(float) *s);
 
  void cspcon_(char *uplo, int *n, Complex!(float) *ap, int *
	ipiv, float *anorm, float *rcond, Complex!(float) *work, int *info);
 
  void cspmv_(char *uplo, int *n, Complex!(float) *alpha, Complex!(float) *
	ap, Complex!(float) *x, int *incx, Complex!(float) *beta, Complex!(float) *y, int *
	incy);
 
  void cspr_(char *uplo, int *n, Complex!(float) *alpha, Complex!(float) *x,
	 int *incx, Complex!(float) *ap);
 
  void csprfs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	ap, Complex!(float) *afp, int *ipiv, Complex!(float) *b, int *ldb, Complex!(float) *x,
	 int *ldx, float *ferr, float *berr, Complex!(float) *work, float *rwork, 
	int *info);
 
  void cspsv_(char *uplo, int *n, int *nrhs, Complex!(float) *
	ap, int *ipiv, Complex!(float) *b, int *ldb, int *info);
 
  void cspsvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(float) *ap, Complex!(float) *afp, int *ipiv, Complex!(float) *b, int *
	ldb, Complex!(float) *x, int *ldx, float *rcond, float *ferr, float *berr, 
	Complex!(float) *work, float *rwork, int *info);
 
  void csptrf_(char *uplo, int *n, Complex!(float) *ap, int *
	ipiv, int *info);
 
  void csptri_(char *uplo, int *n, Complex!(float) *ap, int *
	ipiv, Complex!(float) *work, int *info);
 
  void csptrs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	ap, int *ipiv, Complex!(float) *b, int *ldb, int *info);
 
  void csrot_(int *n, Complex!(float) *cx, int *incx, Complex!(float) *
	cy, int *incy, float *c__, float *s);
 
  void csrscl_(int *n, float *sa, Complex!(float) *sx, int *incx);
 
  void cstedc_(char *compz, int *n, float *d__, float *e, 
	Complex!(float) *z__, int *ldz, Complex!(float) *work, int *lwork, float *
	rwork, int *lrwork, int *iwork, int *liwork, int *
	info);
 
  void cstein_(int *n, float *d__, float *e, int *m, float 
	*w, int *iblock, int *isplit, Complex!(float) *z__, int *ldz, 
	float *work, int *iwork, int *ifail, int *info);
 
  void csteqr_(char *compz, int *n, float *d__, float *e, 
	Complex!(float) *z__, int *ldz, float *work, int *info);
 
  void csycon_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *ipiv, float *anorm, float *rcond, Complex!(float) *work, int *
	info);
 
  void csymv_(char *uplo, int *n, Complex!(float) *alpha, Complex!(float) *
	a, int *lda, Complex!(float) *x, int *incx, Complex!(float) *beta, Complex!(float) *y,
	 int *incy);
 
  void csyr_(char *uplo, int *n, Complex!(float) *alpha, Complex!(float) *x,
	 int *incx, Complex!(float) *a, int *lda);
 
  void csyrfs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	a, int *lda, Complex!(float) *af, int *ldaf, int *ipiv, Complex!(float) *
	b, int *ldb, Complex!(float) *x, int *ldx, float *ferr, float *berr, 
	Complex!(float) *work, float *rwork, int *info);
 
  void csysv_(char *uplo, int *n, int *nrhs, Complex!(float) *a,
	 int *lda, int *ipiv, Complex!(float) *b, int *ldb, Complex!(float) *work,
	 int *lwork, int *info);
 
  void csysvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(float) *a, int *lda, Complex!(float) *af, int *ldaf, int *
	ipiv, Complex!(float) *b, int *ldb, Complex!(float) *x, int *ldx, float *rcond,
	 float *ferr, float *berr, Complex!(float) *work, int *lwork, float *rwork, 
	int *info);
 
  void csytf2_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *ipiv, int *info);
 
  void csytrf_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *ipiv, Complex!(float) *work, int *lwork, int *info);
 
  void csytri_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 int *ipiv, Complex!(float) *work, int *info);
 
  void csytrs_(char *uplo, int *n, int *nrhs, Complex!(float) *
	a, int *lda, int *ipiv, Complex!(float) *b, int *ldb, int *
	info);
 
  void ctbcon_(char *norm, char *uplo, char *diag, int *n, 
	int *kd, Complex!(float) *ab, int *ldab, float *rcond, Complex!(float) *work, 
	float *rwork, int *info);
 
  void ctbrfs_(char *uplo, char *trans, char *diag, int *n, 
	int *kd, int *nrhs, Complex!(float) *ab, int *ldab, Complex!(float) *b, 
	int *ldb, Complex!(float) *x, int *ldx, float *ferr, float *berr, 
	Complex!(float) *work, float *rwork, int *info);
 
  void ctbtrs_(char *uplo, char *trans, char *diag, int *n, 
	int *kd, int *nrhs, Complex!(float) *ab, int *ldab, Complex!(float) *b, 
	int *ldb, int *info);
 
  void ctgevc_(char *side, char *howmny, logical *select, 
	int *n, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, 
	Complex!(float) *vl, int *ldvl, Complex!(float) *vr, int *ldvr, int *mm, 
	int *m, Complex!(float) *work, float *rwork, int *info);
 
  void ctgex2_(logical *wantq, logical *wantz, int *n, 
	Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, Complex!(float) *q, 
	int *ldq, Complex!(float) *z__, int *ldz, int *j1, int *info);
 
  void ctgexc_(logical *wantq, logical *wantz, int *n, 
	Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, Complex!(float) *q, 
	int *ldq, Complex!(float) *z__, int *ldz, int *ifst, int *
	ilst, int *info);
 
  void ctgsen_(int *ijob, logical *wantq, logical *wantz, 
	logical *select, int *n, Complex!(float) *a, int *lda, Complex!(float) *b, 
	int *ldb, Complex!(float) *alpha, Complex!(float) *beta, Complex!(float) *q, int *ldq,
	 Complex!(float) *z__, int *ldz, int *m, float *pl, float *pr, float *
	dif, Complex!(float) *work, int *lwork, int *iwork, int *liwork, 
	int *info);
 
  void ctgsja_(char *jobu, char *jobv, char *jobq, int *m, 
	int *p, int *n, int *k, int *l, Complex!(float) *a, int *
	lda, Complex!(float) *b, int *ldb, float *tola, float *tolb, float *alpha, 
	float *beta, Complex!(float) *u, int *ldu, Complex!(float) *v, int *ldv, 
	Complex!(float) *q, int *ldq, Complex!(float) *work, int *ncycle, int *
	info);
 
  void ctgsna_(char *job, char *howmny, logical *select, 
	int *n, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, 
	Complex!(float) *vl, int *ldvl, Complex!(float) *vr, int *ldvr, float *s, float 
	*dif, int *mm, int *m, Complex!(float) *work, int *lwork, int 
	*iwork, int *info);
 
  void ctgsy2_(char *trans, int *ijob, int *m, int *
	n, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, Complex!(float) *c__, 
	int *ldc, Complex!(float) *d__, int *ldd, Complex!(float) *e, int *lde, 
	Complex!(float) *f, int *ldf, float *scale, float *rdsum, float *rdscal, 
	int *info);
 
  void ctgsyl_(char *trans, int *ijob, int *m, int *
	n, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, Complex!(float) *c__, 
	int *ldc, Complex!(float) *d__, int *ldd, Complex!(float) *e, int *lde, 
	Complex!(float) *f, int *ldf, float *scale, float *dif, Complex!(float) *work, 
	int *lwork, int *iwork, int *info);
 
  void ctpcon_(char *norm, char *uplo, char *diag, int *n, 
	Complex!(float) *ap, float *rcond, Complex!(float) *work, float *rwork, int *info);
 
  void ctprfs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, Complex!(float) *ap, Complex!(float) *b, int *ldb, Complex!(float) *x, 
	int *ldx, float *ferr, float *berr, Complex!(float) *work, float *rwork, 
	int *info);
 
  void ctptri_(char *uplo, char *diag, int *n, Complex!(float) *ap, 
	int *info);
 
  void ctptrs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, Complex!(float) *ap, Complex!(float) *b, int *ldb, int *info);
 
  void ctrcon_(char *norm, char *uplo, char *diag, int *n, 
	Complex!(float) *a, int *lda, float *rcond, Complex!(float) *work, float *rwork, 
	int *info);
 
  void ctrevc_(char *side, char *howmny, logical *select, 
	int *n, Complex!(float) *t, int *ldt, Complex!(float) *vl, int *ldvl, 
	Complex!(float) *vr, int *ldvr, int *mm, int *m, Complex!(float) *work, 
	float *rwork, int *info);
 
  void ctrexc_(char *compq, int *n, Complex!(float) *t, int *
	ldt, Complex!(float) *q, int *ldq, int *ifst, int *ilst, int *
	info);
 
  void ctrrfs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, 
	Complex!(float) *x, int *ldx, float *ferr, float *berr, Complex!(float) *work, float 
	*rwork, int *info);
 
  void ctrsen_(char *job, char *compq, logical *select, int 
	*n, Complex!(float) *t, int *ldt, Complex!(float) *q, int *ldq, Complex!(float) *w, 
	int *m, float *s, float *sep, Complex!(float) *work, int *lwork, 
	int *info);
 
  void ctrsna_(char *job, char *howmny, logical *select, 
	int *n, Complex!(float) *t, int *ldt, Complex!(float) *vl, int *ldvl, 
	Complex!(float) *vr, int *ldvr, float *s, float *sep, int *mm, int *
	m, Complex!(float) *work, int *ldwork, float *rwork, int *info);
 
  void ctrsyl_(char *trana, char *tranb, int *isgn, int 
	*m, int *n, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, 
	Complex!(float) *c__, int *ldc, float *scale, int *info);
 
  void ctrti2_(char *uplo, char *diag, int *n, Complex!(float) *a, 
	int *lda, int *info);
 
  void ctrtri_(char *uplo, char *diag, int *n, Complex!(float) *a, 
	int *lda, int *info);
 
  void ctrtrs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, Complex!(float) *a, int *lda, Complex!(float) *b, int *ldb, 
	int *info);
 
  void ctzrqf_(int *m, int *n, Complex!(float) *a, int *lda,
	 Complex!(float) *tau, int *info);
 
  void ctzrzf_(int *m, int *n, Complex!(float) *a, int *lda,
	 Complex!(float) *tau, Complex!(float) *work, int *lwork, int *info);
 
  void cung2l_(int *m, int *n, int *k, Complex!(float) *a, 
	int *lda, Complex!(float) *tau, Complex!(float) *work, int *info);
 
  void cung2r_(int *m, int *n, int *k, Complex!(float) *a, 
	int *lda, Complex!(float) *tau, Complex!(float) *work, int *info);
 
  void cungbr_(char *vect, int *m, int *n, int *k, 
	Complex!(float) *a, int *lda, Complex!(float) *tau, Complex!(float) *work, int *lwork,
	 int *info);
 
  void cunghr_(int *n, int *ilo, int *ihi, Complex!(float) *
	a, int *lda, Complex!(float) *tau, Complex!(float) *work, int *lwork, int 
	*info);
 
  void cungl2_(int *m, int *n, int *k, Complex!(float) *a, 
	int *lda, Complex!(float) *tau, Complex!(float) *work, int *info);
 
  void cunglq_(int *m, int *n, int *k, Complex!(float) *a, 
	int *lda, Complex!(float) *tau, Complex!(float) *work, int *lwork, int *
	info);
 
  void cungql_(int *m, int *n, int *k, Complex!(float) *a, 
	int *lda, Complex!(float) *tau, Complex!(float) *work, int *lwork, int *
	info);
 
  void cungqr_(int *m, int *n, int *k, Complex!(float) *a, 
	int *lda, Complex!(float) *tau, Complex!(float) *work, int *lwork, int *
	info);
 
  void cungr2_(int *m, int *n, int *k, Complex!(float) *a, 
	int *lda, Complex!(float) *tau, Complex!(float) *work, int *info);
 
  void cungrq_(int *m, int *n, int *k, Complex!(float) *a, 
	int *lda, Complex!(float) *tau, Complex!(float) *work, int *lwork, int *
	info);
 
  void cungtr_(char *uplo, int *n, Complex!(float) *a, int *lda,
	 Complex!(float) *tau, Complex!(float) *work, int *lwork, int *info);
 
  void cunm2l_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(float) *a, int *lda, Complex!(float) *tau, Complex!(float) *c__, 
	int *ldc, Complex!(float) *work, int *info);
 
  void cunm2r_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(float) *a, int *lda, Complex!(float) *tau, Complex!(float) *c__, 
	int *ldc, Complex!(float) *work, int *info);
 
  void cunmbr_(char *vect, char *side, char *trans, int *m, 
	int *n, int *k, Complex!(float) *a, int *lda, Complex!(float) *tau, 
	Complex!(float) *c__, int *ldc, Complex!(float) *work, int *lwork, int *
	info);
 
  void cunmhr_(char *side, char *trans, int *m, int *n, 
	int *ilo, int *ihi, Complex!(float) *a, int *lda, Complex!(float) *tau, 
	Complex!(float) *c__, int *ldc, Complex!(float) *work, int *lwork, int *
	info);
 
  void cunml2_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(float) *a, int *lda, Complex!(float) *tau, Complex!(float) *c__, 
	int *ldc, Complex!(float) *work, int *info);
 
  void cunmlq_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(float) *a, int *lda, Complex!(float) *tau, Complex!(float) *c__, 
	int *ldc, Complex!(float) *work, int *lwork, int *info);
 
  void cunmql_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(float) *a, int *lda, Complex!(float) *tau, Complex!(float) *c__, 
	int *ldc, Complex!(float) *work, int *lwork, int *info);
 
  void cunmqr_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(float) *a, int *lda, Complex!(float) *tau, Complex!(float) *c__, 
	int *ldc, Complex!(float) *work, int *lwork, int *info);
 
  void cunmr2_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(float) *a, int *lda, Complex!(float) *tau, Complex!(float) *c__, 
	int *ldc, Complex!(float) *work, int *info);
 
  void cunmr3_(char *side, char *trans, int *m, int *n, 
	int *k, int *l, Complex!(float) *a, int *lda, Complex!(float) *tau, 
	Complex!(float) *c__, int *ldc, Complex!(float) *work, int *info);
 
  void cunmrq_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(float) *a, int *lda, Complex!(float) *tau, Complex!(float) *c__, 
	int *ldc, Complex!(float) *work, int *lwork, int *info);
 
  void cunmrz_(char *side, char *trans, int *m, int *n, 
	int *k, int *l, Complex!(float) *a, int *lda, Complex!(float) *tau, 
	Complex!(float) *c__, int *ldc, Complex!(float) *work, int *lwork, int *
	info);
 
  void cunmtr_(char *side, char *uplo, char *trans, int *m, 
	int *n, Complex!(float) *a, int *lda, Complex!(float) *tau, Complex!(float) *c__, 
	int *ldc, Complex!(float) *work, int *lwork, int *info);
 
  void cupgtr_(char *uplo, int *n, Complex!(float) *ap, Complex!(float) *
	tau, Complex!(float) *q, int *ldq, Complex!(float) *work, int *info);
 
  void cupmtr_(char *side, char *uplo, char *trans, int *m, 
	int *n, Complex!(float) *ap, Complex!(float) *tau, Complex!(float) *c__, int *ldc, 
	Complex!(float) *work, int *info);
 
  void dbdsdc_(char *uplo, char *compq, int *n, double *
	d__, double *e, double *u, int *ldu, double *vt, 
	int *ldvt, double *q, int *iq, double *work, int *
	iwork, int *info);
 
  void dbdsqr_(char *uplo, int *n, int *ncvt, int *
	nru, int *ncc, double *d__, double *e, double *vt, 
	int *ldvt, double *u, int *ldu, double *c__, int *
	ldc, double *work, int *info);
 
  void ddisna_(char *job, int *m, int *n, double *
	d__, double *sep, int *info);
 
  void dgbbrd_(char *vect, int *m, int *n, int *ncc,
	 int *kl, int *ku, double *ab, int *ldab, double *
	d__, double *e, double *q, int *ldq, double *pt, 
	int *ldpt, double *c__, int *ldc, double *work, 
	int *info);
 
  void dgbcon_(char *norm, int *n, int *kl, int *ku,
	 double *ab, int *ldab, int *ipiv, double *anorm, 
	double *rcond, double *work, int *iwork, int *info);
 
  void dgbequ_(int *m, int *n, int *kl, int *ku,
	 double *ab, int *ldab, double *r__, double *c__, 
	double *rowcnd, double *colcnd, double *amax, int *
	info);
 
  void dgbrfs_(char *trans, int *n, int *kl, int *
	ku, int *nrhs, double *ab, int *ldab, double *afb, 
	int *ldafb, int *ipiv, double *b, int *ldb, 
	double *x, int *ldx, double *ferr, double *berr, 
	double *work, int *iwork, int *info);
 
  void dgbsv_(int *n, int *kl, int *ku, int *
	nrhs, double *ab, int *ldab, int *ipiv, double *b, 
	int *ldb, int *info);
 
  void dgbsvx_(char *fact, char *trans, int *n, int *kl,
	 int *ku, int *nrhs, double *ab, int *ldab, 
	double *afb, int *ldafb, int *ipiv, char *equed, 
	double *r__, double *c__, double *b, int *ldb, 
	double *x, int *ldx, double *rcond, double *ferr, 
	double *berr, double *work, int *iwork, int *info);
 
  void dgbtf2_(int *m, int *n, int *kl, int *ku,
	 double *ab, int *ldab, int *ipiv, int *info);
 
  void dgbtrf_(int *m, int *n, int *kl, int *ku,
	 double *ab, int *ldab, int *ipiv, int *info);
 
  void dgbtrs_(char *trans, int *n, int *kl, int *
	ku, int *nrhs, double *ab, int *ldab, int *ipiv, 
	double *b, int *ldb, int *info);
 
  void dgebak_(char *job, char *side, int *n, int *ilo, 
	int *ihi, double *scale, int *m, double *v, int *
	ldv, int *info);
 
  void dgebal_(char *job, int *n, double *a, int *
	lda, int *ilo, int *ihi, double *scale, int *info);
 
  void dgebd2_(int *m, int *n, double *a, int *
	lda, double *d__, double *e, double *tauq, double *
	taup, double *work, int *info);
 
  void dgebrd_(int *m, int *n, double *a, int *
	lda, double *d__, double *e, double *tauq, double *
	taup, double *work, int *lwork, int *info);
 
  void dgecon_(char *norm, int *n, double *a, int *
	lda, double *anorm, double *rcond, double *work, int *
	iwork, int *info);
 
  void dgeequ_(int *m, int *n, double *a, int *
	lda, double *r__, double *c__, double *rowcnd, double 
	*colcnd, double *amax, int *info);
 
  void dgees_(char *jobvs, char *sort, L_fp select, int *n, 
	double *a, int *lda, int *sdim, double *wr, 
	double *wi, double *vs, int *ldvs, double *work, 
	int *lwork, logical *bwork, int *info);
 
  void dgeesx_(char *jobvs, char *sort, L_fp select, char *
	sense, int *n, double *a, int *lda, int *sdim, 
	double *wr, double *wi, double *vs, int *ldvs, 
	double *rconde, double *rcondv, double *work, int *
	lwork, int *iwork, int *liwork, logical *bwork, int *info);
 
  void dgeev_(char *jobvl, char *jobvr, int *n, double *
	a, int *lda, double *wr, double *wi, double *vl, 
	int *ldvl, double *vr, int *ldvr, double *work, 
	int *lwork, int *info);
 
  void dgeevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, int *n, double *a, int *lda, double *wr, 
	double *wi, double *vl, int *ldvl, double *vr, 
	int *ldvr, int *ilo, int *ihi, double *scale, 
	double *abnrm, double *rconde, double *rcondv, double 
	*work, int *lwork, int *iwork, int *info);
 
  void dgegs_(char *jobvsl, char *jobvsr, int *n, 
	double *a, int *lda, double *b, int *ldb, double *
	alphar, double *alphai, double *beta, double *vsl, 
	int *ldvsl, double *vsr, int *ldvsr, double *work, 
	int *lwork, int *info);
 
  void dgegv_(char *jobvl, char *jobvr, int *n, double *
	a, int *lda, double *b, int *ldb, double *alphar, 
	double *alphai, double *beta, double *vl, int *ldvl, 
	double *vr, int *ldvr, double *work, int *lwork, 
	int *info);
 
  void dgehd2_(int *n, int *ilo, int *ihi, 
	double *a, int *lda, double *tau, double *work, 
	int *info);
 
  void dgehrd_(int *n, int *ilo, int *ihi, 
	double *a, int *lda, double *tau, double *work, 
	int *lwork, int *info);
 
  void dgelq2_(int *m, int *n, double *a, int *
	lda, double *tau, double *work, int *info);
 
  void dgelqf_(int *m, int *n, double *a, int *
	lda, double *tau, double *work, int *lwork, int *info);
 
  void dgels_(char *trans, int *m, int *n, int *
	nrhs, double *a, int *lda, double *b, int *ldb, 
	double *work, int *lwork, int *info);
 
  void dgelsd_(int *m, int *n, int *nrhs, 
	double *a, int *lda, double *b, int *ldb, double *
	s, double *rcond, int *rank, double *work, int *lwork,
	 int *iwork, int *info);
 
  void dgelss_(int *m, int *n, int *nrhs, 
	double *a, int *lda, double *b, int *ldb, double *
	s, double *rcond, int *rank, double *work, int *lwork,
	 int *info);
 
  void dgelsx_(int *m, int *n, int *nrhs, 
	double *a, int *lda, double *b, int *ldb, int *
	jpvt, double *rcond, int *rank, double *work, int *
	info);
 
  void dgelsy_(int *m, int *n, int *nrhs, 
	double *a, int *lda, double *b, int *ldb, int *
	jpvt, double *rcond, int *rank, double *work, int *
	lwork, int *info);
 
  void dgeql2_(int *m, int *n, double *a, int *
	lda, double *tau, double *work, int *info);
 
  void dgeqlf_(int *m, int *n, double *a, int *
	lda, double *tau, double *work, int *lwork, int *info);
 
  void dgeqp3_(int *m, int *n, double *a, int *
	lda, int *jpvt, double *tau, double *work, int *lwork,
	 int *info);
 
  void dgeqpf_(int *m, int *n, double *a, int *
	lda, int *jpvt, double *tau, double *work, int *info);
 
  void dgeqr2_(int *m, int *n, double *a, int *
	lda, double *tau, double *work, int *info);
 
  void dgeqrf_(int *m, int *n, double *a, int *
	lda, double *tau, double *work, int *lwork, int *info);
 
  void dgerfs_(char *trans, int *n, int *nrhs, 
	double *a, int *lda, double *af, int *ldaf, int *
	ipiv, double *b, int *ldb, double *x, int *ldx, 
	double *ferr, double *berr, double *work, int *iwork, 
	int *info);
 
  void dgerq2_(int *m, int *n, double *a, int *
	lda, double *tau, double *work, int *info);
 
  void dgerqf_(int *m, int *n, double *a, int *
	lda, double *tau, double *work, int *lwork, int *info);
 
  void dgesc2_(int *n, double *a, int *lda, 
	double *rhs, int *ipiv, int *jpiv, double *scale);
 
  void dgesdd_(char *jobz, int *m, int *n, double *
	a, int *lda, double *s, double *u, int *ldu, 
	double *vt, int *ldvt, double *work, int *lwork, 
	int *iwork, int *info);
 
  void dgesv_(int *n, int *nrhs, double *a, int 
	*lda, int *ipiv, double *b, int *ldb, int *info);
 
  void dgesvd_(char *jobu, char *jobvt, int *m, int *n, 
	double *a, int *lda, double *s, double *u, int *
	ldu, double *vt, int *ldvt, double *work, int *lwork, 
	int *info);
 
  void dgesvx_(char *fact, char *trans, int *n, int *
	nrhs, double *a, int *lda, double *af, int *ldaf, 
	int *ipiv, char *equed, double *r__, double *c__, 
	double *b, int *ldb, double *x, int *ldx, double *
	rcond, double *ferr, double *berr, double *work, int *
	iwork, int *info);
 
  void dgetc2_(int *n, double *a, int *lda, int 
	*ipiv, int *jpiv, int *info);
 
  void dgetf2_(int *m, int *n, double *a, int *
	lda, int *ipiv, int *info);
 
  void dgetrf_(int *m, int *n, double *a, int *
	lda, int *ipiv, int *info);
 
  void dgetri_(int *n, double *a, int *lda, int 
	*ipiv, double *work, int *lwork, int *info);
 
  void dgetrs_(char *trans, int *n, int *nrhs, 
	double *a, int *lda, int *ipiv, double *b, int *
	ldb, int *info);
 
  void dggbak_(char *job, char *side, int *n, int *ilo, 
	int *ihi, double *lscale, double *rscale, int *m, 
	double *v, int *ldv, int *info);
 
  void dggbal_(char *job, int *n, double *a, int *
	lda, double *b, int *ldb, int *ilo, int *ihi, 
	double *lscale, double *rscale, double *work, int *
	info);
 
  void dgges_(char *jobvsl, char *jobvsr, char *sort, L_fp 
	delctg, int *n, double *a, int *lda, double *b, 
	int *ldb, int *sdim, double *alphar, double *alphai, 
	double *beta, double *vsl, int *ldvsl, double *vsr, 
	int *ldvsr, double *work, int *lwork, logical *bwork, 
	int *info);
 
  void dggesx_(char *jobvsl, char *jobvsr, char *sort, L_fp 
	delctg, char *sense, int *n, double *a, int *lda, 
	double *b, int *ldb, int *sdim, double *alphar, 
	double *alphai, double *beta, double *vsl, int *ldvsl,
	 double *vsr, int *ldvsr, double *rconde, double *
	rcondv, double *work, int *lwork, int *iwork, int *
	liwork, logical *bwork, int *info);
 
  void dggev_(char *jobvl, char *jobvr, int *n, double *
	a, int *lda, double *b, int *ldb, double *alphar, 
	double *alphai, double *beta, double *vl, int *ldvl, 
	double *vr, int *ldvr, double *work, int *lwork, 
	int *info);
 
  void dggevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, int *n, double *a, int *lda, double *b, 
	int *ldb, double *alphar, double *alphai, double *
	beta, double *vl, int *ldvl, double *vr, int *ldvr, 
	int *ilo, int *ihi, double *lscale, double *rscale, 
	double *abnrm, double *bbnrm, double *rconde, double *
	rcondv, double *work, int *lwork, int *iwork, logical *
	bwork, int *info);
 
  void dggglm_(int *n, int *m, int *p, double *
	a, int *lda, double *b, int *ldb, double *d__, 
	double *x, double *y, double *work, int *lwork, 
	int *info);
 
  void dgghrd_(char *compq, char *compz, int *n, int *
	ilo, int *ihi, double *a, int *lda, double *b, 
	int *ldb, double *q, int *ldq, double *z__, int *
	ldz, int *info);
 
  void dgglse_(int *m, int *n, int *p, double *
	a, int *lda, double *b, int *ldb, double *c__, 
	double *d__, double *x, double *work, int *lwork, 
	int *info);
 
  void dggqrf_(int *n, int *m, int *p, double *
	a, int *lda, double *taua, double *b, int *ldb, 
	double *taub, double *work, int *lwork, int *info);
 
  void dggrqf_(int *m, int *p, int *n, double *
	a, int *lda, double *taua, double *b, int *ldb, 
	double *taub, double *work, int *lwork, int *info);
 
  void dggsvd_(char *jobu, char *jobv, char *jobq, int *m, 
	int *n, int *p, int *k, int *l, double *a, 
	int *lda, double *b, int *ldb, double *alpha, 
	double *beta, double *u, int *ldu, double *v, int 
	*ldv, double *q, int *ldq, double *work, int *iwork, 
	int *info);
 
  void dggsvp_(char *jobu, char *jobv, char *jobq, int *m, 
	int *p, int *n, double *a, int *lda, double *b, 
	int *ldb, double *tola, double *tolb, int *k, int 
	*l, double *u, int *ldu, double *v, int *ldv, 
	double *q, int *ldq, int *iwork, double *tau, 
	double *work, int *info);
 
  void dgtcon_(char *norm, int *n, double *dl, 
	double *d__, double *du, double *du2, int *ipiv, 
	double *anorm, double *rcond, double *work, int *
	iwork, int *info);
 
  void dgtrfs_(char *trans, int *n, int *nrhs, 
	double *dl, double *d__, double *du, double *dlf, 
	double *df, double *duf, double *du2, int *ipiv, 
	double *b, int *ldb, double *x, int *ldx, double *
	ferr, double *berr, double *work, int *iwork, int *
	info);
 
  void dgtsv_(int *n, int *nrhs, double *dl, 
	double *d__, double *du, double *b, int *ldb, int 
	*info);
 
  void dgtsvx_(char *fact, char *trans, int *n, int *
	nrhs, double *dl, double *d__, double *du, double *
	dlf, double *df, double *duf, double *du2, int *ipiv, 
	double *b, int *ldb, double *x, int *ldx, double *
	rcond, double *ferr, double *berr, double *work, int *
	iwork, int *info);
 
  void dgttrf_(int *n, double *dl, double *d__, 
	double *du, double *du2, int *ipiv, int *info);
 
  void dgttrs_(char *trans, int *n, int *nrhs, 
	double *dl, double *d__, double *du, double *du2, 
	int *ipiv, double *b, int *ldb, int *info);
 
  void dgtts2_(int *itrans, int *n, int *nrhs, 
	double *dl, double *d__, double *du, double *du2, 
	int *ipiv, double *b, int *ldb);
 
  void dhgeqz_(char *job, char *compq, char *compz, int *n, 
	int *ilo, int *ihi, double *a, int *lda, double *
	b, int *ldb, double *alphar, double *alphai, double *
	beta, double *q, int *ldq, double *z__, int *ldz, 
	double *work, int *lwork, int *info);
 
  void dhsein_(char *side, char *eigsrc, char *initv, logical *
	select, int *n, double *h__, int *ldh, double *wr, 
	double *wi, double *vl, int *ldvl, double *vr, 
	int *ldvr, int *mm, int *m, double *work, int *
	ifaill, int *ifailr, int *info);
 
  void dhseqr_(char *job, char *compz, int *n, int *ilo,
	 int *ihi, double *h__, int *ldh, double *wr, 
	double *wi, double *z__, int *ldz, double *work, 
	int *lwork, int *info);
 
  void dlabad_(double *small, double *large);
 
  void dlabrd_(int *m, int *n, int *nb, double *
	a, int *lda, double *d__, double *e, double *tauq, 
	double *taup, double *x, int *ldx, double *y, int 
	*ldy);
 
  void dlacon_(int *n, double *v, double *x, 
	int *isgn, double *est, int *kase);
 
  void dlacpy_(char *uplo, int *m, int *n, double *
	a, int *lda, double *b, int *ldb);
 
  void dladiv_(double *a, double *b, double *c__, 
	double *d__, double *p, double *q);
 
  void dlae2_(double *a, double *b, double *c__, 
	double *rt1, double *rt2);
 
  void dlaebz_(int *ijob, int *nitmax, int *n, 
	int *mmax, int *minp, int *nbmin, double *abstol, 
	double *reltol, double *pivmin, double *d__, double *
	e, double *e2, int *nval, double *ab, double *c__, 
	int *mout, int *nab, double *work, int *iwork, 
	int *info);
 
  void dlaed0_(int *icompq, int *qsiz, int *n, 
	double *d__, double *e, double *q, int *ldq, 
	double *qstore, int *ldqs, double *work, int *iwork, 
	int *info);
 
  void dlaed1_(int *n, double *d__, double *q, 
	int *ldq, int *indxq, double *rho, int *cutpnt, 
	double *work, int *iwork, int *info);
 
  void dlaed2_(int *k, int *n, int *n1, double *
	d__, double *q, int *ldq, int *indxq, double *rho, 
	double *z__, double *dlamda, double *w, double *q2, 
	int *indx, int *indxc, int *indxp, int *coltyp, 
	int *info);
 
  void dlaed3_(int *k, int *n, int *n1, double *
	d__, double *q, int *ldq, double *rho, double *dlamda,
	 double *q2, int *indx, int *ctot, double *w, 
	double *s, int *info);
 
  void dlaed4_(int *n, int *i__, double *d__, 
	double *z__, double *delta, double *rho, double *dlam,
	 int *info);
 
  void dlaed5_(int *i__, double *d__, double *z__, 
	double *delta, double *rho, double *dlam);
 
  void dlaed6_(int *kniter, logical *orgati, double *
	rho, double *d__, double *z__, double *finit, double *
	tau, int *info);
 
  void dlaed7_(int *icompq, int *n, int *qsiz, 
	int *tlvls, int *curlvl, int *curpbm, double *d__, 
	double *q, int *ldq, int *indxq, double *rho, int 
	*cutpnt, double *qstore, int *qptr, int *prmptr, int *
	perm, int *givptr, int *givcol, double *givnum, 
	double *work, int *iwork, int *info);
 
  void dlaed8_(int *icompq, int *k, int *n, int 
	*qsiz, double *d__, double *q, int *ldq, int *indxq, 
	double *rho, int *cutpnt, double *z__, double *dlamda,
	 double *q2, int *ldq2, double *w, int *perm, int 
	*givptr, int *givcol, double *givnum, int *indxp, int 
	*indx, int *info);
 
  void dlaed9_(int *k, int *kstart, int *kstop, 
	int *n, double *d__, double *q, int *ldq, double *
	rho, double *dlamda, double *w, double *s, int *lds, 
	int *info);
 
  void dlaeda_(int *n, int *tlvls, int *curlvl, 
	int *curpbm, int *prmptr, int *perm, int *givptr, 
	int *givcol, double *givnum, double *q, int *qptr, 
	double *z__, double *ztemp, int *info);
 
  void dlaein_(logical *rightv, logical *noinit, int *n, 
	double *h__, int *ldh, double *wr, double *wi, 
	double *vr, double *vi, double *b, int *ldb, 
	double *work, double *eps3, double *smlnum, double *
	bignum, int *info);
 
  void dlaev2_(double *a, double *b, double *c__, 
	double *rt1, double *rt2, double *cs1, double *sn1);
 
  void dlaexc_(logical *wantq, int *n, double *t, 
	int *ldt, double *q, int *ldq, int *j1, int *n1, 
	int *n2, double *work, int *info);
 
  void dlag2_(double *a, int *lda, double *b, 
	int *ldb, double *safmin, double *scale1, double *
	scale2, double *wr1, double *wr2, double *wi);
 
  void dlags2_(logical *upper, double *a1, double *a2, 
	double *a3, double *b1, double *b2, double *b3, 
	double *csu, double *snu, double *csv, double *snv, 
	double *csq, double *snq);
 
  void dlagtf_(int *n, double *a, double *lambda, 
	double *b, double *c__, double *tol, double *d__, 
	int *input, int *info);
 
  void dlagtm_(char *trans, int *n, int *nrhs, 
	double *alpha, double *dl, double *d__, double *du, 
	double *x, int *ldx, double *beta, double *b, int 
	*ldb);
 
  void dlagts_(int *job, int *n, double *a, 
	double *b, double *c__, double *d__, int *input, 
	double *y, double *tol, int *info);
 
  void dlagv2_(double *a, int *lda, double *b, 
	int *ldb, double *alphar, double *alphai, double *
	beta, double *csl, double *snl, double *csr, double *
	snr);
 
  void dlahqr_(logical *wantt, logical *wantz, int *n, 
	int *ilo, int *ihi, double *h__, int *ldh, double 
	*wr, double *wi, int *iloz, int *ihiz, double *z__, 
	int *ldz, int *info);
 
  void dlahrd_(int *n, int *k, int *nb, double *
	a, int *lda, double *tau, double *t, int *ldt, 
	double *y, int *ldy);
 
  void dlaic1_(int *job, int *j, double *x, 
	double *sest, double *w, double *gamma, double *
	sestpr, double *s, double *c__);
 
  void dlaln2_(logical *ltrans, int *na, int *nw, 
	double *smin, double *ca, double *a, int *lda, 
	double *d1, double *d2, double *b, int *ldb, 
	double *wr, double *wi, double *x, int *ldx, 
	double *scale, double *xnorm, int *info);
 
  void dlals0_(int *icompq, int *nl, int *nr, 
	int *sqre, int *nrhs, double *b, int *ldb, double 
	*bx, int *ldbx, int *perm, int *givptr, int *givcol, 
	int *ldgcol, double *givnum, int *ldgnum, double *
	poles, double *difl, double *difr, double *z__, int *
	k, double *c__, double *s, double *work, int *info);
 
  void dlalsa_(int *icompq, int *smlsiz, int *n, 
	int *nrhs, double *b, int *ldb, double *bx, int *
	ldbx, double *u, int *ldu, double *vt, int *k, 
	double *difl, double *difr, double *z__, double *
	poles, int *givptr, int *givcol, int *ldgcol, int *
	perm, double *givnum, double *c__, double *s, double *
	work, int *iwork, int *info);
 
  void dlalsd_(char *uplo, int *smlsiz, int *n, int 
	*nrhs, double *d__, double *e, double *b, int *ldb, 
	double *rcond, int *rank, double *work, int *iwork, 
	int *info);
 
  void dlamc1_(int *beta, int *t, logical *rnd, logical 
	*ieee1);
 
  void dlamc2_(int *beta, int *t, logical *rnd, 
	double *eps, int *emin, double *rmin, int *emax, 
	double *rmax);
 
  void dlamc4_(int *emin, double *start, int *base);
 
  void dlamc5_(int *beta, int *p, int *emin, 
	logical *ieee, int *emax, double *rmax);
 
  void dlamrg_(int *n1, int *n2, double *a, int 
	*dtrd1, int *dtrd2, int *index);
 
  void dlanv2_(double *a, double *b, double *c__, 
	double *d__, double *rt1r, double *rt1i, double *rt2r,
	 double *rt2i, double *cs, double *sn);
 
  void dlapll_(int *n, double *x, int *incx, 
	double *y, int *incy, double *ssmin);
 
  void dlapmt_(logical *forwrd, int *m, int *n, 
	double *x, int *ldx, int *k);
 
  void dlaqgb_(int *m, int *n, int *kl, int *ku,
	 double *ab, int *ldab, double *r__, double *c__, 
	double *rowcnd, double *colcnd, double *amax, char *equed);
 
  void dlaqge_(int *m, int *n, double *a, int *
	lda, double *r__, double *c__, double *rowcnd, double 
	*colcnd, double *amax, char *equed);
 
  void dlaqp2_(int *m, int *n, int *offset, 
	double *a, int *lda, int *jpvt, double *tau, 
	double *vn1, double *vn2, double *work);
 
  void dlaqps_(int *m, int *n, int *offset, int 
	*nb, int *kb, double *a, int *lda, int *jpvt, 
	double *tau, double *vn1, double *vn2, double *auxv, 
	double *f, int *ldf);
 
  void dlaqsb_(char *uplo, int *n, int *kd, double *
	ab, int *ldab, double *s, double *scond, double *amax,
	 char *equed);
 
  void dlaqsp_(char *uplo, int *n, double *ap, 
	double *s, double *scond, double *amax, char *equed);
 
  void dlaqsy_(char *uplo, int *n, double *a, int *
	lda, double *s, double *scond, double *amax, char *equed);
 
  void dlaqtr_(logical *ltran, logical *lreal, int *n, 
	double *t, int *ldt, double *b, double *w, double 
	*scale, double *x, double *work, int *info);
 
  void dlar1v_(int *n, int *b1, int *bn, double 
	*sigma, double *d__, double *l, double *ld, double *
	lld, double *gersch, double *z__, double *ztz, double 
	*mingma, int *r__, int *isuppz, double *work);
 
  void dlar2v_(int *n, double *x, double *y, 
	double *z__, int *incx, double *c__, double *s, 
	int *incc);
 
  void dlarf_(char *side, int *m, int *n, double *v,
	 int *incv, double *tau, double *c__, int *ldc, 
	double *work);
 
  void dlarfb_(char *side, char *trans, char *direct, char *
	storev, int *m, int *n, int *k, double *v, int *
	ldv, double *t, int *ldt, double *c__, int *ldc, 
	double *work, int *ldwork);
 
  void dlarfg_(int *n, double *alpha, double *x, 
	int *incx, double *tau);
 
  void dlarft_(char *direct, char *storev, int *n, int *
	k, double *v, int *ldv, double *tau, double *t, 
	int *ldt);
 
  void dlarfx_(char *side, int *m, int *n, double *
	v, double *tau, double *c__, int *ldc, double *work);
 
  void dlargv_(int *n, double *x, int *incx, 
	double *y, int *incy, double *c__, int *incc);
 
  void dlarnv_(int *idist, int *iseed, int *n, 
	double *x);
 
  void dlarrb_(int *n, double *d__, double *l, 
	double *ld, double *lld, int *ifirst, int *ilast, 
	double *sigma, double *reltol, double *w, double *
	wgap, double *werr, double *work, int *iwork, int *
	info);
 
  void dlarre_(int *n, double *d__, double *e, 
	double *tol, int *nsplit, int *isplit, int *m, 
	double *w, double *woff, double *gersch, double *work,
	 int *info);
 
  void dlarrf_(int *n, double *d__, double *l, 
	double *ld, double *lld, int *ifirst, int *ilast, 
	double *w, double *dplus, double *lplus, double *work,
	 int *iwork, int *info);
 
  void dlarrv_(int *n, double *d__, double *l, 
	int *isplit, int *m, double *w, int *iblock, 
	double *gersch, double *tol, double *z__, int *ldz, 
	int *isuppz, double *work, int *iwork, int *info);
 
  void dlartg_(double *f, double *g, double *cs, 
	double *sn, double *r__);
 
  void dlartv_(int *n, double *x, int *incx, 
	double *y, int *incy, double *c__, double *s, int 
	*incc);
 
  void dlaruv_(int *iseed, int *n, double *x);
 
  void dlarz_(char *side, int *m, int *n, int *l, 
	double *v, int *incv, double *tau, double *c__, 
	int *ldc, double *work);
 
  void dlarzb_(char *side, char *trans, char *direct, char *
	storev, int *m, int *n, int *k, int *l, double *v,
	 int *ldv, double *t, int *ldt, double *c__, int *
	ldc, double *work, int *ldwork);
 
  void dlarzt_(char *direct, char *storev, int *n, int *
	k, double *v, int *ldv, double *tau, double *t, 
	int *ldt);
 
  void dlas2_(double *f, double *g, double *h__, 
	double *ssmin, double *ssmax);
 
  void dlascl_(char *type__, int *kl, int *ku, 
	double *cfrom, double *cto, int *m, int *n, 
	double *a, int *lda, int *info);
 
  void dlasd0_(int *n, int *sqre, double *d__, 
	double *e, double *u, int *ldu, double *vt, int *
	ldvt, int *smlsiz, int *iwork, double *work, int *
	info);
 
  void dlasd1_(int *nl, int *nr, int *sqre, 
	double *d__, double *alpha, double *beta, double *u, 
	int *ldu, double *vt, int *ldvt, int *idxq, int *
	iwork, double *work, int *info);
 
  void dlasd2_(int *nl, int *nr, int *sqre, int 
	*k, double *d__, double *z__, double *alpha, double *
	beta, double *u, int *ldu, double *vt, int *ldvt, 
	double *dsigma, double *u2, int *ldu2, double *vt2, 
	int *ldvt2, int *idxp, int *idx, int *idxc, int *
	idxq, int *coltyp, int *info);
 
  void dlasd3_(int *nl, int *nr, int *sqre, int 
	*k, double *d__, double *q, int *ldq, double *dsigma, 
	double *u, int *ldu, double *u2, int *ldu2, 
	double *vt, int *ldvt, double *vt2, int *ldvt2, 
	int *idxc, int *ctot, double *z__, int *info);
 
  void dlasd4_(int *n, int *i__, double *d__, 
	double *z__, double *delta, double *rho, double *
	sigma, double *work, int *info);
 
  void dlasd5_(int *i__, double *d__, double *z__, 
	double *delta, double *rho, double *dsigma, double *
	work);
 
  void dlasd6_(int *icompq, int *nl, int *nr, 
	int *sqre, double *d__, double *vf, double *vl, 
	double *alpha, double *beta, int *idxq, int *perm, 
	int *givptr, int *givcol, int *ldgcol, double *givnum,
	 int *ldgnum, double *poles, double *difl, double *
	difr, double *z__, int *k, double *c__, double *s, 
	double *work, int *iwork, int *info);
 
  void dlasd7_(int *icompq, int *nl, int *nr, 
	int *sqre, int *k, double *d__, double *z__, 
	double *zw, double *vf, double *vfw, double *vl, 
	double *vlw, double *alpha, double *beta, double *
	dsigma, int *idx, int *idxp, int *idxq, int *perm, 
	int *givptr, int *givcol, int *ldgcol, double *givnum,
	 int *ldgnum, double *c__, double *s, int *info);
 
  void dlasd8_(int *icompq, int *k, double *d__, 
	double *z__, double *vf, double *vl, double *difl, 
	double *difr, int *lddifr, double *dsigma, double *
	work, int *info);
 
  void dlasd9_(int *icompq, int *ldu, int *k, 
	double *d__, double *z__, double *vf, double *vl, 
	double *difl, double *difr, double *dsigma, double *
	work, int *info);
 
  void dlasda_(int *icompq, int *smlsiz, int *n, 
	int *sqre, double *d__, double *e, double *u, int 
	*ldu, double *vt, int *k, double *difl, double *difr, 
	double *z__, double *poles, int *givptr, int *givcol, 
	int *ldgcol, int *perm, double *givnum, double *c__, 
	double *s, double *work, int *iwork, int *info);
 
  void dlasdq_(char *uplo, int *sqre, int *n, int *
	ncvt, int *nru, int *ncc, double *d__, double *e, 
	double *vt, int *ldvt, double *u, int *ldu, 
	double *c__, int *ldc, double *work, int *info);
 
  void dlasdt_(int *n, int *lvl, int *nd, int *
	inode, int *ndiml, int *ndimr, int *msub);
 
  void dlaset_(char *uplo, int *m, int *n, double *
	alpha, double *beta, double *a, int *lda);
 
  void dlasq1_(int *n, double *d__, double *e, 
	double *work, int *info);
 
  void dlasq2_(int *n, double *z__, int *info);
 
  void dlasq3_(int *i0, int *n0, double *z__, 
	int *pp, double *dmin__, double *sigma, double *desig,
	 double *qmax, int *nfail, int *iter, int *ndiv, 
	logical *ieee);
 
  void dlasq4_(int *i0, int *n0, double *z__, 
	int *pp, int *n0in, double *dmin__, double *dmin1, 
	double *dmin2, double *dn, double *dn1, double *dn2, 
	double *tau, int *ttype);
 
  void dlasq5_(int *i0, int *n0, double *z__, 
	int *pp, double *tau, double *dmin__, double *dmin1, 
	double *dmin2, double *dn, double *dnm1, double *dnm2,
	 logical *ieee);
 
  void dlasq6_(int *i0, int *n0, double *z__, 
	int *pp, double *dmin__, double *dmin1, double *dmin2,
	 double *dn, double *dnm1, double *dnm2);
 
  void dlasr_(char *side, char *pivot, char *direct, int *m,
	 int *n, double *c__, double *s, double *a, int *
	lda);
 
  void dlasrt_(char *id, int *n, double *d__, int *
	info);
 
  void dlassq_(int *n, double *x, int *incx, 
	double *scale, double *sumsq);
 
  void dlasv2_(double *f, double *g, double *h__, 
	double *ssmin, double *ssmax, double *snr, double *
	csr, double *snl, double *csl);
 
  void dlaswp_(int *n, double *a, int *lda, int 
	*k1, int *k2, int *ipiv, int *incx);
 
  void dlasy2_(logical *ltranl, logical *ltranr, int *isgn, 
	int *n1, int *n2, double *tl, int *ldtl, double *
	tr, int *ldtr, double *b, int *ldb, double *scale, 
	double *x, int *ldx, double *xnorm, int *info);
 
  void dlasyf_(char *uplo, int *n, int *nb, int *kb,
	 double *a, int *lda, int *ipiv, double *w, int *
	ldw, int *info);
 
  void dlatbs_(char *uplo, char *trans, char *diag, char *
	normin, int *n, int *kd, double *ab, int *ldab, 
	double *x, double *scale, double *cnorm, int *info);
 
  void dlatdf_(int *ijob, int *n, double *z__, 
	int *ldz, double *rhs, double *rdsum, double *rdscal, 
	int *ipiv, int *jpiv);
 
  void dlatps_(char *uplo, char *trans, char *diag, char *
	normin, int *n, double *ap, double *x, double *scale, 
	double *cnorm, int *info);
 
  void dlatrd_(char *uplo, int *n, int *nb, double *
	a, int *lda, double *e, double *tau, double *w, 
	int *ldw);
 
  void dlatrs_(char *uplo, char *trans, char *diag, char *
	normin, int *n, double *a, int *lda, double *x, 
	double *scale, double *cnorm, int *info);
 
  void dlatrz_(int *m, int *n, int *l, double *
	a, int *lda, double *tau, double *work);
 
  void dlatzm_(char *side, int *m, int *n, double *
	v, int *incv, double *tau, double *c1, double *c2, 
	int *ldc, double *work);
 
  void dlauu2_(char *uplo, int *n, double *a, int *
	lda, int *info);
 
  void dlauum_(char *uplo, int *n, double *a, int *
	lda, int *info);
 
  void dopgtr_(char *uplo, int *n, double *ap, 
	double *tau, double *q, int *ldq, double *work, 
	int *info);
 
  void dopmtr_(char *side, char *uplo, char *trans, int *m, 
	int *n, double *ap, double *tau, double *c__, int 
	*ldc, double *work, int *info);
 
  void dorg2l_(int *m, int *n, int *k, double *
	a, int *lda, double *tau, double *work, int *info);
 
  void dorg2r_(int *m, int *n, int *k, double *
	a, int *lda, double *tau, double *work, int *info);
 
  void dorgbr_(char *vect, int *m, int *n, int *k, 
	double *a, int *lda, double *tau, double *work, 
	int *lwork, int *info);
 
  void dorghr_(int *n, int *ilo, int *ihi, 
	double *a, int *lda, double *tau, double *work, 
	int *lwork, int *info);
 
  void dorgl2_(int *m, int *n, int *k, double *
	a, int *lda, double *tau, double *work, int *info);
 
  void dorglq_(int *m, int *n, int *k, double *
	a, int *lda, double *tau, double *work, int *lwork, 
	int *info);
 
  void dorgql_(int *m, int *n, int *k, double *
	a, int *lda, double *tau, double *work, int *lwork, 
	int *info);
 
  void dorgqr_(int *m, int *n, int *k, double *
	a, int *lda, double *tau, double *work, int *lwork, 
	int *info);
 
  void dorgr2_(int *m, int *n, int *k, double *
	a, int *lda, double *tau, double *work, int *info);
 
  void dorgrq_(int *m, int *n, int *k, double *
	a, int *lda, double *tau, double *work, int *lwork, 
	int *info);
 
  void dorgtr_(char *uplo, int *n, double *a, int *
	lda, double *tau, double *work, int *lwork, int *info);
 
  void dorm2l_(char *side, char *trans, int *m, int *n, 
	int *k, double *a, int *lda, double *tau, double *
	c__, int *ldc, double *work, int *info);
 
  void dorm2r_(char *side, char *trans, int *m, int *n, 
	int *k, double *a, int *lda, double *tau, double *
	c__, int *ldc, double *work, int *info);
 
  void dormbr_(char *vect, char *side, char *trans, int *m, 
	int *n, int *k, double *a, int *lda, double *tau, 
	double *c__, int *ldc, double *work, int *lwork, 
	int *info);
 
  void dormhr_(char *side, char *trans, int *m, int *n, 
	int *ilo, int *ihi, double *a, int *lda, double *
	tau, double *c__, int *ldc, double *work, int *lwork, 
	int *info);
 
  void dorml2_(char *side, char *trans, int *m, int *n, 
	int *k, double *a, int *lda, double *tau, double *
	c__, int *ldc, double *work, int *info);
 
  void dormlq_(char *side, char *trans, int *m, int *n, 
	int *k, double *a, int *lda, double *tau, double *
	c__, int *ldc, double *work, int *lwork, int *info);
 
  void dormql_(char *side, char *trans, int *m, int *n, 
	int *k, double *a, int *lda, double *tau, double *
	c__, int *ldc, double *work, int *lwork, int *info);
 
  void dormqr_(char *side, char *trans, int *m, int *n, 
	int *k, double *a, int *lda, double *tau, double *
	c__, int *ldc, double *work, int *lwork, int *info);
 
  void dormr2_(char *side, char *trans, int *m, int *n, 
	int *k, double *a, int *lda, double *tau, double *
	c__, int *ldc, double *work, int *info);
 
  void dormr3_(char *side, char *trans, int *m, int *n, 
	int *k, int *l, double *a, int *lda, double *tau, 
	double *c__, int *ldc, double *work, int *info);
 
  void dormrq_(char *side, char *trans, int *m, int *n, 
	int *k, double *a, int *lda, double *tau, double *
	c__, int *ldc, double *work, int *lwork, int *info);
 
  void dormrz_(char *side, char *trans, int *m, int *n, 
	int *k, int *l, double *a, int *lda, double *tau, 
	double *c__, int *ldc, double *work, int *lwork, 
	int *info);
 
  void dormtr_(char *side, char *uplo, char *trans, int *m, 
	int *n, double *a, int *lda, double *tau, double *
	c__, int *ldc, double *work, int *lwork, int *info);
 
  void dpbcon_(char *uplo, int *n, int *kd, double *
	ab, int *ldab, double *anorm, double *rcond, double *
	work, int *iwork, int *info);
 
  void dpbequ_(char *uplo, int *n, int *kd, double *
	ab, int *ldab, double *s, double *scond, double *amax,
	 int *info);
 
  void dpbrfs_(char *uplo, int *n, int *kd, int *
	nrhs, double *ab, int *ldab, double *afb, int *ldafb, 
	double *b, int *ldb, double *x, int *ldx, double *
	ferr, double *berr, double *work, int *iwork, int *
	info);
 
  void dpbstf_(char *uplo, int *n, int *kd, double *
	ab, int *ldab, int *info);
 
  void dpbsv_(char *uplo, int *n, int *kd, int *
	nrhs, double *ab, int *ldab, double *b, int *ldb, 
	int *info);
 
  void dpbsvx_(char *fact, char *uplo, int *n, int *kd, 
	int *nrhs, double *ab, int *ldab, double *afb, 
	int *ldafb, char *equed, double *s, double *b, int *
	ldb, double *x, int *ldx, double *rcond, double *ferr,
	 double *berr, double *work, int *iwork, int *info);
 
  void dpbtf2_(char *uplo, int *n, int *kd, double *
	ab, int *ldab, int *info);
 
  void dpbtrf_(char *uplo, int *n, int *kd, double *
	ab, int *ldab, int *info);
 
  void dpbtrs_(char *uplo, int *n, int *kd, int *
	nrhs, double *ab, int *ldab, double *b, int *ldb, 
	int *info);
 
  void dpocon_(char *uplo, int *n, double *a, int *
	lda, double *anorm, double *rcond, double *work, int *
	iwork, int *info);
 
  void dpoequ_(int *n, double *a, int *lda, 
	double *s, double *scond, double *amax, int *info);
 
  void dporfs_(char *uplo, int *n, int *nrhs, 
	double *a, int *lda, double *af, int *ldaf, 
	double *b, int *ldb, double *x, int *ldx, double *
	ferr, double *berr, double *work, int *iwork, int *
	info);
 
  void dposv_(char *uplo, int *n, int *nrhs, double 
	*a, int *lda, double *b, int *ldb, int *info);
 
  void dposvx_(char *fact, char *uplo, int *n, int *
	nrhs, double *a, int *lda, double *af, int *ldaf, 
	char *equed, double *s, double *b, int *ldb, double *
	x, int *ldx, double *rcond, double *ferr, double *
	berr, double *work, int *iwork, int *info);
 
  void dpotf2_(char *uplo, int *n, double *a, int *
	lda, int *info);
 
  void dpotrf_(char *uplo, int *n, double *a, int *
	lda, int *info);
 
  void dpotri_(char *uplo, int *n, double *a, int *
	lda, int *info);
 
  void dpotrs_(char *uplo, int *n, int *nrhs, 
	double *a, int *lda, double *b, int *ldb, int *
	info);
 
  void dppcon_(char *uplo, int *n, double *ap, 
	double *anorm, double *rcond, double *work, int *
	iwork, int *info);
 
  void dppequ_(char *uplo, int *n, double *ap, 
	double *s, double *scond, double *amax, int *info);
 
  void dpprfs_(char *uplo, int *n, int *nrhs, 
	double *ap, double *afp, double *b, int *ldb, 
	double *x, int *ldx, double *ferr, double *berr, 
	double *work, int *iwork, int *info);
 
  void dppsv_(char *uplo, int *n, int *nrhs, double 
	*ap, double *b, int *ldb, int *info);
 
  void dppsvx_(char *fact, char *uplo, int *n, int *
	nrhs, double *ap, double *afp, char *equed, double *s, 
	double *b, int *ldb, double *x, int *ldx, double *
	rcond, double *ferr, double *berr, double *work, int *
	iwork, int *info);
 
  void dpptrf_(char *uplo, int *n, double *ap, int *
	info);
 
  void dpptri_(char *uplo, int *n, double *ap, int *
	info);
 
  void dpptrs_(char *uplo, int *n, int *nrhs, 
	double *ap, double *b, int *ldb, int *info);
 
  void dptcon_(int *n, double *d__, double *e, 
	double *anorm, double *rcond, double *work, int *info);
 
  void dpteqr_(char *compz, int *n, double *d__, 
	double *e, double *z__, int *ldz, double *work, 
	int *info);
 
  void dptrfs_(int *n, int *nrhs, double *d__, 
	double *e, double *df, double *ef, double *b, int 
	*ldb, double *x, int *ldx, double *ferr, double *berr,
	 double *work, int *info);
 
  void dptsv_(int *n, int *nrhs, double *d__, 
	double *e, double *b, int *ldb, int *info);
 
  void dptsvx_(char *fact, int *n, int *nrhs, 
	double *d__, double *e, double *df, double *ef, 
	double *b, int *ldb, double *x, int *ldx, double *
	rcond, double *ferr, double *berr, double *work, int *
	info);
 
  void dpttrf_(int *n, double *d__, double *e, 
	int *info);
 
  void dpttrs_(int *n, int *nrhs, double *d__, 
	double *e, double *b, int *ldb, int *info);
 
  void dptts2_(int *n, int *nrhs, double *d__, 
	double *e, double *b, int *ldb);
 
  void drscl_(int *n, double *sa, double *sx, 
	int *incx);
 
  void dsbev_(char *jobz, char *uplo, int *n, int *kd, 
	double *ab, int *ldab, double *w, double *z__, 
	int *ldz, double *work, int *info);
 
  void dsbevd_(char *jobz, char *uplo, int *n, int *kd, 
	double *ab, int *ldab, double *w, double *z__, 
	int *ldz, double *work, int *lwork, int *iwork, 
	int *liwork, int *info);
 
  void dsbevx_(char *jobz, char *range, char *uplo, int *n, 
	int *kd, double *ab, int *ldab, double *q, int *
	ldq, double *vl, double *vu, int *il, int *iu, 
	double *abstol, int *m, double *w, double *z__, 
	int *ldz, double *work, int *iwork, int *ifail, 
	int *info);
 
  void dsbgst_(char *vect, char *uplo, int *n, int *ka, 
	int *kb, double *ab, int *ldab, double *bb, int *
	ldbb, double *x, int *ldx, double *work, int *info);
 
  void dsbgv_(char *jobz, char *uplo, int *n, int *ka, 
	int *kb, double *ab, int *ldab, double *bb, int *
	ldbb, double *w, double *z__, int *ldz, double *work, 
	int *info);
 
  void dsbgvd_(char *jobz, char *uplo, int *n, int *ka, 
	int *kb, double *ab, int *ldab, double *bb, int *
	ldbb, double *w, double *z__, int *ldz, double *work, 
	int *lwork, int *iwork, int *liwork, int *info);
 
  void dsbgvx_(char *jobz, char *range, char *uplo, int *n, 
	int *ka, int *kb, double *ab, int *ldab, double *
	bb, int *ldbb, double *q, int *ldq, double *vl, 
	double *vu, int *il, int *iu, double *abstol, int 
	*m, double *w, double *z__, int *ldz, double *work, 
	int *iwork, int *ifail, int *info);
 
  void dsbtrd_(char *vect, char *uplo, int *n, int *kd, 
	double *ab, int *ldab, double *d__, double *e, 
	double *q, int *ldq, double *work, int *info);
 
  void dspcon_(char *uplo, int *n, double *ap, int *
	ipiv, double *anorm, double *rcond, double *work, int 
	*iwork, int *info);
 
  void dspev_(char *jobz, char *uplo, int *n, double *
	ap, double *w, double *z__, int *ldz, double *work, 
	int *info);
 
  void dspevd_(char *jobz, char *uplo, int *n, double *
	ap, double *w, double *z__, int *ldz, double *work, 
	int *lwork, int *iwork, int *liwork, int *info);
 
  void dspevx_(char *jobz, char *range, char *uplo, int *n, 
	double *ap, double *vl, double *vu, int *il, int *
	iu, double *abstol, int *m, double *w, double *z__, 
	int *ldz, double *work, int *iwork, int *ifail, 
	int *info);
 
  void dspgst_(int *itype, char *uplo, int *n, 
	double *ap, double *bp, int *info);
 
  void dspgv_(int *itype, char *jobz, char *uplo, int *
	n, double *ap, double *bp, double *w, double *z__, 
	int *ldz, double *work, int *info);
 
  void dspgvd_(int *itype, char *jobz, char *uplo, int *
	n, double *ap, double *bp, double *w, double *z__, 
	int *ldz, double *work, int *lwork, int *iwork, 
	int *liwork, int *info);
 
  void dspgvx_(int *itype, char *jobz, char *range, char *
	uplo, int *n, double *ap, double *bp, double *vl, 
	double *vu, int *il, int *iu, double *abstol, int 
	*m, double *w, double *z__, int *ldz, double *work, 
	int *iwork, int *ifail, int *info);
 
  void dsprfs_(char *uplo, int *n, int *nrhs, 
	double *ap, double *afp, int *ipiv, double *b, 
	int *ldb, double *x, int *ldx, double *ferr, 
	double *berr, double *work, int *iwork, int *info);
 
  void dspsv_(char *uplo, int *n, int *nrhs, double 
	*ap, int *ipiv, double *b, int *ldb, int *info);
 
  void dspsvx_(char *fact, char *uplo, int *n, int *
	nrhs, double *ap, double *afp, int *ipiv, double *b, 
	int *ldb, double *x, int *ldx, double *rcond, 
	double *ferr, double *berr, double *work, int *iwork, 
	int *info);
 
  void dsptrd_(char *uplo, int *n, double *ap, 
	double *d__, double *e, double *tau, int *info);
 
  void dsptrf_(char *uplo, int *n, double *ap, int *
	ipiv, int *info);
 
  void dsptri_(char *uplo, int *n, double *ap, int *
	ipiv, double *work, int *info);
 
  void dsptrs_(char *uplo, int *n, int *nrhs, 
	double *ap, int *ipiv, double *b, int *ldb, int *
	info);
 
  void dstebz_(char *range, char *order, int *n, double 
	*vl, double *vu, int *il, int *iu, double *abstol, 
	double *d__, double *e, int *m, int *nsplit, 
	double *w, int *iblock, int *isplit, double *work, 
	int *iwork, int *info);
 
  void dstedc_(char *compz, int *n, double *d__, 
	double *e, double *z__, int *ldz, double *work, 
	int *lwork, int *iwork, int *liwork, int *info);
 
  void dstegr_(char *jobz, char *range, int *n, double *
	d__, double *e, double *vl, double *vu, int *il, 
	int *iu, double *abstol, int *m, double *w, 
	double *z__, int *ldz, int *isuppz, double *work, 
	int *lwork, int *iwork, int *liwork, int *info);
 
  void dstein_(int *n, double *d__, double *e, 
	int *m, double *w, int *iblock, int *isplit, 
	double *z__, int *ldz, double *work, int *iwork, 
	int *ifail, int *info);
 
  void dsteqr_(char *compz, int *n, double *d__, 
	double *e, double *z__, int *ldz, double *work, 
	int *info);
 
  void dsterf_(int *n, double *d__, double *e, 
	int *info);
 
  void dstev_(char *jobz, int *n, double *d__, 
	double *e, double *z__, int *ldz, double *work, 
	int *info);
 
  void dstevd_(char *jobz, int *n, double *d__, 
	double *e, double *z__, int *ldz, double *work, 
	int *lwork, int *iwork, int *liwork, int *info);
 
  void dstevr_(char *jobz, char *range, int *n, double *
	d__, double *e, double *vl, double *vu, int *il, 
	int *iu, double *abstol, int *m, double *w, 
	double *z__, int *ldz, int *isuppz, double *work, 
	int *lwork, int *iwork, int *liwork, int *info);
 
  void dstevx_(char *jobz, char *range, int *n, double *
	d__, double *e, double *vl, double *vu, int *il, 
	int *iu, double *abstol, int *m, double *w, 
	double *z__, int *ldz, double *work, int *iwork, 
	int *ifail, int *info);
 
  void dsycon_(char *uplo, int *n, double *a, int *
	lda, int *ipiv, double *anorm, double *rcond, double *
	work, int *iwork, int *info);
 
  void dsyev_(char *jobz, char *uplo, int *n, double *a,
	 int *lda, double *w, double *work, int *lwork, 
	int *info);
 
  void dsyevd_(char *jobz, char *uplo, int *n, double *
	a, int *lda, double *w, double *work, int *lwork, 
	int *iwork, int *liwork, int *info);
 
  void dsyevr_(char *jobz, char *range, char *uplo, int *n, 
	double *a, int *lda, double *vl, double *vu, int *
	il, int *iu, double *abstol, int *m, double *w, 
	double *z__, int *ldz, int *isuppz, double *work, 
	int *lwork, int *iwork, int *liwork, int *info);
 
  void dsyevx_(char *jobz, char *range, char *uplo, int *n, 
	double *a, int *lda, double *vl, double *vu, int *
	il, int *iu, double *abstol, int *m, double *w, 
	double *z__, int *ldz, double *work, int *lwork, 
	int *iwork, int *ifail, int *info);
 
  void dsygs2_(int *itype, char *uplo, int *n, 
	double *a, int *lda, double *b, int *ldb, int *
	info);
 
  void dsygst_(int *itype, char *uplo, int *n, 
	double *a, int *lda, double *b, int *ldb, int *
	info);
 
  void dsygv_(int *itype, char *jobz, char *uplo, int *
	n, double *a, int *lda, double *b, int *ldb, 
	double *w, double *work, int *lwork, int *info);
 
  void dsygvd_(int *itype, char *jobz, char *uplo, int *
	n, double *a, int *lda, double *b, int *ldb, 
	double *w, double *work, int *lwork, int *iwork, 
	int *liwork, int *info);
 
  void dsygvx_(int *itype, char *jobz, char *range, char *
	uplo, int *n, double *a, int *lda, double *b, int 
	*ldb, double *vl, double *vu, int *il, int *iu, 
	double *abstol, int *m, double *w, double *z__, 
	int *ldz, double *work, int *lwork, int *iwork, 
	int *ifail, int *info);
 
  void dsyrfs_(char *uplo, int *n, int *nrhs, 
	double *a, int *lda, double *af, int *ldaf, int *
	ipiv, double *b, int *ldb, double *x, int *ldx, 
	double *ferr, double *berr, double *work, int *iwork, 
	int *info);
 
  void dsysv_(char *uplo, int *n, int *nrhs, double 
	*a, int *lda, int *ipiv, double *b, int *ldb, 
	double *work, int *lwork, int *info);
 
  void dsysvx_(char *fact, char *uplo, int *n, int *
	nrhs, double *a, int *lda, double *af, int *ldaf, 
	int *ipiv, double *b, int *ldb, double *x, int *
	ldx, double *rcond, double *ferr, double *berr, 
	double *work, int *lwork, int *iwork, int *info);
 
  void dsytd2_(char *uplo, int *n, double *a, int *
	lda, double *d__, double *e, double *tau, int *info);
 
  void dsytf2_(char *uplo, int *n, double *a, int *
	lda, int *ipiv, int *info);
 
  void dsytrd_(char *uplo, int *n, double *a, int *
	lda, double *d__, double *e, double *tau, double *
	work, int *lwork, int *info);
 
  void dsytrf_(char *uplo, int *n, double *a, int *
	lda, int *ipiv, double *work, int *lwork, int *info);
 
  void dsytri_(char *uplo, int *n, double *a, int *
	lda, int *ipiv, double *work, int *info);
 
  void dsytrs_(char *uplo, int *n, int *nrhs, 
	double *a, int *lda, int *ipiv, double *b, int *
	ldb, int *info);
 
  void dtbcon_(char *norm, char *uplo, char *diag, int *n, 
	int *kd, double *ab, int *ldab, double *rcond, 
	double *work, int *iwork, int *info);
 
  void dtbrfs_(char *uplo, char *trans, char *diag, int *n, 
	int *kd, int *nrhs, double *ab, int *ldab, double 
	*b, int *ldb, double *x, int *ldx, double *ferr, 
	double *berr, double *work, int *iwork, int *info);
 
  void dtbtrs_(char *uplo, char *trans, char *diag, int *n, 
	int *kd, int *nrhs, double *ab, int *ldab, double 
	*b, int *ldb, int *info);
 
  void dtgevc_(char *side, char *howmny, logical *select, 
	int *n, double *a, int *lda, double *b, int *ldb, 
	double *vl, int *ldvl, double *vr, int *ldvr, int 
	*mm, int *m, double *work, int *info);
 
  void dtgex2_(logical *wantq, logical *wantz, int *n, 
	double *a, int *lda, double *b, int *ldb, double *
	q, int *ldq, double *z__, int *ldz, int *j1, int *
	n1, int *n2, double *work, int *lwork, int *info);
 
  void dtgexc_(logical *wantq, logical *wantz, int *n, 
	double *a, int *lda, double *b, int *ldb, double *
	q, int *ldq, double *z__, int *ldz, int *ifst, 
	int *ilst, double *work, int *lwork, int *info);
 
  void dtgsen_(int *ijob, logical *wantq, logical *wantz, 
	logical *select, int *n, double *a, int *lda, double *
	b, int *ldb, double *alphar, double *alphai, double *
	beta, double *q, int *ldq, double *z__, int *ldz, 
	int *m, double *pl, double *pr, double *dif, 
	double *work, int *lwork, int *iwork, int *liwork, 
	int *info);
 
  void dtgsja_(char *jobu, char *jobv, char *jobq, int *m, 
	int *p, int *n, int *k, int *l, double *a, 
	int *lda, double *b, int *ldb, double *tola, 
	double *tolb, double *alpha, double *beta, double *u, 
	int *ldu, double *v, int *ldv, double *q, int *
	ldq, double *work, int *ncycle, int *info);
 
  void dtgsna_(char *job, char *howmny, logical *select, 
	int *n, double *a, int *lda, double *b, int *ldb, 
	double *vl, int *ldvl, double *vr, int *ldvr, 
	double *s, double *dif, int *mm, int *m, double *
	work, int *lwork, int *iwork, int *info);
 
  void dtgsy2_(char *trans, int *ijob, int *m, int *
	n, double *a, int *lda, double *b, int *ldb, 
	double *c__, int *ldc, double *d__, int *ldd, 
	double *e, int *lde, double *f, int *ldf, double *
	scale, double *rdsum, double *rdscal, int *iwork, int 
	*pq, int *info);
 
  void dtgsyl_(char *trans, int *ijob, int *m, int *
	n, double *a, int *lda, double *b, int *ldb, 
	double *c__, int *ldc, double *d__, int *ldd, 
	double *e, int *lde, double *f, int *ldf, double *
	scale, double *dif, double *work, int *lwork, int *
	iwork, int *info);
 
  void dtpcon_(char *norm, char *uplo, char *diag, int *n, 
	double *ap, double *rcond, double *work, int *iwork, 
	int *info);
 
  void dtprfs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, double *ap, double *b, int *ldb, 
	double *x, int *ldx, double *ferr, double *berr, 
	double *work, int *iwork, int *info);
 
  void dtptri_(char *uplo, char *diag, int *n, double *
	ap, int *info);
 
  void dtptrs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, double *ap, double *b, int *ldb, int *
	info);
 
  void dtrcon_(char *norm, char *uplo, char *diag, int *n, 
	double *a, int *lda, double *rcond, double *work, 
	int *iwork, int *info);
 
  void dtrevc_(char *side, char *howmny, logical *select, 
	int *n, double *t, int *ldt, double *vl, int *
	ldvl, double *vr, int *ldvr, int *mm, int *m, 
	double *work, int *info);
 
  void dtrexc_(char *compq, int *n, double *t, int *
	ldt, double *q, int *ldq, int *ifst, int *ilst, 
	double *work, int *info);
 
  void dtrrfs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, double *a, int *lda, double *b, int *
	ldb, double *x, int *ldx, double *ferr, double *berr, 
	double *work, int *iwork, int *info);
 
  void dtrsen_(char *job, char *compq, logical *select, int 
	*n, double *t, int *ldt, double *q, int *ldq, 
	double *wr, double *wi, int *m, double *s, double 
	*sep, double *work, int *lwork, int *iwork, int *
	liwork, int *info);
 
  void dtrsna_(char *job, char *howmny, logical *select, 
	int *n, double *t, int *ldt, double *vl, int *
	ldvl, double *vr, int *ldvr, double *s, double *sep, 
	int *mm, int *m, double *work, int *ldwork, int *
	iwork, int *info);
 
  void dtrsyl_(char *trana, char *tranb, int *isgn, int 
	*m, int *n, double *a, int *lda, double *b, int *
	ldb, double *c__, int *ldc, double *scale, int *info);
 
  void dtrti2_(char *uplo, char *diag, int *n, double *
	a, int *lda, int *info);
 
  void dtrtri_(char *uplo, char *diag, int *n, double *
	a, int *lda, int *info);
 
  void dtrtrs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, double *a, int *lda, double *b, int *
	ldb, int *info);
 
  void dtzrqf_(int *m, int *n, double *a, int *
	lda, double *tau, int *info);
 
  void dtzrzf_(int *m, int *n, double *a, int *
	lda, double *tau, double *work, int *lwork, int *info);
 
int icmax1_(int *n, Complex!(float) *cx, int *incx);
 
int ieeeck_(int *ispec, float *zero, float *one);
 
int ilaenv_(int *ispec, char *name__, char *opts, int *n1, 
	int *n2, int *n3, int *n4, ftnlen name_len, ftnlen 
	opts_len);
 
int izmax1_(int *n, Complex!(double) *cx, int *incx);
 
  void sbdsdc_(char *uplo, char *compq, int *n, float *d__, 
	float *e, float *u, int *ldu, float *vt, int *ldvt, float *q, 
	int *iq, float *work, int *iwork, int *info);
 
  void sbdsqr_(char *uplo, int *n, int *ncvt, int *
	nru, int *ncc, float *d__, float *e, float *vt, int *ldvt, float *
	u, int *ldu, float *c__, int *ldc, float *work, int *info);
 
  void sdisna_(char *job, int *m, int *n, float *d__, 
	float *sep, int *info);
 
  void sgbbrd_(char *vect, int *m, int *n, int *ncc,
	 int *kl, int *ku, float *ab, int *ldab, float *d__, float *
	e, float *q, int *ldq, float *pt, int *ldpt, float *c__, int 
	*ldc, float *work, int *info);
 
  void sgbcon_(char *norm, int *n, int *kl, int *ku,
	 float *ab, int *ldab, int *ipiv, float *anorm, float *rcond, 
	float *work, int *iwork, int *info);
 
  void sgbequ_(int *m, int *n, int *kl, int *ku,
	 float *ab, int *ldab, float *r__, float *c__, float *rowcnd, float *
	colcnd, float *amax, int *info);
 
  void sgbrfs_(char *trans, int *n, int *kl, int *
	ku, int *nrhs, float *ab, int *ldab, float *afb, int *ldafb,
	 int *ipiv, float *b, int *ldb, float *x, int *ldx, float *
	ferr, float *berr, float *work, int *iwork, int *info);
 
  void sgbsv_(int *n, int *kl, int *ku, int *
	nrhs, float *ab, int *ldab, int *ipiv, float *b, int *ldb, 
	int *info);
 
  void sgbsvx_(char *fact, char *trans, int *n, int *kl,
	 int *ku, int *nrhs, float *ab, int *ldab, float *afb, 
	int *ldafb, int *ipiv, char *equed, float *r__, float *c__, 
	float *b, int *ldb, float *x, int *ldx, float *rcond, float *ferr,
	 float *berr, float *work, int *iwork, int *info);
 
  void sgbtf2_(int *m, int *n, int *kl, int *ku,
	 float *ab, int *ldab, int *ipiv, int *info);
 
  void sgbtrf_(int *m, int *n, int *kl, int *ku,
	 float *ab, int *ldab, int *ipiv, int *info);
 
  void sgbtrs_(char *trans, int *n, int *kl, int *
	ku, int *nrhs, float *ab, int *ldab, int *ipiv, float *b, 
	int *ldb, int *info);
 
  void sgebak_(char *job, char *side, int *n, int *ilo, 
	int *ihi, float *scale, int *m, float *v, int *ldv, int 
	*info);
 
  void sgebal_(char *job, int *n, float *a, int *lda, 
	int *ilo, int *ihi, float *scale, int *info);
 
  void sgebd2_(int *m, int *n, float *a, int *lda, 
	float *d__, float *e, float *tauq, float *taup, float *work, int *info);
 
  void sgebrd_(int *m, int *n, float *a, int *lda, 
	float *d__, float *e, float *tauq, float *taup, float *work, int *
	lwork, int *info);
 
  void sgecon_(char *norm, int *n, float *a, int *lda, 
	float *anorm, float *rcond, float *work, int *iwork, int *info);
 
  void sgeequ_(int *m, int *n, float *a, int *lda, 
	float *r__, float *c__, float *rowcnd, float *colcnd, float *amax, int 
	*info);
 
  void sgees_(char *jobvs, char *sort, L_fp select, int *n, 
	float *a, int *lda, int *sdim, float *wr, float *wi, float *vs, 
	int *ldvs, float *work, int *lwork, logical *bwork, int *
	info);
 
  void sgeesx_(char *jobvs, char *sort, L_fp select, char *
	sense, int *n, float *a, int *lda, int *sdim, float *wr, 
	float *wi, float *vs, int *ldvs, float *rconde, float *rcondv, float *
	work, int *lwork, int *iwork, int *liwork, logical *bwork,
	 int *info);
 
  void sgeev_(char *jobvl, char *jobvr, int *n, float *a, 
	int *lda, float *wr, float *wi, float *vl, int *ldvl, float *vr, 
	int *ldvr, float *work, int *lwork, int *info);
 
  void sgeevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, int *n, float *a, int *lda, float *wr, float *wi, float *
	vl, int *ldvl, float *vr, int *ldvr, int *ilo, int *
	ihi, float *scale, float *abnrm, float *rconde, float *rcondv, float *work,
	 int *lwork, int *iwork, int *info);
 
  void sgegs_(char *jobvsl, char *jobvsr, int *n, float *a, 
	int *lda, float *b, int *ldb, float *alphar, float *alphai, float 
	*beta, float *vsl, int *ldvsl, float *vsr, int *ldvsr, float *
	work, int *lwork, int *info);
 
  void sgegv_(char *jobvl, char *jobvr, int *n, float *a, 
	int *lda, float *b, int *ldb, float *alphar, float *alphai, float 
	*beta, float *vl, int *ldvl, float *vr, int *ldvr, float *work, 
	int *lwork, int *info);
 
  void sgehd2_(int *n, int *ilo, int *ihi, float *a, 
	int *lda, float *tau, float *work, int *info);
 
  void sgehrd_(int *n, int *ilo, int *ihi, float *a, 
	int *lda, float *tau, float *work, int *lwork, int *info);
 
  void sgelq2_(int *m, int *n, float *a, int *lda, 
	float *tau, float *work, int *info);
 
  void sgelqf_(int *m, int *n, float *a, int *lda, 
	float *tau, float *work, int *lwork, int *info);
 
  void sgels_(char *trans, int *m, int *n, int *
	nrhs, float *a, int *lda, float *b, int *ldb, float *work, 
	int *lwork, int *info);
 
  void sgelsd_(int *m, int *n, int *nrhs, float *a, 
	int *lda, float *b, int *ldb, float *s, float *rcond, int *
	rank, float *work, int *lwork, int *iwork, int *info);
 
  void sgelss_(int *m, int *n, int *nrhs, float *a, 
	int *lda, float *b, int *ldb, float *s, float *rcond, int *
	rank, float *work, int *lwork, int *info);
 
  void sgelsx_(int *m, int *n, int *nrhs, float *a, 
	int *lda, float *b, int *ldb, int *jpvt, float *rcond, 
	int *rank, float *work, int *info);
 
  void sgelsy_(int *m, int *n, int *nrhs, float *a, 
	int *lda, float *b, int *ldb, int *jpvt, float *rcond, 
	int *rank, float *work, int *lwork, int *info);
 
  void sgeql2_(int *m, int *n, float *a, int *lda, 
	float *tau, float *work, int *info);
 
  void sgeqlf_(int *m, int *n, float *a, int *lda, 
	float *tau, float *work, int *lwork, int *info);
 
  void sgeqp3_(int *m, int *n, float *a, int *lda, 
	int *jpvt, float *tau, float *work, int *lwork, int *info);
 
  void sgeqpf_(int *m, int *n, float *a, int *lda, 
	int *jpvt, float *tau, float *work, int *info);
 
  void sgeqr2_(int *m, int *n, float *a, int *lda, 
	float *tau, float *work, int *info);
 
  void sgeqrf_(int *m, int *n, float *a, int *lda, 
	float *tau, float *work, int *lwork, int *info);
 
  void sgerfs_(char *trans, int *n, int *nrhs, float *a, 
	int *lda, float *af, int *ldaf, int *ipiv, float *b, 
	int *ldb, float *x, int *ldx, float *ferr, float *berr, float *
	work, int *iwork, int *info);
 
  void sgerq2_(int *m, int *n, float *a, int *lda, 
	float *tau, float *work, int *info);
 
  void sgerqf_(int *m, int *n, float *a, int *lda, 
	float *tau, float *work, int *lwork, int *info);
 
  void sgesc2_(int *n, float *a, int *lda, float *rhs, 
	int *ipiv, int *jpiv, float *scale);
 
  void sgesdd_(char *jobz, int *m, int *n, float *a, 
	int *lda, float *s, float *u, int *ldu, float *vt, int *ldvt,
	 float *work, int *lwork, int *iwork, int *info);
 
  void sgesv_(int *n, int *nrhs, float *a, int *lda, 
	int *ipiv, float *b, int *ldb, int *info);
 
  void sgesvd_(char *jobu, char *jobvt, int *m, int *n, 
	float *a, int *lda, float *s, float *u, int *ldu, float *vt, 
	int *ldvt, float *work, int *lwork, int *info);
 
  void sgesvx_(char *fact, char *trans, int *n, int *
	nrhs, float *a, int *lda, float *af, int *ldaf, int *ipiv, 
	char *equed, float *r__, float *c__, float *b, int *ldb, float *x, 
	int *ldx, float *rcond, float *ferr, float *berr, float *work, 
	int *iwork, int *info);
 
  void sgetc2_(int *n, float *a, int *lda, int *ipiv,
	 int *jpiv, int *info);
 
  void sgetf2_(int *m, int *n, float *a, int *lda, 
	int *ipiv, int *info);
 
  void sgetrf_(int *m, int *n, float *a, int *lda, 
	int *ipiv, int *info);
 
  void sgetri_(int *n, float *a, int *lda, int *ipiv,
	 float *work, int *lwork, int *info);
 
  void sgetrs_(char *trans, int *n, int *nrhs, float *a, 
	int *lda, int *ipiv, float *b, int *ldb, int *info);
 
  void sggbak_(char *job, char *side, int *n, int *ilo, 
	int *ihi, float *lscale, float *rscale, int *m, float *v, 
	int *ldv, int *info);
 
  void sggbal_(char *job, int *n, float *a, int *lda, 
	float *b, int *ldb, int *ilo, int *ihi, float *lscale, float 
	*rscale, float *work, int *info);
 
  void sgges_(char *jobvsl, char *jobvsr, char *sort, L_fp 
	selctg, int *n, float *a, int *lda, float *b, int *ldb, 
	int *sdim, float *alphar, float *alphai, float *beta, float *vsl, 
	int *ldvsl, float *vsr, int *ldvsr, float *work, int *lwork,
	 logical *bwork, int *info);
 
  void sggesx_(char *jobvsl, char *jobvsr, char *sort, L_fp 
	selctg, char *sense, int *n, float *a, int *lda, float *b, 
	int *ldb, int *sdim, float *alphar, float *alphai, float *beta, 
	float *vsl, int *ldvsl, float *vsr, int *ldvsr, float *rconde, 
	float *rcondv, float *work, int *lwork, int *iwork, int *
	liwork, logical *bwork, int *info);
 
  void sggev_(char *jobvl, char *jobvr, int *n, float *a, 
	int *lda, float *b, int *ldb, float *alphar, float *alphai, float 
	*beta, float *vl, int *ldvl, float *vr, int *ldvr, float *work, 
	int *lwork, int *info);
 
  void sggevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, int *n, float *a, int *lda, float *b, int *ldb, float 
	*alphar, float *alphai, float *beta, float *vl, int *ldvl, float *vr, 
	int *ldvr, int *ilo, int *ihi, float *lscale, float *rscale,
	 float *abnrm, float *bbnrm, float *rconde, float *rcondv, float *work, 
	int *lwork, int *iwork, logical *bwork, int *info);
 
  void sggglm_(int *n, int *m, int *p, float *a, 
	int *lda, float *b, int *ldb, float *d__, float *x, float *y, 
	float *work, int *lwork, int *info);
 
  void sgghrd_(char *compq, char *compz, int *n, int *
	ilo, int *ihi, float *a, int *lda, float *b, int *ldb, float 
	*q, int *ldq, float *z__, int *ldz, int *info);
 
  void sgglse_(int *m, int *n, int *p, float *a, 
	int *lda, float *b, int *ldb, float *c__, float *d__, float *x, 
	float *work, int *lwork, int *info);
 
  void sggqrf_(int *n, int *m, int *p, float *a, 
	int *lda, float *taua, float *b, int *ldb, float *taub, float *
	work, int *lwork, int *info);
 
  void sggrqf_(int *m, int *p, int *n, float *a, 
	int *lda, float *taua, float *b, int *ldb, float *taub, float *
	work, int *lwork, int *info);
 
  void sggsvd_(char *jobu, char *jobv, char *jobq, int *m, 
	int *n, int *p, int *k, int *l, float *a, int *lda,
	 float *b, int *ldb, float *alpha, float *beta, float *u, int *
	ldu, float *v, int *ldv, float *q, int *ldq, float *work, 
	int *iwork, int *info);
 
  void sggsvp_(char *jobu, char *jobv, char *jobq, int *m, 
	int *p, int *n, float *a, int *lda, float *b, int *ldb, 
	float *tola, float *tolb, int *k, int *l, float *u, int *ldu,
	 float *v, int *ldv, float *q, int *ldq, int *iwork, float *
	tau, float *work, int *info);
 
  void sgtcon_(char *norm, int *n, float *dl, float *d__, 
	float *du, float *du2, int *ipiv, float *anorm, float *rcond, float *
	work, int *iwork, int *info);
 
  void sgtrfs_(char *trans, int *n, int *nrhs, float *dl,
	 float *d__, float *du, float *dlf, float *df, float *duf, float *du2, 
	int *ipiv, float *b, int *ldb, float *x, int *ldx, float *
	ferr, float *berr, float *work, int *iwork, int *info);
 
  void sgtsv_(int *n, int *nrhs, float *dl, float *d__, 
	float *du, float *b, int *ldb, int *info);
 
  void sgtsvx_(char *fact, char *trans, int *n, int *
	nrhs, float *dl, float *d__, float *du, float *dlf, float *df, float *duf, 
	float *du2, int *ipiv, float *b, int *ldb, float *x, int *
	ldx, float *rcond, float *ferr, float *berr, float *work, int *iwork, 
	int *info);
 
  void sgttrf_(int *n, float *dl, float *d__, float *du, float *
	du2, int *ipiv, int *info);
 
  void sgttrs_(char *trans, int *n, int *nrhs, float *dl,
	 float *d__, float *du, float *du2, int *ipiv, float *b, int *ldb,
	 int *info);
 
  void sgtts2_(int *itrans, int *n, int *nrhs, float 
	*dl, float *d__, float *du, float *du2, int *ipiv, float *b, int *
	ldb);
 
  void shgeqz_(char *job, char *compq, char *compz, int *n, 
	int *ilo, int *ihi, float *a, int *lda, float *b, int *
	ldb, float *alphar, float *alphai, float *beta, float *q, int *ldq, 
	float *z__, int *ldz, float *work, int *lwork, int *info);
 
  void shsein_(char *side, char *eigsrc, char *initv, logical *
	select, int *n, float *h__, int *ldh, float *wr, float *wi, float 
	*vl, int *ldvl, float *vr, int *ldvr, int *mm, int *m, 
	float *work, int *ifaill, int *ifailr, int *info);
 
  void shseqr_(char *job, char *compz, int *n, int *ilo,
	 int *ihi, float *h__, int *ldh, float *wr, float *wi, float *z__,
	 int *ldz, float *work, int *lwork, int *info);
 
  void slabad_(float *small, float *large);
 
  void slabrd_(int *m, int *n, int *nb, float *a, 
	int *lda, float *d__, float *e, float *tauq, float *taup, float *x, 
	int *ldx, float *y, int *ldy);
 
  void slacon_(int *n, float *v, float *x, int *isgn, 
	float *est, int *kase);
 
  void slacpy_(char *uplo, int *m, int *n, float *a, 
	int *lda, float *b, int *ldb);
 
  void sladiv_(float *a, float *b, float *c__, float *d__, float *p, 
	float *q);
 
  void slae2_(float *a, float *b, float *c__, float *rt1, float *rt2);
 
  void slaebz_(int *ijob, int *nitmax, int *n, 
	int *mmax, int *minp, int *nbmin, float *abstol, float *
	reltol, float *pivmin, float *d__, float *e, float *e2, int *nval, 
	float *ab, float *c__, int *mout, int *nab, float *work, int 
	*iwork, int *info);
 
  void slaed0_(int *icompq, int *qsiz, int *n, float 
	*d__, float *e, float *q, int *ldq, float *qstore, int *ldqs, 
	float *work, int *iwork, int *info);
 
  void slaed1_(int *n, float *d__, float *q, int *ldq, 
	int *indxq, float *rho, int *cutpnt, float *work, int *
	iwork, int *info);
 
  void slaed2_(int *k, int *n, int *n1, float *d__, 
	float *q, int *ldq, int *indxq, float *rho, float *z__, float *
	dlamda, float *w, float *q2, int *indx, int *indxc, int *
	indxp, int *coltyp, int *info);
 
  void slaed3_(int *k, int *n, int *n1, float *d__, 
	float *q, int *ldq, float *rho, float *dlamda, float *q2, int *
	indx, int *ctot, float *w, float *s, int *info);
 
  void slaed4_(int *n, int *i__, float *d__, float *z__, 
	float *delta, float *rho, float *dlam, int *info);
 
  void slaed5_(int *i__, float *d__, float *z__, float *delta, 
	float *rho, float *dlam);
 
  void slaed6_(int *kniter, logical *orgati, float *rho, 
	float *d__, float *z__, float *finit, float *tau, int *info);
 
  void slaed7_(int *icompq, int *n, int *qsiz, 
	int *tlvls, int *curlvl, int *curpbm, float *d__, float *q, 
	int *ldq, int *indxq, float *rho, int *cutpnt, float *
	qstore, int *qptr, int *prmptr, int *perm, int *
	givptr, int *givcol, float *givnum, float *work, int *iwork, 
	int *info);
 
  void slaed8_(int *icompq, int *k, int *n, int 
	*qsiz, float *d__, float *q, int *ldq, int *indxq, float *rho, 
	int *cutpnt, float *z__, float *dlamda, float *q2, int *ldq2, 
	float *w, int *perm, int *givptr, int *givcol, float *
	givnum, int *indxp, int *indx, int *info);
 
  void slaed9_(int *k, int *kstart, int *kstop, 
	int *n, float *d__, float *q, int *ldq, float *rho, float *dlamda,
	 float *w, float *s, int *lds, int *info);
 
  void slaeda_(int *n, int *tlvls, int *curlvl, 
	int *curpbm, int *prmptr, int *perm, int *givptr, 
	int *givcol, float *givnum, float *q, int *qptr, float *z__, 
	float *ztemp, int *info);
 
  void slaein_(logical *rightv, logical *noinit, int *n, 
	float *h__, int *ldh, float *wr, float *wi, float *vr, float *vi, float 
	*b, int *ldb, float *work, float *eps3, float *smlnum, float *bignum, 
	int *info);
 
  void slaev2_(float *a, float *b, float *c__, float *rt1, float *
	rt2, float *cs1, float *sn1);
 
  void slaexc_(logical *wantq, int *n, float *t, int *
	ldt, float *q, int *ldq, int *j1, int *n1, int *n2, 
	float *work, int *info);
 
  void slag2_(float *a, int *lda, float *b, int *ldb, 
	float *safmin, float *scale1, float *scale2, float *wr1, float *wr2, float *
	wi);
 
  void slags2_(logical *upper, float *a1, float *a2, float *a3, 
	float *b1, float *b2, float *b3, float *csu, float *snu, float *csv, float *
	snv, float *csq, float *snq);
 
  void slagtf_(int *n, float *a, float *lambda, float *b, float 
	*c__, float *tol, float *d__, int *input, int *info);
 
  void slagtm_(char *trans, int *n, int *nrhs, float *
	alpha, float *dl, float *d__, float *du, float *x, int *ldx, float *
	beta, float *b, int *ldb);
 
  void slagts_(int *job, int *n, float *a, float *b, float 
	*c__, float *d__, int *input, float *y, float *tol, int *info);
 
  void slagv2_(float *a, int *lda, float *b, int *ldb, 
	float *alphar, float *alphai, float *beta, float *csl, float *snl, float *
	csr, float *snr);
 
  void slahqr_(logical *wantt, logical *wantz, int *n, 
	int *ilo, int *ihi, float *h__, int *ldh, float *wr, float *
	wi, int *iloz, int *ihiz, float *z__, int *ldz, int *
	info);
 
  void slahrd_(int *n, int *k, int *nb, float *a, 
	int *lda, float *tau, float *t, int *ldt, float *y, int *ldy);
 
  void slaic1_(int *job, int *j, float *x, float *sest, 
	float *w, float *gamma, float *sestpr, float *s, float *c__);
 
  void slaln2_(logical *ltrans, int *na, int *nw, float *
	smin, float *ca, float *a, int *lda, float *d1, float *d2, float *b, 
	int *ldb, float *wr, float *wi, float *x, int *ldx, float *scale, 
	float *xnorm, int *info);
 
  void slals0_(int *icompq, int *nl, int *nr, 
	int *sqre, int *nrhs, float *b, int *ldb, float *bx, 
	int *ldbx, int *perm, int *givptr, int *givcol, 
	int *ldgcol, float *givnum, int *ldgnum, float *poles, float *
	difl, float *difr, float *z__, int *k, float *c__, float *s, float *
	work, int *info);
 
  void slalsa_(int *icompq, int *smlsiz, int *n, 
	int *nrhs, float *b, int *ldb, float *bx, int *ldbx, float *
	u, int *ldu, float *vt, int *k, float *difl, float *difr, float *
	z__, float *poles, int *givptr, int *givcol, int *ldgcol, 
	int *perm, float *givnum, float *c__, float *s, float *work, int *
	iwork, int *info);
 
  void slalsd_(char *uplo, int *smlsiz, int *n, int 
	*nrhs, float *d__, float *e, float *b, int *ldb, float *rcond, 
	int *rank, float *work, int *iwork, int *info);
 
  void slamc1_(int *beta, int *t, logical *rnd, logical 
	*ieee1);
 
  void slamc2_(int *beta, int *t, logical *rnd, float *
	eps, int *emin, float *rmin, int *emax, float *rmax);
 
  void slamc4_(int *emin, float *start, int *base);
 
  void slamc5_(int *beta, int *p, int *emin, 
	logical *ieee, int *emax, float *rmax);
 
  void slamrg_(int *n1, int *n2, float *a, int *
	strd1, int *strd2, int *index);
 
  void slanv2_(float *a, float *b, float *c__, float *d__, float *
	rt1r, float *rt1i, float *rt2r, float *rt2i, float *cs, float *sn);
 
  void slapll_(int *n, float *x, int *incx, float *y, 
	int *incy, float *ssmin);
 
  void slapmt_(logical *forwrd, int *m, int *n, float *x,
	 int *ldx, int *k);
 
  void slaqgb_(int *m, int *n, int *kl, int *ku,
	 float *ab, int *ldab, float *r__, float *c__, float *rowcnd, float *
	colcnd, float *amax, char *equed);
 
  void slaqge_(int *m, int *n, float *a, int *lda, 
	float *r__, float *c__, float *rowcnd, float *colcnd, float *amax, char *
	equed);
 
  void slaqp2_(int *m, int *n, int *offset, float *a,
	 int *lda, int *jpvt, float *tau, float *vn1, float *vn2, float *
	work);
 
  void slaqps_(int *m, int *n, int *offset, int 
	*nb, int *kb, float *a, int *lda, int *jpvt, float *tau, 
	float *vn1, float *vn2, float *auxv, float *f, int *ldf);
 
  void slaqsb_(char *uplo, int *n, int *kd, float *ab, 
	int *ldab, float *s, float *scond, float *amax, char *equed);
 
  void slaqsp_(char *uplo, int *n, float *ap, float *s, float *
	scond, float *amax, char *equed);
 
  void slaqsy_(char *uplo, int *n, float *a, int *lda, 
	float *s, float *scond, float *amax, char *equed);
 
  void slaqtr_(logical *ltran, logical *lreal, int *n, float 
	*t, int *ldt, float *b, float *w, float *scale, float *x, float *work, 
	int *info);
 
  void slar1v_(int *n, int *b1, int *bn, float *
	sigma, float *d__, float *l, float *ld, float *lld, float *gersch, float *
	z__, float *ztz, float *mingma, int *r__, int *isuppz, float *
	work);
 
  void slar2v_(int *n, float *x, float *y, float *z__, int 
	*incx, float *c__, float *s, int *incc);
 
  void slarf_(char *side, int *m, int *n, float *v, 
	int *incv, float *tau, float *c__, int *ldc, float *work);
 
  void slarfb_(char *side, char *trans, char *direct, char *
	storev, int *m, int *n, int *k, float *v, int *ldv, 
	float *t, int *ldt, float *c__, int *ldc, float *work, int *
	ldwork);
 
  void slarfg_(int *n, float *alpha, float *x, int *incx, 
	float *tau);
 
  void slarft_(char *direct, char *storev, int *n, int *
	k, float *v, int *ldv, float *tau, float *t, int *ldt);
 
  void slarfx_(char *side, int *m, int *n, float *v, 
	float *tau, float *c__, int *ldc, float *work);
 
  void slargv_(int *n, float *x, int *incx, float *y, 
	int *incy, float *c__, int *incc);
 
  void slarnv_(int *idist, int *iseed, int *n, float 
	*x);
 
  void slarrb_(int *n, float *d__, float *l, float *ld, float *
	lld, int *ifirst, int *ilast, float *sigma, float *reltol, float 
	*w, float *wgap, float *werr, float *work, int *iwork, int *info);
 
  void slarre_(int *n, float *d__, float *e, float *tol, 
	int *nsplit, int *isplit, int *m, float *w, float *woff, 
	float *gersch, float *work, int *info);
 
  void slarrf_(int *n, float *d__, float *l, float *ld, float *
	lld, int *ifirst, int *ilast, float *w, float *dplus, float *
	lplus, float *work, int *iwork, int *info);
 
  void slarrv_(int *n, float *d__, float *l, int *isplit, 
	int *m, float *w, int *iblock, float *gersch, float *tol, float *
	z__, int *ldz, int *isuppz, float *work, int *iwork, 
	int *info);
 
  void slartg_(float *f, float *g, float *cs, float *sn, float *r__);
 
  void slartv_(int *n, float *x, int *incx, float *y, 
	int *incy, float *c__, float *s, int *incc);
 
  void slaruv_(int *iseed, int *n, float *x);
 
  void slarz_(char *side, int *m, int *n, int *l, 
	float *v, int *incv, float *tau, float *c__, int *ldc, float *
	work);
 
  void slarzb_(char *side, char *trans, char *direct, char *
	storev, int *m, int *n, int *k, int *l, float *v, 
	int *ldv, float *t, int *ldt, float *c__, int *ldc, float *
	work, int *ldwork);
 
  void slarzt_(char *direct, char *storev, int *n, int *
	k, float *v, int *ldv, float *tau, float *t, int *ldt);
 
  void slas2_(float *f, float *g, float *h__, float *ssmin, float *
	ssmax);
 
  void slascl_(char *type__, int *kl, int *ku, float *
	cfrom, float *cto, int *m, int *n, float *a, int *lda, 
	int *info);
 
  void slasd0_(int *n, int *sqre, float *d__, float *e, 
	float *u, int *ldu, float *vt, int *ldvt, int *smlsiz, 
	int *iwork, float *work, int *info);
 
  void slasd1_(int *nl, int *nr, int *sqre, float *
	d__, float *alpha, float *beta, float *u, int *ldu, float *vt, 
	int *ldvt, int *idxq, int *iwork, float *work, int *
	info);
 
  void slasd2_(int *nl, int *nr, int *sqre, int 
	*k, float *d__, float *z__, float *alpha, float *beta, float *u, int *
	ldu, float *vt, int *ldvt, float *dsigma, float *u2, int *ldu2, 
	float *vt2, int *ldvt2, int *idxp, int *idx, int *idxc,
	 int *idxq, int *coltyp, int *info);
 
  void slasd3_(int *nl, int *nr, int *sqre, int 
	*k, float *d__, float *q, int *ldq, float *dsigma, float *u, int *
	ldu, float *u2, int *ldu2, float *vt, int *ldvt, float *vt2, 
	int *ldvt2, int *idxc, int *ctot, float *z__, int *
	info);
 
  void slasd4_(int *n, int *i__, float *d__, float *z__, 
	float *delta, float *rho, float *sigma, float *work, int *info);
 
  void slasd5_(int *i__, float *d__, float *z__, float *delta, 
	float *rho, float *dsigma, float *work);
 
  void slasd6_(int *icompq, int *nl, int *nr, 
	int *sqre, float *d__, float *vf, float *vl, float *alpha, float *beta,
	 int *idxq, int *perm, int *givptr, int *givcol, 
	int *ldgcol, float *givnum, int *ldgnum, float *poles, float *
	difl, float *difr, float *z__, int *k, float *c__, float *s, float *
	work, int *iwork, int *info);
 
  void slasd7_(int *icompq, int *nl, int *nr, 
	int *sqre, int *k, float *d__, float *z__, float *zw, float *vf, 
	float *vfw, float *vl, float *vlw, float *alpha, float *beta, float *dsigma,
	 int *idx, int *idxp, int *idxq, int *perm, int *
	givptr, int *givcol, int *ldgcol, float *givnum, int *
	ldgnum, float *c__, float *s, int *info);
 
  void slasd8_(int *icompq, int *k, float *d__, float *
	z__, float *vf, float *vl, float *difl, float *difr, int *lddifr, 
	float *dsigma, float *work, int *info);
 
  void slasd9_(int *icompq, int *ldu, int *k, float *
	d__, float *z__, float *vf, float *vl, float *difl, float *difr, float *
	dsigma, float *work, int *info);
 
  void slasda_(int *icompq, int *smlsiz, int *n, 
	int *sqre, float *d__, float *e, float *u, int *ldu, float *vt, 
	int *k, float *difl, float *difr, float *z__, float *poles, int *
	givptr, int *givcol, int *ldgcol, int *perm, float *givnum,
	 float *c__, float *s, float *work, int *iwork, int *info);
 
  void slasdq_(char *uplo, int *sqre, int *n, int *
	ncvt, int *nru, int *ncc, float *d__, float *e, float *vt, 
	int *ldvt, float *u, int *ldu, float *c__, int *ldc, float *
	work, int *info);
 
  void slasdt_(int *n, int *lvl, int *nd, int *
	inode, int *ndiml, int *ndimr, int *msub);
 
  void slaset_(char *uplo, int *m, int *n, float *alpha, 
	float *beta, float *a, int *lda);
 
  void slasq1_(int *n, float *d__, float *e, float *work, 
	int *info);
 
  void slasq2_(int *n, float *z__, int *info);
 
  void slasq3_(int *i0, int *n0, float *z__, int *pp,
	 float *dmin__, float *sigma, float *desig, float *qmax, int *nfail, 
	int *iter, int *ndiv, logical *ieee);
 
  void slasq4_(int *i0, int *n0, float *z__, int *pp,
	 int *n0in, float *dmin__, float *dmin1, float *dmin2, float *dn, 
	float *dn1, float *dn2, float *tau, int *ttype);
 
  void slasq5_(int *i0, int *n0, float *z__, int *pp,
	 float *tau, float *dmin__, float *dmin1, float *dmin2, float *dn, float *
	dnm1, float *dnm2, logical *ieee);
 
  void slasq6_(int *i0, int *n0, float *z__, int *pp,
	 float *dmin__, float *dmin1, float *dmin2, float *dn, float *dnm1, float *
	dnm2);
 
  void slasr_(char *side, char *pivot, char *direct, int *m,
	 int *n, float *c__, float *s, float *a, int *lda);
 
  void slasrt_(char *id, int *n, float *d__, int *info);
 
  void slassq_(int *n, float *x, int *incx, float *scale, 
	float *sumsq);
 
  void slasv2_(float *f, float *g, float *h__, float *ssmin, float *
	ssmax, float *snr, float *csr, float *snl, float *csl);
 
  void slaswp_(int *n, float *a, int *lda, int *k1, 
	int *k2, int *ipiv, int *incx);
 
  void slasy2_(logical *ltranl, logical *ltranr, int *isgn, 
	int *n1, int *n2, float *tl, int *ldtl, float *tr, int *
	ldtr, float *b, int *ldb, float *scale, float *x, int *ldx, float 
	*xnorm, int *info);
 
  void slasyf_(char *uplo, int *n, int *nb, int *kb,
	 float *a, int *lda, int *ipiv, float *w, int *ldw, int 
	*info);
 
  void slatbs_(char *uplo, char *trans, char *diag, char *
	normin, int *n, int *kd, float *ab, int *ldab, float *x, 
	float *scale, float *cnorm, int *info);
 
  void slatdf_(int *ijob, int *n, float *z__, int *
	ldz, float *rhs, float *rdsum, float *rdscal, int *ipiv, int *
	jpiv);
 
  void slatps_(char *uplo, char *trans, char *diag, char *
	normin, int *n, float *ap, float *x, float *scale, float *cnorm, 
	int *info);
 
  void slatrd_(char *uplo, int *n, int *nb, float *a, 
	int *lda, float *e, float *tau, float *w, int *ldw);
 
  void slatrs_(char *uplo, char *trans, char *diag, char *
	normin, int *n, float *a, int *lda, float *x, float *scale, float 
	*cnorm, int *info);
 
  void slatrz_(int *m, int *n, int *l, float *a, 
	int *lda, float *tau, float *work);
 
  void slatzm_(char *side, int *m, int *n, float *v, 
	int *incv, float *tau, float *c1, float *c2, int *ldc, float *
	work);
 
  void slauu2_(char *uplo, int *n, float *a, int *lda, 
	int *info);
 
  void slauum_(char *uplo, int *n, float *a, int *lda, 
	int *info);
 
  void sopgtr_(char *uplo, int *n, float *ap, float *tau, 
	float *q, int *ldq, float *work, int *info);
 
  void sopmtr_(char *side, char *uplo, char *trans, int *m, 
	int *n, float *ap, float *tau, float *c__, int *ldc, float *work, 
	int *info);
 
  void sorg2l_(int *m, int *n, int *k, float *a, 
	int *lda, float *tau, float *work, int *info);
 
  void sorg2r_(int *m, int *n, int *k, float *a, 
	int *lda, float *tau, float *work, int *info);
 
  void sorgbr_(char *vect, int *m, int *n, int *k, 
	float *a, int *lda, float *tau, float *work, int *lwork, int 
	*info);
 
  void sorghr_(int *n, int *ilo, int *ihi, float *a, 
	int *lda, float *tau, float *work, int *lwork, int *info);
 
  void sorgl2_(int *m, int *n, int *k, float *a, 
	int *lda, float *tau, float *work, int *info);
 
  void sorglq_(int *m, int *n, int *k, float *a, 
	int *lda, float *tau, float *work, int *lwork, int *info);
 
  void sorgql_(int *m, int *n, int *k, float *a, 
	int *lda, float *tau, float *work, int *lwork, int *info);
 
  void sorgqr_(int *m, int *n, int *k, float *a, 
	int *lda, float *tau, float *work, int *lwork, int *info);
 
  void sorgr2_(int *m, int *n, int *k, float *a, 
	int *lda, float *tau, float *work, int *info);
 
  void sorgrq_(int *m, int *n, int *k, float *a, 
	int *lda, float *tau, float *work, int *lwork, int *info);
 
  void sorgtr_(char *uplo, int *n, float *a, int *lda, 
	float *tau, float *work, int *lwork, int *info);
 
  void sorm2l_(char *side, char *trans, int *m, int *n, 
	int *k, float *a, int *lda, float *tau, float *c__, int *ldc,
	 float *work, int *info);
 
  void sorm2r_(char *side, char *trans, int *m, int *n, 
	int *k, float *a, int *lda, float *tau, float *c__, int *ldc,
	 float *work, int *info);
 
  void sormbr_(char *vect, char *side, char *trans, int *m, 
	int *n, int *k, float *a, int *lda, float *tau, float *c__, 
	int *ldc, float *work, int *lwork, int *info);
 
  void sormhr_(char *side, char *trans, int *m, int *n, 
	int *ilo, int *ihi, float *a, int *lda, float *tau, float *
	c__, int *ldc, float *work, int *lwork, int *info);
 
  void sorml2_(char *side, char *trans, int *m, int *n, 
	int *k, float *a, int *lda, float *tau, float *c__, int *ldc,
	 float *work, int *info);
 
  void sormlq_(char *side, char *trans, int *m, int *n, 
	int *k, float *a, int *lda, float *tau, float *c__, int *ldc,
	 float *work, int *lwork, int *info);
 
  void sormql_(char *side, char *trans, int *m, int *n, 
	int *k, float *a, int *lda, float *tau, float *c__, int *ldc,
	 float *work, int *lwork, int *info);
 
  void sormqr_(char *side, char *trans, int *m, int *n, 
	int *k, float *a, int *lda, float *tau, float *c__, int *ldc,
	 float *work, int *lwork, int *info);
 
  void sormr2_(char *side, char *trans, int *m, int *n, 
	int *k, float *a, int *lda, float *tau, float *c__, int *ldc,
	 float *work, int *info);
 
  void sormr3_(char *side, char *trans, int *m, int *n, 
	int *k, int *l, float *a, int *lda, float *tau, float *c__, 
	int *ldc, float *work, int *info);
 
  void sormrq_(char *side, char *trans, int *m, int *n, 
	int *k, float *a, int *lda, float *tau, float *c__, int *ldc,
	 float *work, int *lwork, int *info);
 
  void sormrz_(char *side, char *trans, int *m, int *n, 
	int *k, int *l, float *a, int *lda, float *tau, float *c__, 
	int *ldc, float *work, int *lwork, int *info);
 
  void sormtr_(char *side, char *uplo, char *trans, int *m, 
	int *n, float *a, int *lda, float *tau, float *c__, int *ldc,
	 float *work, int *lwork, int *info);
 
  void spbcon_(char *uplo, int *n, int *kd, float *ab, 
	int *ldab, float *anorm, float *rcond, float *work, int *iwork, 
	int *info);
 
  void spbequ_(char *uplo, int *n, int *kd, float *ab, 
	int *ldab, float *s, float *scond, float *amax, int *info);
 
  void spbrfs_(char *uplo, int *n, int *kd, int *
	nrhs, float *ab, int *ldab, float *afb, int *ldafb, float *b, 
	int *ldb, float *x, int *ldx, float *ferr, float *berr, float *
	work, int *iwork, int *info);
 
  void spbstf_(char *uplo, int *n, int *kd, float *ab, 
	int *ldab, int *info);
 
  void spbsv_(char *uplo, int *n, int *kd, int *
	nrhs, float *ab, int *ldab, float *b, int *ldb, int *info);
 
  void spbsvx_(char *fact, char *uplo, int *n, int *kd, 
	int *nrhs, float *ab, int *ldab, float *afb, int *ldafb, 
	char *equed, float *s, float *b, int *ldb, float *x, int *ldx, 
	float *rcond, float *ferr, float *berr, float *work, int *iwork, 
	int *info);
 
  void spbtf2_(char *uplo, int *n, int *kd, float *ab, 
	int *ldab, int *info);
 
  void spbtrf_(char *uplo, int *n, int *kd, float *ab, 
	int *ldab, int *info);
 
  void spbtrs_(char *uplo, int *n, int *kd, int *
	nrhs, float *ab, int *ldab, float *b, int *ldb, int *info);
 
  void spocon_(char *uplo, int *n, float *a, int *lda, 
	float *anorm, float *rcond, float *work, int *iwork, int *info);
 
  void spoequ_(int *n, float *a, int *lda, float *s, float 
	*scond, float *amax, int *info);
 
  void sporfs_(char *uplo, int *n, int *nrhs, float *a, 
	int *lda, float *af, int *ldaf, float *b, int *ldb, float *x,
	 int *ldx, float *ferr, float *berr, float *work, int *iwork, 
	int *info);
 
  void sposv_(char *uplo, int *n, int *nrhs, float *a, 
	int *lda, float *b, int *ldb, int *info);
 
  void sposvx_(char *fact, char *uplo, int *n, int *
	nrhs, float *a, int *lda, float *af, int *ldaf, char *equed, 
	float *s, float *b, int *ldb, float *x, int *ldx, float *rcond, 
	float *ferr, float *berr, float *work, int *iwork, int *info);
 
  void spotf2_(char *uplo, int *n, float *a, int *lda, 
	int *info);
 
  void spotrf_(char *uplo, int *n, float *a, int *lda, 
	int *info);
 
  void spotri_(char *uplo, int *n, float *a, int *lda, 
	int *info);
 
  void spotrs_(char *uplo, int *n, int *nrhs, float *a, 
	int *lda, float *b, int *ldb, int *info);
 
  void sppcon_(char *uplo, int *n, float *ap, float *anorm, 
	float *rcond, float *work, int *iwork, int *info);
 
  void sppequ_(char *uplo, int *n, float *ap, float *s, float *
	scond, float *amax, int *info);
 
  void spprfs_(char *uplo, int *n, int *nrhs, float *ap, 
	float *afp, float *b, int *ldb, float *x, int *ldx, float *ferr, 
	float *berr, float *work, int *iwork, int *info);
 
  void sppsv_(char *uplo, int *n, int *nrhs, float *ap, 
	float *b, int *ldb, int *info);
 
  void sppsvx_(char *fact, char *uplo, int *n, int *
	nrhs, float *ap, float *afp, char *equed, float *s, float *b, int *
	ldb, float *x, int *ldx, float *rcond, float *ferr, float *berr, float 
	*work, int *iwork, int *info);
 
  void spptrf_(char *uplo, int *n, float *ap, int *info);
 
  void spptri_(char *uplo, int *n, float *ap, int *info);
 
  void spptrs_(char *uplo, int *n, int *nrhs, float *ap, 
	float *b, int *ldb, int *info);
 
  void sptcon_(int *n, float *d__, float *e, float *anorm, 
	float *rcond, float *work, int *info);
 
  void spteqr_(char *compz, int *n, float *d__, float *e, 
	float *z__, int *ldz, float *work, int *info);
 
  void sptrfs_(int *n, int *nrhs, float *d__, float *e, 
	float *df, float *ef, float *b, int *ldb, float *x, int *ldx, 
	float *ferr, float *berr, float *work, int *info);
 
  void sptsv_(int *n, int *nrhs, float *d__, float *e, 
	float *b, int *ldb, int *info);
 
  void sptsvx_(char *fact, int *n, int *nrhs, float *d__,
	 float *e, float *df, float *ef, float *b, int *ldb, float *x, int 
	*ldx, float *rcond, float *ferr, float *berr, float *work, int *info);
 
  void spttrf_(int *n, float *d__, float *e, int *info);
 
  void spttrs_(int *n, int *nrhs, float *d__, float *e, 
	float *b, int *ldb, int *info);
 
  void sptts2_(int *n, int *nrhs, float *d__, float *e, 
	float *b, int *ldb);
 
  void srscl_(int *n, float *sa, float *sx, int *incx);
 
  void ssbev_(char *jobz, char *uplo, int *n, int *kd, 
	float *ab, int *ldab, float *w, float *z__, int *ldz, float *work,
	 int *info);
 
  void ssbevd_(char *jobz, char *uplo, int *n, int *kd, 
	float *ab, int *ldab, float *w, float *z__, int *ldz, float *work,
	 int *lwork, int *iwork, int *liwork, int *info);
 
  void ssbevx_(char *jobz, char *range, char *uplo, int *n, 
	int *kd, float *ab, int *ldab, float *q, int *ldq, float *vl,
	 float *vu, int *il, int *iu, float *abstol, int *m, float *
	w, float *z__, int *ldz, float *work, int *iwork, int *
	ifail, int *info);
 
  void ssbgst_(char *vect, char *uplo, int *n, int *ka, 
	int *kb, float *ab, int *ldab, float *bb, int *ldbb, float *
	x, int *ldx, float *work, int *info);
 
  void ssbgv_(char *jobz, char *uplo, int *n, int *ka, 
	int *kb, float *ab, int *ldab, float *bb, int *ldbb, float *
	w, float *z__, int *ldz, float *work, int *info);
 
  void ssbgvd_(char *jobz, char *uplo, int *n, int *ka, 
	int *kb, float *ab, int *ldab, float *bb, int *ldbb, float *
	w, float *z__, int *ldz, float *work, int *lwork, int *
	iwork, int *liwork, int *info);
 
  void ssbgvx_(char *jobz, char *range, char *uplo, int *n, 
	int *ka, int *kb, float *ab, int *ldab, float *bb, int *
	ldbb, float *q, int *ldq, float *vl, float *vu, int *il, int 
	*iu, float *abstol, int *m, float *w, float *z__, int *ldz, float 
	*work, int *iwork, int *ifail, int *info);
 
  void ssbtrd_(char *vect, char *uplo, int *n, int *kd, 
	float *ab, int *ldab, float *d__, float *e, float *q, int *ldq, 
	float *work, int *info);
 
  void sspcon_(char *uplo, int *n, float *ap, int *ipiv, 
	float *anorm, float *rcond, float *work, int *iwork, int *info);
 
  void sspev_(char *jobz, char *uplo, int *n, float *ap, 
	float *w, float *z__, int *ldz, float *work, int *info);
 
  void sspevd_(char *jobz, char *uplo, int *n, float *ap, 
	float *w, float *z__, int *ldz, float *work, int *lwork, int 
	*iwork, int *liwork, int *info);
 
  void sspevx_(char *jobz, char *range, char *uplo, int *n, 
	float *ap, float *vl, float *vu, int *il, int *iu, float *abstol, 
	int *m, float *w, float *z__, int *ldz, float *work, int *
	iwork, int *ifail, int *info);
 
  void sspgst_(int *itype, char *uplo, int *n, float *ap,
	 float *bp, int *info);
 
  void sspgv_(int *itype, char *jobz, char *uplo, int *
	n, float *ap, float *bp, float *w, float *z__, int *ldz, float *work, 
	int *info);
 
  void sspgvd_(int *itype, char *jobz, char *uplo, int *
	n, float *ap, float *bp, float *w, float *z__, int *ldz, float *work, 
	int *lwork, int *iwork, int *liwork, int *info);
 
  void sspgvx_(int *itype, char *jobz, char *range, char *
	uplo, int *n, float *ap, float *bp, float *vl, float *vu, int *il,
	 int *iu, float *abstol, int *m, float *w, float *z__, int *
	ldz, float *work, int *iwork, int *ifail, int *info);
 
  void ssprfs_(char *uplo, int *n, int *nrhs, float *ap, 
	float *afp, int *ipiv, float *b, int *ldb, float *x, int *
	ldx, float *ferr, float *berr, float *work, int *iwork, int *
	info);
 
  void sspsv_(char *uplo, int *n, int *nrhs, float *ap, 
	int *ipiv, float *b, int *ldb, int *info);
 
  void sspsvx_(char *fact, char *uplo, int *n, int *
	nrhs, float *ap, float *afp, int *ipiv, float *b, int *ldb, float 
	*x, int *ldx, float *rcond, float *ferr, float *berr, float *work, 
	int *iwork, int *info);
 
  void ssptrd_(char *uplo, int *n, float *ap, float *d__, 
	float *e, float *tau, int *info);
 
  void ssptrf_(char *uplo, int *n, float *ap, int *ipiv, 
	int *info);
 
  void ssptri_(char *uplo, int *n, float *ap, int *ipiv, 
	float *work, int *info);
 
  void ssptrs_(char *uplo, int *n, int *nrhs, float *ap, 
	int *ipiv, float *b, int *ldb, int *info);
 
  void sstebz_(char *range, char *order, int *n, float *vl, 
	float *vu, int *il, int *iu, float *abstol, float *d__, float *e, 
	int *m, int *nsplit, float *w, int *iblock, int *
	isplit, float *work, int *iwork, int *info);
 
  void sstedc_(char *compz, int *n, float *d__, float *e, 
	float *z__, int *ldz, float *work, int *lwork, int *iwork, 
	int *liwork, int *info);
 
  void sstegr_(char *jobz, char *range, int *n, float *d__, 
	float *e, float *vl, float *vu, int *il, int *iu, float *abstol, 
	int *m, float *w, float *z__, int *ldz, int *isuppz, float *
	work, int *lwork, int *iwork, int *liwork, int *info);
 
  void sstein_(int *n, float *d__, float *e, int *m, float 
	*w, int *iblock, int *isplit, float *z__, int *ldz, float *
	work, int *iwork, int *ifail, int *info);
 
  void ssteqr_(char *compz, int *n, float *d__, float *e, 
	float *z__, int *ldz, float *work, int *info);
 
  void ssterf_(int *n, float *d__, float *e, int *info);
 
  void sstev_(char *jobz, int *n, float *d__, float *e, float *
	z__, int *ldz, float *work, int *info);
 
  void sstevd_(char *jobz, int *n, float *d__, float *e, float 
	*z__, int *ldz, float *work, int *lwork, int *iwork, 
	int *liwork, int *info);
 
  void sstevr_(char *jobz, char *range, int *n, float *d__, 
	float *e, float *vl, float *vu, int *il, int *iu, float *abstol, 
	int *m, float *w, float *z__, int *ldz, int *isuppz, float *
	work, int *lwork, int *iwork, int *liwork, int *info);
 
  void sstevx_(char *jobz, char *range, int *n, float *d__, 
	float *e, float *vl, float *vu, int *il, int *iu, float *abstol, 
	int *m, float *w, float *z__, int *ldz, float *work, int *
	iwork, int *ifail, int *info);
 
  void ssycon_(char *uplo, int *n, float *a, int *lda, 
	int *ipiv, float *anorm, float *rcond, float *work, int *iwork, 
	int *info);
 
  void ssyev_(char *jobz, char *uplo, int *n, float *a, 
	int *lda, float *w, float *work, int *lwork, int *info);
 
  void ssyevd_(char *jobz, char *uplo, int *n, float *a, 
	int *lda, float *w, float *work, int *lwork, int *iwork, 
	int *liwork, int *info);
 
  void ssyevr_(char *jobz, char *range, char *uplo, int *n, 
	float *a, int *lda, float *vl, float *vu, int *il, int *iu, 
	float *abstol, int *m, float *w, float *z__, int *ldz, int *
	isuppz, float *work, int *lwork, int *iwork, int *liwork, 
	int *info);
 
  void ssyevx_(char *jobz, char *range, char *uplo, int *n, 
	float *a, int *lda, float *vl, float *vu, int *il, int *iu, 
	float *abstol, int *m, float *w, float *z__, int *ldz, float *
	work, int *lwork, int *iwork, int *ifail, int *info);
 
  void ssygs2_(int *itype, char *uplo, int *n, float *a, 
	int *lda, float *b, int *ldb, int *info);
 
  void ssygst_(int *itype, char *uplo, int *n, float *a, 
	int *lda, float *b, int *ldb, int *info);
 
  void ssygv_(int *itype, char *jobz, char *uplo, int *
	n, float *a, int *lda, float *b, int *ldb, float *w, float *work, 
	int *lwork, int *info);
 
  void ssygvd_(int *itype, char *jobz, char *uplo, int *
	n, float *a, int *lda, float *b, int *ldb, float *w, float *work, 
	int *lwork, int *iwork, int *liwork, int *info);
 
  void ssygvx_(int *itype, char *jobz, char *range, char *
	uplo, int *n, float *a, int *lda, float *b, int *ldb, float *
	vl, float *vu, int *il, int *iu, float *abstol, int *m, 
	float *w, float *z__, int *ldz, float *work, int *lwork, int 
	*iwork, int *ifail, int *info);
 
  void ssyrfs_(char *uplo, int *n, int *nrhs, float *a, 
	int *lda, float *af, int *ldaf, int *ipiv, float *b, 
	int *ldb, float *x, int *ldx, float *ferr, float *berr, float *
	work, int *iwork, int *info);
 
  void ssysv_(char *uplo, int *n, int *nrhs, float *a, 
	int *lda, int *ipiv, float *b, int *ldb, float *work, 
	int *lwork, int *info);
 
  void ssysvx_(char *fact, char *uplo, int *n, int *
	nrhs, float *a, int *lda, float *af, int *ldaf, int *ipiv, 
	float *b, int *ldb, float *x, int *ldx, float *rcond, float *ferr,
	 float *berr, float *work, int *lwork, int *iwork, int *
	info);
 
  void ssytd2_(char *uplo, int *n, float *a, int *lda, 
	float *d__, float *e, float *tau, int *info);
 
  void ssytf2_(char *uplo, int *n, float *a, int *lda, 
	int *ipiv, int *info);
 
  void ssytrd_(char *uplo, int *n, float *a, int *lda, 
	float *d__, float *e, float *tau, float *work, int *lwork, int *
	info);
 
  void ssytrf_(char *uplo, int *n, float *a, int *lda, 
	int *ipiv, float *work, int *lwork, int *info);
 
  void ssytri_(char *uplo, int *n, float *a, int *lda, 
	int *ipiv, float *work, int *info);
 
  void ssytrs_(char *uplo, int *n, int *nrhs, float *a, 
	int *lda, int *ipiv, float *b, int *ldb, int *info);
 
  void stbcon_(char *norm, char *uplo, char *diag, int *n, 
	int *kd, float *ab, int *ldab, float *rcond, float *work, 
	int *iwork, int *info);
 
  void stbrfs_(char *uplo, char *trans, char *diag, int *n, 
	int *kd, int *nrhs, float *ab, int *ldab, float *b, int 
	*ldb, float *x, int *ldx, float *ferr, float *berr, float *work, 
	int *iwork, int *info);
 
  void stbtrs_(char *uplo, char *trans, char *diag, int *n, 
	int *kd, int *nrhs, float *ab, int *ldab, float *b, int 
	*ldb, int *info);
 
  void stgevc_(char *side, char *howmny, logical *select, 
	int *n, float *a, int *lda, float *b, int *ldb, float *vl, 
	int *ldvl, float *vr, int *ldvr, int *mm, int *m, float 
	*work, int *info);
 
  void stgex2_(logical *wantq, logical *wantz, int *n, float 
	*a, int *lda, float *b, int *ldb, float *q, int *ldq, float *
	z__, int *ldz, int *j1, int *n1, int *n2, float *work, 
	int *lwork, int *info);
 
  void stgexc_(logical *wantq, logical *wantz, int *n, float 
	*a, int *lda, float *b, int *ldb, float *q, int *ldq, float *
	z__, int *ldz, int *ifst, int *ilst, float *work, int *
	lwork, int *info);
 
  void stgsen_(int *ijob, logical *wantq, logical *wantz, 
	logical *select, int *n, float *a, int *lda, float *b, int *
	ldb, float *alphar, float *alphai, float *beta, float *q, int *ldq, 
	float *z__, int *ldz, int *m, float *pl, float *pr, float *dif, 
	float *work, int *lwork, int *iwork, int *liwork, int *
	info);
 
  void stgsja_(char *jobu, char *jobv, char *jobq, int *m, 
	int *p, int *n, int *k, int *l, float *a, int *lda,
	 float *b, int *ldb, float *tola, float *tolb, float *alpha, float *
	beta, float *u, int *ldu, float *v, int *ldv, float *q, int *
	ldq, float *work, int *ncycle, int *info);
 
  void stgsna_(char *job, char *howmny, logical *select, 
	int *n, float *a, int *lda, float *b, int *ldb, float *vl, 
	int *ldvl, float *vr, int *ldvr, float *s, float *dif, int *
	mm, int *m, float *work, int *lwork, int *iwork, int *
	info);
 
  void stgsy2_(char *trans, int *ijob, int *m, int *
	n, float *a, int *lda, float *b, int *ldb, float *c__, int *
	ldc, float *d__, int *ldd, float *e, int *lde, float *f, int 
	*ldf, float *scale, float *rdsum, float *rdscal, int *iwork, int 
	*pq, int *info);
 
  void stgsyl_(char *trans, int *ijob, int *m, int *
	n, float *a, int *lda, float *b, int *ldb, float *c__, int *
	ldc, float *d__, int *ldd, float *e, int *lde, float *f, int 
	*ldf, float *scale, float *dif, float *work, int *lwork, int *
	iwork, int *info);
 
  void stpcon_(char *norm, char *uplo, char *diag, int *n, 
	float *ap, float *rcond, float *work, int *iwork, int *info);
 
  void stprfs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, float *ap, float *b, int *ldb, float *x, int *ldx,
	 float *ferr, float *berr, float *work, int *iwork, int *info);
 
  void stptri_(char *uplo, char *diag, int *n, float *ap, 
	int *info);
 
  void stptrs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, float *ap, float *b, int *ldb, int *info);
 
  void strcon_(char *norm, char *uplo, char *diag, int *n, 
	float *a, int *lda, float *rcond, float *work, int *iwork, 
	int *info);
 
  void strevc_(char *side, char *howmny, logical *select, 
	int *n, float *t, int *ldt, float *vl, int *ldvl, float *vr, 
	int *ldvr, int *mm, int *m, float *work, int *info);
 
  void strexc_(char *compq, int *n, float *t, int *ldt, 
	float *q, int *ldq, int *ifst, int *ilst, float *work, 
	int *info);
 
  void strrfs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, float *a, int *lda, float *b, int *ldb, float *x, 
	int *ldx, float *ferr, float *berr, float *work, int *iwork, 
	int *info);
 
  void strsen_(char *job, char *compq, logical *select, int 
	*n, float *t, int *ldt, float *q, int *ldq, float *wr, float *wi, 
	int *m, float *s, float *sep, float *work, int *lwork, int *
	iwork, int *liwork, int *info);
 
  void strsna_(char *job, char *howmny, logical *select, 
	int *n, float *t, int *ldt, float *vl, int *ldvl, float *vr, 
	int *ldvr, float *s, float *sep, int *mm, int *m, float *
	work, int *ldwork, int *iwork, int *info);
 
  void strsyl_(char *trana, char *tranb, int *isgn, int 
	*m, int *n, float *a, int *lda, float *b, int *ldb, float *
	c__, int *ldc, float *scale, int *info);
 
  void strti2_(char *uplo, char *diag, int *n, float *a, 
	int *lda, int *info);
 
  void strtri_(char *uplo, char *diag, int *n, float *a, 
	int *lda, int *info);
 
  void strtrs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, float *a, int *lda, float *b, int *ldb, int *
	info);
 
  void stzrqf_(int *m, int *n, float *a, int *lda, 
	float *tau, int *info);
 
  void stzrzf_(int *m, int *n, float *a, int *lda, 
	float *tau, float *work, int *lwork, int *info);
 
  void xerbla_(char *srname, int *info);
 
  void zbdsqr_(char *uplo, int *n, int *ncvt, int *
	nru, int *ncc, double *d__, double *e, Complex!(double) *vt, 
	int *ldvt, Complex!(double) *u, int *ldu, Complex!(double) *c__, 
	int *ldc, double *rwork, int *info);
 
  void zdrot_(int *n, Complex!(double) *cx, int *incx, 
	Complex!(double) *cy, int *incy, double *c__, double *s);
 
  void zdrscl_(int *n, double *sa, Complex!(double) *sx, 
	int *incx);
 
  void zgbbrd_(char *vect, int *m, int *n, int *ncc,
	 int *kl, int *ku, Complex!(double) *ab, int *ldab, 
	double *d__, double *e, Complex!(double) *q, int *ldq, 
	Complex!(double) *pt, int *ldpt, Complex!(double) *c__, int *ldc, 
	Complex!(double) *work, double *rwork, int *info);
 
  void zgbcon_(char *norm, int *n, int *kl, int *ku,
	 Complex!(double) *ab, int *ldab, int *ipiv, double *anorm, 
	double *rcond, Complex!(double) *work, double *rwork, int *
	info);
 
  void zgbequ_(int *m, int *n, int *kl, int *ku,
	 Complex!(double) *ab, int *ldab, double *r__, double *c__, 
	double *rowcnd, double *colcnd, double *amax, int *
	info);
 
  void zgbrfs_(char *trans, int *n, int *kl, int *
	ku, int *nrhs, Complex!(double) *ab, int *ldab, Complex!(double) *
	afb, int *ldafb, int *ipiv, Complex!(double) *b, int *ldb, 
	Complex!(double) *x, int *ldx, double *ferr, double *berr, 
	Complex!(double) *work, double *rwork, int *info);
 
  void zgbsv_(int *n, int *kl, int *ku, int *
	nrhs, Complex!(double) *ab, int *ldab, int *ipiv, Complex!(double) *
	b, int *ldb, int *info);
 
  void zgbsvx_(char *fact, char *trans, int *n, int *kl,
	 int *ku, int *nrhs, Complex!(double) *ab, int *ldab, 
	Complex!(double) *afb, int *ldafb, int *ipiv, char *equed, 
	double *r__, double *c__, Complex!(double) *b, int *ldb, 
	Complex!(double) *x, int *ldx, double *rcond, double *ferr, 
	double *berr, Complex!(double) *work, double *rwork, int *
	info);
 
  void zgbtf2_(int *m, int *n, int *kl, int *ku,
	 Complex!(double) *ab, int *ldab, int *ipiv, int *info);
 
  void zgbtrf_(int *m, int *n, int *kl, int *ku,
	 Complex!(double) *ab, int *ldab, int *ipiv, int *info);
 
  void zgbtrs_(char *trans, int *n, int *kl, int *
	ku, int *nrhs, Complex!(double) *ab, int *ldab, int *ipiv, 
	Complex!(double) *b, int *ldb, int *info);
 
  void zgebak_(char *job, char *side, int *n, int *ilo, 
	int *ihi, double *scale, int *m, Complex!(double) *v, 
	int *ldv, int *info);
 
  void zgebal_(char *job, int *n, Complex!(double) *a, int 
	*lda, int *ilo, int *ihi, double *scale, int *info);
 
  void zgebd2_(int *m, int *n, Complex!(double) *a, 
	int *lda, double *d__, double *e, Complex!(double) *tauq, 
	Complex!(double) *taup, Complex!(double) *work, int *info);
 
  void zgebrd_(int *m, int *n, Complex!(double) *a, 
	int *lda, double *d__, double *e, Complex!(double) *tauq, 
	Complex!(double) *taup, Complex!(double) *work, int *lwork, int *
	info);
 
  void zgecon_(char *norm, int *n, Complex!(double) *a, 
	int *lda, double *anorm, double *rcond, Complex!(double) *
	work, double *rwork, int *info);
 
  void zgeequ_(int *m, int *n, Complex!(double) *a, 
	int *lda, double *r__, double *c__, double *rowcnd, 
	double *colcnd, double *amax, int *info);
 
  void zgees_(char *jobvs, char *sort, L_fp select, int *n, 
	Complex!(double) *a, int *lda, int *sdim, Complex!(double) *w, 
	Complex!(double) *vs, int *ldvs, Complex!(double) *work, int *lwork,
	 double *rwork, logical *bwork, int *info);
 
  void zgeesx_(char *jobvs, char *sort, L_fp select, char *
	sense, int *n, Complex!(double) *a, int *lda, int *sdim, 
	Complex!(double) *w, Complex!(double) *vs, int *ldvs, double *
	rconde, double *rcondv, Complex!(double) *work, int *lwork, 
	double *rwork, logical *bwork, int *info);
 
  void zgeev_(char *jobvl, char *jobvr, int *n, 
	Complex!(double) *a, int *lda, Complex!(double) *w, Complex!(double) *vl, 
	int *ldvl, Complex!(double) *vr, int *ldvr, Complex!(double) *work, 
	int *lwork, double *rwork, int *info);
 
  void zgeevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, int *n, Complex!(double) *a, int *lda, Complex!(double) *w, 
	Complex!(double) *vl, int *ldvl, Complex!(double) *vr, int *ldvr, 
	int *ilo, int *ihi, double *scale, double *abnrm, 
	double *rconde, double *rcondv, Complex!(double) *work, int *
	lwork, double *rwork, int *info);
 
  void zgegs_(char *jobvsl, char *jobvsr, int *n, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	Complex!(double) *alpha, Complex!(double) *beta, Complex!(double) *vsl, 
	int *ldvsl, Complex!(double) *vsr, int *ldvsr, Complex!(double) *
	work, int *lwork, double *rwork, int *info);
 
  void zgegv_(char *jobvl, char *jobvr, int *n, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	Complex!(double) *alpha, Complex!(double) *beta, Complex!(double) *vl, int 
	*ldvl, Complex!(double) *vr, int *ldvr, Complex!(double) *work, int 
	*lwork, double *rwork, int *info);
 
  void zgehd2_(int *n, int *ilo, int *ihi, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *info);
 
  void zgehrd_(int *n, int *ilo, int *ihi, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *lwork, int *info);
 
  void zgelq2_(int *m, int *n, Complex!(double) *a, 
	int *lda, Complex!(double) *tau, Complex!(double) *work, int *info);
 
  void zgelqf_(int *m, int *n, Complex!(double) *a, 
	int *lda, Complex!(double) *tau, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zgels_(char *trans, int *m, int *n, int *
	nrhs, Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	Complex!(double) *work, int *lwork, int *info);
 
  void zgelsx_(int *m, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	int *jpvt, double *rcond, int *rank, Complex!(double) *work, 
	double *rwork, int *info);
 
  void zgelsy_(int *m, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	int *jpvt, double *rcond, int *rank, Complex!(double) *work, 
	int *lwork, double *rwork, int *info);
 
  void zgeql2_(int *m, int *n, Complex!(double) *a, 
	int *lda, Complex!(double) *tau, Complex!(double) *work, int *info);
 
  void zgeqlf_(int *m, int *n, Complex!(double) *a, 
	int *lda, Complex!(double) *tau, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zgeqp3_(int *m, int *n, Complex!(double) *a, 
	int *lda, int *jpvt, Complex!(double) *tau, Complex!(double) *work, 
	int *lwork, double *rwork, int *info);
 
  void zgeqpf_(int *m, int *n, Complex!(double) *a, 
	int *lda, int *jpvt, Complex!(double) *tau, Complex!(double) *work, 
	double *rwork, int *info);
 
  void zgeqr2_(int *m, int *n, Complex!(double) *a, 
	int *lda, Complex!(double) *tau, Complex!(double) *work, int *info);
 
  void zgeqrf_(int *m, int *n, Complex!(double) *a, 
	int *lda, Complex!(double) *tau, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zgerfs_(char *trans, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, Complex!(double) *af, int *ldaf, 
	int *ipiv, Complex!(double) *b, int *ldb, Complex!(double) *x, 
	int *ldx, double *ferr, double *berr, Complex!(double) *work,
	 double *rwork, int *info);
 
  void zgerq2_(int *m, int *n, Complex!(double) *a, 
	int *lda, Complex!(double) *tau, Complex!(double) *work, int *info);
 
  void zgerqf_(int *m, int *n, Complex!(double) *a, 
	int *lda, Complex!(double) *tau, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zgesc2_(int *n, Complex!(double) *a, int *lda, 
	Complex!(double) *rhs, int *ipiv, int *jpiv, double *scale);
 
  void zgesv_(int *n, int *nrhs, Complex!(double) *a, 
	int *lda, int *ipiv, Complex!(double) *b, int *ldb, int *
	info);
 
  void zgesvx_(char *fact, char *trans, int *n, int *
	nrhs, Complex!(double) *a, int *lda, Complex!(double) *af, int *
	ldaf, int *ipiv, char *equed, double *r__, double *c__, 
	Complex!(double) *b, int *ldb, Complex!(double) *x, int *ldx, 
	double *rcond, double *ferr, double *berr, Complex!(double) *
	work, double *rwork, int *info);
 
  void zgetc2_(int *n, Complex!(double) *a, int *lda, 
	int *ipiv, int *jpiv, int *info);
 
  void zgetf2_(int *m, int *n, Complex!(double) *a, 
	int *lda, int *ipiv, int *info);
 
  void zgetrf_(int *m, int *n, Complex!(double) *a, 
	int *lda, int *ipiv, int *info);
 
  void zgetri_(int *n, Complex!(double) *a, int *lda, 
	int *ipiv, Complex!(double) *work, int *lwork, int *info);
 
  void zgetrs_(char *trans, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, int *ipiv, Complex!(double) *b, 
	int *ldb, int *info);
 
  void zggbak_(char *job, char *side, int *n, int *ilo, 
	int *ihi, double *lscale, double *rscale, int *m, 
	Complex!(double) *v, int *ldv, int *info);
 
  void zggbal_(char *job, int *n, Complex!(double) *a, int 
	*lda, Complex!(double) *b, int *ldb, int *ilo, int *ihi, 
	double *lscale, double *rscale, double *work, int *
	info);
 
  void zgges_(char *jobvsl, char *jobvsr, char *sort, L_fp 
	delctg, int *n, Complex!(double) *a, int *lda, Complex!(double) *b, 
	int *ldb, int *sdim, Complex!(double) *alpha, Complex!(double) *
	beta, Complex!(double) *vsl, int *ldvsl, Complex!(double) *vsr, int 
	*ldvsr, Complex!(double) *work, int *lwork, double *rwork, 
	logical *bwork, int *info);
 
  void zggesx_(char *jobvsl, char *jobvsr, char *sort, L_fp 
	delctg, char *sense, int *n, Complex!(double) *a, int *lda, 
	Complex!(double) *b, int *ldb, int *sdim, Complex!(double) *alpha, 
	Complex!(double) *beta, Complex!(double) *vsl, int *ldvsl, 
	Complex!(double) *vsr, int *ldvsr, double *rconde, double *
	rcondv, Complex!(double) *work, int *lwork, double *rwork, 
	int *iwork, int *liwork, logical *bwork, int *info);
 
  void zggev_(char *jobvl, char *jobvr, int *n, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	Complex!(double) *alpha, Complex!(double) *beta, Complex!(double) *vl, int 
	*ldvl, Complex!(double) *vr, int *ldvr, Complex!(double) *work, int 
	*lwork, double *rwork, int *info);
 
  void zggevx_(char *balanc, char *jobvl, char *jobvr, char *
	sense, int *n, Complex!(double) *a, int *lda, Complex!(double) *b, 
	int *ldb, Complex!(double) *alpha, Complex!(double) *beta, 
	Complex!(double) *vl, int *ldvl, Complex!(double) *vr, int *ldvr, 
	int *ilo, int *ihi, double *lscale, double *rscale, 
	double *abnrm, double *bbnrm, double *rconde, double *
	rcondv, Complex!(double) *work, int *lwork, double *rwork, 
	int *iwork, logical *bwork, int *info);
 
  void zggglm_(int *n, int *m, int *p, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	Complex!(double) *d__, Complex!(double) *x, Complex!(double) *y, Complex!(double) 
	*work, int *lwork, int *info);
 
  void zgghrd_(char *compq, char *compz, int *n, int *
	ilo, int *ihi, Complex!(double) *a, int *lda, Complex!(double) *b, 
	int *ldb, Complex!(double) *q, int *ldq, Complex!(double) *z__, 
	int *ldz, int *info);
 
  void zgglse_(int *m, int *n, int *p, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	Complex!(double) *c__, Complex!(double) *d__, Complex!(double) *x, 
	Complex!(double) *work, int *lwork, int *info);
 
  void zggqrf_(int *n, int *m, int *p, 
	Complex!(double) *a, int *lda, Complex!(double) *taua, Complex!(double) *b,
	 int *ldb, Complex!(double) *taub, Complex!(double) *work, int *
	lwork, int *info);
 
  void zggrqf_(int *m, int *p, int *n, 
	Complex!(double) *a, int *lda, Complex!(double) *taua, Complex!(double) *b,
	 int *ldb, Complex!(double) *taub, Complex!(double) *work, int *
	lwork, int *info);
 
  void zggsvd_(char *jobu, char *jobv, char *jobq, int *m, 
	int *n, int *p, int *k, int *l, Complex!(double) *a, 
	int *lda, Complex!(double) *b, int *ldb, double *alpha, 
	double *beta, Complex!(double) *u, int *ldu, Complex!(double) *v, 
	int *ldv, Complex!(double) *q, int *ldq, Complex!(double) *work, 
	double *rwork, int *iwork, int *info);
 
  void zggsvp_(char *jobu, char *jobv, char *jobq, int *m, 
	int *p, int *n, Complex!(double) *a, int *lda, Complex!(double) 
	*b, int *ldb, double *tola, double *tolb, int *k, 
	int *l, Complex!(double) *u, int *ldu, Complex!(double) *v, int 
	*ldv, Complex!(double) *q, int *ldq, int *iwork, double *
	rwork, Complex!(double) *tau, Complex!(double) *work, int *info);
 
  void zgtcon_(char *norm, int *n, Complex!(double) *dl, 
	Complex!(double) *d__, Complex!(double) *du, Complex!(double) *du2, int *
	ipiv, double *anorm, double *rcond, Complex!(double) *work, 
	int *info);
 
  void zgtrfs_(char *trans, int *n, int *nrhs, 
	Complex!(double) *dl, Complex!(double) *d__, Complex!(double) *du, 
	Complex!(double) *dlf, Complex!(double) *df, Complex!(double) *duf, 
	Complex!(double) *du2, int *ipiv, Complex!(double) *b, int *ldb, 
	Complex!(double) *x, int *ldx, double *ferr, double *berr, 
	Complex!(double) *work, double *rwork, int *info);
 
  void zgtsv_(int *n, int *nrhs, Complex!(double) *dl, 
	Complex!(double) *d__, Complex!(double) *du, Complex!(double) *b, int *ldb,
	 int *info);
 
  void zgtsvx_(char *fact, char *trans, int *n, int *
	nrhs, Complex!(double) *dl, Complex!(double) *d__, Complex!(double) *du, 
	Complex!(double) *dlf, Complex!(double) *df, Complex!(double) *duf, 
	Complex!(double) *du2, int *ipiv, Complex!(double) *b, int *ldb, 
	Complex!(double) *x, int *ldx, double *rcond, double *ferr, 
	double *berr, Complex!(double) *work, double *rwork, int *
	info);
 
  void zgttrf_(int *n, Complex!(double) *dl, Complex!(double) *
	d__, Complex!(double) *du, Complex!(double) *du2, int *ipiv, int *
	info);
 
  void zgttrs_(char *trans, int *n, int *nrhs, 
	Complex!(double) *dl, Complex!(double) *d__, Complex!(double) *du, 
	Complex!(double) *du2, int *ipiv, Complex!(double) *b, int *ldb, 
	int *info);
 
  void zgtts2_(int *itrans, int *n, int *nrhs, 
	Complex!(double) *dl, Complex!(double) *d__, Complex!(double) *du, 
	Complex!(double) *du2, int *ipiv, Complex!(double) *b, int *ldb);
 
  void zhbev_(char *jobz, char *uplo, int *n, int *kd, 
	Complex!(double) *ab, int *ldab, double *w, Complex!(double) *z__, 
	int *ldz, Complex!(double) *work, double *rwork, int *info);
 
  void zhbevd_(char *jobz, char *uplo, int *n, int *kd, 
	Complex!(double) *ab, int *ldab, double *w, Complex!(double) *z__, 
	int *ldz, Complex!(double) *work, int *lwork, double *rwork, 
	int *lrwork, int *iwork, int *liwork, int *info);
 
  void zhbevx_(char *jobz, char *range, char *uplo, int *n, 
	int *kd, Complex!(double) *ab, int *ldab, Complex!(double) *q, 
	int *ldq, double *vl, double *vu, int *il, int *
	iu, double *abstol, int *m, double *w, Complex!(double) *z__,
	 int *ldz, Complex!(double) *work, double *rwork, int *iwork,
	 int *ifail, int *info);
 
  void zhbgst_(char *vect, char *uplo, int *n, int *ka, 
	int *kb, Complex!(double) *ab, int *ldab, Complex!(double) *bb, 
	int *ldbb, Complex!(double) *x, int *ldx, Complex!(double) *work, 
	double *rwork, int *info);
 
  void zhbgv_(char *jobz, char *uplo, int *n, int *ka, 
	int *kb, Complex!(double) *ab, int *ldab, Complex!(double) *bb, 
	int *ldbb, double *w, Complex!(double) *z__, int *ldz, 
	Complex!(double) *work, double *rwork, int *info);
 
  void zhbgvx_(char *jobz, char *range, char *uplo, int *n, 
	int *ka, int *kb, Complex!(double) *ab, int *ldab, 
	Complex!(double) *bb, int *ldbb, Complex!(double) *q, int *ldq, 
	double *vl, double *vu, int *il, int *iu, double *
	abstol, int *m, double *w, Complex!(double) *z__, int *ldz, 
	Complex!(double) *work, double *rwork, int *iwork, int *
	ifail, int *info);
 
  void zhbtrd_(char *vect, char *uplo, int *n, int *kd, 
	Complex!(double) *ab, int *ldab, double *d__, double *e, 
	Complex!(double) *q, int *ldq, Complex!(double) *work, int *info);
 
  void zhecon_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *ipiv, double *anorm, double *rcond, 
	Complex!(double) *work, int *info);
 
  void zheev_(char *jobz, char *uplo, int *n, Complex!(double) 
	*a, int *lda, double *w, Complex!(double) *work, int *lwork, 
	double *rwork, int *info);
 
  void zheevd_(char *jobz, char *uplo, int *n, 
	Complex!(double) *a, int *lda, double *w, Complex!(double) *work, 
	int *lwork, double *rwork, int *lrwork, int *iwork, 
	int *liwork, int *info);
 
  void zheevr_(char *jobz, char *range, char *uplo, int *n, 
	Complex!(double) *a, int *lda, double *vl, double *vu, 
	int *il, int *iu, double *abstol, int *m, double *
	w, Complex!(double) *z__, int *ldz, int *isuppz, Complex!(double) *
	work, int *lwork, double *rwork, int *lrwork, int *
	iwork, int *liwork, int *info);
 
  void zheevx_(char *jobz, char *range, char *uplo, int *n, 
	Complex!(double) *a, int *lda, double *vl, double *vu, 
	int *il, int *iu, double *abstol, int *m, double *
	w, Complex!(double) *z__, int *ldz, Complex!(double) *work, int *
	lwork, double *rwork, int *iwork, int *ifail, int *
	info);
 
  void zhegs2_(int *itype, char *uplo, int *n, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	int *info);
 
  void zhegst_(int *itype, char *uplo, int *n, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	int *info);
 
  void zhegv_(int *itype, char *jobz, char *uplo, int *
	n, Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	double *w, Complex!(double) *work, int *lwork, double *rwork,
	 int *info);
 
  void zhegvd_(int *itype, char *jobz, char *uplo, int *
	n, Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	double *w, Complex!(double) *work, int *lwork, double *rwork,
	 int *lrwork, int *iwork, int *liwork, int *info);
 
  void zhegvx_(int *itype, char *jobz, char *range, char *
	uplo, int *n, Complex!(double) *a, int *lda, Complex!(double) *b, 
	int *ldb, double *vl, double *vu, int *il, int *
	iu, double *abstol, int *m, double *w, Complex!(double) *z__,
	 int *ldz, Complex!(double) *work, int *lwork, double *rwork,
	 int *iwork, int *ifail, int *info);
 
  void zherfs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, Complex!(double) *af, int *ldaf, 
	int *ipiv, Complex!(double) *b, int *ldb, Complex!(double) *x, 
	int *ldx, double *ferr, double *berr, Complex!(double) *work,
	 double *rwork, int *info);
 
  void zhesv_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, int *ipiv, Complex!(double) *b, 
	int *ldb, Complex!(double) *work, int *lwork, int *info);
 
  void zhesvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(double) *a, int *lda, Complex!(double) *af, int *
	ldaf, int *ipiv, Complex!(double) *b, int *ldb, Complex!(double) *x,
	 int *ldx, double *rcond, double *ferr, double *berr, 
	Complex!(double) *work, int *lwork, double *rwork, int *info);
 
  void zhetf2_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *ipiv, int *info);
 
  void zhetrd_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, double *d__, double *e, Complex!(double) *tau, 
	Complex!(double) *work, int *lwork, int *info);
 
  void zhetrf_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *ipiv, Complex!(double) *work, int *lwork, 
	int *info);
 
  void zhetri_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *ipiv, Complex!(double) *work, int *info);
 
  void zhetrs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, int *ipiv, Complex!(double) *b, 
	int *ldb, int *info);
 
  void zhgeqz_(char *job, char *compq, char *compz, int *n, 
	int *ilo, int *ihi, Complex!(double) *a, int *lda, 
	Complex!(double) *b, int *ldb, Complex!(double) *alpha, Complex!(double) *
	beta, Complex!(double) *q, int *ldq, Complex!(double) *z__, int *
	ldz, Complex!(double) *work, int *lwork, double *rwork, int *
	info);
 
  void zhpcon_(char *uplo, int *n, Complex!(double) *ap, 
	int *ipiv, double *anorm, double *rcond, Complex!(double) *
	work, int *info);
 
  void zhpev_(char *jobz, char *uplo, int *n, Complex!(double) 
	*ap, double *w, Complex!(double) *z__, int *ldz, Complex!(double) *
	work, double *rwork, int *info);
 
  void zhpevd_(char *jobz, char *uplo, int *n, 
	Complex!(double) *ap, double *w, Complex!(double) *z__, int *ldz, 
	Complex!(double) *work, int *lwork, double *rwork, int *
	lrwork, int *iwork, int *liwork, int *info);
 
  void zhpevx_(char *jobz, char *range, char *uplo, int *n, 
	Complex!(double) *ap, double *vl, double *vu, int *il, 
	int *iu, double *abstol, int *m, double *w, 
	Complex!(double) *z__, int *ldz, Complex!(double) *work, double *
	rwork, int *iwork, int *ifail, int *info);
 
  void zhpgst_(int *itype, char *uplo, int *n, 
	Complex!(double) *ap, Complex!(double) *bp, int *info);
 
  void zhpgv_(int *itype, char *jobz, char *uplo, int *
	n, Complex!(double) *ap, Complex!(double) *bp, double *w, Complex!(double) 
	*z__, int *ldz, Complex!(double) *work, double *rwork, int *
	info);
 
  void zhpgvd_(int *itype, char *jobz, char *uplo, int *
	n, Complex!(double) *ap, Complex!(double) *bp, double *w, Complex!(double) 
	*z__, int *ldz, Complex!(double) *work, int *lwork, double *
	rwork, int *lrwork, int *iwork, int *liwork, int *
	info);
 
  void zhpgvx_(int *itype, char *jobz, char *range, char *
	uplo, int *n, Complex!(double) *ap, Complex!(double) *bp, double *
	vl, double *vu, int *il, int *iu, double *abstol, 
	int *m, double *w, Complex!(double) *z__, int *ldz, 
	Complex!(double) *work, double *rwork, int *iwork, int *
	ifail, int *info);
 
  void zhprfs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *ap, Complex!(double) *afp, int *ipiv, Complex!(double) *
	b, int *ldb, Complex!(double) *x, int *ldx, double *ferr, 
	double *berr, Complex!(double) *work, double *rwork, int *
	info);
 
  void zhpsv_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *ap, int *ipiv, Complex!(double) *b, int *ldb, 
	int *info);
 
  void zhpsvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(double) *ap, Complex!(double) *afp, int *ipiv, 
	Complex!(double) *b, int *ldb, Complex!(double) *x, int *ldx, 
	double *rcond, double *ferr, double *berr, Complex!(double) *
	work, double *rwork, int *info);
 
  void zhptrd_(char *uplo, int *n, Complex!(double) *ap, 
	double *d__, double *e, Complex!(double) *tau, int *info);
 
  void zhptrf_(char *uplo, int *n, Complex!(double) *ap, 
	int *ipiv, int *info);
 
  void zhptri_(char *uplo, int *n, Complex!(double) *ap, 
	int *ipiv, Complex!(double) *work, int *info);
 
  void zhptrs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *ap, int *ipiv, Complex!(double) *b, int *ldb, 
	int *info);
 
  void zhsein_(char *side, char *eigsrc, char *initv, logical *
	select, int *n, Complex!(double) *h__, int *ldh, Complex!(double) *
	w, Complex!(double) *vl, int *ldvl, Complex!(double) *vr, int *ldvr,
	 int *mm, int *m, Complex!(double) *work, double *rwork, 
	int *ifaill, int *ifailr, int *info);
 
  void zhseqr_(char *job, char *compz, int *n, int *ilo,
	 int *ihi, Complex!(double) *h__, int *ldh, Complex!(double) *w, 
	Complex!(double) *z__, int *ldz, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zlabrd_(int *m, int *n, int *nb, 
	Complex!(double) *a, int *lda, double *d__, double *e, 
	Complex!(double) *tauq, Complex!(double) *taup, Complex!(double) *x, int *
	ldx, Complex!(double) *y, int *ldy);
 
  void zlacgv_(int *n, Complex!(double) *x, int *incx);
 
  void zlacon_(int *n, Complex!(double) *v, Complex!(double) *x, 
	double *est, int *kase);
 
  void zlacp2_(char *uplo, int *m, int *n, double *
	a, int *lda, Complex!(double) *b, int *ldb);
 
  void zlacpy_(char *uplo, int *m, int *n, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb);
 
  void zlacrm_(int *m, int *n, Complex!(double) *a, 
	int *lda, double *b, int *ldb, Complex!(double) *c__, 
	int *ldc, double *rwork);
 
  void zlacrt_(int *n, Complex!(double) *cx, int *incx, 
	Complex!(double) *cy, int *incy, Complex!(double) *c__, Complex!(double) *
	s);
 
  void zlaed0_(int *qsiz, int *n, double *d__, 
	double *e, Complex!(double) *q, int *ldq, Complex!(double) *qstore, 
	int *ldqs, double *rwork, int *iwork, int *info);
 
  void zlaed7_(int *n, int *cutpnt, int *qsiz, 
	int *tlvls, int *curlvl, int *curpbm, double *d__, 
	Complex!(double) *q, int *ldq, double *rho, int *indxq, 
	double *qstore, int *qptr, int *prmptr, int *perm, 
	int *givptr, int *givcol, double *givnum, Complex!(double) *
	work, double *rwork, int *iwork, int *info);
 
  void zlaed8_(int *k, int *n, int *qsiz, 
	Complex!(double) *q, int *ldq, double *d__, double *rho, 
	int *cutpnt, double *z__, double *dlamda, Complex!(double) *
	q2, int *ldq2, double *w, int *indxp, int *indx, 
	int *indxq, int *perm, int *givptr, int *givcol, 
	double *givnum, int *info);
 
  void zlaein_(logical *rightv, logical *noinit, int *n, 
	Complex!(double) *h__, int *ldh, Complex!(double) *w, Complex!(double) *v, 
	Complex!(double) *b, int *ldb, double *rwork, double *eps3, 
	double *smlnum, int *info);
 
  void zlaesy_(Complex!(double) *a, Complex!(double) *b, 
	Complex!(double) *c__, Complex!(double) *rt1, Complex!(double) *rt2, 
	Complex!(double) *evscal, Complex!(double) *cs1, Complex!(double) *sn1);
 
  void zlaev2_(Complex!(double) *a, Complex!(double) *b, 
	Complex!(double) *c__, double *rt1, double *rt2, double *cs1,
	 Complex!(double) *sn1);
 
  void zlags2_(logical *upper, double *a1, Complex!(double) *
	a2, double *a3, double *b1, Complex!(double) *b2, double *b3,
	 double *csu, Complex!(double) *snu, double *csv, Complex!(double) *
	snv, double *csq, Complex!(double) *snq);
 
  void zlagtm_(char *trans, int *n, int *nrhs, 
	double *alpha, Complex!(double) *dl, Complex!(double) *d__, 
	Complex!(double) *du, Complex!(double) *x, int *ldx, double *beta, 
	Complex!(double) *b, int *ldb);
 
  void zlahef_(char *uplo, int *n, int *nb, int *kb,
	 Complex!(double) *a, int *lda, int *ipiv, Complex!(double) *w, 
	int *ldw, int *info);
 
  void zlahqr_(logical *wantt, logical *wantz, int *n, 
	int *ilo, int *ihi, Complex!(double) *h__, int *ldh, 
	Complex!(double) *w, int *iloz, int *ihiz, Complex!(double) *z__, 
	int *ldz, int *info);
 
  void zlahrd_(int *n, int *k, int *nb, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *t, 
	int *ldt, Complex!(double) *y, int *ldy);
 
  void zlaic1_(int *job, int *j, Complex!(double) *x, 
	double *sest, Complex!(double) *w, Complex!(double) *gamma, double *
	sestpr, Complex!(double) *s, Complex!(double) *c__);
 
  void zlals0_(int *icompq, int *nl, int *nr, 
	int *sqre, int *nrhs, Complex!(double) *b, int *ldb, 
	Complex!(double) *bx, int *ldbx, int *perm, int *givptr, 
	int *givcol, int *ldgcol, double *givnum, int *ldgnum,
	 double *poles, double *difl, double *difr, double *
	z__, int *k, double *c__, double *s, double *rwork, 
	int *info);
 
  void zlalsa_(int *icompq, int *smlsiz, int *n, 
	int *nrhs, Complex!(double) *b, int *ldb, Complex!(double) *bx, 
	int *ldbx, double *u, int *ldu, double *vt, int *
	k, double *difl, double *difr, double *z__, double *
	poles, int *givptr, int *givcol, int *ldgcol, int *
	perm, double *givnum, double *c__, double *s, double *
	rwork, int *iwork, int *info);
 
  void zlapll_(int *n, Complex!(double) *x, int *incx, 
	Complex!(double) *y, int *incy, double *ssmin);
 
  void zlapmt_(logical *forwrd, int *m, int *n, 
	Complex!(double) *x, int *ldx, int *k);
 
  void zlaqgb_(int *m, int *n, int *kl, int *ku,
	 Complex!(double) *ab, int *ldab, double *r__, double *c__, 
	double *rowcnd, double *colcnd, double *amax, char *equed);
 
  void zlaqge_(int *m, int *n, Complex!(double) *a, 
	int *lda, double *r__, double *c__, double *rowcnd, 
	double *colcnd, double *amax, char *equed);
 
  void zlaqhb_(char *uplo, int *n, int *kd, 
	Complex!(double) *ab, int *ldab, double *s, double *scond, 
	double *amax, char *equed);
 
  void zlaqhe_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, double *s, double *scond, double *amax, 
	char *equed);
 
  void zlaqhp_(char *uplo, int *n, Complex!(double) *ap, 
	double *s, double *scond, double *amax, char *equed);
 
  void zlaqp2_(int *m, int *n, int *offset, 
	Complex!(double) *a, int *lda, int *jpvt, Complex!(double) *tau, 
	double *vn1, double *vn2, Complex!(double) *work);
 
  void zlaqps_(int *m, int *n, int *offset, int 
	*nb, int *kb, Complex!(double) *a, int *lda, int *jpvt, 
	Complex!(double) *tau, double *vn1, double *vn2, Complex!(double) *
	auxv, Complex!(double) *f, int *ldf);
 
  void zlaqsb_(char *uplo, int *n, int *kd, 
	Complex!(double) *ab, int *ldab, double *s, double *scond, 
	double *amax, char *equed);
 
  void zlaqsp_(char *uplo, int *n, Complex!(double) *ap, 
	double *s, double *scond, double *amax, char *equed);
 
  void zlaqsy_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, double *s, double *scond, double *amax, 
	char *equed);
 
  void zlar1v_(int *n, int *b1, int *bn, double 
	*sigma, double *d__, double *l, double *ld, double *
	lld, double *gersch, Complex!(double) *z__, double *ztz, 
	double *mingma, int *r__, int *isuppz, double *work);
 
  void zlar2v_(int *n, Complex!(double) *x, Complex!(double) *y, 
	Complex!(double) *z__, int *incx, double *c__, Complex!(double) *s, 
	int *incc);
 
  void zlarcm_(int *m, int *n, double *a, int *
	lda, Complex!(double) *b, int *ldb, Complex!(double) *c__, int *ldc,
	 double *rwork);
 
  void zlarf_(char *side, int *m, int *n, Complex!(double) 
	*v, int *incv, Complex!(double) *tau, Complex!(double) *c__, int *
	ldc, Complex!(double) *work);
 
  void zlarfb_(char *side, char *trans, char *direct, char *
	storev, int *m, int *n, int *k, Complex!(double) *v, int 
	*ldv, Complex!(double) *t, int *ldt, Complex!(double) *c__, int *
	ldc, Complex!(double) *work, int *ldwork);
 
  void zlarfg_(int *n, Complex!(double) *alpha, Complex!(double) *
	x, int *incx, Complex!(double) *tau);
 
  void zlarft_(char *direct, char *storev, int *n, int *
	k, Complex!(double) *v, int *ldv, Complex!(double) *tau, Complex!(double) *
	t, int *ldt);
 
  void zlarfx_(char *side, int *m, int *n, 
	Complex!(double) *v, Complex!(double) *tau, Complex!(double) *c__, int *
	ldc, Complex!(double) *work);
 
  void zlargv_(int *n, Complex!(double) *x, int *incx, 
	Complex!(double) *y, int *incy, double *c__, int *incc);
 
  void zlarnv_(int *idist, int *iseed, int *n, 
	Complex!(double) *x);
 
  void zlarrv_(int *n, double *d__, double *l, 
	int *isplit, int *m, double *w, int *iblock, 
	double *gersch, double *tol, Complex!(double) *z__, int *ldz,
	 int *isuppz, double *work, int *iwork, int *info);
 
  void zlartg_(Complex!(double) *f, Complex!(double) *g, double *
	cs, Complex!(double) *sn, Complex!(double) *r__);
 
  void zlartv_(int *n, Complex!(double) *x, int *incx, 
	Complex!(double) *y, int *incy, double *c__, Complex!(double) *s, 
	int *incc);
 
  void zlarz_(char *side, int *m, int *n, int *l, 
	Complex!(double) *v, int *incv, Complex!(double) *tau, Complex!(double) *
	c__, int *ldc, Complex!(double) *work);
 
  void zlarzb_(char *side, char *trans, char *direct, char *
	storev, int *m, int *n, int *k, int *l, Complex!(double) 
	*v, int *ldv, Complex!(double) *t, int *ldt, Complex!(double) *c__, 
	int *ldc, Complex!(double) *work, int *ldwork);
 
  void zlarzt_(char *direct, char *storev, int *n, int *
	k, Complex!(double) *v, int *ldv, Complex!(double) *tau, Complex!(double) *
	t, int *ldt);
 
  void zlascl_(char *type__, int *kl, int *ku, 
	double *cfrom, double *cto, int *m, int *n, 
	Complex!(double) *a, int *lda, int *info);
 
  void zlaset_(char *uplo, int *m, int *n, 
	Complex!(double) *alpha, Complex!(double) *beta, Complex!(double) *a, int *
	lda);
 
  void zlasr_(char *side, char *pivot, char *direct, int *m,
	 int *n, double *c__, double *s, Complex!(double) *a, 
	int *lda);
 
  void zlassq_(int *n, Complex!(double) *x, int *incx, 
	double *scale, double *sumsq);
 
  void zlaswp_(int *n, Complex!(double) *a, int *lda, 
	int *k1, int *k2, int *ipiv, int *incx);
 
  void zlasyf_(char *uplo, int *n, int *nb, int *kb,
	 Complex!(double) *a, int *lda, int *ipiv, Complex!(double) *w, 
	int *ldw, int *info);
 
  void zlatbs_(char *uplo, char *trans, char *diag, char *
	normin, int *n, int *kd, Complex!(double) *ab, int *ldab, 
	Complex!(double) *x, double *scale, double *cnorm, int *info);
 
  void zlatdf_(int *ijob, int *n, Complex!(double) *z__, 
	int *ldz, Complex!(double) *rhs, double *rdsum, double *
	rdscal, int *ipiv, int *jpiv);
 
  void zlatps_(char *uplo, char *trans, char *diag, char *
	normin, int *n, Complex!(double) *ap, Complex!(double) *x, double *
	scale, double *cnorm, int *info);
 
  void zlatrd_(char *uplo, int *n, int *nb, 
	Complex!(double) *a, int *lda, double *e, Complex!(double) *tau, 
	Complex!(double) *w, int *ldw);
 
  void zlatrs_(char *uplo, char *trans, char *diag, char *
	normin, int *n, Complex!(double) *a, int *lda, Complex!(double) *x, 
	double *scale, double *cnorm, int *info);
 
  void zlatrz_(int *m, int *n, int *l, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work);
 
  void zlatzm_(char *side, int *m, int *n, 
	Complex!(double) *v, int *incv, Complex!(double) *tau, Complex!(double) *
	c1, Complex!(double) *c2, int *ldc, Complex!(double) *work);
 
  void zlauu2_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *info);
 
  void zlauum_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *info);
 
  void zpbcon_(char *uplo, int *n, int *kd, 
	Complex!(double) *ab, int *ldab, double *anorm, double *
	rcond, Complex!(double) *work, double *rwork, int *info);
 
  void zpbequ_(char *uplo, int *n, int *kd, 
	Complex!(double) *ab, int *ldab, double *s, double *scond, 
	double *amax, int *info);
 
  void zpbrfs_(char *uplo, int *n, int *kd, int *
	nrhs, Complex!(double) *ab, int *ldab, Complex!(double) *afb, int *
	ldafb, Complex!(double) *b, int *ldb, Complex!(double) *x, int *ldx,
	 double *ferr, double *berr, Complex!(double) *work, double *
	rwork, int *info);
 
  void zpbstf_(char *uplo, int *n, int *kd, 
	Complex!(double) *ab, int *ldab, int *info);
 
  void zpbsv_(char *uplo, int *n, int *kd, int *
	nrhs, Complex!(double) *ab, int *ldab, Complex!(double) *b, int *
	ldb, int *info);
 
  void zpbsvx_(char *fact, char *uplo, int *n, int *kd, 
	int *nrhs, Complex!(double) *ab, int *ldab, Complex!(double) *afb, 
	int *ldafb, char *equed, double *s, Complex!(double) *b, int 
	*ldb, Complex!(double) *x, int *ldx, double *rcond, double *
	ferr, double *berr, Complex!(double) *work, double *rwork, 
	int *info);
 
  void zpbtf2_(char *uplo, int *n, int *kd, 
	Complex!(double) *ab, int *ldab, int *info);
 
  void zpbtrf_(char *uplo, int *n, int *kd, 
	Complex!(double) *ab, int *ldab, int *info);
 
  void zpbtrs_(char *uplo, int *n, int *kd, int *
	nrhs, Complex!(double) *ab, int *ldab, Complex!(double) *b, int *
	ldb, int *info);
 
  void zpocon_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, double *anorm, double *rcond, Complex!(double) *
	work, double *rwork, int *info);
 
  void zpoequ_(int *n, Complex!(double) *a, int *lda, 
	double *s, double *scond, double *amax, int *info);
 
  void zporfs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, Complex!(double) *af, int *ldaf, 
	Complex!(double) *b, int *ldb, Complex!(double) *x, int *ldx, 
	double *ferr, double *berr, Complex!(double) *work, double *
	rwork, int *info);
 
  void zposv_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	int *info);
 
  void zposvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(double) *a, int *lda, Complex!(double) *af, int *
	ldaf, char *equed, double *s, Complex!(double) *b, int *ldb, 
	Complex!(double) *x, int *ldx, double *rcond, double *ferr, 
	double *berr, Complex!(double) *work, double *rwork, int *
	info);
 
  void zpotf2_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *info);
 
  void zpotrf_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *info);
 
  void zpotri_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *info);
 
  void zpotrs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	int *info);
 
  void zppcon_(char *uplo, int *n, Complex!(double) *ap, 
	double *anorm, double *rcond, Complex!(double) *work, double 
	*rwork, int *info);
 
  void zppequ_(char *uplo, int *n, Complex!(double) *ap, 
	double *s, double *scond, double *amax, int *info);
 
  void zpprfs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *ap, Complex!(double) *afp, Complex!(double) *b, int *ldb,
	 Complex!(double) *x, int *ldx, double *ferr, double *berr, 
	Complex!(double) *work, double *rwork, int *info);
 
  void zppsv_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *ap, Complex!(double) *b, int *ldb, int *info);
 
  void zppsvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(double) *ap, Complex!(double) *afp, char *equed, double *
	s, Complex!(double) *b, int *ldb, Complex!(double) *x, int *ldx, 
	double *rcond, double *ferr, double *berr, Complex!(double) *
	work, double *rwork, int *info);
 
  void zpptrf_(char *uplo, int *n, Complex!(double) *ap, 
	int *info);
 
  void zpptri_(char *uplo, int *n, Complex!(double) *ap, 
	int *info);
 
  void zpptrs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *ap, Complex!(double) *b, int *ldb, int *info);
 
  void zptcon_(int *n, double *d__, Complex!(double) *e, 
	double *anorm, double *rcond, double *rwork, int *
	info);
 
  void zptrfs_(char *uplo, int *n, int *nrhs, 
	double *d__, Complex!(double) *e, double *df, Complex!(double) *ef, 
	Complex!(double) *b, int *ldb, Complex!(double) *x, int *ldx, 
	double *ferr, double *berr, Complex!(double) *work, double *
	rwork, int *info);
 
  void zptsv_(int *n, int *nrhs, double *d__, 
	Complex!(double) *e, Complex!(double) *b, int *ldb, int *info);
 
  void zptsvx_(char *fact, int *n, int *nrhs, 
	double *d__, Complex!(double) *e, double *df, Complex!(double) *ef, 
	Complex!(double) *b, int *ldb, Complex!(double) *x, int *ldx, 
	double *rcond, double *ferr, double *berr, Complex!(double) *
	work, double *rwork, int *info);
 
  void zpttrf_(int *n, double *d__, Complex!(double) *e, 
	int *info);
 
  void zpttrs_(char *uplo, int *n, int *nrhs, 
	double *d__, Complex!(double) *e, Complex!(double) *b, int *ldb, 
	int *info);
 
  void zptts2_(int *iuplo, int *n, int *nrhs, 
	double *d__, Complex!(double) *e, Complex!(double) *b, int *ldb);
 
  void zrot_(int *n, Complex!(double) *cx, int *incx, 
	Complex!(double) *cy, int *incy, double *c__, Complex!(double) *s);
 
  void zspcon_(char *uplo, int *n, Complex!(double) *ap, 
	int *ipiv, double *anorm, double *rcond, Complex!(double) *
	work, int *info);
 
  void zspmv_(char *uplo, int *n, Complex!(double) *alpha, 
	Complex!(double) *ap, Complex!(double) *x, int *incx, Complex!(double) *
	beta, Complex!(double) *y, int *incy);
 
  void zspr_(char *uplo, int *n, Complex!(double) *alpha, 
	Complex!(double) *x, int *incx, Complex!(double) *ap);
 
  void zsprfs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *ap, Complex!(double) *afp, int *ipiv, Complex!(double) *
	b, int *ldb, Complex!(double) *x, int *ldx, double *ferr, 
	double *berr, Complex!(double) *work, double *rwork, int *
	info);
 
  void zspsv_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *ap, int *ipiv, Complex!(double) *b, int *ldb, 
	int *info);
 
  void zspsvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(double) *ap, Complex!(double) *afp, int *ipiv, 
	Complex!(double) *b, int *ldb, Complex!(double) *x, int *ldx, 
	double *rcond, double *ferr, double *berr, Complex!(double) *
	work, double *rwork, int *info);
 
  void zsptrf_(char *uplo, int *n, Complex!(double) *ap, 
	int *ipiv, int *info);
 
  void zsptri_(char *uplo, int *n, Complex!(double) *ap, 
	int *ipiv, Complex!(double) *work, int *info);
 
  void zsptrs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *ap, int *ipiv, Complex!(double) *b, int *ldb, 
	int *info);
 
  void zstedc_(char *compz, int *n, double *d__, 
	double *e, Complex!(double) *z__, int *ldz, Complex!(double) *work, 
	int *lwork, double *rwork, int *lrwork, int *iwork, 
	int *liwork, int *info);
 
  void zstein_(int *n, double *d__, double *e, 
	int *m, double *w, int *iblock, int *isplit, 
	Complex!(double) *z__, int *ldz, double *work, int *iwork, 
	int *ifail, int *info);
 
  void zsteqr_(char *compz, int *n, double *d__, 
	double *e, Complex!(double) *z__, int *ldz, double *work, 
	int *info);
 
  void zsycon_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *ipiv, double *anorm, double *rcond, 
	Complex!(double) *work, int *info);
 
  void zsymv_(char *uplo, int *n, Complex!(double) *alpha, 
	Complex!(double) *a, int *lda, Complex!(double) *x, int *incx, 
	Complex!(double) *beta, Complex!(double) *y, int *incy);
 
  void zsyr_(char *uplo, int *n, Complex!(double) *alpha, 
	Complex!(double) *x, int *incx, Complex!(double) *a, int *lda);
 
  void zsyrfs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, Complex!(double) *af, int *ldaf, 
	int *ipiv, Complex!(double) *b, int *ldb, Complex!(double) *x, 
	int *ldx, double *ferr, double *berr, Complex!(double) *work,
	 double *rwork, int *info);
 
  void zsysv_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, int *ipiv, Complex!(double) *b, 
	int *ldb, Complex!(double) *work, int *lwork, int *info);
 
  void zsysvx_(char *fact, char *uplo, int *n, int *
	nrhs, Complex!(double) *a, int *lda, Complex!(double) *af, int *
	ldaf, int *ipiv, Complex!(double) *b, int *ldb, Complex!(double) *x,
	 int *ldx, double *rcond, double *ferr, double *berr, 
	Complex!(double) *work, int *lwork, double *rwork, int *info);
 
  void zsytf2_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *ipiv, int *info);
 
  void zsytrf_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *ipiv, Complex!(double) *work, int *lwork, 
	int *info);
 
  void zsytri_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, int *ipiv, Complex!(double) *work, int *info);
 
  void zsytrs_(char *uplo, int *n, int *nrhs, 
	Complex!(double) *a, int *lda, int *ipiv, Complex!(double) *b, 
	int *ldb, int *info);
 
  void ztbcon_(char *norm, char *uplo, char *diag, int *n, 
	int *kd, Complex!(double) *ab, int *ldab, double *rcond, 
	Complex!(double) *work, double *rwork, int *info);
 
  void ztbrfs_(char *uplo, char *trans, char *diag, int *n, 
	int *kd, int *nrhs, Complex!(double) *ab, int *ldab, 
	Complex!(double) *b, int *ldb, Complex!(double) *x, int *ldx, 
	double *ferr, double *berr, Complex!(double) *work, double *
	rwork, int *info);
 
  void ztbtrs_(char *uplo, char *trans, char *diag, int *n, 
	int *kd, int *nrhs, Complex!(double) *ab, int *ldab, 
	Complex!(double) *b, int *ldb, int *info);
 
  void ztgevc_(char *side, char *howmny, logical *select, 
	int *n, Complex!(double) *a, int *lda, Complex!(double) *b, int 
	*ldb, Complex!(double) *vl, int *ldvl, Complex!(double) *vr, int *
	ldvr, int *mm, int *m, Complex!(double) *work, double *rwork,
	 int *info);
 
  void ztgex2_(logical *wantq, logical *wantz, int *n, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	Complex!(double) *q, int *ldq, Complex!(double) *z__, int *ldz, 
	int *j1, int *info);
 
  void ztgexc_(logical *wantq, logical *wantz, int *n, 
	Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	Complex!(double) *q, int *ldq, Complex!(double) *z__, int *ldz, 
	int *ifst, int *ilst, int *info);
 
  void ztgsen_(int *ijob, logical *wantq, logical *wantz, 
	logical *select, int *n, Complex!(double) *a, int *lda, 
	Complex!(double) *b, int *ldb, Complex!(double) *alpha, Complex!(double) *
	beta, Complex!(double) *q, int *ldq, Complex!(double) *z__, int *
	ldz, int *m, double *pl, double *pr, double *dif, 
	Complex!(double) *work, int *lwork, int *iwork, int *liwork, 
	int *info);
 
  void ztgsja_(char *jobu, char *jobv, char *jobq, int *m, 
	int *p, int *n, int *k, int *l, Complex!(double) *a, 
	int *lda, Complex!(double) *b, int *ldb, double *tola, 
	double *tolb, double *alpha, double *beta, Complex!(double) *
	u, int *ldu, Complex!(double) *v, int *ldv, Complex!(double) *q, 
	int *ldq, Complex!(double) *work, int *ncycle, int *info);
 
  void ztgsna_(char *job, char *howmny, logical *select, 
	int *n, Complex!(double) *a, int *lda, Complex!(double) *b, int 
	*ldb, Complex!(double) *vl, int *ldvl, Complex!(double) *vr, int *
	ldvr, double *s, double *dif, int *mm, int *m, 
	Complex!(double) *work, int *lwork, int *iwork, int *info);
 
  void ztgsy2_(char *trans, int *ijob, int *m, int *
	n, Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	Complex!(double) *c__, int *ldc, Complex!(double) *d__, int *ldd, 
	Complex!(double) *e, int *lde, Complex!(double) *f, int *ldf, 
	double *scale, double *rdsum, double *rdscal, int *
	info);
 
  void ztgsyl_(char *trans, int *ijob, int *m, int *
	n, Complex!(double) *a, int *lda, Complex!(double) *b, int *ldb, 
	Complex!(double) *c__, int *ldc, Complex!(double) *d__, int *ldd, 
	Complex!(double) *e, int *lde, Complex!(double) *f, int *ldf, 
	double *scale, double *dif, Complex!(double) *work, int *
	lwork, int *iwork, int *info);
 
  void ztpcon_(char *norm, char *uplo, char *diag, int *n, 
	Complex!(double) *ap, double *rcond, Complex!(double) *work, double 
	*rwork, int *info);
 
  void ztprfs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, Complex!(double) *ap, Complex!(double) *b, int *ldb, 
	Complex!(double) *x, int *ldx, double *ferr, double *berr, 
	Complex!(double) *work, double *rwork, int *info);
 
  void ztptri_(char *uplo, char *diag, int *n, 
	Complex!(double) *ap, int *info);
 
  void ztptrs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, Complex!(double) *ap, Complex!(double) *b, int *ldb, 
	int *info);
 
  void ztrcon_(char *norm, char *uplo, char *diag, int *n, 
	Complex!(double) *a, int *lda, double *rcond, Complex!(double) *
	work, double *rwork, int *info);
 
  void ztrevc_(char *side, char *howmny, logical *select, 
	int *n, Complex!(double) *t, int *ldt, Complex!(double) *vl, 
	int *ldvl, Complex!(double) *vr, int *ldvr, int *mm, int 
	*m, Complex!(double) *work, double *rwork, int *info);
 
  void ztrexc_(char *compq, int *n, Complex!(double) *t, 
	int *ldt, Complex!(double) *q, int *ldq, int *ifst, int *
	ilst, int *info);
 
  void ztrrfs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, Complex!(double) *a, int *lda, Complex!(double) *b, 
	int *ldb, Complex!(double) *x, int *ldx, double *ferr, 
	double *berr, Complex!(double) *work, double *rwork, int *
	info);
 
  void ztrsen_(char *job, char *compq, logical *select, int 
	*n, Complex!(double) *t, int *ldt, Complex!(double) *q, int *ldq, 
	Complex!(double) *w, int *m, double *s, double *sep, 
	Complex!(double) *work, int *lwork, int *info);
 
  void ztrsna_(char *job, char *howmny, logical *select, 
	int *n, Complex!(double) *t, int *ldt, Complex!(double) *vl, 
	int *ldvl, Complex!(double) *vr, int *ldvr, double *s, 
	double *sep, int *mm, int *m, Complex!(double) *work, 
	int *ldwork, double *rwork, int *info);
 
  void ztrsyl_(char *trana, char *tranb, int *isgn, int 
	*m, int *n, Complex!(double) *a, int *lda, Complex!(double) *b, 
	int *ldb, Complex!(double) *c__, int *ldc, double *scale, 
	int *info);
 
  void ztrti2_(char *uplo, char *diag, int *n, 
	Complex!(double) *a, int *lda, int *info);
 
  void ztrtri_(char *uplo, char *diag, int *n, 
	Complex!(double) *a, int *lda, int *info);
 
  void ztrtrs_(char *uplo, char *trans, char *diag, int *n, 
	int *nrhs, Complex!(double) *a, int *lda, Complex!(double) *b, 
	int *ldb, int *info);
 
  void ztzrqf_(int *m, int *n, Complex!(double) *a, 
	int *lda, Complex!(double) *tau, int *info);
 
  void ztzrzf_(int *m, int *n, Complex!(double) *a, 
	int *lda, Complex!(double) *tau, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zung2l_(int *m, int *n, int *k, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *info);
 
  void zung2r_(int *m, int *n, int *k, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *info);
 
  void zungbr_(char *vect, int *m, int *n, int *k, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *lwork, int *info);
 
  void zunghr_(int *n, int *ilo, int *ihi, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *lwork, int *info);
 
  void zungl2_(int *m, int *n, int *k, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *info);
 
  void zunglq_(int *m, int *n, int *k, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *lwork, int *info);
 
  void zungql_(int *m, int *n, int *k, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *lwork, int *info);
 
  void zungqr_(int *m, int *n, int *k, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *lwork, int *info);
 
  void zungr2_(int *m, int *n, int *k, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *info);
 
  void zungrq_(int *m, int *n, int *k, 
	Complex!(double) *a, int *lda, Complex!(double) *tau, Complex!(double) *
	work, int *lwork, int *info);
 
  void zungtr_(char *uplo, int *n, Complex!(double) *a, 
	int *lda, Complex!(double) *tau, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zunm2l_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(double) *a, int *lda, Complex!(double) *tau, 
	Complex!(double) *c__, int *ldc, Complex!(double) *work, int *info);
 
  void zunm2r_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(double) *a, int *lda, Complex!(double) *tau, 
	Complex!(double) *c__, int *ldc, Complex!(double) *work, int *info);
 
  void zunmbr_(char *vect, char *side, char *trans, int *m, 
	int *n, int *k, Complex!(double) *a, int *lda, Complex!(double) 
	*tau, Complex!(double) *c__, int *ldc, Complex!(double) *work, int *
	lwork, int *info);
 
  void zunmhr_(char *side, char *trans, int *m, int *n, 
	int *ilo, int *ihi, Complex!(double) *a, int *lda, 
	Complex!(double) *tau, Complex!(double) *c__, int *ldc, Complex!(double) *
	work, int *lwork, int *info);
 
  void zunml2_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(double) *a, int *lda, Complex!(double) *tau, 
	Complex!(double) *c__, int *ldc, Complex!(double) *work, int *info);
 
  void zunmlq_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(double) *a, int *lda, Complex!(double) *tau, 
	Complex!(double) *c__, int *ldc, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zunmql_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(double) *a, int *lda, Complex!(double) *tau, 
	Complex!(double) *c__, int *ldc, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zunmqr_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(double) *a, int *lda, Complex!(double) *tau, 
	Complex!(double) *c__, int *ldc, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zunmr2_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(double) *a, int *lda, Complex!(double) *tau, 
	Complex!(double) *c__, int *ldc, Complex!(double) *work, int *info);
 
  void zunmr3_(char *side, char *trans, int *m, int *n, 
	int *k, int *l, Complex!(double) *a, int *lda, Complex!(double) 
	*tau, Complex!(double) *c__, int *ldc, Complex!(double) *work, int *
	info);
 
  void zunmrq_(char *side, char *trans, int *m, int *n, 
	int *k, Complex!(double) *a, int *lda, Complex!(double) *tau, 
	Complex!(double) *c__, int *ldc, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zunmrz_(char *side, char *trans, int *m, int *n, 
	int *k, int *l, Complex!(double) *a, int *lda, Complex!(double) 
	*tau, Complex!(double) *c__, int *ldc, Complex!(double) *work, int *
	lwork, int *info);
 
  void zunmtr_(char *side, char *uplo, char *trans, int *m, 
	int *n, Complex!(double) *a, int *lda, Complex!(double) *tau, 
	Complex!(double) *c__, int *ldc, Complex!(double) *work, int *lwork,
	 int *info);
 
  void zupgtr_(char *uplo, int *n, Complex!(double) *ap, 
	Complex!(double) *tau, Complex!(double) *q, int *ldq, Complex!(double) *
	work, int *info);
 
  void zupmtr_(char *side, char *uplo, char *trans, int *m, 
	int *n, Complex!(double) *ap, Complex!(double) *tau, Complex!(double) *c__,
	 int *ldc, Complex!(double) *work, int *info);

} 





