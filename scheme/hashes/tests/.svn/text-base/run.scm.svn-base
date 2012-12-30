(use
	RJMXHash TWMXHash TWSHMXHash TWSHMLMXHash TWMGMXHash TWUserMixHash FNVHash
	FNVAHash PHSFHash RSHash JSHash PJWHash ELFHash BKDRHash SDBMHash DJBHash
	NDJBHash DEKHash APHash BRPHash PYHash RJL3Hash ISPLHash CRCHash)
(use rabin-karp)
(use hash-utils)
(use srfi-1 message-digest)
(use test)

;;;

(define-constant unsigned-integer32-size 4)

(define TSTSTR "The large brown fox jumped over the lazy dog.")

(define TSTSTR-LEN        (string-length TSTSTR))
(define TSTSTR-HALF-LEN   (fx/ TSTSTR-LEN 2))

;;;

(test-group "Hash Functions"

	(test-group "*Hash Half Length"

		(test "RJMXHash"      1495738488  (*RJMXHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "TWMXHash"      1783737257  (*TWMXHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "TWSHMXHash"    4294724170  (*TWSHMXHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "TWSHMLMXHash"  3316553819  (*TWSHMLMXHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "TWMGMXHash"    3799230039  (*TWMGMXHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "RSHash"        561364390   (*RSHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "JSHash"        1096595314  (*JSHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "PJWHash"       11905       (*PJWHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "ELFHash"       125889045   (*ELFHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "BKDRHash"      2233812262  (*BKDRHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "SDBMHash"      1850315706  (*SDBMHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "DJBHash"       664891301   (*DJBHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "NDJBHash"      786919305   (*NDJBHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "DEKHash"       4159618807  (*DEKHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "APHash"        3373256484  (*APHash TSTSTR TSTSTR-HALF-LEN 0))
	  (test "CRCHash"       4140400781  (*CRCHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "PHSFHash"      4232571984  (*PHSFHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "FNVHash"       2129953783  (*FNVHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "FNVAHash"      1173052123  (*FNVAHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "BRPHash"       1793202933  (*BRPHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "PYHash"        10752       (*PYHash TSTSTR TSTSTR-HALF-LEN 0))
		(test "RJL3Hash"      1304064403   (*RJL3Hash TSTSTR TSTSTR-HALF-LEN 0))
		(test "ISPLHash"      2015390325  (*ISPLHash TSTSTR TSTSTR-HALF-LEN 0))
	)

	(test-group "Hash Full Length"

		(test "RJMXHash" (*RJMXHash TSTSTR TSTSTR-LEN 0) (RJMXHash TSTSTR))
		(test "TWMXHash" (*TWMXHash TSTSTR TSTSTR-LEN 0) (TWMXHash TSTSTR))
		(test "TWSHMXHash" (*TWSHMXHash TSTSTR TSTSTR-LEN 0) (TWSHMXHash TSTSTR))
		(test "TWSHMLMXHash" (*TWSHMLMXHash TSTSTR TSTSTR-LEN 0) (TWSHMLMXHash TSTSTR))
		(test "TWMGMXHash" (*TWMGMXHash TSTSTR TSTSTR-LEN 0) (TWMGMXHash TSTSTR))
		(test "RSHash" (*RSHash TSTSTR TSTSTR-LEN 0) (RSHash TSTSTR))
		(test "JSHash" (*JSHash TSTSTR TSTSTR-LEN 0) (JSHash TSTSTR))
		(test "PJWHash" (*PJWHash TSTSTR TSTSTR-LEN 0) (PJWHash TSTSTR))
		(test "ELFHash" (*ELFHash TSTSTR TSTSTR-LEN 0) (ELFHash TSTSTR))
		(test "BKDRHash" (*BKDRHash TSTSTR TSTSTR-LEN 0) (BKDRHash TSTSTR))
		(test "SDBMHash" (*SDBMHash TSTSTR TSTSTR-LEN 0) (SDBMHash TSTSTR))
		(test "DJBHash" (*DJBHash TSTSTR TSTSTR-LEN 0) (DJBHash TSTSTR))
		(test "NDJBHash" (*NDJBHash TSTSTR TSTSTR-LEN 0) (NDJBHash TSTSTR))
		(test "DEKHash" (*DEKHash TSTSTR TSTSTR-LEN 0) (DEKHash TSTSTR))
		(test "APHash" (*APHash TSTSTR TSTSTR-LEN 0) (APHash TSTSTR))
		(test "CRCHash" (*CRCHash TSTSTR TSTSTR-LEN 0) (CRCHash TSTSTR))
		(test "PHSFHash" (*PHSFHash TSTSTR TSTSTR-LEN 0) (PHSFHash TSTSTR))
		(test "FNVHash" (*FNVHash TSTSTR TSTSTR-LEN 0) (FNVHash TSTSTR))
		(test "FNVAHash" (*FNVAHash TSTSTR TSTSTR-LEN 0) (FNVAHash TSTSTR))
		(test "BRPHash" (*BRPHash TSTSTR TSTSTR-LEN 0) (BRPHash TSTSTR))
		(test "PYHash" (*PYHash TSTSTR TSTSTR-LEN 0) (PYHash TSTSTR))
		(test "RJL3Hash" (*RJL3Hash TSTSTR TSTSTR-LEN 0) (RJL3Hash TSTSTR))
		(test "ISPLHash" (*ISPLHash TSTSTR TSTSTR-LEN 0) (ISPLHash TSTSTR))
	)

	(test-group "Hash Length Arg"

		(test "RJMXHash" (*RJMXHash TSTSTR TSTSTR-HALF-LEN 0) (RJMXHash TSTSTR TSTSTR-HALF-LEN))
		(test "TWMXHash" (*TWMXHash TSTSTR TSTSTR-HALF-LEN 0) (TWMXHash TSTSTR TSTSTR-HALF-LEN))
		(test "TWSHMXHash" (*TWSHMXHash TSTSTR TSTSTR-HALF-LEN 0) (TWSHMXHash TSTSTR TSTSTR-HALF-LEN))
		(test "TWSHMLMXHash" (*TWSHMLMXHash TSTSTR TSTSTR-HALF-LEN 0) (TWSHMLMXHash TSTSTR TSTSTR-HALF-LEN))
		(test "TWMGMXHash" (*TWMGMXHash TSTSTR TSTSTR-HALF-LEN 0) (TWMGMXHash TSTSTR TSTSTR-HALF-LEN))
		(test "RSHash" (*RSHash TSTSTR TSTSTR-HALF-LEN 0) (RSHash TSTSTR TSTSTR-HALF-LEN))
		(test "JSHash" (*JSHash TSTSTR TSTSTR-HALF-LEN 0) (JSHash TSTSTR TSTSTR-HALF-LEN))
		(test "PJWHash" (*PJWHash TSTSTR TSTSTR-HALF-LEN 0) (PJWHash TSTSTR TSTSTR-HALF-LEN))
		(test "ELFHash" (*ELFHash TSTSTR TSTSTR-HALF-LEN 0) (ELFHash TSTSTR TSTSTR-HALF-LEN))
		(test "BKDRHash" (*BKDRHash TSTSTR TSTSTR-HALF-LEN 0) (BKDRHash TSTSTR TSTSTR-HALF-LEN))
		(test "SDBMHash" (*SDBMHash TSTSTR TSTSTR-HALF-LEN 0) (SDBMHash TSTSTR TSTSTR-HALF-LEN))
		(test "DJBHash" (*DJBHash TSTSTR TSTSTR-HALF-LEN 0) (DJBHash TSTSTR TSTSTR-HALF-LEN))
		(test "NDJBHash" (*NDJBHash TSTSTR TSTSTR-HALF-LEN 0) (NDJBHash TSTSTR TSTSTR-HALF-LEN))
		(test "DEKHash" (*DEKHash TSTSTR TSTSTR-HALF-LEN 0) (DEKHash TSTSTR TSTSTR-HALF-LEN))
		(test "APHash" (*APHash TSTSTR TSTSTR-HALF-LEN 0) (APHash TSTSTR TSTSTR-HALF-LEN))
		(test "CRCHash" (*CRCHash TSTSTR TSTSTR-HALF-LEN 0) (CRCHash TSTSTR TSTSTR-HALF-LEN))
		(test "PHSFHash" (*PHSFHash TSTSTR TSTSTR-HALF-LEN 0) (PHSFHash TSTSTR TSTSTR-HALF-LEN))
		(test "FNVHash" (*FNVHash TSTSTR TSTSTR-HALF-LEN 0) (FNVHash TSTSTR TSTSTR-HALF-LEN))
		(test "FNVAHash" (*FNVAHash TSTSTR TSTSTR-HALF-LEN 0) (FNVAHash TSTSTR TSTSTR-HALF-LEN))
		(test "BRPHash" (*BRPHash TSTSTR TSTSTR-HALF-LEN 0) (BRPHash TSTSTR TSTSTR-HALF-LEN))
		(test "PYHash" (*PYHash TSTSTR TSTSTR-HALF-LEN 0) (PYHash TSTSTR TSTSTR-HALF-LEN))
		(test "RJL3Hash" (*RJL3Hash TSTSTR TSTSTR-HALF-LEN 0) (RJL3Hash TSTSTR TSTSTR-HALF-LEN))
		(test "ISPLHash" (*ISPLHash TSTSTR TSTSTR-HALF-LEN 0) (ISPLHash TSTSTR TSTSTR-HALF-LEN))
	)

	(test-group "Digest"

		(test-assert "RJMXHash" (message-digest-string (RJMXHash-primitive) TSTSTR))
		(test-assert "TWMXHash" (message-digest-string (TWMXHash-primitive) TSTSTR))
		(test-assert "TWSHMXHash" (message-digest-string (TWSHMXHash-primitive) TSTSTR))
		(test-assert "TWSHMLMXHash" (message-digest-string (TWSHMLMXHash-primitive) TSTSTR))
		(test-assert "TWMGMXHash" (message-digest-string (TWMGMXHash-primitive) TSTSTR))
		(test-assert "RSHash" (message-digest-string (RSHash-primitive) TSTSTR))
		(test-assert "JSHash" (message-digest-string (JSHash-primitive) TSTSTR))
		(test-assert "PJWHash" (message-digest-string (PJWHash-primitive) TSTSTR))
		(test-assert "ELFHash" (message-digest-string (ELFHash-primitive) TSTSTR))
		(test-assert "BKDRHash" (message-digest-string (BKDRHash-primitive) TSTSTR))
		(test-assert "SDBMHash" (message-digest-string (SDBMHash-primitive) TSTSTR))
		(test-assert "DJBHash" (message-digest-string (DJBHash-primitive) TSTSTR))
		(test-assert "NDJBHash" (message-digest-string (NDJBHash-primitive) TSTSTR))
		(test-assert "DEKHash" (message-digest-string (DEKHash-primitive) TSTSTR))
		(test-assert "APHash" (message-digest-string (APHash-primitive) TSTSTR))
		(test-assert "CRCHash" (message-digest-string (CRCHash-primitive) TSTSTR))
		(test-assert "PHSFHash" (message-digest-string (PHSFHash-primitive) TSTSTR))
		(test-assert "FNVHash" (message-digest-string (FNVHash-primitive) TSTSTR))
		(test-assert "FNVAHash" (message-digest-string (FNVAHash-primitive) TSTSTR))
		(test-assert "BRPHash" (message-digest-string (BRPHash-primitive) TSTSTR))
		(test-assert "PYHash" (message-digest-string (PYHash-primitive) TSTSTR))
		(test-assert "RJL3Hash" (message-digest-string (RJL3Hash-primitive) TSTSTR))
		(test-assert "ISPLHash" (message-digest-string (ISPLHash-primitive) TSTSTR))
	)
)

(test-group "Utilities"

  (define tstr (make-string unsigned-integer32-size))
  (define bnd-hsh #f)
  (define str TSTSTR)

  (test-assert "u32 set" (unsigned-integer32-set! tstr (arithmetic-shift 1 31)))
  (test "u32 ref" (arithmetic-shift 1 31) (unsigned-integer32-ref tstr))

  (test-assert (procedure? (make-bounded-hash *RJMXHash)))
  (set! bnd-hsh (make-bounded-hash *RJMXHash))
  (test-assert "number bound override" (number? (bnd-hsh str 12345678901234567890)))
  (test-assert "within bounds" (> 1456 (bnd-hsh str 1456)))

  (test-assert (procedure? (make-fixnum-bounded-hash *RJMXHash)))
  (set! bnd-hsh (make-fixnum-bounded-hash *RJMXHash))
  (test-assert "fixnum bound override" (fixnum? (bnd-hsh str 1456)))
  (test-assert "within bounds" (> 1456 (bnd-hsh str 1456)))
)

;; This tests whether memory is being corrupted
;; There was a problem w/ syntax-case & RJL3Hash

(test-group "RJL3Hash Idempotent?"
  (test "1" 2110415480 (RJL3Hash TSTSTR))
  (test "2" 2110415480 (RJL3Hash TSTSTR))
  (test "3" 2110415480 (RJL3Hash TSTSTR))
  (test "4" 2110415480 (RJL3Hash TSTSTR))
  (test "5" 2110415480 (RJL3Hash TSTSTR))
  (test "6" 2110415480 (RJL3Hash TSTSTR))
  (test "7" 2110415480 (RJL3Hash TSTSTR))
  (test "8" 2110415480 (RJL3Hash TSTSTR))
  (test "9" 2110415480 (RJL3Hash TSTSTR))
)

(test-group "TWUserMixHash"

  (define (mix key) key)
  (define usrmixhsh)
  (define *hash)
  (define hash)
  (define digest-primitive)

  (test-assert "TWUserMixHash Make" (= 3 (length (receive (make-TWUserMixHash mix #t)))))
  (set! usrmixhsh (receive (make-TWUserMixHash mix #t)))
  (set! *hash (first usrmixhsh))
  (set! hash (second usrmixhsh))
  (set! digest-primitive (third usrmixhsh))
  (test-assert (procedure? *hash))
  (test-assert (procedure? hash))
  (test-assert (message-digest-primitive? digest-primitive))

  (test "TWUserMixHash Hash" (*hash TSTSTR TSTSTR-LEN 0) (hash TSTSTR))
  (test "TWUserMixHash Length Arg" (*hash TSTSTR TSTSTR-HALF-LEN 0) (hash TSTSTR TSTSTR-HALF-LEN))

  (test "*TWUserMixHash full-length" 1128165222 (*hash TSTSTR TSTSTR-LEN 0))
  (test "*TWUserMixHash half-length" 4224816547 (*hash TSTSTR TSTSTR-HALF-LEN 0))

  (test "TWUserMixHash Digest" "433e6f66" (message-digest-string digest-primitive TSTSTR))
)

(test-group "Rabin-Karp Search"

	(define substrs '("quick" "foo" "brown" "dog" "skasfdskjsalksafnsalsfsdsdjkldsajlfsalsk"))
  (define hashp)
  (define rksp)

  (test-assert (procedure? (make-rabin-karp-string-search substrs)))
  (set! rksp (make-rabin-karp-string-search substrs))

  (test "W/O start & end" '("brown" (10 15)) (rksp TSTSTR))
  (test "W/ start & end" '("dog" (41 44)) (rksp TSTSTR 41 TSTSTR-LEN))

  (test-assert (procedure? (make-fixnum-bounded-hash *RJL3Hash)))
  (set! hashp (make-fixnum-bounded-hash *RJL3Hash))
  (test-assert (procedure? (make-fixnum-bounded-hash *RJL3Hash)))
  (set! hashp (make-fixnum-bounded-hash *RJL3Hash))

  (test "W/O start & end" '("brown" (10 15)) (rksp TSTSTR))
  (test "W/ start & end" '("dog" (41 44)) (rksp TSTSTR 41 TSTSTR-LEN))
)

(newline)
(print "*** Hash results are platform specific. You may see many failures. ***")

(test-exit)
