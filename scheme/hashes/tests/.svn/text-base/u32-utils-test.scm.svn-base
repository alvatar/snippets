(use hash-utils extras lolevel miscmacros)

(define-constant NUM-TIMES 1000)

(define (random-integer high #!optional (low 0))
  (+ low (random (+ 1 (- high low)))) )

(define-constant BUFFER-MAXIMUM-LENGTH 1000)

(define (make-string/random-length #!optional (limit BUFFER-MAXIMUM-LENGTH))
  (make-string (random-integer limit 1)) )

(define (make-blob/random-length #!optional (limit BUFFER-MAXIMUM-LENGTH))
  (make-blob (random-integer limit 1)) )

(define (make-buffer size)
  (set-finalizer! (allocate size) free) )

(set-gc-report! #t)

(repeat NUM-TIMES
  (let* ((obj (make-string/random-length))
         (num-words (quotient (string-length obj) 4)) )
    (do ((idx 0 (add1 idx)))
        ((= idx num-words))
      (let ((num (random most-positive-fixnum)))
        (unsigned-integer32-set! obj num idx)
        (assert (= num (unsigned-integer32-ref obj idx))) ) ) ) )

;(gc)

(repeat NUM-TIMES
  (let* ((size (random-integer BUFFER-MAXIMUM-LENGTH 1))
         (obj (make-buffer size))
         (num-words (quotient size 4)) )
    (do ((idx 0 (add1 idx)))
        ((= idx num-words))
      (let ((num (random most-positive-fixnum)))
        (unsigned-integer32-set! obj num idx)
        (assert (= num (unsigned-integer32-ref obj idx))) ) ) ) )

;(gc)

(repeat NUM-TIMES
  (let* ((obj (make-blob/random-length))
         (num-words (quotient (blob-size obj) 4)) )
    (do ((idx 0 (add1 idx)))
        ((= idx num-words))
      (let ((num (random most-positive-fixnum)))
        (unsigned-integer32-set! obj num idx)
        (assert (= num (unsigned-integer32-ref obj idx))) ) ) ) )
