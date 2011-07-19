;;; -*- Mode: Scheme -*-

;;;; Comparative Examples of Looping Idioms

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; Looping idioms compared:
;;;
;;; . Standard combinators -- MAP, FOLD, &c. (where applicable)
;;; . Named LET
;;; . DO
;;; . Scheme48 ITERATE
;;;     <http://mumble.net/~campbell/s48-refman/html/
;;;        Macros-for-writing-loops.html>
;;; . SRFI 42
;;;     <http://srfi.schemers.org/srfi-42/srfi-42.html>
;;; . foof-loop
;;;     <http://mumble.net/~campbell/scheme/foof-loop.scm>
;;;     <http://mumble.net/~campbell/scheme/foof-loop.txt>
;;; . foof-loop + comprehensions (COLLECT-LIST &c.)
;;;     <http://mumble.net/~campbell/tmp/nested-foof-loop.scm>
;;;     <http://mumble.net/~campbell/tmp/nested-foof-loop.txt>

;;;; IOTA

;;; Recursive.  Note that this is incorrect if START and STEP are
;;; inexact values with very different magnitudes; we really need the
;;; multiplication.

(define (iota count start step)
  (if (zero? count)
      '()
      (cons start (iota (- count 1) (+ start step) step))))

;;; Unfold, right->left:

(define (iota count start step)
  (unfold-right zero?
                (lambda (n) (+ start (* (- n 1) step)))
                (lambda (n) (- n 1))
                count))

;;; Unfold, left->right:

(define (iota count start step)
  (unfold (lambda (n) (= n count))
          (lambda (n) (+ start (* n step)))
          (lambda (n) (+ n 1))
          0))

;;; Named LET:

(define (iota count start step)
  (let loop ((n count) (result '()))
    (if (zero? n)
        result
        (let ((n (- n 1)))
          (loop n (cons (+ start (* n step)) result))))))

;;; Note the duplication of the subtraction in DO:

(define (iota count start step)
  (do ((n (- count 1) (- n 1))
       (list '() (cons (+ start (* n step)) list)))
      ((< n 0) list)))

;;; Different DO, with subtraction somewhere else so that we can avoid
;;; dipping into negative numbers:

(define (iota count start step)
  (do ((n count (- n 1))
       (list '() (cons (+ start (* (- n 1) step)) list)))
      ((= n 0) list)))

;;; foof-loop, with reverse optimization:

(define (iota count start step)
  (loop ((for n (down-from count (to 0)))
         (for result (listing-reverse (+ start (* n step)))))
    => result))

;;; foof-loop + comprehensions, with reverse optimization:

(define (iota count start step)
  (collect-list-reverse (for n (down-from count (to 0)))
    (+ start (* n step))))

;;; foof-loop:

(define (iota count start step)
  (loop ((for n (up-from 0 (to count)))
         (for result (listing (+ start (* n step)))))
    => result))

;;; foof-loop + comprehensions:

(define (iota count start step)
  (collect-list (for n (up-from 0 (to count)))
    (+ start (* n step))))

;;;; READ-LINE

;;; Wrapper around the actual procedures to compare.

(define (read-line input-port)
  (let ((initial (peek-char input-port)))
    (if (eof-object? initial)
        initial
        (*read-line input-port))))

;;; String-unfold:

(define (*read-line input-port)
  (string-unfold (lambda (char)
                   (or (eof-object? char)
                       (char=? char #\newline)))
                 (lambda (char) char)
                 (lambda (char)
                   char                 ;ignore
                   (read-char input-port))
                 (read-char input-port)))

;;; Named LET:

(define (*read-line input-port)
  (let continue ((chars '()))
    (let ((char (read-char input-port)))
      (if (or (eof-object? char)
              (char=? char #\newline))
          (list->string (reverse chars))
          (continue (cons char chars))))))

;;; Note the duplication of the READ-CHAR call in DO:

(define (*read-line input-port)
  (do ((char (read-char input-port)
             (read-char input-port))
       (chars '() (cons char chars)))
      ((or (eof-object? char)
           (char=? char #\newline))
       (list->string (reverse chars)))))

;;; Scheme48 ITERATE:

(define (*read-line input-port)
  (let ((finish (lambda (chars) (list->string (reverse chars)))))
    (iterate continue ((input* char input-port read-char))
        ((chars '()))
      (if (char=? char #\newline)
          (finish chars)
          (continue (cons char chars)))
      (finish chars))))

;;; SRFI 42:

(define (*read-line input-port)
  ;++ This is not quite right; it includes the trailing newline.  I
  ;++ don't know how to avoid this.
  (string-ec (:until (:port char input-port read-char)
                     (char=? char #\newline))
    char))

;;; foof-loop:

(define (*read-line input-port)
  (loop ((for char (in-port input-port))
         (until (char=? char #\newline))
         (for line (stringing char)))
    => line))

;;; foof-loop + comprehensions:

(define (*read-line input-port)
  (collect-string (parallel (for char (in-port input-port))
                            (until (char=? char #\newline)))
    char))

;;;; READ-NON-EMPTY-LINES

;;; Unfold:

(define (read-non-empty-lines input-port)
  (unfold (lambda (line)
            (or (eof-object? line)
                (string-empty? line)))
          (lambda (line) line)
          (lambda (line)
            line                        ;ignore
            (read-line input-port))
          (read-line input-port)))

;;; Named LET:

(define (read-non-empty-lines input-port)
  (let continue ((lines-rev '()))
    (let ((line (read-line input-port)))
      (if (or (eof-object? line)
              (string-empty? line))
          (reverse lines-rev)
          (continue (cons line lines-rev))))))

;;; DO:

(define (read-non-empty-lines input-port)
  (do ((line (read-line input-port)
             (read-line input-port))
       (lines '() (cons line lines)))
      ((or (eof-object? line)
           (string-empty? line))
       (reverse lines))))

;;; Scheme48 ITERATE:

(define (read-non-empty-lines input-port)
  (iterate continue ((input* line input-port read-line))
      ((lines '()))
    (if (string-empty? line)
        (reverse lines)
        (continue (cons line lines)))
    (reverse lines)))

;;; SRFI 42:

(define (read-non-empty-lines input-port)
  (list-ec (:until (:port line input-port read-line)
                   (string-empty? line))
    line))

;;; foof-loop:

(define (read-non-empty-lines input-port)
  (loop ((for line (in-port input-port read-line))
         (until (string-empty? line))
         (for lines (listing line)))
    => lines))

;;; foof-loop + comprehensions:

(define (read-non-empty-lines input-port)
  (collect-list (parallel (for line (in-port input-port read-line))
                          (until (string-empty? line)))
    line))

;;; Auxiliary utility.

(define (string-empty? string)
  (zero? (string-length string)))

;;;; Cartesian Product

;;; List combinators:

(define (cartesian-product lists)
  (if (null? lists)
      '(())
      (append-map (let ((tails (cartesian-product (cdr lists))))
                    (lambda (first)
                      (map (lambda (rest)
                             (cons first rest))
                           tails)))
                  (car lists))))

;;; Fold:

(define (cartesian-product lists)
  (fold-right (lambda (list tails)
                (fold-right (lambda (item product)
                              (fold-right (lambda (tail product)
                                            (cons (cons item tail)
                                                  product))
                                          product
                                          tails))
                            '()
                            list))
              '(())
              lists))

;;; Named LET:

(define (cartesian-product lists)
  (let recur ((lists lists))
    (if (null? lists)
        '(())
        (reverse
         (let ((tails (recur (cdr lists))))
           (let outer ((list (car lists))
                       (product '()))
             (if (null? list)
                 product
                 (let ((item (car list))
                       (more (cdr list)))
                   (let inner ((tails tails)
                               (product product))
                     (if (null? tails)
                         (outer more product)
                         (inner (cdr tails)
                                (cons (cons item (car tails))
                                      product))))))))))))

;;; Writing this with DO is an exercise left for the reader.

;;; Scheme48 ITERATE:

(define (cartesian-product lists)
  (iterate recur ((list* list lists))
      ()
    (let ((tails (recur)))
      (reduce ((list* first list))
          ((product '()))
        (reduce ((list* rest tails))
            ((product product))
          (cons (cons first rest) product))
        (reverse product)))
    '(())))

;;; SRFI 42:

(define (cartesian-product lists)
  (if (null? lists)
      '(())
      (let ((tails (cartesian-product (cdr lists))))
        (list-ec (:list first (car lists))
                  (:list rest tails)
          (cons first rest)))))

;;; foof-loop:

(define (cartesian-product lists)
  (loop recur ((for list (in-list lists)))
    => '(())
    (let ((tails (recur)))
      (loop continue ((with result '())
                      (for first (in-list list)))
        => (reverse result)
        (loop ((for rest (in-list tails))
               (for result
                    (listing-reverse (initial result)
                                     (cons first rest))))
          => (continue result))))))

;;; foof-loop + comprehensions:

(define (cartesian-product lists)
  (loop recur ((for list (in-list lists)))
    => '(())
    (let ((tails (recur)))
      (collect-list (for first (in-list list))
                     (for rest (in-list tails))
        (cons first rest)))))

(define (cartesian-product lists)
  (if (null? lists)
      '(())
      (let ((tails (cartesian-product (cdr lists))))
        (collect-list (for first (in-list (car lists)))
                       (for rest (in-list tails))
          (cons first rest)))))

;;;; Pythagorean Triples

;;; I don't think there are any standard combinator idioms which can
;;; express this without kludging it worse than named LET.

;;; Named LET:

(define (pythagorean-triples n)
  (reverse
   (let ((n^2 (* n n)))
     (let a-loop ((a 1) (triples '()))
       (if (> a n)
           triples
           ((lambda (triples*)
              (a-loop (+ a 1) triples*))
            (let ((a^2 (* a a)))
              (let b-loop ((b a) (triples triples))
                (if (> b n)
                    triples
                    ((lambda (triples*)
                       (b-loop (+ b 1) triples*))
                     (let* ((b^2 (* b b))
                            (c^2 (+ a^2 b^2)))
                       (if (> c^2 n^2)
                           triples
                           (let c-loop ((c b) (triples triples))
                             (if (> c n)
                                 triples
                                 (c-loop (+ c 1)
                                         (if (= (* c c) c^2)
                                             (cons (list a b c)
                                                   triples)
                                             triples))))))))))))))))

;;; DO:

;;; Thanks to kilimanjaro for taking the time to grovel through this
;;; loop to find that I wrote (+ A 1) initially instead of (+ B 1),
;;; causing it to diverge.  I wish him a cookie.

(define (pythagorean-triples n)
  (reverse
   (do ((n^2 (* n n))
        (a 1 (+ a 1))
        (triples '()
                 (do ((a^2 (* a a))
                      (b a (+ b 1))
                      (triples triples
                               (let* ((b^2 (* b b))
                                      (c^2 (+ a^2 b^2)))
                                 (if (> c^2 n^2)
                                     triples
                                     (do ((c b (+ c 1))
                                          (triples triples
                                                   (if (= (* c c) c^2)
                                                       (cons
                                                        (list a b c)
                                                        triples)
                                                       triples)))
                                         ((> c n) triples))))))
                     ((> b n) triples))))
       ((> a n) triples))))

;;; Scheme48 ITERATE:

(define (pythagorean-triples n)
  (reverse
   (let ((n+1 (+ n 1))
         (n^2 (* n n)))
     (reduce ((count* a 1 n+1))
         ((triples '()))
       (let ((a^2 (* a a)))
         (reduce ((count* b a n+1))
             ((triples triples))
           (let* ((b^2 (* b b))
                  (c^2 (+ a^2 b^2)))
             (if (> c^2 n^2)
                 triples
                 (reduce ((count* c b n+1))
                     ((triples triples))
                   (if (= (* c c) c^2)
                       (cons (list a b c) triples)
                       triples))))))))))

;;; SRFI 42:

(define (pythagorean-triples n)
  (list-ec (:let n+1 (+ n 1))
           (:let n^2 (* n n))
           (:range a 1 n+1)
            (:let a^2 (* a a))
            (:range b a n+1)
             (:let b^2 (* b b))
             (:let c^2 (+ a^2 b^2))
             (if (<= c^2 n^2))
              (:range c b (+ n 1))
              (if (= (* c c) c^2))
   (list a b c)))

;;; foof-loop:

(define (pythagorean-triples n)
  (reverse
   (let ((n+1 (+ n 1))
         (n^2 (* n n)))
     (loop a-loop ((with triples '())
                   (for a (up-from 1 (to n+1))))
       => triples
       (a-loop
        (let ((a^2 (* a a)))
          (loop b-loop ((with triples triples)
                        (for b (up-from a (to n+1))))
            => triples
            (b-loop
             (let* ((b^2 (* b b))
                    (c^2 (+ a^2 b^2)))
               (if (> c^2 n^2)
                   triples
                   (loop c-loop ((with triples triples)
                                 (for c (up-from b (to n+1))))
                     => triples
                     (c-loop
                      (if (= (* c c) c^2)
                          (cons (list a b c) triples)
                          triples)))))))))))))

;;; foof-loop + comprehensions:

(define (pythagorean-triples n)
  (collect-list (let n+1 (+ n 1))
                (let n^2 (* n n))
                (for a (up-from 1 (to n+1)))
                 (let a^2 (* a a))
                 (for b (up-from a (to n+1)))
                  (let b^2 (* b b))
                  (let c^2 (+ a^2 b^2))
                  (if (<= c^2 n^2))
                  (for c (up-from b (to n+1)))
                   (if (= (* c c) c^2))
    (list a b c)))

;;;; Sieve of Eratosthenes

;;; Auxiliary macro to remove some unnecessary weight from the compared
;;; procedures.

(define-syntax with-prime-table
  (syntax-rules ()
    ((WITH-PRIME-TABLE size (prime? not-prime!) body0 body1 ...)
     (LET ((TABLE (MAKE-BIT-STRING (- size 2) #T)))
       (DEFINE (PRIME? K) (BIT-STRING-REF TABLE (- K 2)))
       (DEFINE (NOT-PRIME! K) (BIT-STRING-CLEAR! TABLE (- K 2)))
       body0 body1 ...))))

;;; Standard Scheme implementation of bit strings, if you actually
;;; want to run the code.

(define (make-bit-string size flag)
  (make-string size (if flag #\1 #\0)))

(define (bit-string-ref bs i) (char=? #\1 (string-ref bs i)))
(define (bit-string-set! bs i) (string-set! bs i #\1))
(define (bit-string-clear! bs i) (string-set! bs i #\0))

;;; Combinators, two-pass:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (let ((numbers (iota (- n 2) 2 1)))
      (for-each
       (lambda (i)
         (if (prime? i)
             (for-each not-prime!
                       (cdr (iota (quotient (- n 1) i) i i)))))
       numbers)
      (fold-right (lambda (i tail)
                    (if (prime? i)
                        (cons i tail)
                        tail))
                  '()
                  numbers))))

;;; Combinators, one-pass:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (reverse
     (fold (lambda (i tail)
             (if (prime? i)
                 (begin
                   (for-each not-prime!
                             (cdr (iota (quotient (- n 1) i) i i)))
                   (cons i tail))
                 tail))
           '()
           (iota (- n 2) 2 1)))))

;;; The above combinator versions use a very obscure application of
;;; IOTA to generate the list of primes to strike out.  They pessimally
;;; start at (+ I I), not at (* I I), because I have more important
;;; things to do than to obfuscate the IOTA incantations any further.

;;; DO, two-pass:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (do ((i 2 (+ i 1)))
        ((>= i n))
      (if (prime? i)
          (do ((j (* i i) (+ j i)))
              ((>= j n))
            (not-prime! j))))
    (do ((i 2 (+ i 1))
         (primes '() (if (prime? i) (cons i primes) primes)))
        ((>= i n)
         (reverse primes)))))

;;; DO, one-pass:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (define (purge-multiples i)
      (do ((j (* i i) (+ j i)))
          ((>= j n))
        (not-prime! j)))
    (do ((i 2 (+ i 1))
         (primes '()
                 (if (prime? i)
                     (begin (purge-multiples i)
                            (cons i primes))
                     primes)))
        ((>= i n)
         (reverse primes)))))

;;; Named LET, one-pass:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (define (purge-multiples i)
      (let loop ((j (* i i)))
        (if (< j n)
            (begin
              (not-prime! j)
              (loop (+ j i))))))
    (let recur ((i 2))
      (cond ((>= i n)
             '())
            ((prime? i)
             (purge-multiples i)
             (cons i (recur (+ i 1))))
            (else
             (recur (+ i 1)))))))

;;; SRFI 42, two-pass:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (do-ec (:range i 2 n)
            (if (prime? i))
            (:range j (* i i) n i)
      (not-prime! j))
    (list-ec (:range i 2 n)
              (if (prime? i))
      i)))

;;; SRFI 42, one-pass:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (define (purge-multiples i)
      (do-ec (:range j (* i i) n i)
        (not-prime! j)))
    (list-ec (:range i 2 n)
              (if (prime? i))
              (begin (purge-multiples i))
      i)))

;;; foof-loop, two-pass:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (loop ((for i (up-from 2 (to n))))
      (if (prime? i)
          (loop ((for j (up-from (* i i) (to n) (by i))))
            (not-prime! j))))
    (loop ((for i (up-from 2 (to n)))
           (for primes (listing i (if (prime? i)))))
      => primes)))

;;; foof-loop, one-pass with manual accumulation:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (define (purge-multiples i)
      (loop ((for j (up-from (* i i) (to n) (by i))))
        (not-prime! j)))
    (loop continue ((for i (up-from 2 (to n)))
                    (with primes '()))
      => (reverse primes)
      (if (prime? i)
          (begin (purge-multiples i)
                 (continue (=> primes (cons i primes))))
          (continue)))))

;;; foof-loop, one-pass with automatic accumulation:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (define (purge-multiples i)
      (loop ((for j (up-from (* i i) (to n) (by i))))
        (not-prime! j)))
    (loop ((for i (up-from 2 (to n)))
           (for primes
                (listing (begin (purge-multiples i) i)
                         (if (prime? i)))))
      => primes)))

;;; foof-loop + comprehensions, two-pass:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (loop ((for i (up-from 2 (to n))))
      (if (prime? i)
          (loop ((for j (up-from (* i i) (to n) (by i))))
            (not-prime! j))))
    (collect-list
        (for i (up-from 2 (to n)))
         (if (prime? i))
      i)))

;;; foof-loop + comprehensions, one-pass:

(define (sieve-of-eratosthenes n)
  (with-prime-table n (prime? not-prime!)
    (define (purge-multiples i)
      (loop ((for j (up-from (* i i) (to n) (by i))))
        (not-prime! j)))
    (collect-list
        (for i (up-from 2 (to n)))
         (if (prime? i))
         (do (purge-multiples i))
      i)))

;;;; Clause Parsing

;;; Named LET:

(define (parse-clauses clauses)
  (let loop ((clauses clauses)
             (open-clauses '())
             (access-clauses '())
             (for-syntax-clauses '())
             (files-clauses '())
             (other-clauses '()))
    (if (null? clauses)
        (values open-clauses
                access-clauses
                for-syntax-clauses
                files-clauses
                other-clauses)
        (let ((clause (car clauses))
              (clauses (cdr clauses)))
          (if (not (valid-clause? clause))
              (begin (warn "discarding invalid clause" clause)
                     (loop clauses
                           open-clauses
                           access-clauses
                           for-syntax-clauses
                           files-clauses
                           other-clauses))
              (case (car clause)
                ((OPEN)
                 (loop clauses
                       (cons clause open-clauses)
                       access-clauses
                       for-syntax-clauses
                       files-clauses
                       other-clauses))
                ((ACCESS)
                 (loop clauses
                       open-clauses
                       (cons clause access-clauses)
                       for-syntax-clauses
                       files-clauses
                       other-clauses))
                ((FOR-SYNTAX)
                 (loop clauses
                       open-clauses
                       access-clauses
                       (cons clause for-syntax-clauses)
                       files-clauses
                       other-clauses))
                ((FILES)
                 (loop clauses
                       open-clauses
                       access-clauses
                       for-syntax-clauses
                       (cons clause files-clauses)
                       other-clauses))
                (else
                 (loop clauses
                       open-clauses
                       access-clauses
                       for-syntax-clauses
                       files-clauses
                       (cons clause other-clauses)))))))))

;;; No standard unfolding combinators collect multiple lists.

;;; DO has no way to express loop-wide conditionals.

;;; With Scheme48's ITERATE, this code looks no different except for
;;; the mechanism of iteration through the list of clauses.

;;; SRFI 42 has no way to express multiple simultaneous collection.

;;; foof-loop:

(define (parse-clauses clauses)
  (loop continue ((for clause (in-list clauses))
                  (open-clauses '())
                  (access-clauses '())
                  (for-syntax-clauses '())
                  (files-clauses '())
                  (other-clauses '()))
    => (values open-clauses
               access-clauses
               for-syntax-clauses
               files-clauses
               other-clauses)
    (if (not (valid-clause? clause))
        (begin (warn "discarding invalid clause" clause)
               (continue))
        (case (car clause)
          ((OPEN)
           (continue (=> open-clauses (cons clause open-clauses))))
          ((ACCESS)
           (continue (=> access-clauses (cons clause access-clauses))))
          ((FOR-SYNTAX)
           (continue
            (=> for-syntax-clauses (cons clause for-syntax-clauses))))
          ((FILES)
           (continue (=> files-clauses (cons clause files-clauses))))
          (else
           (continue
            (=> other-clauses (cons clause other-clauses))))))))

;;; foof-loop + comprehensions has no way to express multiple
;;; simultaneous collection, at the moment.  (It may be possible to
;;; extend it, though...)
