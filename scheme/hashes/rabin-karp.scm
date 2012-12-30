;;;; rabin-karp.scm
;;;; Kon Lovett, Apr '06
;;;; Kon Lovett, Aug '10

(module rabin-karp

  (;export
    make-rabin-karp-string-search)

  (import scheme chicken)

  (use data-structures srfi-1 srfi-13 srfi-69 type-checks)

;;;

;; Rabin-Karp hashing string search.
;;
;; Takes a list of strings, the search substrings, and two optional arguments, an
;; equivalence function and a hash function.
;;
;; Returns a procedure of one argument, the search string. The search procedure
;; returns a list, the matching substring & a list of the start & end positions of
;; the match in the search string, or #f when no match.

(define (make-rabin-karp-string-search substrs #!optional (test string=?) (hash string-hash))
  ;
  (check-procedure 'make-rabin-karp-string-search test)
  (check-procedure 'make-rabin-karp-string-search hash)
  ; Return a matching procedure for the given search strings.
  (let (
      ; Total order of search string lengths.
      ; Ensures fewer character comparisons are performed to effect
      ; a match.
      (substrs-lens (sort! (map (cut string-length <>) substrs) <) )
      ; Search string lookup table
      (substrs-tbl (make-hash-table test hash) ) )
    ; Load search string lookup table from the search strings.
    (for-each (cut hash-table-set! substrs-tbl <> #t) substrs)
    ; Return a procedure returning the position of a matching search
    ; string in the target string, otherwise #f.
    (lambda (str #!optional (start 0) (end (string-length str)))
      (let (
          ; Any matching search string at this position?
          (match@
            (lambda (pos)
              (let (
                  ; Any matching search string of this length at this position?
                  (substr@
                    (lambda (sublen)
                      (let ((last (+ pos sublen)))
                        (and (<= last end)
                             (let ((substr (substring/shared str pos last)))
                               (and (hash-table-exists? substrs-tbl substr)
                                    `(,substr (,pos ,last)) ) ) ) ) ) ) )
                (let loop ((lens substrs-lens))
                  (and (not (null? lens))
                       (or (substr@ (car lens))
                           (loop (cdr lens)) ) ) ) ) ) ) )
        ; Any matching search string?
        (let loop ((pos start))
          (and (< pos end)
               (or (match@ pos)
                   (loop (+ pos 1)) ) ) ) ) ) ) )

) ;module rabin-karp
