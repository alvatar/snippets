;;; Copyright (c) 2009, Taylor Venable
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

;;; CVS Path: /Programs/Libraries/Scheme/json.scm
;;; Last Change: $Date$
;;; Revision: $Revision$



;;; This code implements a JSON parser to convert JSON objects into hash-tables
;;; and arrays into lists.  There are several extensions scattered throughout,
;;; labelled in the source code with EXTENSION.  Eventually I plan to bundle
;;; these up and allow them to be turned on / off.  Most JSON parsers only
;;; operate in a single mode and if you want to modify their behaviour you've
;;; got to hack it.  This particular implementation aims to make these things
;;; configurable without forcing you to edit the source.
;;;
;;; This code was written for use in Gambit, but is probably portable enough to
;;; be used elsewhere with little to no effort.  Requires the char-info
;;; "library" also available on metasyntax.net for determining character classes
;;; in the absence of an SRFI-14 implementation.



;(tcv-gambit-require '(char))



;; Read characters from <input> as long as they satisfy <pred>.
;; The result of the read, which is possibly the empty string, is returned.

(define read-while
  (lambda (input pred)
    (if (not (port? input))
        (error "can only read from a port")
        (let %get-until-space ((s ""))
          (let ((c (peek-char input)))
            (if (not (pred c input)) s
                (begin
                  (read-char input)     ; discard the character from the stream
                  (%get-until-space (string-append s (string c))))))))))

;; Get the next token from the input stream.  Returns a list where the car is
;; the token type and the cdr is the token text.

(define next-token
  (lambda (input)
    (if (not (port? input))
        (error "can only read tokens from a port")
        (let ((c (peek-char input)))
          (cond ((eof-object? c)
                 (list 'eof #f))
                ((char#space? c)
                 (read-char input)
                 (next-token input))
                ((char=? c #\{)
                 (read-char input)
                 (list 'open-brace #f))
                ((char=? c #\})
                 (read-char input)
                 (list 'close-brace #f))
                ((char=? c #\[)
                 (read-char input)
                 (list 'open-bracket #f))
                ((char=? c #\])
                 (read-char input)
                 (list 'close-bracket #f))
                ((char=? c #\,)
                 (read-char input)
                 (list 'comma #f))
                ((char=? c #\:)
                 (read-char input)
                 (list 'colon #f))
                ((or (char=? c #\-)
                     (char=? c #\+)
                     (char=? c #\.)
                     (char#digit? c))
                 (list 'number (lex-number input)))
                ((or (char=? c #\t)
                     (char=? c #\f))
                 (list 'boolean (lex-boolean input)))
                ((char=? c #\n)
                 (lex-null input) ; For error checking and to advance the stream.
                 (list 'null #f))
                ((char=? c #\")
                 (list 'string (lex-string input)))
                (else
                 (error "unexpected character" c)))))))

(define lex-literally
  (lambda (input literal)
    (if (null? literal) #t
    (let ((c (peek-char input)))
      (if (eof? input)
          (error "unexpected EOF"))
      (if (char=? c (car literal))
          (begin
            (read-char input)
            (lex-literally input (cdr literal)))
          (error "unexpected character" c))))))

(define lex-null
  (lambda (input)
    (if (lex-literally input (list #\n #\u #\l #\l))
        'null
        (error "UNREACHABLE: lex-literally returned false"))))

(define lex-boolean
  (lambda (input)
    (let ((c (peek-char input)))
      (if (eof? input)
          (error "unexpected EOF"))
      (cond ((char=? c #\t)
             (or (and (lex-literally input (list #\t #\r #\u #\e))
                      'true)
                 (error "UNREACHABLE: lex-literally returned false")))
            ((char=? c #\f)
             (or (and (lex-literally input (list #\f #\a #\l #\s #\e))
                      'false)
                 (error "UNREACHABLE: lex-literally returned false")))
            (else
             (error "no boolean here to lex"))))))

;; Read everything up until the end of a string, returning the result.
;; Signals an error when there is no string token next in the stream.

(define lex-string
  (lambda (input)
    (let ((next (peek-char input)))
      (if (or (eof-object? next)
              (not (char=? next #\")))
          (error "no string here to lex")
          (begin
            (read-char input)
            (let continue ((result "")
                           (last (peek-char input))
                           (current (read-char input)))
              (if (eof-object? current)
                  (error "unexpected EOF before end-of-string token"))
              (cond ((char=? current #\\)
                     (continue result current (read-char input)))
                    ((char=? current #\")
                     (if (char=? last #\\)
                         (continue (string-append result (string #\")) current (read-char input))
                         result))
                    (else
                     (continue (string-append result (string current)) current (read-char input))))))))))

(define char->number
  (lambda (c)
    (if (not (char? c))
        (error "can't convert non-char to number"))
    (- (char->integer c) (char->integer #\0))))

(define eof?
  (lambda (input)
    (if (not (input-port? input))
        (error "can't check for EOF when not an input port"))
    (eof-object? (peek-char input))))

(define lex-number-integral
  (lambda (input)
    (let ((next (peek-char input)))
      (if (eof? input)
          (error "no number here to lex"))

      ; EXTENSION - allow number format like ".5"
      (if (not (char=? next #\.))
          (begin
            (let %lex-number-integral
                ((result 0)
                 (current (peek-char input)))
              (if (eof-object? current)
                  result
                  (if (char#digit? current)
                      (begin
                        (read-char input)
                        (%lex-number-integral (+ (* result 10) (char->number current))
                                              (peek-char input)))
                      result))))
          0))))

(define lex-number-fractional
  (lambda (input)
    (let ((next (peek-char input)))
      (if (eof? input)
          (error "no fractional part here to lex"))
      (if (not (char=? next #\.))
          (error "no fractional part here to lex"))
      (read-char input)
      ; EXTENSION - allow numbers to end with a decimal = zero fractional part.
      (let %lex-number-fractional ((result 0)
                                   (position 10)
                                   (current (peek-char input)))
        (if (eof-object? current)
            result
            (if (char#digit? current)
                (begin
                  (read-char input)
                  (%lex-number-fractional (+ (exact->inexact (/ (char->number current) position)))
                                          (* position 10)
                                          (peek-char input)))
                result))))))

(define lex-number-exponent
  (lambda (input)
    (let ((next (peek-char input)))
      ; Look for start of exponent marker.
      (if (or (eof? input)
              (and (not (char=? next #\e))
                   (not (char=? next #\E))))
          (error "no exponent here to lex"))

      (begin
        (read-char input)

        ; EOF after 'e' or 'E' is invalid.
        (if (eof? input)
            (error "unexpected EOF"))

        (begin
          (set! next (peek-char input))
          (let ((sign (cond ((char=? (peek-char input) #\-) (read-char input) 'negative)
                            ((char=? (peek-char input) #\+) (read-char input) 'positive)
                            (else 'positive))))

            ; EOF after a sign only means fail.
            (if (eof? input)
                (error "unexpected EOF"))

            (let continue ((value 0)
                           (current (peek-char input)))

              ; EOF at this point means we've finished.
              (if (eof? input)
                  (if (eqv? sign 'negative) (* value -1) value)

                  (if (char#digit? current)
                      (begin
                        (read-char input)
                        (continue (+ (* value 10) (char->number current)) (peek-char input)))
                      (if (eqv? sign 'negative) (* value -1) value))))))))))

(define lex-number
  (lambda (input)
    (let ((next (peek-char input)))
      (let ((sign (cond ((char=? next #\-) (read-char input) 'negative)
                        ; EXTENSION - allow explicit positive numbers.
                        ((char=? next #\+) (read-char input) 'positive)
                        (else 'positive)))
                (value 0)
                (exponent 0))

        (set! value (lex-number-integral input))

        (if (and (not (eof? input))
                 (char=? (peek-char input) #\.))
            (set! value (+ value (lex-number-fractional input))))

        (if (and (not (eof? input))
                 (or (char=? (peek-char input) #\e)
                     (char=? (peek-char input) #\E)))
            (set! exponent (lex-number-exponent input)))

        (let ((value (* value (expt 10 exponent))))
          (if (eqv? sign 'positive) value
              (* value -1)))))))

;;; ============================================================================
;;;  PARSER CODE
;;; ============================================================================

(define parse-object
  (lambda (input)
    (let %parse-object ((data (make-table)))
      (let ((key   (next-token input))
            (colon (next-token input))
            (value (next-token input)))

        (if (not (eq? (car key) 'string))
          (error "map keys must be strings"))
        (if (not (eq? (car colon) 'colon))
          (error "map keys and values must be separated by colons"))

        (set! value (parse-continue input value))
        (table-set! data (cadr key) value)

        (let ((next (next-token input)))
          (cond ((eq? (car next) 'comma) (%parse-object data))
                ((eq? (car next) 'close-brace) data)
                (else (error "unexpected token; expected comma or close-brace, found "
                             (symbol->string (car next))))))))))

(define parse-list
  (lambda (input)
    (let %parse-list ((data '()))
      (let ((current (next-token input)))
        (if (eq? (car current) 'close-bracket)
          (list)
          (let ((datum (parse-continue input current))
                (next  (next-token input)))
            (cond ((eq? (car next) 'eof) (error "unexpected EOF"))
                  ((eq? (car next) 'comma) (%parse-list (cons datum data)))
                  ((eq? (car next) 'close-bracket) (reverse (cons datum data)))
                  (else (error "unexpected token; expected comma or close-bracket, found "
                               (symbol->string (car next)))))))))))

(define parse-continue
  (lambda (input current)
    (let ((type  (car current))
          (value (cadr current)))
      (cond ((eq? type 'open-bracket) (parse-list input))
            ((eq? type 'open-brace) (parse-object input))
            (else value)))))

(define parse
  (lambda (input)
    (let ((first (read-char input)))
      (cond ((char=? first #\{) (parse-object input))
            ((char=? first #\[) (parse-list input))
            (else (error "unexpected token; expected open-brace or open-bracket, found " (string first)))))))

;;; ============================================================================
;;;  ACCESS FUNCTIONS
;;; ============================================================================

(define-syntax get-json
  (syntax-rules (-> @)
    ((_ json (key))
     (and (pair? json) (assoc key json) (cdr (assoc key json))))
    ((_ json (@ index))
     (and (pair? json) (list-ref json index)))
    ((_ json (key1 -> key2 ...))
     (get-json (get-json json (key1)) (key2 ...)))
    ((_ json (key @ index ...))
     (get-json (get-json json (key)) (@ index ...)))
    ((_ json (@ index -> key ...))
     (get-json (get-json json (@ index)) (key ...)))
    ((_ json (@ index1 @ index2 ...))
     (get-json (get-json json (@ index1)) (@ index2 ...)))))

;;; ============================================================================
;;;  TEST CODE
;;; ============================================================================

(define tests-number
  '(("42"    (number 42))
    ("-42"   (number -42))
    ("+42"   (number 42))
    ("42."   (number 42))
    ("0"     (number 0))
    ("+0"    (number 0))
    ("-0"    (number 0))
    (".5"    (number 0.5))
    ("0.5"   (number 0.5))
    ("+.5"   (number 0.5))
    ("-.5"   (number -0.5))
    ("2e0"   (number 2))
    ("2e1"   (number 20))
    ("2E0"   (number 2))
    ("2E1"   (number 20))
    ("2e+0"  (number 2))
    ("2e-0"  (number 2))
    ("2.e0"  (number 2))
    ("2.e+0" (number 2))
    ("2.e-0" (number 2))))

(define tests-string
  '(("\"text\""       (string "text"))
    ("\"foo\\\"bar\"" (string "foo\"bar"))
    ("\"foo"          (error))))

(define tests-null
  '(("null"           (null))
    ("nullx"          (error))
    ("nul"            (error))
    ("foo"            (error))))

(define tests-boolean
  '(("true"           (boolean true))
    ("false"          (boolean false))
    ("truex"          (error))
    ("falsex"         (error))
    ("tru"            (error))
    ("fals"           (error))
    ("xyzzy"          (error))))

(define tests-list
  '(("[[[[[[[[[[[[[[[[[[[[[[[[[[]]]]]]]]]]]]]]]]]]]]]]]]]]"
     (((((((((((((((((((((((((()))))))))))))))))))))))))))
    ("[]"               ())
    ("[1]"              (1))
    ("[1, 2]"           (1 2))
    ("[1, [2]]"         (1 (2)))
    ("["                (error))
    ("[1"               (error))
    ("[1,"              (error))
    ("[1,]"             (1))
    ("[1,2"             (error))
    ("[1,2,"            (error))
    ("[1,2,]"           (1 2))))

(define tests-run
  (lambda (test-fun which . show-errors)
    (map (lambda (test)
           (let ((result (with-exception-catcher
                          (lambda (e)
                            (if (and (not (null? show-errors))
                                     (car show-errors))
                                (cond ((error-exception? e)
                                       (display (error-exception-message e) (current-error-port)))))
                            '(error))
                          (lambda () (call-with-input-string (car test) test-fun)))))
             (if (equal? result (cadr test))
                 (display ".")
                 (begin
                   (display "\nE[")
                   (display (car test))
                   (display " => ")
                   (display result)
                   (display " / ")
                   (display (cadr test))
                   (display "]"))))) which)
    (newline)))
