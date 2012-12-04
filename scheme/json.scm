; JSON serializer/deserializer
; Taken from PLT 3.0
; Updates copyright (c) 2010-2012 Mikael More
; Blackhole version and packaging (c) 2012 Álvaro Castro-Castilla

; Works both in Gambit and Bigloo.

; Several hacks to make it work in Bigloo: [..]:s replaced with (..), and two #\sth replaced.

; Exports: (object->json-string x) (json-string->object s)

; Example use:
; (object->json-string '(1 2 3)) => "[1, 2, 3]"
; (json-string->object "true") => #t

; Can be made Much faster by replacing the use of I/O routines with a string and string-ref.

; (define peek-char* ##peek-char)
; (define read-char* ##read-char)
; (define (read-char . a) (let ((v (apply read-char* a))) (print "read-char got " v "\n") v))
; (define (peek-char . a) (let ((v (apply peek-char* a))) (print "peek-char got " v "\n") v))

; # History
;  - 2010-10-12: Added support to read negative numbers
;  - 2011-03-02: Added support to write hashtables
;  - 2011-03-29: Made so that numbers ending with a dot, are serialized to a form without dot
;                I.e. (object->json-string 7.) => "7"
;  - 2012-01-30: Simplified procedure names from jsexpr->json and json->jsexpr to
;                object->json-string and json-string->object .

;(import ../util/util)

; (cond-expand (bigloo (define open-unencoded-output-string open-output-string)))

(define-macro (unless* cond . code)
  `(if (not ,cond) (begin ,@code)))

(define-macro (begin0 exp1 . rest)
  (let ((v (gensym)))
    `(let ((,v ,exp1))
       ,@rest
       ,v)))

(define-macro (for/list decl processor)
  `(let ((,(caar decl) (let ((v ,(cadar decl))) (if (list? v) v (string->list v)))))
     (map (lambda (,(caar decl)) ,processor) ,(caar decl))))

(define-macro (when* cond . stuff)
  `(if ,cond (begin ,@stuff)))

(define (for-each-w-idx* thunk list)
  (let loop ((list list) (idx 0))
    (if (not (null? list))
        (begin
          (thunk idx (car list))
          (loop (cdr list) (+ idx 1))))))

(define (write-json json #!optional (port (current-output-port)))
  (cond
   [(table? json)
    (display "{" port)
    (let ((i 0))
      (table-for-each
       (lambda (key value)
         (if (> i 0)
             (display ", " port))
         (set! i (+ i 1))
         (print port: port "\"" key "\":") ; (fprintf port "\"[tilde]a\"" key) (display ": " port)
         (write-json value port))
       json)
      (display "}" port))]
    ((list? json)
     (display "[" port)
     ; (for ([(value i) (in-indexed json)])
     (for-each-w-idx* (lambda (i value)
                       (when* (> i 0)
                         (display ", " port))
                       (write-json value port))
                     json)
     (display "]" port))
    ((and (number? json) (inexact? json))
     (let* ((s (number->string json))
            (s-length (string-length s)))
       (display (if (eq? #\. (string-ref s (- s-length 1)))
                    (substring s 0 (- s-length 1))
                    s)
                port)))
    ((or (string? json) (and (number? json) (or (integer? json) (inexact? json))))
     (write json port))
    ((boolean? json) (write (if json 'true 'false) port))
    ((null-jsexpr? json) (write 'null port))
    (else (error 'json "bad json value:" json))))

(define (read-json #!optional (port (current-input-port)))
  ; (print "(read-json) got " (peek-char port) "\n")
  (case (peek-char port)
    ((#\{) (read/hash port))
    ((#\[) (read/list port))
    ((#\") (read/string port))
    ((#\t) (read/true port))
    ((#\f) (read/false port))
    ((#\n) (read/null port))
    (else (read/number port))))

(define (expect ch . expected)
  (unless* (memq ch expected)
    (error 'read "expected / got:" expected ch))
  ch)

(define (expect-string port expected)
  (list->string (for/list ((ch expected))
                  (expect (read-char port) ch))))

(define (skip-whitespace port)
  (let ((ch (peek-char port)))
    (when* (and (not (eof-object? ch)) (char-whitespace? ch))
      (read-char port)
      (skip-whitespace port))))

(define (in-port-until port reader done?)
  (let loop ((r '()))
    ;(let ((v (reader port)))
    ;  (if (done? port)
    ;      (reverse (cons v r))
    ;      (loop (cons v r))))
      (if (done? port)
          (reverse r)
          (let ((v (reader port)))
          (loop (cons v r))))))

(define (read/hash port)
  (expect (read-char port) #\{)
  (skip-whitespace port)
  (begin0 (for/hasheq (((key value)
                        (in-port-until port
                                       (lambda (port)
                                         (let ((key (read/string port)))
                                           (unless* (string? key)
                                             (error 'read "expected: string, got:" key))
                                           (skip-whitespace port)
                                           (expect (read-char port) #\:)
                                           (skip-whitespace port)
                                           (let ((value (read-json port)))
                                             (skip-whitespace port)
                                             (expect (peek-char port) #\, #\})
                                             (values (string->symbol key) value))))
                                       (lambda (port)
                                         (eq? (peek-char port) #\})))))
            (when* (eq? (peek-char port) #\,)
              (read-char port))
            (skip-whitespace port)
            (values key value))
          (expect (read-char port) #\})))

(define (read/list port)
  (expect (read-char port) #\[)
  (begin0 (for/list ((value
                      (in-port-until port
                                     (lambda (port)
                                       (skip-whitespace port)
                                       (begin0
                                        (read-json port)
                                        ; (let ((v (read-json port))) (print "(read-json port) of read/list gave ") (write v) (print "\n") v)
                                        (skip-whitespace port)
                                        (expect (peek-char port) #\, #\])

                                        (when* (eq? (peek-char port) #\,)
                                               (read-char port))
                                        ))
                                     (lambda (port)
                                       (eq? (peek-char port) #\])))))
            
            value)
          (expect (read-char port) #\])))

(define (read/string port)
  (expect (read-char port) #\")
  (begin0 (list->string
           (for/list ((ch (in-port-until port
                                         (lambda (port)
                                           (let ((ch (read-char port)))
                                             (when* (eof-object? ch)
                                               (error 'read "unexpected EOF"))
                                             (if (eq? ch #\\)
                                                 (let ((esc (read-char port)))
                                                   (when* (eof-object? ch)
                                                     (error 'read "unexpected EOF"))
                                                   (case esc
                                                     ((#\b) (integer->char 8)) ; for Bigloo. from #\backspace.
                                                     ((#\n) #\newline)
                                                     ((#\r) #\return)
                                                     ((#\f) (integer->char 12)) ; for Bigloo. from #\page.
                                                     ((#\t) #\tab)
                                                     ((#\\) #\\)
                                                     ((#\") #\")
                                                     ((#\/) #\/)
                                                     ((#\u) (unescape (let ((s "    ")) ; Was: (read-string 4 port). This
                                                                        (read-substring s 0 4 port) ; should do the work.
                                                                        s)))
                                                     (else esc)))
                                                 ch)))
                                         (lambda (port)
                                           (eq? (peek-char port) #\")))))
             ch))
          (expect (read-char port) #\")))

(define (unescape str)
  ; (unless* (regexp-match #px"[a-fA-F0-9]{4}" str)
  ;   (error 'read "bad unicode escape sequence: \"\\u[tilde]a\"" str))
  (integer->char (string->number str 16)))

(define (read/true port)
  (expect-string port "true")
  #t)

(define (read/false port)
  (expect-string port "false")
  #f)

(define (read/null port)
  (expect-string port "null")
  null-jsexpr)

(define (read/digits port)
  (let ((digits (for/list ((digit (in-port-until port
                                                 read-char
                                                 (lambda (port)
                                                   (let ((ch (peek-char port)))
                                                     (or (eof-object? ch)
                                                         (not (char-numeric? ch))))))))
                  digit)))
    (when* (and (null? digits) (eof-object? (peek-char port)))
      (error 'read "unexpected EOF"))
    (when* (null? digits)
      (error 'read "expected: digits, got:" (peek-char port)))
    digits))

(define (read/exponent port)
  (expect (read-char port) #\e #\E)
  (let ((sign (case (peek-char port)
                ((#\- #\+) (list (read-char port)))
                (else '()))))
    (append sign (read/digits port))))

(define (read/number port)
  (let* ((sign (if (eq? (peek-char port) #\-) (begin (read-char port) '(#\-)) '()))
         (digits (read/digits port))
         (frac (if (eq? (peek-char port) #\.) (read/digits port) '()))
         (exp (if (memq (peek-char port) '(#\e #\E)) (read/exponent port) '())))
    (string->number
     (list->string
      (append sign digits frac exp)))))

(define (jsexpr? x)
  (or (integer? x)
      (and (number? x) (inexact? x))
      (null-jsexpr? x)
      (boolean? x)
      (string? x)
      (null? x)
      (array-jsexpr? x)
      (object-jsexpr? x)))

(define (array-jsexpr? x)
  (or (null? x)
      (and (pair? x)
           (jsexpr? (car x))
           (array-jsexpr? (cdr x)))))

(define (object-jsexpr? x)
  (let/ec return
    (and (hash? x)
         (for (((key value) x))
           (unless* (and (symbol? key) (jsexpr? value))
             (return #f)))
         #t)))

(define (null-jsexpr? x)
  (eq? x 'null) ; originally: (eqv? x # \null)
  )

(define null-jsexpr 'null) ; originally: #\null)

(define (object->json-string x)
  (let ((out (open-unencoded-output-string)))
    (write-json x out)
    (get-output-string out)))

(define (json-string->object s)
  (let ((in (open-input-string s)))
    (read-json in)))
