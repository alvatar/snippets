;;;; hashes-macros.scm
;;;; Kon Lovett, Aug '06
;;;; Kon Lovett, Aug '10

;An Include File

;;

(define-constant DEFAULT-HASH-SEED 0)
(define-constant maximum-unsigned-integer32 4294967295)
(define-constant unsigned-integer32-size 4)

;;

(define-for-syntax (gen-algo-proc-sym name)
  (string->symbol (string-append (symbol->string (strip-syntax name)) "-primitive")) )

(define-for-syntax (gen-prim-proc-sym name)
  (string->symbol (string-append "*" (symbol->string (strip-syntax name)))) )

(define-for-syntax (gen-update-proc-sym name)
  (string->symbol (string-append (symbol->string (strip-syntax name)) "-update")) )

;;

(define-syntax (gen-hash-api f r c)
  (let ((?name (cadr f)))
    `(,(r 'begin)
       (,(r 'define) ,(gen-prim-proc-sym ?name)
         (,(r 'foreign-lambda) ,(r 'unsigned-integer32)
                               ,(symbol->string (strip-syntax ?name))
                               ,(r 'scheme-pointer)
                               ,(r 'unsigned-integer32)
                               ,(r 'unsigned-integer32)) )
       (,(r 'define) (,?name dat . args)
         (,(r 'let-optionals) args ((len (,(r '##sys#size) dat)) (initval 0))
           (,(gen-prim-proc-sym ?name) dat len initval) ) ) ) ) )

#;
(define-syntax (gen-update-proc f r c)
  (let ((?name (cadr f)))
    `(,(r 'define) ,(gen-update-proc-sym ?name)
       (,(r 'foreign-lambda*) ,(r 'void) ((,(r 'c-pointer) ctx)
                                          (,(r 'scheme-pointer) dat)
                                          (,(r 'unsigned-integer32) len))
         ,(string-append
            "((hashctx *) ctx)->hash = "
            (symbol->string (strip-syntax ?name))
            "( (uint8_t *) dat, (uint32_t) len, ((hashctx *) ctx)->hash );"))) ) )

(define-syntax (gen-update-proc f r c)
  (let ((?name (cadr f)))
    `(,(r 'define) ,(gen-update-proc-sym ?name)
       (,(r 'foreign-lambda*) void ((,(r 'c-pointer) ctx)
                                    (,(r 'scheme-pointer) dat)
                                    (,(r 'unsigned-integer32) len))
         ,(string-append
            "((hashctx *) ctx)->hash = "
            (symbol->string (strip-syntax ?name))
            "( (uint8_t *) dat, (uint32_t) len, ((hashctx *) ctx)->hash );"))) ) )

(define-syntax (gen-md-api f r c)
  (let ((?name (cadr f)))
    (let ((PN (gen-algo-proc-sym ?name))
          (UN (gen-update-proc-sym ?name)))
      `(,(r 'define) ,PN
        (,(r 'let) ((the-prim #f))
          (,(r 'lambda) ()
            (,(r 'or) the-prim
                (,(r 'begin)
                  (,(r 'set!) the-prim
                        (,(r 'make-message-digest-primitive)
                           ,(r 'hash-context-size)
                           ,(r 'unsigned-integer32-size)
                           ,(r 'generic-init)
                           ,UN
                           ,(r 'generic-final)
                           ',PN))
                  the-prim ) ) ) ) ) ) ) )
