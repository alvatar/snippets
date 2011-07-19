;;; -*- Mode: Scheme -*-

;;;; Extensible Looping Macros, version 8

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; This is a variation on Alex Shinn's looping macros described in
;;; message-id <1157562097.001179.11470@i42g2000cwa.googlegroups.com>.
;;; It has diverged substantially from the original macros, and is now
;;; documented at <http://mumble.net/~campbell/scheme/foof-loop.txt>.
;;;
;;; This file depends on syn-param.scm, also by Taylor R. Campbell, and
;;; SRFI 11 (LET-VALUES).  Ideally, the implementation of LET-VALUES
;;; should gracefully handle single-value clauses to elide superfluous
;;; uses of CALL-WITH-VALUES.

;;; [This file has been modified to include both of the dependencies,
;;; which start on the next page.]

;;; -*- Mode: Scheme -*-

;;;; Operators with Extended Parameter Syntax

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

;;; Example:
;;;
;;;   (define (foo x y z) ...)
;;;
;;;   (with-extended-parameter-operators
;;;       ((foo* (foo (x . 0) (y . 0) (z . 0))))
;;;     (foo* 5 (=> z 3) (=> y 1)))
;;; <=>
;;;   (foo 5 1 3)

;;; I have *voluminously* commented this hideous macro of astonishing
;;; complexity in the hopes that it can be read by any other than
;;; macrological deities.  I use syntactic continuation-passing style
;;; in one small place, for a discussion of which the reader should see
;;; the Hilsdale paper; everything else is just mutually tail-recursive
;;; local macros.

;;; The question mark prefix indicates pattern variables.
;;; The number of question marks indicates the nesting depth
;;; of the macro which introduced the pattern variable.
;;; An asterisk marks a syntactic continuation's environment.

(module foof-loop
 (loop lazy-loop listing listing-reverse listing! listing-into!
appending appending-reverse summing multiplying maximizing minimizing
in-list in-lists in-vector in-vector-reverse in-string
in-string-reverse in-port in-file up-from down-from %cars&cdrs)

(import scheme chicken)

(define-syntax with-extended-parameter-operators
  (syntax-rules ()
    ((with-extended-parameter-operators
         ((?labelled-argument-macro-name
           (?positional-form-name (?parameter . ?default)
                                  ...))
          ...)
       ?body0
       ?body1
       ...)

     (letrec-syntax
         ((?labelled-argument-macro-name
           (syntax-rules ()
             ((?labelled-argument-macro-name . ??arguments)
              (letrec-syntax
                  ((apply-positional
                    (syntax-rules ()
                      ((apply-positional ???positionals)
                       (reverse-apply ?positional-form-name ???positionals))))

                   ;; Process all of the leading positional arguments.
                   ;; Once we reach a named argument, pass control on
                   ;; to PROCESS-NAMED.
                   ;;
                   ;; ???PARAMETERS is the list of remaining parameter
                   ;; specifiers (i.e. (parameter . default)) to
                   ;; process, in order.
                   ;;
                   ;; ???POSITIONALS is the current reversed list of
                   ;; positional argument expressions accumulated.
                   ;;
                   ;; ???ARGUMENTS is the list of remaining argument
                   ;; expressions in the input.
                   (process-positionals
                    (syntax-rules (=>)

                      ;; No more parameters -- ignore the remaining
                      ;; arguments (signal a syntax error?), and just
                      ;; do positional application.  There were no
                      ;; named arguments.
                      ((process-positionals () ???positionals . ???arguments)
                       (apply-positional ???positionals))

                      ;; No more positional arguments; fill in default
                      ;; values for the remaining parameters.
                      ((process-positionals ???parameters ???positionals)
                       (process-defaults ???parameters ???positionals))

                      ;; Named argument -- move on to
                      ;; PROCESS-NAMED.
                      ((process-positionals ???parameters
                                            ???positionals
                                            (=> ???parameter ???argument)
                                            . ???arguments)
                       (process-named ???parameters
                                      ???positionals
                                      (=> ???parameter ???argument)
                                      . ???arguments))

                      ;; Positional argument -- accumulate and
                      ;; proceed.
                      ((process-positionals (???parameter . ???parameters)
                                            ???positionals
                                            ???positional
                                            . ???arguments)
                       (process-positionals ???parameters
                                            (???positional . ???positionals)
                                            . ???arguments))))

                   ;; If we ran out of positional arguments, for each
                   ;; remaining parameter specifier, fill in its
                   ;; default expression.
                   (process-defaults
                    (syntax-rules ()

                      ((process-defaults () ???positionals)
                       (apply-positional ???positionals))

                      ((process-defaults ((???parameter . ???default)
                                          . ???parameters/defaults)
                                         ???positionals)
                       (process-defaults ???parameters/defaults
                                         (???default . ???positionals)))))

                   ;; Find the named argument corresponding with each
                   ;; parameter specifier, in order.
                   ;;
                   ;; ???PARAMETERS is the list of remaining parameter
                   ;; specifiers to process, in order.
                   ;;
                   ;; ???POSITIONALS is the currently accumulated list
                   ;; of positional argument expressions, in reverse
                   ;; order.
                   ;;
                   ;; ???ARGUMENTS is the list of remaining arguments
                   ;; to process.  No more positional arguments are
                   ;; allowed at this point in the game, and we never
                   ;; take anything off of this list.
                   (process-named
                    (syntax-rules ()

                      ;; No more pararmeters -- apply.
                      ((process-named () ???positionals . ???arguments)
                       (apply-positional ???positionals))

                      ;; No more arguments -- fill in defaults.
                      ((process-named ???parameters ???postionals)
                       (process-defaults ???parameters ???positionals))

                      ;; Match up this parameter with its argument
                      ;; expression; then go on with the remaining
                      ;; parameters, and all of the arguments.
                      ((process-named ((???parameter . ???default)
                                       . ???parameters)
                                      ???positionals
                                      . ???arguments)
                       (match-parameter-by-name
                        ???arguments
                        ???parameter
                        ???default
                        (process-named-continuation ???positionals
                                                    ???parameters
                                                    . ???arguments)))))

                   ;; Continuation for the named parameter matcher.
                   ;; When we get a value, add it to the saved list of
                   ;; positionals, and proceed with the saved list of
                   ;; remaining parameter specifiers, and the saved
                   ;; list of argument expressions.
                   (process-named-continuation
                    (syntax-rules ()
                      ((process-named-continuation ???value
                                                   ???positionals*
                                                   ???parameters*
                                                   . ???arguments*)
                       (process-named ???parameters*
                                      (???value . ???positionals*)
                                      . ???arguments*))))

                   ;; Find the named argument corresponding with a
                   ;; parameter specifier.  If none exists, use the
                   ;; default given.
                   (match-parameter-by-name
                    (syntax-rules (=> ?parameter ...)

                      ;; For each of the possible named parameters, if
                      ;; it matches this one, use it -- add the
                      ;; corresponding argument expression to the list
                      ;; of positionals.
                      ((match-parameter-by-name
                        ((=> ?parameter ???value) . ???arguments)
                        ?parameter
                        ???default
                        (???continuation . ???environment))
                       (???continuation ???value . ???environment))
                      ...               ;***

                      ;; Argument does not match -- skip it.
                      ((match-parameter-by-name (???argument . ???arguments)
                                                ???parameter
                                                ???default
                                                ???continuation)
                       (match-parameter-by-name ???arguments
                                                ???parameter
                                                ???default
                                                ???continuation))

                      ;; No more arguments -- use the default.
                      ((match-parameter-by-name
                        ()
                        ???parameter
                        ???default
                        (???continuation . ???environment))
                       (???continuation ???default . ???environment))))

                   ;; Apply ???OPERATOR to the reversal of the arguments.
                   (reverse-apply
                    (syntax-rules ()

                      ((reverse-apply ???operator ???reversed-arguments)
                       (reverse-apply ???operator ???reversed-arguments ()))

                      ((reverse-apply ???operator
                                      (???argument . ???more)
                                      ???arguments)
                       (reverse-apply ???operator
                                      ???more
                                      (???argument . ???arguments)))

                      ((reverse-apply ???operator () ???arguments)
                       (???operator . ???arguments)))))

                ;; Start the whole process.
                (process-positionals ((?parameter . ?default) ...)
                                     ()
                                     . ??arguments)))))
          ...)

       ?body0
       ?body1
       ...))))

;;; -*- Mode: Scheme -*-

;;;; Multiple-Value Binding Macros

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (receive-values consumer producer)
  (call-with-values producer consumer))

(define-syntax receive
  (syntax-rules ()
    ((receive (variable) producer body0 body1+ ...)
     (let ((variable producer)) body0 body1+ ...))

    ((receive bvl producer body0 body1+ ...)
     (call-with-values (lambda () producer)
       (lambda bvl body0 body1+ ...)))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body0 body1+ ...)
     (let () body0 body1+ ...))

    ((let*-values ((bvl producer)) body0 body1+ ...)
     (receive bvl producer body0 body1+ ...))

    ((let*-values ((bvl0 producer0)
                   (bvl1+ producer1+)
                   ...)
       body0
       body1+
       ...)
     (receive bvl0 producer0
       (let*-values ((bvl1+ producer1+) ...)
         body0
         body1+
         ...)))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values () body0 body1+ ...)
     (let () body0 body1+ ...))

    ((let-values ((bvl producer)) body0 body1+ ...)
     (receive bvl producer body0 body1+ ...))

    ((let-values ((bvl producer) ...) body0 body1+ ...)
     (let-values/process-clauses ((bvl producer) ...)
                                 (let-values/make-output body0 body1+ ...)))))

(define-syntax let-values/process-clauses
  (syntax-rules ()
    ((let-values/process-clauses clauses continuation)
     (let-values/process-clauses () () clauses continuation))

    ((let-values/process-clauses let-bindings
                                 bvls&producers
                                 ()
                                 (continuation . environment))
     (continuation let-bindings bvls&producers . environment))

    ((let-values/process-clauses let-bindings
                                 bvls&producers
                                 (((variable) producer) . clauses)
                                 continuation)
     (let-values/process-clauses ((variable producer) . let-bindings)
                                 bvls&producers
                                 clauses
                                 continuation))

    ((let-values/process-clauses let-bindings
                                 bvls&producers
                                 ((bvl producer) . clauses)
                                 continuation)
     (let-values/generate-temporaries
      bvl
      (let-values/continuation producer
                               let-bindings
                               bvls&producers
                               clauses
                               continuation)))))

(define-syntax let-values/continuation
  (syntax-rules ()
    ((let-values/continuation bvl
                              (let-binding ...)
                              producer
                              let-bindings
                              bvls&producers
                              clauses
                              continuation)
     (let-values/process-clauses (let-binding ... . let-bindings)
                                 ((bvl producer) . bvls&producers)
                                 clauses
                                 continuation))))

(define-syntax let-values/generate-temporaries
  (syntax-rules ()
    ((let-values/generate-temporaries bvl continuation)
     (let-values/generate-temporaries () () bvl continuation))

    ((let-values/generate-temporaries (bvl-out ...)
                                      let-bindings
                                      (variable . bvl-in)
                                      continuation)
     (let-values/generate-temporaries (bvl-out ... temporary)
                                      ((variable temporary) . let-bindings)
                                      bvl-in
                                      continuation))

    ((let-values/generate-temporaries bvl
                                      let-bindings
                                      ()
                                      (continuation . environment))
     (continuation bvl let-bindings . environment))

    ((let-values/generate-temporaries (bvl ...)
                                      let-bindings
                                      rest-variable
                                      (continuation . environment))
     (continuation (bvl ... . temporary)
                   ((rest-variable temporary) . let-bindings)
                   . environment))))

(define-syntax let-values/make-output
  (syntax-rules ()
    ((let-values/make-output let-bindings () . body)
     (let let-bindings . body))

    ((let-values/make-output let-bindings
                             ((bvl producer) . bvls&producers)
                             . body)
     (receive bvl producer
       (let-values/make-output let-bindings bvls&producers . body)))))

;;; [Here starts the actual foof-loop.scm.]

(define-syntax loop
  (syntax-rules ()
    ((loop ((loop-clause0 loop-clause1 ...) ...)
       body
       ...)
     (loop anonymous-loop ((loop-clause0 loop-clause1 ...) ...)
       body
       ...
       (anonymous-loop)))

    ((loop name ((loop-clause0 loop-clause1 ...) ...) body ...)
     (%loop start name ((loop-clause0 loop-clause1 ...) ...) (body ...)))))

;;; We must be very careful about where to add laziness annotations.
;;; In particular, we don't want to wrap only the loop's body, because
;;; if we did that, the outer bindings produced by the iterators would
;;; be evaluate eagerly, which is too soon.  So instead, we wrap the
;;; whole thing in a LAZY, and then wrap every call to the loop as
;;; well.

(define-syntax lazy-loop
  (syntax-rules (=>)
    ((lazy-loop name (iterator ...) => result body0 body1 ...)
     (lazy (loop eager-loop (iterator ...)
             => result
             (let-syntax ((name
                           (syntax-rules ()
                             ((name . arguments)
                              (lazy (eager-loop . arguments))))))
               body0 body1 ...))))))

;;; Use this definition of SYNTACTIC-ERROR if your favourite Scheme
;;; doesn't have one already.  Note that this is distinct from a
;;; SYNTAX-ERROR procedure, since it must signal a compile-time error.

;;; [Kludge for Chicken.]

(define-syntax syntactic-error
  (syntax-rules (SYNTAX-ERROR)
    ((syntactic-error SYNTAX-ERROR)
     'SYNTAX-ERROR)))

;;; Utility for reporting syntax errors in LOOP clauses.

(define-syntax loop-clause-error
  (syntax-rules ()
    ((loop-clause-error (macro (variable ...) arguments message))
     (syntactic-error message (for variable ... (macro . arguments))))))

;;;; The Guts of LOOP

(define-syntax %loop
  (syntax-rules (=> for with let let-values while until
                    start go parse-for continue finish simplify-body)

    ((%loop start name loop-clauses body)
     (%loop go name (() () () () () () () ()) loop-clauses body))

    ;; Simple case of a single variable, for clarity.
    ((%loop go name state
            ((for variable (looper argument ...))
             . loop-clauses)
            body)
     (looper (variable) (argument ...)
             %loop continue name state loop-clauses body))

    ;; FOR handler with tail patterns.  Unfortunately, tail patterns are non-
    ;; standard...
    ;; 
    ;; ((%loop go name state
    ;;         ((for variable0 variable1 ... (looper argument ...))
    ;;          . loop-clauses)
    ;;         body)
    ;;  (looper (variable0 variable1 ...)
    ;;          (argument ...)
    ;;          %loop continue name state loop-clauses body))

;;;;; FOR Clauses: Dealing with Iterators

    ((%loop go name state
            ((for variable0 variable1 variable2 ...) . loop-clauses)
            body)
     (%loop parse-for (variable0 variable1 variable2 ...)
            ()
            (for variable0 variable1 variable2 ...)   ;Copy for error message.
            name state loop-clauses body))

    ((%loop parse-for ((looper argument ...))
            variables
            original-clause name state loop-clauses body)
     (looper variables (argument ...)
             %loop continue name state loop-clauses body))

    ((%loop parse-for (next-variable more0 more1 ...)
            (variable ...)
            original-clause name state loop-clauses body)
     (%loop parse-for (more0 more1 ...)
            (variable ... next-variable)
            original-clause name state loop-clauses body))

    ((%loop parse-for (non-list)
            variables
            original-clause name state loop-clauses body)
     (syntactic-error "Malformed FOR clause in LOOP:" original-clause))

    ((%loop ((outer-bvl outer-producer) ...)
            ((loop-variable loop-initializer loop-stepper) ...)
            ((entry-bvl entry-producer) ...)
            (termination-condition ...)
            ((body-bvl body-producer) ...)
            ((final-bvl final-producer) ...)
            continue
            name
            ((loop-variables ...)
             user-bindings
             user-termination-conditions
             outer-bindings
             entry-bindings
             termination-conditions
             body-bindings
             final-bindings)
            loop-clauses
            body)
     (%loop go name
            (;; Preserve the order of loop variables, so that the user
             ;; can put hers first and still use positional arguments.
             (loop-variables ...
                             (loop-variable loop-initializer loop-stepper) ...)
             user-bindings
             user-termination-conditions
             ((outer-bvl outer-producer) ... . outer-bindings)
             ((entry-bvl entry-producer) ... . entry-bindings)
             (termination-condition ... . termination-conditions)
             ((body-bvl body-producer) ... . body-bindings)
             ((final-bvl final-producer) ... . final-bindings))
            loop-clauses
            body))

;;;;; User-Directed Clauses

    ((%loop go name state
            ((with variable initializer) . loop-clauses)
            body)
     (%loop go name state
            ((with variable initializer variable) . loop-clauses)
            body))

    ((%loop go name
            ((loop-variable ...) . more-state)
            ((with variable initializer stepper) . loop-clauses)
            body)
     (%loop go name
            ;; Preserve ordering of the user's loop variables.
            ((loop-variable ... (variable initializer stepper))
             . more-state)
            loop-clauses
            body))

    ((%loop go name state ((let variable expression) . loop-clauses) body)
     (%loop go name state ((let-values (variable) expression) . loop-clauses)
            body))

    ((%loop go name (loop-variables (user-binding ...) . more-state)
            ((let-values user-bvl user-producer) . loop-clauses)
            body)
     (%loop go name (loop-variables
                     ;; Preserve order of the user's termination conditions.
                     (user-binding ... (user-bvl user-producer))
                     . more-state)
            loop-clauses
            body))

    ((%loop go name state ((while condition) . loop-clauses) body)
     (%loop go name state ((until (not condition)) . loop-clauses) body))

    ((%loop go name (loop-variables
                     user-bindings
                     (user-termination-condition ...)
                     . more-state)
            ((until user-termination-condition*) . loop-clauses)
            body)
     (%loop go name
            (loop-variables
             user-bindings
             (user-termination-condition ... user-termination-condition*)
             . more-state)
            loop-clauses
            body))

    ;; Compatibility forms.  These clauses *must* come after all
    ;; others, because there is no keyword, so these would shadow any
    ;; clauses with keywords.

    ((%loop go name state ((variable initializer) . loop-clauses) body)
     (%loop go name state ((with variable initializer) . loop-clauses) body))

    ((%loop go name state ((variable initializer stepper) . loop-clauses) body)
     (%loop go name state ((with variable initializer stepper) . loop-clauses)
            body))

    ((%loop go name state (clause . loop-clauses) body)
     (syntactic-error "Malformed LOOP clause:" clause))

;;;;; Finishing -- Generating Output

    ((%loop go name state () (=> result-form . body))
     (%loop finish name state result-form body))

    ((%loop go name state () body)
     (%loop finish name state (if #f #f) body))

    ((%loop finish name
            (((loop-variable loop-initializer loop-stepper) ...)
             user-bindings
             user-termination-conditions
             outer-bindings
             entry-bindings
             termination-conditions
             body-bindings
             final-bindings)
            result-form
            body)
     (let-values outer-bindings
       (let ((loop-procedure
              (rec (loop-procedure loop-variable ...)
                   (let-values entry-bindings
                     (%loop simplify-body
                            termination-conditions
                            (let-values final-bindings
                              (with-extended-parameter-operators
                               ((name
                                 (loop-procedure (loop-variable . loop-stepper)
                                                 ...)))
                               result-form))
                            body-bindings
                            user-bindings
                            user-termination-conditions
                            (with-extended-parameter-operators
                             ((name
                               (loop-procedure (loop-variable . loop-stepper)
                                               ...)))
                             . body))))))
         (loop-procedure loop-initializer ...))))

;;;;;; Simplifying the Body

    ;; No iterator- or user-introduced termination conditions at all.
    ;; No test or closure needed.
    ((%loop simplify-body
            ()
            final-form
            body-bindings
            user-bindings
            ()
            body-form)
     (let-values body-bindings
       (let-values user-bindings
         body-form)))

    ;; Iterator-introduced termination conditions only.  One test and
    ;; no closure needed.
    ((%loop simplify-body
            (termination-condition ...)
            final-form
            body-bindings
            user-bindings
            ()                          ;No user termination conditions
            body-form)
     (if (or termination-condition ...)
         final-form
         (let-values body-bindings
           (let-values user-bindings
             body-form))))

    ;; The closure is needed here because the body bindings shouldn't
    ;; be visible in the final form.
    ((%loop simplify-body
            ()
            final-form
            body-bindings
            user-bindings
            (user-termination-condition ...)
            body-form)
     (let ((finish (lambda () final-form)))
       (let-values body-bindings
         (let-values user-bindings
           (if (or user-termination-condition ...)
               (finish)
               body-form)))))

    ((%loop simplify-body
            (termination-condition ...)
            final-form
            body-bindings
            user-bindings
            (user-termination-condition ...)
            body-form)
     (let ((finish (lambda () final-form)))
       (if (or termination-condition ...)
           (finish)
           (let-values body-bindings
             (let-values user-bindings
               (if (or user-termination-condition ...)
                   (finish)
                   body-form))))))))

;;;; Accumulators

;;; Accumulators have the following syntax:
;;;
;;;   (FOR <result> (ACCUMULATING <generator>))
;;;   (FOR <result> (ACCUMULATING <generator> (IF <condition>)))
;;;   (FOR <result> (ACCUMULATING <generator> => <mapper>))    ;COND-style
;;;   (FOR <result> (ACCUMULATING <generator> <tester>         ;SRFI-61-style
;;;                               => <mapper>))
;;;
;;; In addition, some of them support initial values, which are
;;; specified with an optional first argument of (INITIAL <initial
;;; value>).  For example, to accumulate a list starting with some tail
;;; <tail>, write
;;;
;;;   (FOR <result-list> (LISTING (INITIAL <tail>) <element>)).

(define-syntax listing
  (syntax-rules (initial)
    ((listing variables ((initial tail-expression) . arguments) next . rest)
     (%accumulating variables arguments (((tail) tail-expression))
                    ('() cons (lambda (result)
                                (append-reverse result tail)))
                    (listing variables
                             ((initial tail-expression) . arguments)
                             "Malformed LISTING clause in LOOP:")
                    next . rest))

    ((listing variables arguments next . rest)
     (%accumulating variables arguments ()
                    ('() cons reverse)
                    (listing variables arguments
                             "Malformed LISTING clause in LOOP:")
                    next . rest))))

(define-syntax listing-reverse
  (syntax-rules (initial)
    ((listing-reverse variables ((initial tail-expression) . arguments)
                      next . rest)
     (%accumulating variables arguments (((tail) tail-expression))
                    (tail cons)
                    (listing-reverse
                     variables ((initial tail-expression) . arguments)
                     "Malformed LISTING-REVERSE clause in LOOP:")
                    next . rest))

    ((listing-reverse variables arguments next . rest)
     (%accumulating variables arguments ()
                    ('() cons)
                    (listing-reverse
                     variables arguments
                     "Malformed LISTING-REVERSE clause in LOOP:")
                    next . rest))))

;;; This is non-reentrant but produces precisely one garbage cons cell.

(define-syntax listing!
  (syntax-rules ()
    ((listing! variables arguments next . rest)
     (%listing! variables arguments (cons #f '())
                (listing! variables arguments
                          "Malformed LISTING! clause in LOOP:")
                next . rest))))

(define-syntax listing-into!
  (syntax-rules ()
    ((listing-into! variables (first-expression . arguments) next . rest)
     (%listing! variables arguments first-expression
                (listing-into! variables
                               (first-expression . arguments)
                               "Malformed LISTING-INTO! clause in LOOP:")
                next . rest))))

(define-syntax %listing!
  (syntax-rules (initial)
    ((%listing! variables ((initial tail-expression) . arguments)
                first-expression
                error-context
                next . rest)
     (%accumulating variables arguments
                    (((first tail)
                      (let ((first first-expression)
                            (tail tail-expression))
                        (set-cdr! first tail)
                        (values first tail))))
                    (first (lambda (datum previous-cell)
                             (let ((next-cell (cons datum tail)))
                               (set-cdr! previous-cell next-cell)
                               next-cell))
                           (lambda (cell) cell (cdr first)))
                    error-context
                    next . rest))

    ((%listing! variables arguments first-expression error-context next . rest)
     (%listing! variables ((initial '()) . arguments)
                first-expression
                error-context
                next . rest))))

;;;;; List Appending Accumulators

(define-syntax appending
  (syntax-rules (initial)
    ((appending variables ((initial tail-expression) . arguments)
                next . rest)
     (%accumulating variables arguments (((tail) tail-expression))
                    ('() append-reverse (lambda (result)
                                          (append-reverse result tail)))
                    (appending variables
                               ((initial tail-expression) . arguments)
                               "Malformed APPENDING clause in LOOP:")
                    next . rest))

    ((appending variables arguments next . rest)
     (%accumulating variables arguments ()
                    ('() append-reverse reverse)
                    (APPENDING variables arguments
                               "Malformed APPENDING clause in LOOP:")
                    next . rest))))

(define-syntax appending-reverse
  (syntax-rules (initial)
    ((appending-reverse variables ((initial tail-expression) . arguments)
                        next . rest)
     (%accumulating variables arguments (((tail) tail-expression))
                    (tail append-reverse)
                    (appending-reverse
                     variables ((initial tail-expression) . arguments)
                     "Malformed APPENDING-REVERSE clause in LOOP:")
                    next . rest))

    ((appending-reverse variables arguments next . rest)
     (%accumulating variables arguments ()
                    ('() append-reverse)
                    (appending-reverse
                     variables arguments
                     "Malformed APPENDING-REVERSE clause in LOOP:")
                    next . rest))))

;; (define (append-reverse list tail)
;;   (loop ((for elt (in-list list))
;;          (for result (listing-reverse (initial tail) elt)))
;;     => result))

(define (append-reverse list tail)
  (if (pair? list)
      (append-reverse (cdr list) (cons (car list) tail))
      tail))

;;;;; Numerical Accumulators

(define-syntax summing
  (syntax-rules (initial)
    ((summing variables ((initial initial-expression) . arguments) next . rest)
     (%accumulating variables arguments () (initial-expression +)
                    (summing variables
                             ((initial initial-expression) . arguments)
                             "Malformed SUMMING clause in LOOP:")
                    next . rest))

    ((summing variables arguments next . rest)
     (%accumulating variables arguments () (0 +)
                    (summing variables arguments
                             "Malformed SUMMING clause in LOOP:")
                    next . rest))))

(define-syntax multiplying
  (syntax-rules (initial)
    ((multiplying variables ((initial initial-expression) . arguments)
                  next . rest)
     (%accumulating variables arguments () (initial-expression *)
                    (multiplying variables
                                 ((initial initial-expression) . arguments)
                                 "Malformed MULTIPLYING clause in LOOP:")
                    next . rest))

    ((multiplying variables arguments next . rest)
     (%accumulating variables arguments () (1 *)
                    (multiplying variables arguments
                                 "Malformed MULTIPLYING clause in LOOP:")
                    next . rest))))

(define-syntax maximizing
  (syntax-rules ()
    ((maximizing variables arguments next . rest)
     (%extremizing variables arguments max
                   (maximizing variables arguments
                               "Malformed MAXIMIZING clause in LOOP:")
                   next . rest))))

(define-syntax minimizing
  (syntax-rules ()
    ((minimizing variables arguments next . rest)
     (%extremizing variables arguments min
                   (minimizing variables arguments
                               "Malformed MINIMIZING clause in LOOP:")
                   next . rest))))

(define-syntax %extremizing
  (syntax-rules (initial)
    ((%extremizing variables ((initial initial-expression) . arguments)
                   chooser
                   error-context next . rest)
     (%accumulating variables arguments (((initial-value) initial-expression))
                    (initial-value chooser)
                    error-context next . rest))

    ((%extremizing variables arguments chooser error-context next . rest)
     (%accumulating variables arguments ()
                    (#f (lambda (datum extreme)
                          (if (and datum extreme)
                              (chooser datum extreme)
                              (or datum extreme))))
                    error-context next . rest))))

(define-syntax %accumulating
  (syntax-rules ()

    ;; There is a finalization step, so the result variable cannot be
    ;; the accumulator variable, and we must apply the finalizer at the
    ;; end.
    ((%accumulating (result-variable) arguments outer-bindings
                    (initializer combiner finalizer)
                    error-context
                    next . rest)
     (%%accumulating arguments (accumulator initializer combiner)
                     outer-bindings
                     (((result-variable) (finalizer accumulator)))
                     error-context
                     next . rest))

    ;; There is no finalizer step, so the accumulation is incremental,
    ;; and can be exploited; therefore, the result variable and the
    ;; accumulator variable are one and the same.
    ((%accumulating (accumulator-variable) arguments outer-bindings
                    (initializer combiner)
                    error-context
                    next . rest)
     (%%accumulating arguments (accumulator-variable initializer combiner)
                     outer-bindings
                     ()
                     error-context
                     next . rest))

    ;; The user supplied more than one variable.  Lose lose.
    ((%accumulating variables arguments outer-bindings parameters
                    error-context next . rest)
     (loop-clause-error error-context))))

(define-syntax %%%accumulating
  (syntax-rules ()
    ((%%%accumulating outer-bindings loop-variable final-bindings next . rest)
     (next outer-bindings
           (loop-variable)
           ()                           ;Entry bindings
           ()                           ;Termination conditions
           ()                           ;Body bindings
           final-bindings
           . rest))))

(define-syntax %%accumulating
  (syntax-rules (if =>)
    ((%%accumulating (generator)        ;No conditional
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%accumulating outer-bindings
                      (accumulator initializer       ;Loop variable
                                   (combiner generator accumulator))
                      final-bindings next . rest))

    ((%%accumulating (generator (if condition))
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%accumulating outer-bindings
                      (accumulator initializer       ;Loop variable
                                   (if condition
                                       (combiner generator accumulator)
                                       accumulator))
                      final-bindings next . rest))

    ((%%accumulating (generator => mapper)
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%accumulating outer-bindings
                      (accumulator initializer       ;Loop variable
                                   (cond (generator
                                          => (lambda (datum)
                                               (combiner (mapper datum)
                                                         accumulator)))
                                         (else accumulator)))
                      final-bindings next . rest))

    ((%%accumulating (generator tester => mapper)
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%accumulating outer-bindings
                      (accumulator initializer       ;Loop variable
                                   (receive args generator
                                     (if (apply tester args)
                                         (combiner (apply mapper args)
                                                   accumulator)
                                         accumulator)))
                      final-bindings next . rest))

    ((%%ACCUMULATING arguments parameters outer-bindings final-bindings
                     error-context next . rest)
     (loop-clause-error error-context))))

;;;; List Iteration

;;; (FOR <elt> [<pair>] (IN-LIST <list> [<successor>]))
;;;   Step across <list>, letting <pair> be each successive pair in
;;;   <list>, stepping by (<successor> <pair>), or (CDR <pair>) if no
;;;   successor procedure is explicitly provided.  Let <elt> be the car
;;;   of <pair> in the body of the loop.

(define-syntax in-list
  (syntax-rules ()
    ((in-list (element-variable pair-variable)
              (list-expression successor-expression)
              next . rest)
     (next (((list) list-expression)                  ;Outer bindings
            ((successor) successor-expression))
           ((pair-variable list tail))                ;Loop variables
           ()                                         ;Entry bindings
           ((not (pair? pair-variable)))              ;Termination conditions
           (((element-variable) (car pair-variable))  ;Body bindings
            ((tail)             (successor pair-variable)))
           ()                                         ;Final bindings
           . rest))

    ((in-list (element-variable pair-variable) (list-expression) next . rest)
     (in-list (element-variable pair-variable) (list-expression cdr)
              next . rest))

    ((in-list (element-variable) (list-expression successor) next . rest)
     (in-list (element-variable pair) (list-expression successor) next . rest))

    ((in-list (element-variable) (list-expression) next . rest)
     (in-list (element-variable pair) (list-expression cdr) next . rest))

    ((in-list variables arguments next . rest)
     (loop-clause-error (in-list variables arguments
                                 "Malformed IN-LIST clause in LOOP:")))))

;;;;; Parallel List Iteration

(define-syntax in-lists
  (syntax-rules ()
    ((in-lists (elements-variable pairs-variable)
               (lists-expression tail-expression)
               next . rest)
     (next (((lists) lists-expression))   ;Outer bindings
           ((pairs-variable lists cdrs))  ;Loop variables
           (((lose? cars cdrs)            ;Entry bindings
             (%cars&cdrs pairs-variable tail-expression '())))
           (lose?)                        ;Termination conditions
           (((elements-variable) cars))   ;Body bindings
           ()                             ;Final bindings
           . rest))

    ((in-lists (elements-variable pairs-variable) (lists) next . rest)
     (in-lists (elements-variable pairs-variable) (lists '()) next . rest))

    ((in-lists (elements-variable) (lists tail) next . rest)
     (in-lists (elements-variable pairs) (lists tail) next . rest))

    ((in-lists (elements-variable) (lists) next . rest)
     (in-lists (elements-variable pairs) (lists '()) next . rest))

    ((in-lists variables arguments next . rest)
     (loop-clause-error (in-lists variables arguments
                                  "Malformed IN-LISTS clause in LOOP:")))))

(define (%cars&cdrs lists cars-tail cdrs-tail)
  (loop proceed ((for list (in-list lists))
                 (for cars (listing (initial cars-tail) (car list)))
                 (for cdrs (listing (initial cdrs-tail) (cdr list))))
    => (values #f cars cdrs)
    (if (pair? list)
        (proceed)
        (values #t #f #f))))

;;;; Vector and String Iteration

;;; (FOR <elt> [<index>] (IN-VECTOR <vector> [<start> [<end>]]))
;;;
;;; IN-VECTOR-REVERSE, IN-STRING, and IN-STRING-REVERSE all have the
;;; same syntax.
;;;
;;; The reverse iterators run from end to start; the bounds are still
;;; given in the same order as the forward iterators.

(define-syntax in-vector
  (syntax-rules ()
    ((in-vector variables (vector-expression start/end ...) next . rest)
     (%in-vector (forward vector-ref vector 0 (vector-length vector))
                 variables (vector-expression start/end ...)
                 (in-vector variables (vector-expression start/end ...)
                            "Malformed IN-VECTOR clause in LOOP:")
                 next . rest))))

(define-syntax in-vector-reverse
  (syntax-rules ()
    ((in-vector-reverse variables (vector-expression start/end ...)
                        next . rest)
     (%in-vector (backward vector-ref vector (vector-length vector) 0)
                 variables (vector-expression start/end ...)
                 (in-vector-reverse
                  variables (vector-expression start/end ...)
                  "Malformed IN-VECTOR-REVERSE clause in LOOP:")
                 next . rest))))

(define-syntax in-string
  (syntax-rules ()
    ((in-string variables (vector-expression start/end ...) next . rest)
     (%in-vector (forward string-ref string 0 (string-length string))
                 variables (vector-expression start/end ...)
                 (in-string variables (vector-expression start/end ...)
                            "Malformed IN-STRING clause in LOOP:")
                 next . rest))))

(define-syntax in-string-reverse
  (syntax-rules ()
    ((in-string-reverse variables (string-expression start/end ...)
                        next . rest)
     (%in-vector (backward string-ref string (string-length string) 0)
                 variables (string-expression start/end ...)
                 (in-string-reverse
                  variables (string-expression start/end ...)
                  "Malformed IN-STRING-REVERSE clause in LOOP:")
                 next . rest))))

;;;;; Random-Access Sequence Generalization

(define-syntax %in-vector
  (syntax-rules (forward backward)
    ((%in-vector (forward vector-ref vector-variable default-start default-end)
                 (element-variable index-variable)
                 (vector-expression start-expression end-expression)
                 error-context next . rest)
     (next (((vector-variable start end);Outer bindings
             (let ((vector-variable vector-expression))
               (values vector-variable start-expression end-expression))))
           ((index-variable start       ;Loop variables
                            (+ index-variable 1)))
           ()                           ;Entry bindings
           ((>= index-variable end))    ;Termination conditions
           (((element-variable)         ;Body bindings
             (vector-ref vector-variable index-variable)))
           ()                           ;Final bindings
           . rest))

    ((%in-vector (backward
                  vector-ref vector-variable default-start default-end)
                 (element-variable index-variable)
                 (vector-expression start-expression end-expression)
                 error-context next . rest)
     (next (((vector-variable start end);Outer bindings
             (let ((vector-variable vector-expression))
               (values vector-variable start-expression end-expression))))
           ((index-variable start       ;Loop variables
                            index-variable))
           ()                           ;Entry bindings
           ((<= index-variable end))    ;Termination conditions
           (((index-variable)           ;Body bindings
             (- index-variable 1))
            ((element-variable)
             (vector-ref vector-variable (- index-variable 1))))
           ()                           ;Final bindings
           . rest))

    ;; Supply an index variable if absent.
    ((%in-vector iteration-parameters (element-variable) arguments
                 error-context next . rest)
     (%in-vector iteration-parameters (element-variable index) arguments
                 error-context next . rest))

    ;; Supply the default start index if necessary.
    ((%in-vector (direction vector-ref variable default-start default-end)
                 variables (vector-expression)
                 error-context next . rest)
     (%in-vector (direction vector-ref variable default-start default-end)
                 variables (vector-expression default-start)
                 error-context next . rest))

    ;; Supply the default end index if necessary.
    ((%in-vector (direction vector-ref variable default-start default-end)
                 variables (vector-expression start-expression)
                 error-context next . rest)
     (%in-vector (direction vector-ref variable default-start default-end)
                 variables (vector-expression start-expression default-end)
                 error-context next . rest))

    ((%in-vector iteration-parameters modified-variables modified-arguments
                 error-context next . rest)
     (loop-clause-error error-context))))

;;;; Input

;;; (FOR <item> (IN-PORT <input-port> [<reader> [<eof?>]]))
;;;
;;; IN-FILE has the same syntax, but with a pathname in the place of
;;; the input port.

(define-syntax in-port
  (syntax-rules ()
    ((in-port (datum-variable)
              (port-expression reader-expression eof-predicate)
              next . rest)
     (next (((port) port-expression)              ;Outer bindings
            ((reader) reader-expression)
            ((eof?) eof-predicate))
           ()                                     ;Loop variables
           (((datum-variable) (reader port)))     ;Entry bindings
           ((eof? datum-variable))                ;Termination conditions
           ()                                     ;Body bindings
           ()                                     ;Final bindings
           . rest))

    ;; Supply a reader if absent.
    ((in-port (datum-variable) (port-expression) next . rest)
     (in-port (datum-variable) (port-expression read-char) next . rest))

    ;; Supply an EOF predicate if absent.
    ((in-port (datum-variable) (port-expression reader-expression) next . rest)
     (in-port (datum-variable) (port-expression reader-expression eof-object?)
              next . rest))

    ((in-port variables arguments next . rest)
     (loop-clause-error (in-port variables arguments
                                 "Malformed IN-PORT clause in LOOP:")))))

(define-syntax in-file
  (syntax-rules ()
    ((in-file (datum-variable)
              (pathname-expression reader-expression eof-predicate)
              next . rest)
     (next (((port)                               ;Outer bindings
             (open-input-file pathname-expression))
            ((reader) reader-expression)
            ((eof?) eof-predicate))
           ()                                     ;Loop variables
           (((datum-variable) (reader port)))     ;Entry bindings
           ((eof? datum-variable))                ;Termination conditions
           ()                                     ;Body bindings
           ((()                                   ;Final bindings
             (begin (close-input-port port)
                    (values))))
           . rest))

    ;; Supply a reader if absent.
    ((in-file (datum-variable) (pathname-expression) next . rest)
     (in-file (datum-variable) (pathname-expression read-char) next . rest))

    ;; Supply an EOF predicate if absent.
    ((in-file (datum-variable) (pathname-expression reader) next . rest)
     (in-file (datum-variable) (pathname-expression reader eof-object?)
              next . rest))

    ((in-file variables arguments next . rest)
     (loop-clause-error (in-file variables arguments
                                 "Malformed IN-FILE clause in LOOP:")))))

;;;; Iterating Up through Numbers

(define-syntax up-from
  (syntax-rules (to by)
    ((up-from (variable)
              (start-expression (to end-expression)
                                (by step-expression))
              next . rest)
     (next (((start) start-expression)  ;Outer bindings
            ((end) end-expression)
            ((step) step-expression))
           ((variable start             ;Loop variables
                      (+ variable step)))
           ()                           ;Entry bindings
           ((>= variable end))          ;Termination conditions
           ()                           ;Body bindings
           ()                           ;Final bindings
           . rest))

    ((up-from (variable)
              (start-expression (by step-expression))
              next . rest)
     (next (((start) start-expression)  ;Outer bindings
            ((step) step-expression))
           ((variable start             ;Loop variables
                      (+ variable step)))
           ()                           ;Entry bindings
           ()                           ;Termination conditions
           ()                           ;Body bindings
           ()                           ;Final bindings
           . rest))

    ;; Add a default step of 1.
    ((up-from (variable)
              (start-expression (to end-expression))
              next . rest)
     (up-from (variable)
              (start-expression (to end-expression) (by 1))
              next . rest))

    ((up-from (variable)
              (start-expression)
              next . rest)
     (up-from (variable)
              (start-expression (by 1))
              next . rest))

    ((up-from variables arguments next . rest)
     (loop-clause-error (up-from variables arguments
                                 "Malformed UP-FROM clause in LOOP:")))))

;;;; Iterating Down through Numbers

(define-syntax down-from
  (syntax-rules (to by)
    ((down-from (variable)
                (start-expression (to end-expression)
                                  (by step-expression))
                next . rest)
     (next (((start) start-expression)  ;Outer bindings
            ((end) end-expression)
            ((step) step-expression))
           ((variable start variable))  ;Loop variables
           ()                           ;Entry bindings
           ((<= variable end))          ;Termination conditions
           (((variable)                 ;Body bindings
             (- variable step)))
           ()                           ;Final bindings
           . rest))

    ((down-from (variable)
                (start-expression (by step-expression))
                next . rest)
     (next (((start) start-expression)  ;Outer bindings
            ((step) step-expression))
           ((variable start variable))  ;Loop variables
           ()                           ;Entry bindings
           ()                           ;Termination conditions
           (((variable)                 ;Body bindings
             (- variable step)))
           ()                           ;Final bindings
           . rest))

    ;; Add a default step of 1.
    ((down-from (variable)
                (start-expression (to end-expression))
                next . rest)
     (down-from (variable)
                (start-expression (to end-expression)
                                  (by 1))
                next . rest))

    ((down-from (variable)
                (start-expression)
                next . rest)
     (down-from (variable)
                (start-expression (by 1))
                next . rest))

    ((down-from variables arguments next . rest)
     (loop-clause-error (down-from variables arguments
                                   "Malformed DOWN-FROM clause in LOOP:")))))
)
