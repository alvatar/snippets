From posting-system@google.com Sun May  2 23:14:29 2004
Date: Sun, 2 May 2004 16:14:04 -0700
From: oleg-at-pobox.com
Newsgroups: comp.lang.scheme
Subject: Macros and procedures with labeled arguments
Message-ID: <7eb8ac3e.0405021514.541b1a47@posting.google.com>
Status: OR

When a procedure or a special form has many (positional) arguments,
writing them all in an invocation quickly becomes tedious and
error-prone. Some Scheme systems implement DSSSL extensions, which let
us pass arguments to procedures as keyword-value pairs. Function
invocations become lucid: It is clear which argument receives which
value. The programmer no longer has to memorize the precise order of
arguments to a procedure. The programmer can even omit some arguments
if the defaults are appropriate.

Alas, these extensions do not apply to special forms. When invoking
macros, we still have to use positional arguments. This article
introduces portable, R5RS-compliant keyword (labeled) arguments for
macros and procedures. To be more precise, we demonstrate a meta
macro. It takes the name of an ordinary (positional-argument) macro
and the description of its arguments and defines a new macro, which
accepts labeled arguments. A label is *anything* that can be used for
identification: in the simplest case, a label is an
identifier. However, strings, numbers, and DSSSL keywords (if
available), can all be used as labels. We show many examples of that
below. If we label an argument by a string, that string may be the
argument's documentation. Macro/procedure invocations become more
lucid indeed.  The approach in this article can also be used for
regular procedures. So we can enjoy keyword arguments for
(first-order) functions even on those systems that don't support
DSSSL. We shall see a few examples below.  Labeled (aka keyword)
arguments are present in many languages, e.g., Tcl/Tk, Ocaml, Perl --
and arguably contribute to expressiveness. The same argument can be
made for Scheme.

All the code in this article is R5RS compliant and self-contained. It has
been tested on Petite Chez Scheme 6.0a, scheme48, and SCM.


We introduce labeled arguments by way of many examples. We start with
a trivial one. Suppose we have a macro with three positional arguments

(define-syntax testm
  (syntax-rules ()
   ((testm a b c) (list 'a 'b 'c))))

To define the corresponding labeled-argument macro, we enter

(ssax:define-labeled-arg-macro testm-named
   (testm
       ; the following are the descriptors for testm's three arguments
     (a "default-value-for-a") ; descriptor for the 1st argument of testm
     (b)
     (c a-symbol-for-c)))

The form ssax:define-labeled-arg-macro (see Appendix A) is the
positional-to-labeled-argument transformer. It takes the name of the
labeled-argument macro to define (testm-named), the name of the
corresponding positional argument macro (testm), and the descriptors
of the latter's arguments. The number and the order of the descriptors
should correspond to the number and the order of the positional
arguments of testm. An argument descriptor specifies the label for the
argument, and the default value. The descriptor "(b)" above introduces
the label `b' for the second argument of testm, and no default
value. That means that the argument is required.

We can now do

(testm-named a 1 b 2 c 3)
; ==> (1 2 3)

The order of the labeled arguments doesn't matter, so we can just as
well enter

(testm-named c 3 a 1 b 2)
; ==> (1 2 3)

Furthermore, the arguments 'a' and 'c' have default values, so 
they may be omitted

(testm-named c 3 b 2)
; ==> ("default-value-for-a" 2 3)

(testm-named b 2 a 1)
; ==> (1 2 a-symbol-for-c)

(testm-named  a 1)
Error: invalid syntax (find ((1 testm) (c a-symbol-for-c)) (b)).

Argument 'b' has no default, so it may not be omitted.


As in DSSSL, "If the same argument name occurs more than once in the
list of actual arguments, then the first value is used."

(testm-named b 2 a 1 b 3)
; ==> (1 2 a-symbol-for-c)
(testm-named b 2 a 1 b 3 a 5)
; ==> (1 2 a-symbol-for-c)


We don't have to use identifiers for labels. Numbers would do as well
(cf. numerical labels of SML records). For example,

(gen:define-labeled-arg-macro testm-numb
   (testm
       ; the following are the descriptors for testm's three arguments
     (1 "default-value-for-a") ; descriptor for the 1st argument of testm
     (2)
     (3 a-symbol-for-c)))


When the labels are not identifiers, we have to use a more general
meta-macro, gen:define-labeled-arg-macro, defined in Appendix B.

(testm-numb 1 arg1 2 arg2 3 arg3)
; ==> (arg1 arg2 arg3)

Of course the arguments can be in any order:

(testm-numb 3 arg3 2 arg2 1 arg1)
; ==> (arg1 arg2 arg3)

We can document our arguments if we label the arguments with strings.
We illustrate the use of strings as argument labels along with the use
of gen:define-labeled-arg-macro to give labeled arguments to regular
(first-order) procedures. Let us define a general procedure to look up
an association in an associative list.

(define (lookup pred key lst)
  (let loop ((lst lst))
    (cond
      ((null? lst) #f)
      ((pred key (caar lst)) (car lst))
      (else (loop (cdr lst))))))

That procedure relies on a user-supplied key equality predicate. We can
define the corresponding labeled-argument macro. The predicate is now
an optional argument, with a default value eq?. 

(gen:define-labeled-arg-macro llookup
  (lookup
    ("the predicate" eq?)
    ("key")
    ("the associative list")
))


(llookup 
  "the associative list" '((a 1) (b 2))
  "key" 'b)
; ==> (b 2)

The arguments become self-documenting. We took advantage of the
default value for the "pred" argument.  Note that this default applies
to the _first_ argument of the procedure lookup. Well, with labeled
arguments, the order is arbitrary anyway:

(let ((key 1) (lst '((2 4) (1 5))))
  (llookup "key" key
    "the predicate" equal?
    "the associative list" lst))
; ==> (1 5)

Incidentally, the use of strings as argument labels lets us write the
argument labels in Unicode in one's native language. There is far more
agreement on supporting Unicode in strings than in supporting Unicode
in identifiers. 

As we have mentioned, argument labels can be any datum. For example,
characters and booleans are acceptable, too. It seems especially
fitting to use booleans to mark branches on an `if' form:

(gen:define-labeled-arg-macro lif
  (if
    (#\?)     ; Character '? marks the test expression
    (#t)      ; then-branch, required
    (#f 17)   ; In deference to the R5RS Editor
))

(let ((x 1)) (lif #\? (positive? x) #t "OK" #f (/ x 0)))
; ==> "OK"

The ability to shift the test expression to the very end of `if' should
no doubt appeal to former Perl programmers:

(let loop ((x 5))
  (lif #t 1
    #f (lif #f (+ (loop (- x 1)) (loop (- x 2)))
	 #t 1
	 #\? (= x 1))
    #\? (= x 0)))



Appendix A. The macro ssax:define-labeled-arg-macro itself is not that
complex. As the prefix 'ssax:' indicates, the macro is the part of the
SSAX/SXML project: 
	  http://ssax.sourceforge.net

More specifically, the macro appears in the main module of the SSAX
XML parser: SSAX.scm, version 5.0. The file can be retrieved from the
project's CVS repository; it is also available at
	  http://pobox.com/~oleg/ftp/Scheme/lib/SSAX.scm

That file shows an "industrial" example of a macro with labeled
arguments, ssax:make-parser. The parser is R5RS compliant and runs on
many Scheme platforms. An interested reader might find in SSAX.scm a
few other curious R5RS macros.


; The following meta-macro turns a regular macro (with positional
; arguments) into a form with keyword (labeled) arguments.  We later
; use the meta-macro to convert ssax:make-parser/positional-args into
; ssax:make-parser. The latter provides a prettier (with labeled
; arguments and defaults) interface to
; ssax:make-parser/positional-args
;
; ssax:define-labeled-arg-macro LABELED-ARG-MACRO-NAME 
;		(POS-MACRO-NAME ARG-DESCRIPTOR ...)
; expands into the definition of a macro
;	LABELED-ARG-MACRO-NAME KW-NAME KW-VALUE KW-NAME1 KW-VALUE1 ...
; which, in turn, expands into
;	POS-MACRO-NAME ARG1 ARG2 ...
; where each ARG1 etc. comes either from KW-VALUE or from
; the deafult part of ARG-DESCRIPTOR. ARG1 corresponds to the first
; ARG-DESCRIPTOR, ARG2 corresponds to the second descriptor, etc.
; Here ARG-DESCRIPTOR describes one argument of the positional macro.
; It has the form 
;	(ARG-NAME DEFAULT-VALUE)
; or
;	(ARG-NAME)
; In the latter form, the default value is not given, so that the
; invocation of LABELED-ARG-MACRO-NAME must mention the
; corresponding parameter.
; ARG-NAME can be anything: an identifier, a string, or even a number.
;
; We can easily generalize define-labeled-arg-macro to
; let-labeled-arg-syntax or letrec-labeled-arg-syntax

(define-syntax ssax:define-labeled-arg-macro
  (syntax-rules ()
    ((ssax:define-labeled-arg-macro
       labeled-arg-macro-name
       (positional-macro-name
	 (arg-name . arg-def) ...))
      (define-syntax labeled-arg-macro-name
	(syntax-rules ()
	  ((labeled-arg-macro-name . kw-val-pairs)
	    (letrec-syntax
	      ((find 
		 (syntax-rules (arg-name ...)
		   ((find k-args (arg-name . default) arg-name
		      val . others)	   ; found arg-name among kw-val-pairs
		    (next val . k-args)) ...
		   ((find k-args key arg-no-match-name val . others)
		     (find k-args key . others))
		   ((find k-args (arg-name default)) ; default must be here
		     (next default . k-args)) ...
		   ))
		(next			; pack the continuation to find
		  (syntax-rules ()
		    ((next val vals key . keys)
		      (find ((val . vals) . keys) key . kw-val-pairs))
		    ((next val vals)	; processed all arg-descriptors
		      (rev-apply (val) vals))))
		(rev-apply
		  (syntax-rules ()
		    ((rev-apply form (x . xs))
		      (rev-apply (x . form) xs))
		    ((rev-apply form ()) form))))
	      (next positional-macro-name () 
		(arg-name . arg-def) ...))))))))

Appendix B.
A generalization of ssax:define-labeled-arg-macro to non-symbolic
labels.

(define-syntax symbol??
  (syntax-rules ()
    ((symbol?? (x . y) kt kf) kf)	; It's a pair, not a symbol
    ((symbol?? #(x ...) kt kf) kf)	; It's a vector, not a symbol
    ((symbol?? maybe-symbol kt kf)
      (let-syntax
	((test
	   (syntax-rules ()
	     ((test maybe-symbol t f) t)
	     ((test x t f) f))))
	(test abracadabra kt kf)))))

; A more general define-labeled-arg-macro where labels can be anything
(define-syntax gen:define-labeled-arg-macro
  (syntax-rules ()
    ((gen:define-labeled-arg-macro
       labeled-arg-macro-name
       (positional-macro-name
	 (arg-name . arg-def) ...))
      (define-syntax labeled-arg-macro-name
	(syntax-rules ()
	  ((labeled-arg-macro-name . kw-val-pairs)
	    (select-symbols (arg-name ...) 
	    (letrec-syntax
	      ((find 
		 (syntax-rules symbols
		   ((find k-args (arg-name . default) arg-name
		      val . others)	   ; found arg-name among kw-val-pairs
		    (next val . k-args)) ...
		   ((find k-args key arg-no-match-name val . others)
		     (find k-args key . others))
		   ((find k-args (arg-name default)) ; default must be here
		     (next default . k-args)) ...
		   ))
		(next			; pack the continuation to find
		  (syntax-rules ()
		    ((next val vals key . keys)
		      (find ((val . vals) . keys) key . kw-val-pairs))
		    ((next val vals)	; processed all arg-descriptors
		      (rev-apply (val) vals))))
		(rev-apply
		  (syntax-rules ()
		    ((rev-apply form (x . xs))
		      (rev-apply (x . form) xs))
		    ((rev-apply form ()) form))))
	      (next positional-macro-name () 
		(arg-name . arg-def) ...)))))))))

(define-syntax select-symbols
  (syntax-rules ()
    ((select-symbols "l"  
       (letrec-syntax ((find (srules symbols . find-etc)) . ls-etc)
	 ls-body)
       sure-symbols ())
      (letrec-syntax ((find (srules sure-symbols . find-etc)) . ls-etc)
	 ls-body))
    ((select-symbols "l" k (symbol ...) (el . lst))
      (symbol?? el
	; el is a symbol
	(select-symbols "l" k (symbol ... el) lst)
	; el is not a symbol
	(select-symbols "l" k (symbol ...) lst)))
    ((select-symbols lst k)
      (select-symbols "l" k () lst))))

