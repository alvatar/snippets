I posted a pattern matcher to this newsgroup a few months ago. The
code is reposted below, with two changes from Derrick Eddington, and
will work in either R5RS or R6RS.

My matcher is simpler than some of the other matchers mentioned in
this
discussion, but that's okay; it does everything I need it to do, and
is
simple both to use and to implement. I prefer simple things I can
understand to complicated things I cannot, and this simple list
matcher
is fine for me. At just thirty-three lines of code, it's a
lightweight
that performs like a heavyweight.

I know you are aware of this matcher, because you responded to the
original posting. Is it somehow inadequate for your needs?

Phil

Pattern matching on lists is provided by the list-match macro. The
syntax (list-match expr clause ...) takes an input expr that evaluates
to a list. Clauses are of the form (pattern [fender] expr),
consisting
of a pattern that matches a list of a particular shape, an optional
fender that must succeed if the pattern is to match, and an expr that
is evaluated if the pattern matches. There are four types of
patterns:

- () -- Matches the null list.

- (pat0 pat1 ...) -- Matches a list with length exactly equal to the
number of pattern elements.

- (pat0 pat1 ... . patRest) -- Matches a list with length at least
as great as the number of pattern elements before the literal dot.
PatRest is a list containing the remaining elements of the input
list after the initial prefix of the list before the literal dot.

- pat -- Matches an entire list. Should always appear as the last
clause; it's not an error to appear elsewhere, but subsequent
clauses could never match.

Each pattern element may be:

- An identifier -- Matches any list element. Additionally, the
value of the list element is bound to the variable named by the
identifier, which is in scope in the fender and expr of the
corresponding clause. Each identifier in a single pattern must
be unique.

- A literal underscore -- Matches any list element, but creates no
bindings.

- A constant -- Matches if the expression equals the constant value,
but creates no bindings.

- A quote expression -- Matches if the expression equals the quote
expression, but creates no bindings.

- A quasiquote expression -- Matches if the expression equals the
quasiquote expression, but creates no bindings.

All comparisons are made with equal?. The patterns are tested in
order,
left to right, until a matching pattern is found; if fender is
present,
it must evaluate as non-#f for the match to be successful. Pattern
variables are bound in the corresponding fender and expression. Once
the matching pattern is found, the corresponding expr is evaluated and
returned as the result of the match. An error is signaled if no
pattern matches the input list.

A simple match expression that computes the length of a list is given
below; the first pattern is the null list, which forms the base of the
recursion, and the second pattern matches a non-null input list, using
an underscore to signal that the value of the list element is not
bound in the result expression:

(define (len xs)
(list-match xs
(() 0)
((_ . xs) (+ 1 (len xs)))))

Fenders can test the common case where two list elements must be
identical; (unique eql? xs) casts out adjacent duplicates from an
input list:

(define (unique eql? xs)
(list-match xs
(() '())
((x) (list x))
((x y . rest) (eql? x y) (unique eql? (cons y rest)))
((x . rest) (cons x (unique eql? rest)))))

A more complex example uses two nested matchers to merge two input
lists ordered by the lt? predicate:

(define (list-merge lt? xx yy)
(list-match xx
(() yy)
((x . xs)
(list-match yy
(() xx)
((y . ys)
(if (lt? y x)
(cons y (list-merge lt? xx ys))
(cons x (list-merge lt? xs yy))))))))

Pattern matching is performed by a macro that expands into a cond
expression with one clause per pattern; an auxiliary macro handles the
various types of pattern elements. The complete implementation, which
is based on an idea of Jos Koot, is given below:

(define-syntax list-match
(syntax-rules ()
((_ expr (pattern fender ... template) ...)
(let ((obj expr))
(cond ((list-match-aux obj pattern fender ...
(list template)) => car) ...
(else (error 'list-match "pattern failure")))))))

(define-syntax list-match-aux
(lambda (stx)
(define (underscore? x)
(and (identifier? x) (free-identifier=? x (syntax _))))
(syntax-case stx (quote quasiquote)
((_ obj pattern template)
(syntax (list-match-aux obj pattern #t template)))
((_ obj () fender template)
(syntax (and (null? obj) fender template)))
((_ obj underscore fender template)
(underscore? (syntax underscore))
(syntax (and fender template)))
((_ obj var fender template)
(identifier? (syntax var))
(syntax (let ((var obj)) (and fender template))))
((_ obj (quote datum) fender template)
(syntax (and (equal? obj (quote datum)) fender template)))
((_ obj (quasiquote datum) fender template)
(syntax (and (equal? obj (quasiquote datum)) fender
template)))
((_ obj (kar . kdr) fender template)
(syntax (and (pair? obj)
(let ((kar-obj (car obj)) (kdr-obj (cdr obj)))
(list-match-aux kar-obj kar
(list-match-aux kdr-obj kdr fender
template))))))
((_ obj const fender template)
(syntax (and (equal? obj const) fender template))))))
