;;;; srfi-101.scm  -*- Hen -*-

(module srfi-101

  (;export
    quote
    pair?
    cons
    car
    cdr
    caar
    cadr
    cddr
    cdar
    caaar
    caadr
    caddr
    cadar
    cdaar
    cdadr
    cdddr
    cddar
    caaaar
    caaadr
    caaddr
    caadar
    cadaar
    cadadr
    cadddr
    caddar
    cdaaar
    cdaadr
    cdaddr
    cdadar
    cddaar
    cddadr
    cddddr
    cdddar
    null
    null?
    list?
    list
    make-list
    length
    append
    reverse
    list-tail
    list-ref
    list-set
    list-ref/update
    map
    for-each
    random-access-list->linear-access-list
    linear-access-list->random-access-list)

  (import (rename scheme
                  (quote? r5:quote?)
                  (pair? r5:pair?)
                  (cons r5:cons)
                  (car r5:car)
                  (cdr r5:cdr)
                  (caar r5:caar)
                  (cadr r5:cadr)
                  (cddr r5:cddr)
                  (cdar r5:cdar)
                  (caaar r5:caaar)
                  (caadr r5:caadr)
                  (caddr r5:caddr)
                  (cadar r5:cadar)
                  (cdaar r5:cdaar)
                  (cdadr r5:cdadr)
                  (cdddr r5:cdddr)
                  (cddar r5:cddar)
                  (caaaar r5:caaaar)
                  (caaadr r5:caaadr)
                  (caaddr r5:caaddr)
                  (caadar r5:caadar)
                  (cadaar r5:cadaar)
                  (cadadr r5:cadadr)
                  (cadddr r5:cadddr)
                  (caddar r5:caddar)
                  (cdaaar r5:cdaaar)
                  (cdaadr r5:cdaadr)
                  (cdaddr r5:cdaddr)
                  (cdadar r5:cdadar)
                  (cddaar r5:cddaar)
                  (cddadr r5:cddadr)
                  (cddddr r5:cddddr)
                  (cdddar r5:cdddar)
                  (null? r5:null?)
                  (list? r5:list?)
                  (list r5:list)
                  (make-list r5:make-list)
                  (length r5:length)
                  (append r5:append)
                  (reverse r5:reverse)
                  (list-tail r5:list-tail)
                  (list-ref r5:list-ref)
                  (map r5:map)
                  (for-each r5:for-each))
          chicken
          functional-lists)

  (require-library functional-lists)

;;;

(define quote? ra:quote?)
(define pair? ra:pair?)
(define cons ra:cons)
(define car ra:car)
(define cdr ra:cdr)
(define caar ra:caar)
(define cadr ra:cadr)
(define cddr ra:cddr)
(define cdar ra:cdar)
(define caaar ra:caaar)
(define caadr ra:caadr)
(define caddr ra:caddr)
(define cadar ra:cadar)
(define cdaar ra:cdaar)
(define cdadr ra:cdadr)
(define cdddr ra:cdddr)
(define cddar ra:cddar)
(define caaaar ra:caaaar)
(define caaadr ra:caaadr)
(define caaddr ra:caaddr)
(define caadar ra:caadar)
(define cadaar ra:cadaar)
(define cadadr ra:cadadr)
(define cadddr ra:cadddr)
(define caddar ra:caddar)
(define cdaaar ra:cdaaar)
(define cdaadr ra:cdaadr)
(define cdaddr ra:cdaddr)
(define cdadar ra:cdadar)
(define cddaar ra:cddaar)
(define cddadr ra:cddadr)
(define cddddr ra:cddddr)
(define cdddar ra:cdddar)
(define null ra:null)
(define null? ra:null?)
(define list? ra:list?)
(define list ra:list)
(define make-list ra:make-list)
(define length ra:length)
(define append ra:append)
(define reverse ra:reverse)
(define list-tail ra:list-tail)
(define list-ref ra:list-ref)
(define list-set ra:list-set)
(define list-ref/update ra:list-ref/update)
(define map ra:map)
(define for-each ra:for-each)
(define linear-access-list->random-access-list ra:linear-access-list->random-access-list)
(define random-access-list->linear-access-list ra:random-access-list->linear-access-list)

) ;module srfi-101
