#lang typed/racket/base

(provide updates None Some)

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     syntax/parse
                     syntax/stx)
         racket/match)
(begin-for-syntax
  (define-syntax-class type
    (pattern name:id
             #:attr [param 1] '()
             #:attr [field-id 1] '())
    (pattern (name:id param ...+)
             #:attr [field-id 1] (generate-temporaries #'(param ...)))))

(define-syntax define-datatype
  (syntax-parser
    [(_ type-name:type data-constructor:type ...)

     (define/with-syntax [data-type ...]
       (for/list ([name (in-syntax #'(data-constructor.name ...))])
         (if (stx-null? #'(type-name.param ...))
             name
             #`(#,name type-name.param ...))))

     #'(begin
         (struct (type-name.param ...) data-constructor.name
           ([data-constructor.field-id : data-constructor.param] ...)) ...
         (define-type type-name (U data-type ...)))]))

(define-type Loc String)
(struct Plus())
(struct GTEQ())
(define-type (Operator c) (U Plus GTEQ))

(: operator : (U Plus GTEQ)->  Char)
(define (operator c )
  (match c
    ['Plus #\+]
    ['GTEQ #\=]))

(define-datatype Expr
  (Value (U Boolean Number))
  (Op  Expr (U Plus GTEQ) Expr)
  (If (U Expr Expr Expr))
  (Assign (U Loc Expr))
  ( Deref Loc )
  ( Seq Expr Expr)
  ( While Expr Expr)
    Skip
)


(: printexpr (Expr -> Void ))
(define (printexpr expr)
  (match expr
    [(? number? n) (printf " ~a~n" n )]
    [(? boolean? b) (format " ~a~n" b )]
    ;; [( Op opr  )  (printf "~a" opr)]

    [( Op e1 operate e2  )
            (printf "( ~a ~a ~a~n)"  (printexpr e1)  (operator operate)
            (printexpr e2 ))]
 )
  )
(struct None ()
    #:transparent)
(struct (i) Some ([v : i])
    #:transparent)
(define-type (Opt a) (U None (Some a)))

(: lookup  ((Listof Number)  Number -> Number))
(define (lookup ls l)
  (match ls
    ['()  0]
    [(cons (cons (== l) n) ls) n]
    [(cons _ ls) (lookup ls l)]))


(: updates ((Listof Any) Number ->
                                 (Opt (Listof Any))))
(define (updates ls l)
  (match ls
    ['()   (None)]
    [(cons (cons (== l) n) ls) (Some (append ls (list (list l n))))]
    [(cons _ ls) (updates ls l)]))
