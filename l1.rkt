#lang typed/racket
(require racket/list)

(define-type Loc String)

(: operator : Char -> (Operator Char))
(define (operator c )
  (match c
     ['+' Plus]
     ['>=' GTEQ]))

(struct Plus())
(struct GTEQ())
(struct Expr ())
(define-type Value (U Integer Char))
(define-type If (U Expr Expr Expr))
(define-type (Op c) (U Expr (Operator c) Expr))
(define-type Assign (U Loc Expr))
(struct Deref())
(struct Seq())
(struct While())
(struct Skip())
(define-type (Operator c) (U Plus GTEQ))

(define-type (Expression c)
 (U Value

    (Op c)

    If

    Assign

    (U Deref Loc )

    (U Seq Expr Expr)

    (U While Expr Expr)

    Skip)
  )


