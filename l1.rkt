#lang typed/racket/base
(provide LocValue Loc printreduce printexpr updates BoolValue IntValue None Some Seq Deref Assign)

;;  Macro by Alexis King
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
;; End of Macro by Alexis King

(define-datatype Store
  ( Loc String)
  ( LocValue Number )
)

(struct Plus())
(struct GTEQ())
(define-type (Operator c) (U Plus GTEQ))

(: operator : (U Plus GTEQ)->  Char)
(define (operator c )
  (match c
    ['Plus #\+]
    ['GTEQ #\>=]))

(define-datatype Expr
  ( BoolValue Boolean  )
  ( IntValue  Number )
  ( Op  Expr (U Plus GTEQ) Expr)
  ( If  Expr Expr Expr)
  ( Assign  String Expr)
  ( Deref String)
  ( Seq Expr Expr)
  ( While Expr Expr)
    Skip
)


(: printexpr (Expr -> String))
(define (printexpr expr)
  (match expr
    [(IntValue n) (string-append (format  "~a" n))]
    [(BoolValue b) (string-append (format  "~a" b))]
    [(Deref l)  (string-append (format "( ~a ~a )" "!"  l))]
    [(If e1 e2 e3)
     (string-append (format "if ~a  then ~a else ~a"  (printexpr e1 )  (printexpr e2)
       (printexpr e3)))]
    [( Op e1 operate e2  )
            (string-append (format "( ~a ~c ~a)"  (printexpr e1)  (operator operate)
            (printexpr e2 )))]
    [( If e1 e2 e3  )
            (string-append (format "( ~a ~a ~a)"  (printexpr e1)  (printexpr e2 )(printexpr e2 )))]
    [ (Assign l e ) =  (string-append (format "~a := ~a" l (printexpr e )))]
    [ (Skip) ( string-append "skip")]
    [ (Seq e1 e2 )   (string-append (format "~a ;  ~a" (printexpr e1 )
                                      (printexpr e2)))]
    [ (While  e1 e2 ) (string-append  (format "while ~a do ~a " (printexpr e1 )
                                          (printexpr e2)))]
  ))

(struct None ()
    #:transparent)
(struct (i) Some ([v : i])
    #:transparent)
(define-type (Opt a) (U None (Some a)))

(: lookup  ((Listof Number)  Number -> Number))
(define (lookup ls l)
  (match ls
    [(cons (cons (== l) n) ls) n]
    [(cons _ ls) (lookup ls l)]))


(: updates ((Listof Any) Number ->
                                 (Opt (Listof Any))))
(define (updates ls l)
  (match ls
    ['()   (None)]
    [(cons (cons (== l) n) ls) (Some (append ls (list (list l n))))]
    [(cons _ ls) (updates ls l)]))

(: reduce (Expr (Listof Store) ->
                                 (Opt (Listof Any))))
(define (reduce expr store )
  (match expr
    [  (list 'Op  (? integer? n1) `Plus (? integer? n2)) store   (Some (list (IntValue (+ n1  n2)) store))]
    [  (list 'Op  (? integer? n1) `GTEQ (? integer? n2)) store   (Some (list (BoolValue (>=  n1  n2)) store))]
    [  (list 'Op  (? integer? n1) `Skip (? boolean? n2)) store
             (match  (reduce  n2 store)
               [ (Some (list (IntValue nn2) store))  (Some (list ((Op n1 'Skip nn2) store)))]
               [ (None)  (None) ]
               )
             (match  (reduce  n1 store)
               [ (Some (list (IntValue nn1) store))  (Some (list ((Op nn1 'Skip n2) store)))]
               [ (None)  (None) ]
               )]
    [  (list 'If e1 e2 e3) store
             (match e1
               [#t  (Some(list (e2 store  )))]
               [#f  (Some(list ( e3 store  )))]
               [_   (match (reduce e1 store )
                      [(Some (list (IntValue ee1) store )) (Some (list ((If ee1 e2 e3) store  )))]
                      [ (None)  (None) ]
               )]
    )]
    [ (list 'Deref l) store
             (match (lookup  store l)
               [ (Some n )  (Some (list n store))]
               [ (None)  (None) ]
               )]
    [  (list 'Assign l e ) store
             (match e
               [(IntValue n)
                (match (updates store (list l n))
                [ (Some stores)  (Some (list(  'Skip  stores)))]
                [ (None)  (None) ]
                [ _  (match (reduce e store )
                        [(Some (list e1 stores))  (Some (list ('Assign l e1) stores))]
                        [ (None)  (None) ]
                     )
                ]
               )]
               )]
   [ (list 'While e1 e2) store
           (Some( list(  'If e1 ('Seq e2 ('While e1 e2)) 'Skip) store  ))]
   [ 'Skip store ( None )]
   [ (list 'Seq e1 e2)  (list 'Store );; Matching two patterns may be required
            (match e1
              ['Skip  (Some (list( e2 store  )))]
              [ _  ( match (reduce e1 store )
                    [ ( Some (list ee1 stores )) ( Some (list ('Seq  ee1 e2)  stores  ))]
                    [ (None)  (None) ]

               )]
               )]
))

(: reduce1 (Expr (Listof Store) -> String))
(define (reduce1 e store)
    (match (reduce e store)
       [  (list  e store )
        ( format ("~n --> ~a " (printconfig e store )))
        (reduce1 e store )
       ]
       [ (None)  (format "~n -/->  "
                 (match e
                     ['Skip (string-append (format "(a value)~n"))]
                     [ _   (string-append (format "(stuck - not a value)"))]))
       ]
       )
)

(: rawprintstore : ( Any -> String))
(define (rawprintstore ls)
       (match ls
         ['() (string-append "")]
         [(cons( cons l n) tail) (string-append (format "l  = ~a " n )
                               (rawprintstore tail ))]
         )
)


(: printconfig (Expr (Listof Store) -> String))
(define (printconfig e store)
  (string-append (format "< ~a , ~a  >" (printexpr e)
                     (printstore store )))
)

(: printreduce (Expr (Listof Store) -> String))
(define (printreduce e store)
  (string-append (printconfig e store))
  (reduce1 e store)
)


(define (sort pairs)
  (cond
    ['() '()]
    [(cons pairs) (insert (car pairs)
                        (sort (cdr  pairs)))]))

(define (insert n pairs)
  (cond
    ['()  (cons n '())]
    [else (cond
            [(> n (car (car pairs))) (cons n pairs)]
            [else (cons (car pairs)
    (insert n (cdr pairs )))])]))

(define (printstore pairs)
    (let* ([pairs (sort pairs )])
       rawprintstore pairs )
)
