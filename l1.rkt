#lang typed/racket/base
(provide Opt lookup sort LocValue Loc printreduce printexpr updates BoolValue IntValue None Some Seq Deref Assign)
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
    [(IntValue n) (printf( string-append (format  "~a" n))) ""]
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
    [ (Assign l e ) =  (printf (format "Assign ~a := ~a" l (printexpr e ))) ""]
    [ Skip  ( string-append "skip")]
    [ (Seq e1 e2 )   (printf   " ~a ;  ~a" (printexpr e1 )
                                      (printexpr e2)) ""]
    [ (While  e1 e2 ) (printf   "while ~a do ~a " (printexpr e1 )
                                          (printexpr e2)) ""]
  ))

(struct None ()
    #:transparent)
(struct (i) Some ([v : i])
    #:transparent)
(define-type (Opt a) (U None (Some a)))

(: lookup  ((Listof  (Pairof Loc LocValue)) String  ->
            (Pairof (Opt Number) (Listof  (Pairof Loc LocValue)))))
(define (lookup ls l)
  (match ls
    ['()   (cons (None) ls)]
    [(cons (cons (Loc s) (LocValue n)) rest)
     (if (string=? s  l)
      (cons (Some n) ls)
     (lookup rest l))]
    ))


(: updates ((Listof (Pairof Loc LocValue)) (Pairof Loc LocValue) ->
                         (Opt (Listof  (Pairof Loc LocValue)))))
(define (updates ls l)
  (match ls
    ['()   (None)]
    [(cons (cons (Loc s) (LocValue n)) rest)
     (if (string=? s (match (car l) [(cons (Loc x) _) x]))
         (Some (cons l (cdr ls)))
         (match (updates rest l)
           [(Some updated) (Some (cons (cons (Loc s) (LocValue n)) updated))]
           [(None) (None)]))]))



(: reduce ((Pairof (Opt Expr)  (Listof (Pairof Loc LocValue))) ->
                      (Pairof (Opt Expr ) (Listof  (Pairof Loc LocValue)))))
(define (reduce expr)
  (match expr
    [  (cons (Some  (Op  (? integer? n1) Plus (? integer? n2)))
             store)
       (cons (Some  (IntValue (+ n1  n2))) store)]
    [  (cons (Some ( Op  (? integer? n1) GTEQ (? integer? n2 ))) store)
       (cons (Some  (BoolValue (>=  n1  'n2))) store)]
    [  (cons (Some (Op  (? integer? n1) Skip (? boolean? n2))) store)
             (match  (reduce (cons (Some n2) store))
               [ (cons (Some  (IntValue nn2)) store)  (cons (Some ((Op n1 Skip nn2))) store)]
               [ (None)  (None) ]
               )
             (match  (reduce  n1 store)
               [ (cons (Some   (IntValue nn1)) store)  (cons (Some (Op nn1 Skip n2)) store)]
               [ (None)  (None) ]
               )]
    [ (cons (Some (IntValue n )) store) (cons (None) store )]
    [ (cons (Some (If e1 e2 e3)) store)
             (match e1
               [#t  (cons (Some e2) store)  ]
               [#f  (cons (Some e3) store) ]
               [_   (match (reduce (cons (Some e1) store ))
                     [ (cons (Some e1) store) ( cons (Some (If e1 e2 e3)) store) ]
                     [ (None)  (None) ]
               )]
     )]
    [ (cons (Some (Deref l)) store)
             (match (lookup  store l)
               [ (cons (Some n ) store )  (cons (Some  (IntValue n)) store)]
               [ (cons (None) store)  (cons (None) store )]
     )]
    [ (cons (Some (Assign l e )) store)
             (match e
               [(IntValue n)
                (match (updates store (cons (Loc l) (LocValue n)))
                [ (Some store ) (cons  (Some (Skip))  store)]
                [ (None)  (cons (None) store ) ]
                [ _  (match (reduce (cons (Some e) store))
                        [(cons (Some  e) store)  (cons (Some (Assign l e)) store)]
                        [ (None)  (None) ]
                     )
                ]
               )]
               )]

   [  (cons (Some (Seq e1 e2))  store)
            (match e1
              [Skip  (cons (Some  e2) store) ]
              [ _  ( match (reduce (cons (Some e1) store ))
                    [  (cons (Some  e1) store)
                       (cons (Some  (Seq  e1 e2))  store ) ]
                    [ (None)  (None) ]

               )]
               )]
   [ (cons (Some (Skip))  store) ( cons (None) store )]

   [ (cons (Some (While e1 e2)) store)
     (cons (Some  ( If e1 (Seq e2 (While e1 e2)) (Skip))) store)  ]

))

(: reduce1 (Expr (Listof (Pairof Loc LocValue)) -> String))
(define (reduce1 e store)
    (match (reduce (cons (Some e) store))
       [  (cons (Some e) store)
          ( printf (format "~n --> ~a " (printconfig e store )))
          ( printf ( string-append (reduce1 e store )))
          ""
       ]
       [ (cons (None)  store)
         (string-append "~n -/->  "
                 (match e
                     ['Skip (string-append (format "(a value)~n"))]
                     [ _   (string-append (format "(stuck - not a value)"))]))
       ]
       )
)

(: rawprintstore : (  (Listof (Pairof Loc LocValue))  -> String))
(define (rawprintstore ls)
       (printf "Print Store")
       (match ls
         ['() (string-append "")]
         [(cons( cons l n) tail) (string-append (format " l  = ~a " n )
                               (rawprintstore tail ))]
         )
)


(: printconfig (Expr (Listof (Pairof Loc LocValue)) -> String))
(define (printconfig e store)
  (string-append (format "< ~a , ~a  >" (printexpr e)
                     (printstore store )))
)

(: printreduce (Expr (Listof (Pairof Loc LocValue)) -> Void))
(define (printreduce e store)
  (printf (printconfig e store))
  (printf (reduce1 e store))

)


(define (sort pairs)
  (cond
    ['() pairs]
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


 ( printreduce (Seq( Assign "l1" (IntValue 3)) (Deref "l1")) (list( cons( Loc "l1" )(LocValue 1))))
