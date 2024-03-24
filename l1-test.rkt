#lang typed/racket

(require typed/rackunit "l1.rkt")
(require racket/match)

 ( check-equal? (updates (list ( Loc "l1" )(LocValue 1)) 3) (None) "Test successfull")
 ( printreduce (Seq( Assign "l1" (IntValue 3)) (Deref "l1"))  (list ( Loc "l1" )(LocValue 1)))

(define (matcher obj)
  (match obj
    [(list 'Op (? number? n1) (or 'Plus 'GTEQ) (? number? n2))
     'first]
    [_
     'Error]))
