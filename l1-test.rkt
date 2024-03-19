#lang typed/racket

(require typed/rackunit "l1.rkt")
(require racket/match)

 ( check-equal? (updates '((2,1)) 3) (None) "Test successfull")
 ( check-equal? (updates '((3,1)) 3) (Some '((3 (,1)))) "Test successfull")
 ( check-equal? (updates '((3,1)) 3) (Some '((3 (,1)))) "Test successfull")
 ( printexpr (Seq( Assign "l1" (Deref "l1")) (Deref "l1") ))
 ( printexpr (Seq( Assign "l1" (IntValue 3)) (Deref "l1") ))
 ( printreduce (Seq( Assign "l1" (IntValue 3)) (Deref "l1"))  (list ( Loc "l1" )(LocValue 1)))


(define (matcher obj)
  (match obj
    [(list 'Op (? number? n1) (or 'Plus 'GTEQ) (? number? n2))
     'first]
    [_
     'Error]))
