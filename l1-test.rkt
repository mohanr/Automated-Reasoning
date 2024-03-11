#lang typed/racket

(require typed/rackunit "l1.rkt")


 (print( check-equal? (updates '((2,1)) 3) (None) "Test successfull"))
 (print( check-equal? (updates '((3,1)) 3) (Some '((3 (,1)))) "Test successfull"))
 ( check-equal? (updates '((3,1)) 3) (Some '((3 (,1)))) "Test successfull")
