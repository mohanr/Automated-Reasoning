#lang typed/racket

;; (require typed/rackunit "l1.rkt")

(require racket/match)
(require typed/rackunit)

 ;; ( check-equal? (updates (list ( Loc "l1" )(LocValue 1)) 3) (None) "Test unsuccessfull")
 ;; ( check-equal? (updates (list ( Loc "l1" )(LocValue 1)) 1) (None) "Test unsuccessfull")
 ;; ( printf "~n [~a] " (sort (list (cons 1 4) (cons 2 9)) ))

 ;; ( printreduce (Seq( Assign "l1" (IntValue 3)) (Deref "l1"))  (list( cons( Loc "l1" )(LocValue 1))))

(define (matcher obj)
  (match obj
    [(list 'Op (? number? n1) (or 'Plus 'GTEQ) (? number? n2))
     'first]
    [_
     'Error]))

(struct None ()
    #:transparent)
(struct (i) Some ([v : i])
    #:transparent)
(define-type (Opt a) (U None (Some a)))


(: lookups  ((Listof (Pairof (U String Number)(U String Number))) String ->
                                 (Opt (U String Number))))
(define (lookups ls l)
  (match ls
    ['()   (None)]
    [(cons (cons (== l) n) ls) (Some n)]
    [(cons _ ls) (lookups ls l)]))

 ( check-equal? (lookups (list (cons  "l1" 11)(cons  "l2" 1)) "l2") (Some 1) "Test unsuccessfull")


(: update ((Listof (Pairof String  Number))
                   (Pairof String Number)->
          (Opt (Listof (Pairof String Number)))))
(define (update ls l)
  (match ls
    ['()   (None)]
    [(cons (cons s n) rest)
     (if (string=? s (car l))
         (Some (cons l (cdr ls)))
         (match (update rest l)
           [(Some updated) (Some (cons (cons s n) updated))]
           [(None) (None)]))]))


 ( check-equal? (update (list (cons  "l1" 1)(cons "l2" 2)) (cons "l1" 5)) (Some '(("l1" . 5) ("l2" . 2))) "Test unsuccessfull")
