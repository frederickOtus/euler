#lang racket

(define denom (reverse '(1 2 5 10 20 50 100 200)))

(define (solve denoms remain)
  (cond ((= remain 0) 1)
        ((null? denoms) 0) 
        ((>= remain (car denoms)) (+ (solve denoms (- remain (car denoms)))
                                    (solve (cdr denoms) remain)))
        (else (solve (cdr denoms) remain))
        ))
      