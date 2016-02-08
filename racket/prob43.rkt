#lang racket

(define test-vals
  (list '(2 . 2)
        '(3 . 3)
        '(4 . 5)
        '(5 . 7)
        '(6 . 11)
        '(7 . 13)
        '(8 . 17)))

(define div-test
  (lambda (numstring start divisor)
    (= 0 (modulo (quotient (modulo numstring (expt 10 (- 11 start))) (expt 10 (- 8 start))) divisor)))) 

(define truncate
  (lambda (numstring start)
    (quotient (modulo numstring (expt 10 (- 11 start))) (expt 10 (- 8 start)))
    ))

(define test-num
  (lambda (n)
    (andmap (lambda (tst)
              (div-test n (car tst) (cdr tst)))
            test-vals)))



