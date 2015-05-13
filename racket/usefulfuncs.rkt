#lang racket

(define prime-sieve
  (lambda (n)
    (letrec ((sieve (make-vector n 1))
             (sieve-pass (lambda (x inc)
                           (if (< x n)
                               (begin (vector-set! sieve x 0) (sieve-pass (+ x inc) inc))
                               (run-sieve (+ 1 inc)))))
             (run-sieve (lambda (num)
                          (cond ((>= num n) sieve)
                                ((> (vector-ref sieve num) 0) (begin (vector-set! sieve num num) (sieve-pass (* 2 num) num)))
                                (else (run-sieve (add1 num)))))))
      (vector-set! sieve 0 0)
      (vector-set! sieve 1 0)
      (filter (lambda (x) (> x 0)) (vector->list (run-sieve 2))))))