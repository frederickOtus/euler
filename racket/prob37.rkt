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
      (run-sieve 2))))

(define member?
  (lambda (m l)
    (if (null? l)
        #f
        (or (eq? m (car l)) (member? m (cdr l))))))

(define mysieve (prime-sieve (expt 10 6)))
(define primes (filter (lambda (x) (> x 0)) (vector->list mysieve)))

(define prime?
  (lambda (n) (< 0 (vector-ref mysieve n))))

(define shift-ltr
  (lambda (n)
    (modulo n (expt 10 (inexact->exact (floor (/ (log n) (log 10))))))))

(define ltr-truncate
  (lambda (n)
    (if (< n 10)
        (prime? n)
        (and (prime? n) 
             (ltr-truncate (shift-ltr n)))
        )))

(define rtl-truncate
  (lambda (n)
    (if (< n 10)
        (prime? n)
        (and (prime? n)
             (rtl-truncate (quotient n 10))))))

(define t-prime
  (lambda (n)
    (and (> n 10)
         (ltr-truncate n)
         (rtl-truncate n))))

(foldl + 0 (filter t-prime primes))