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

(define primes (prime-sieve (expt 10 6)))

(define rotate
  (lambda (n)
    (let ((mag (inexact->exact (floor (/ (log n) (log 10))))))
      (+ (* (expt 10 mag) (modulo n 10))
         (quotient n 10)))))

(define member?
  (lambda (m l)
    (if (null? l)
        #f
        (or (eq? m (car l)) (member? m (cdr l))))))

(define rotations
  (lambda (n)
    (letrec ((mag (inexact->exact (floor (/ (log n) (log 10)))))
             (rotate (lambda (num) (+ (* (expt 10 mag) (modulo num 10))
                                      (quotient num 10))))
             (process-all (lambda (num rot l)
                            (if  (= 0 rot)
                                 l
                                 (process-all (rotate num) (sub1 rot) (cons (rotate num) l))
                                  ))))
      (process-all n mag (list)))))

(define has-prime-rotations
  (lambda (n) (andmap (lambda (x) (member? x primes)) (rotations n))))

(length (filter has-prime-rotations primes))