#lang racket

#|

So memoizing is a caching strategy to reduce computation time. In this case, I'm associating a hashmap with a function. Everytime that function
is called, it checks the hashmap to see if that problem has a stored solution, if so, return it, else calculate and store the result before returning
it.

This memoize function takes in a function, and returns a memoized version of that function

This implementation is generic enough as it assumes:
-the function you are memoizing takes a single argument
-the function you are memoizing returns booleans

This are easy enough to fix, o well

|#
(define memoize
  (lambda (f)
    (let ((hsh (make-hash '())))
      (lambda (n)
        (let ((res (hash-ref hsh n -1)))
          (if (boolean? res) res
              (let ((nres (f n)))
                (hash-ref! hsh n nres)
                ;(printf (string-append "Stored: " (number->string n) " \n"))
                nres)))))))
                       

(define is-prime
    (lambda (n)
      (if (< n 0)
          #f
          (let ((max (inexact->exact (floor (sqrt n))))) ;when testing prime-ness, you only need to check divisors up to sqrt(n)
            (not (ormap (lambda (x) (= 0 (modulo n x))) (range 2 (+ 1 max))))))));this line is a doozy, explaination continues:
#|
So:

(not (ormap (lambda (x) (= 0 (modulo n x))) (range 2 (+ 1 max))))

(range 2 (+ 1 max)) will give the list of all numbers I want to test divisibility by
-(range a b) creates a list of nums from a to (b - 1)

ormap is a combination of a map and a fold. It takes 2 args:
-a function that takes 1 arg and returns a boolean
-a list

It maps that function (applies it to every element in the list) and then returns True if atleast one of those mapped values is true, and false
if they all are.

(lambda (x) (= 0 (modulo n x))) is a simple divisbility test

So, (not (ormap <divisiblity test> <divisors to test>)) returns true if no number divides n, and is thus prime
|#

(define mis-prime (memoize is-prime)) ;mis-prime is a memoized version. If you want to run my solution w/o memoization, comment this line and uncomment the next
;(define mis-prime is-prime)

;I found that the memoizing cut execution time in half


(define quadradic
  (lambda (a b n)
    (+ (* n n) (* n a) b))) ;this is the n^2 + n*a + b from the problem, nothing special

(define sequence-length ;this function test the number of primes that a function can find
  (lambda (a b n)       ;passing n as an argument allows for tailcall optimization
    (if (mis-prime (quadradic a b n))
        (sequence-length a b (+ 1 n))
        (- n 1))))


(define test-pairs
  (lambda (a b enda endb)
    (letrec
        ((next-best (lambda (olda oldb newa newb oldmax) ;next best takes the last best, and a new pair to test and calls test-next with the the
                      (let ((newmax (sequence-length newa newb 0))) ;pair with the longest sequence
                        (if (> newmax 0)
                            (test-next newa newb (+ 1 newa) newb newmax)
                            (test-next olda oldb (+ 1 newa) newb oldmax)))))
         (test-next (lambda (olda oldb newa newb oldmax)
                      (cond ((and (> newa enda) ;tests basecase,and generates the next pairs to test with next-best
                                  (> newb endb)) (list olda oldb oldmax))
                            ((= 0 (modulo newb 2)) (test-next olda oldb newa (+ newb 1) oldmax))
                            ((> newa enda) (test-next olda oldb a (+ newb 2) oldmax))
                            (else (next-best olda oldb newa newb oldmax))))))
      (test-next a b a b 0))))

(time (test-pairs -999 -999 999 999))
;fun story, if you run test pairs 2x, the second one should run faster as it has all the prime values memoized