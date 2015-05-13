#lang racket

(define (pentagon n)
  (/ (* n (- (* 3 n) 1)) 2))

(define (solve-quadratic-equation a b c)
  (define disc (sqrt (- (* b b)
                        (* 4.0 a c))))
  (/ (+ (- b) disc)
     (* 2.0 a)))

(define (is-pentagon n)
    (= n (pentagon (pent-val n))))

(define pent-val
  (lambda (n)
    (inexact->exact (floor (solve-quadratic-equation 3 -1 (* -2 n))))))

(define find-diff-partners
  (lambda (n)
    (letrec ((solve (lambda (test sols)
                      (let ((pn (pentagon n))
                            (ptest (pentagon test)))
                                
                      (cond ((is-pentagon (+ pn ptest)) 
                             (solve (+ test 1) (cons (cons test (pent-val (+ pn ptest))) sols)))
                            ((< (+ (pentagon n)
                                   (pentagon test))
                                (pentagon (+ 1 test))) sols)
                            (else (solve (+ 1 test) sols)))))))
      (solve 1 '()))))

(define (solve)
  (letrec ((is-sol (lambda (lsols)
                     (cond ((null? lsols) #f)
                           ((is-pentagon (+ (pentagon (car (car lsols)))
                                            (pentagon (cdr (car lsols))))) #t)
                           (else (is-sol (cdr lsols))))))
           (solve-iter (lambda (n)
                        
                           (if (is-sol (find-diff-partners n))
                               n
                               (solve-iter (+ 1 n))))))
    (solve-iter 1)))

;sol slow as balls, needs optimized