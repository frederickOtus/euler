#lang racket

(define maxval 354294)
(define pow 5)

(define (power a b)
  (letrec
      ((solve (lambda (a b n)
                (if (= b 0)
                    n
                    (solve a (- b 1) (* a n))))))
    (solve a b 1)))

(define (split n)
  (letrec
      ((solve (lambda (n lst)
                (if (> n 9)
                    (solve (quotient n 10) (cons (modulo n 10) lst))
                    (cons n lst)))))
    (solve n '())))

(define (is-n-digit-power n pow)
  (= n (foldl + 0 (map (lambda (n) (power n pow)) (split n)))))

(define (solve-30 n pow lst)
  (cond ((= 1 n) lst)
        ((is-n-digit-power n pow) (solve-30 (- n 1) pow (cons n lst)))
        (else (solve-30 (- n 1) pow lst))))

(foldl + 0 (solve-30 maxval pow '()))
