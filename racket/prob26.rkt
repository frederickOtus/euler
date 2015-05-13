#lang racket

(require racket/fixnum)

(define divpair
  (lambda (tar div)
    (cons (fxquotient tar div) (fxmodulo tar div))))

(define resind
  (lambda (lst res ind)
    (if (null? lst) -1
        (let ((a (car (car lst)))
              (b (cdr (car lst))))
          (if (and (fx= a (car res))
                   (fx= b (cdr res))) ind
                                      (resind (cdr lst) res (+ 1 ind)))
          )
        )))

(define iterate 
  (lambda (rem n res)
    (let* ((a (quotient rem n))
          (b (modulo rem n))
          (pair (cons a b)))
      (if (= 0 b) 0
          (let ((ind (resind res pair 0)))
            (if (= ind  -1) (iterate (* b 10) n (cons pair res))
                (+ 1 ind))
            )))))
            

(define test-case
  (lambda (n) (iterate 10 n '())))

(define all (map (lambda (n) (cons n (test-case n))) (range 2 1000)))
(define max-pair (lambda (pairs)
                   (foldl (lambda (p1 p2) (if (> (cdr p1) (cdr p2)) p1 p2)) (cons 0 0) pairs)))


(max-pair all)