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

(define vector-swap!
  (lambda (v inda indb)
    (let ((a (vector-ref v inda))
          (b (vector-ref v indb)))
      (vector-set! v inda b)
      (vector-set! v indb a))))

(define vector-sorted?
  (lambda (v comp)
    (let ((size (vector-length v)))
      (andmap (lambda (n)
                (comp (vector-ref v n)
                      (vector-ref v (+ 1 n))))
              (range (sub1 size))))))

(define comp-subvector
  (lambda (v start comp)
    (let ((size (vector-length v)))
      (foldl (lambda (new old) (if (comp (vector-ref v new)
                                         (vector-ref v old))
                                   new
                                   old))
             start
             (range start size)))))

(define subvector-sort!
  (lambda (v comp start)
    (let* ((size (vector-length v))
           (comp-ind (comp-subvector v start comp)))
      (if (>= start size)
          #t
          (begin (vector-swap! v start comp-ind) (subvector-sort! v comp (add1 start)))))))

(define find-next-swap
  (lambda (v comp)
    (letrec ((iter (lambda (ind)
                     (cond ((= ind 0) -1)
                           ((comp (vector-ref v ind)
                                  (vector-ref v (sub1 ind))) (sub1 ind))
                           (else (iter (sub1 ind)))))))
      (iter (sub1 (vector-length v))))))

(define set-next-perm!
  (lambda (v lt gt)
    (let* ((next-swap (find-next-swap v gt))
           (min-swap (comp-subvector v (add1 next-swap) (lambda (a b) (and (lt a b)
                                                                           (gt a (vector-ref v next-swap)))))))
      (vector-swap! v next-swap min-swap)
      (subvector-sort! v lt (add1 next-swap))
      v)))

(define v (vector 1 2 3 4 5))
(set-next-perm! v < >)