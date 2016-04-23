#!/usr/bin/csi -s

;; prototype to make sure the problem has been understood

(define (is-p? n)
  (define (loop m)
    (cond
     ((<= m 1) 't)
     ((= (modulo n m) 0) #f)
     (else (loop (sub1 m)))))
  (loop (sub1 n)))

(define (primes n)
  (cond
   ((<= n 1)  '())
   ((is-p? n) (cons n (primes (sub1 n))))
   (else (primes (sub1 n)))))

(define (pfs n)
  (define (loop n ps)
    (cond
     ((null? ps) '())
     ((= 0 (modulo n (car ps)))
      (cons (car ps) (loop (/ n (car ps)) (cdr ps))))
     (else (loop n (cdr ps)))))
  (loop  n (primes n)))

(define (foo n)
  (case  n ((3) 'pling) ((5) 'plong) ((7) 'plang) (else n)))

(let ((pfs (reverse (pfs (* 2  3 5 7 17)))))
  (print pfs)
  (print (map foo pfs)))
