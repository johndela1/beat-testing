(define (square x)
	(* x x))

(define (sum-of-squares x y)
	(+ (square x) (square y)))

(define (larger x y)
	(if (> x y) x y))

(define (largest-two-square-sum x y z)
	(if (= x (larger x y))
		(sum-of-squares x (larger y z))
		(sum-of-squares y (larger x z))))


(define (a-plus-abs-b a b)
	((if (< b 0) - +) a b))


	
(define (jj-sqrt x)

	(define (good-enough guess)
		(< (abs (- x (square guess))) .001))
	(define (average x y) (/ (+ x y) 2))
	(define (improve-guess guess)
		(average guess (/ x guess)))

	(define (sqrt-iter guess)
		(if (good-enough guess)
			guess
			(sqrt-iter (improve-guess  guess))))


	(sqrt-iter 1.0))

(define (linear-recursive-fact n)
 (if (<=  n 1) 1
              (* n (linear-recursive-fact (- n 1)))))

(define (linear_iterative_process n)
	(define (iter prod count)	
		(if (>= count n) (* n prod)
			(iter (* count prod) (+ 1 count))))
	(iter 1 1))

(define (fib n)
	(cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 2)) (fib (- n 1))))))

(define (ifib n)
	(define (iter count f1 f2)
		(if (= count 0) f1
			(iter (- count 1) f2 (+ f1 f2))))
	(iter n 0 1))



(define (first-denom kinds)
	(cond ((= kinds 1) 1)
	      ((= kinds 2) 5)
	      ((= kinds 3) 10)
	      ((= kinds 4) 25)
	      ((= kinds 5) 50)
	      ))

(define (cc amount kinds path)
	(print "path: " path  " amount:" amount " kinds:" kinds) 
	(cond ((= 0 amount) 1)
	      ((< amount 0) 0)
	      ((= 0 kinds) 0)
	      (else (+ (cc amount (- kinds 1) (cons 'a path))
	      	       (cc (- amount (first-denom kinds)) kinds
			 (cons 'b path))))))

(define (it-exp b n a)
  (if (= n 0)
      a
      (it-exp b (sub1 n) (* b a))))

(define (iexp b n)
  (define (iter b n prod)
    (if (= n 0)
        prod
        (iter b (sub1 n) (* prod b))))
  (iter b n 1))

(define (jexp b n)
  (if (= 0 n) 1 (* b (jexp b (sub1 n)))))

(define (fast-expt b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

(define (jmul a b)
  (if (= b 0)
      0
      (+ a (jmul a (sub1 b)))))
