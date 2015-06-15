#!/usr/local/bin/csi -s

(use posix)
(use srfi-1)

(define (make-ts l)
	(define (make-ts-iter l t)
		(cond
			((null? l) '())
			((eq? 0 (car l)) (make-ts-iter (cdr l) (add1 t)))
			(else (cons t (make-ts-iter (cdr l) (add1 t))))))
	(make-ts-iter l 0))

(define (start-at-zero l)
	(let ((offset (car l)))
		(map (lambda (x) (- x offset)) l)))

(define (scale l tot_time)
	(let ((factor  (/ tot_time (last l))))
		(map inexact->exact
			(map round
				(map (lambda (x) (* x factor)) l)))))
	
(define (pass? ref input)
		(not (any (lambda (x) (print x) (> x 60))
			(map (lambda (pair) (abs (- (car pair) (cadr pair))))
			     (zip ref input)))))

(define (read-pattern n samples)
	(cond
		((= n 0) (map inexact->exact (reverse samples)))
		(else
			(file-select '(0) '() 3)
			(let ((ts (current-milliseconds)))
				(read-byte)
				(read-pattern (sub1 n) (cons ts samples))))))
(define (align ref input)
	(cond
		(else (print "This wont be trivial"))))

;(print (scale (start-at-zero (make-ts '(1 0 0 0 1 0 0 0 1))) 1000))
;(print (map (lambda (x) (* (+ 100 x) 10)) (make-ts '(1 0 0 0 1 0 0 0 1))))
;(print (map (lambda (x) (+ (* 10 x) 100)) (make-ts '(1 0 0 0 1 0 0 0 1))))

(define (go)
	(let (
		(input (scale
			(start-at-zero (read-pattern 8 '())) 1000))
		(ref (scale
			(make-ts '(x 0 x 0 x 0 x 0 0 x 0 x 0 x 0 x)) 1000)))

		(print (pass? ref input)))
	(loop))
(loop)
