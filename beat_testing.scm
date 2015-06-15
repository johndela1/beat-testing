#!/usr/local/bin/csi -s

(use srfi-1)

(define (make-ts l)
	(define (make-ts-iter l t)
		(cond
			((null? l) '())
			(( = 0 (car l)) (make-ts-iter (cdr l) (add1 t)))
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
	
(define (errors ref input)
	(map (lambda (pair) ((zip ref input))

(print (make-ts '(1 0 0 0 1 0 0 0 1)))
(print (map (lambda (x) (* (+ 100 x) 10)) (make-ts '(1 0 0 0 1 0 0 0 1))))
(print (map (lambda (x) (+ (* 10 x) 100)) (make-ts '(1 0 0 0 1 0 0 0 1))))

(print (start-at-zero '(0 1 2)))
(print (scale (start-at-zero '(10 14 18)) 2))
(print (scale (start-at-zero '(100 140 181)) 2))
(print (scale (start-at-zero (make-ts '(1 0 0 0 1 0 0 0 1))) 2))


(print (errors '(1 2 3) '(1 2 4)))
