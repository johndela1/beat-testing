#!/usr/bin/csi -s

(use posix)
(use srfi-1)
(use srfi-18)
(use sdl-mixer)
(open-audio)
(define *boom* (load-sample "boom.vorbis"))


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
	
(define (pass? song input)
		(not (any (lambda (x) (print x) (> x 60))
			(map (lambda (pair) (abs (- (car pair) (cadr pair))))
			     (zip song input)))))

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

(define (count-strikes song)
	(define (loop song acc)
		(if (null? song)
			0
			(loop (cdr song) (add1 acc))))
	(loop song 0))

(define (play song)
	(if (null? song)
		't
		(begin
			(thread-sleep! (/ (car song) 500))
			(play-sample *boom*)
			(print 'XXXXXXXXX)
			(play (cdr song)))))
		
(define (ts->period song)
	(define (loop song prev_ts)
		(if (null? song)
			'()
			(let ((ts (car song)))
				(cons (- ts prev_ts) (loop (cdr song) ts)))))
	(loop song 0))

(define song (scale (make-ts '(x 0 0 0 x 0 0 0 0 0 0 0 x 0 0 0)) 1000))
;(define song (scale (make-ts '(x 0 x 0 x 0 x 0 0 x 0 x 0 x 0 x)) 1000))
(define song (scale (make-ts '(X 0 X X X 0)) 1000));two  over three


(print (length song))
(sleep 1)
(play (ts->period song))
(define (loop)
	(let ((input (scale (start-at-zero (read-pattern (length song) '())) 1000)))
		(print (pass? song input)))
;	(loop))
)
(loop)
;(print song)
;(print (ts->period (scale song 2000)))
;(play (ts->period (scale song 4000)))
