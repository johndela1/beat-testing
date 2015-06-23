#!/usr/bin/csi -s

(use posix)
(use srfi-1)
(use srfi-18)
(use sdl-mixer)

(define TOLER 300)

(open-audio)
(define noise (load-sample "boom.vorbis"))

(define (play song)
	(if (null? song)
		't
		(begin
			(thread-sleep! (/ (car song) 1000))
			(play-sample noise)
			(print 'XXXXXXXXX)
			(play (cdr song)))))
		
(define (ts->deltas song)
	(define (loop song prev_ts)
		(if (null? song)
			'()
			(let ((ts (car song)))
				(cons (- ts prev_ts) (loop (cdr song) ts)))))
	(loop song 0))

(define (read-pattern song)
	(define (loop budget acc)
		(cond
		((<= budget 0) (map inexact->exact (reverse acc)))
		(else
			(let
			((t1 (current-milliseconds))
			(io_s (not (null?
				(file-select '(0) '() (/ budget 1000)))))
			(t2 (current-milliseconds)))
				(loop (- budget (- t2 t1))
					(if io_s
					(begin
						(read-byte)
						(cons t2 acc))
					acc))))))
	(if (null? song)
		'()
		(loop (+ (last song) TOLER) '())))

(define (loop-song song n)
	(if (= n 0)
		'()
		(append song (loop-song song (sub1 n)))))

(define song 
	(loop-song '(b b 0 b b 0 b 0 b b 0 b b 0 0 b) 2));honky tonk cowbell

(define (analyze song input acc)
	(define (close-enough t1 t2)
		(<= (abs (- t1 t2)) TOLER))
	(define (remove i l)
		(cond
			((null? l) '())
			((eqv? (car l) i) (cdr l))
			(else (cons (car l) (remove i (cdr l))))))
	(define (find-match ts l)
		(cond
			((null? l) '())
			((close-enough (car l) ts) (car l))
			(else (find-match ts (cdr l)))))
	(cond
		((null? song) (list (reverse acc) input))
		(else (let ((match (find-match (car song) input)))
			(if (null? match)
			(analyze (cdr song) input
				(cons (cons 'missed (car song) ) acc))
			(analyze (cdr song)
			 	 (remove match input)
				 (cons (cons 'err_tup
				 	(cons (car song) (- match (car song))))
				 	acc)))))))
(define (start-at-zero l)
	(let ((offset (car l)))
		(map (lambda (x) (- x offset)) l)))

(define song '(0 1000 2000 3000))
;(play (ts->deltas song))
(define (main-loop)
	(let ((input (start-at-zero (read-pattern song))))
		(print 'song: song)
		(print 'input: input)
		(print 'error-report: (analyze song input '()))))

(main-loop)
