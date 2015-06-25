#!/usr/bin/csi -s

(use posix)
(use srfi-1)
(use srfi-18)
(use sdl-mixer)

(define TOLER 300)

(open-audio)

(define noise (load-sample "boom.vorbis"))

(define (millis->secs ts)
	(/ ts 1000))

(define (play song)
	(if (null? song)
		't
		(begin
			(thread-sleep! (millis->secs (car song)))
			(play-sample noise)
			;(print 'note)
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
		((<= budget 0)
			(define (grace budget acc)
			(cond
			((<= budget 0) acc)
			(else (let
			((t1 (current-milliseconds))
			(timeout (null?
				(file-select '(0) '() (millis->secs budget))))
			(t2 (current-milliseconds)))
				(grace (- budget (- t2 t1))
					(if timeout
						(begin
							acc)
						(begin
							(read-byte)
							(cons t2 acc))))))))
			(map inexact->exact (reverse
				(append acc (grace TOLER '())))))
		(else
			(let
			((t1 (current-milliseconds))
			(timeout (null?
				(file-select '(0) '() (millis->secs budget))))
			(t2 (current-milliseconds)))
				(loop (- (- budget (- t2 t1)) TOLER)
					(if timeout
						(begin
							(play-sample noise)
							(print 'note)
							acc)
						(begin
							(read-byte)
							(cons t2 acc))))))))
	(if (null? song)
		'()
		(append (loop (car song) '())
			(read-pattern (cdr song)))))
	;(if (null? song)
	;	'()
	;	(loop (+ (last song) TOLER) '())))

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
				(cons (cons (car song) 'missed) acc))
			(analyze (cdr song)
			 	 (remove match input)
				 (cons (cons 'err_tup
				 	(cons (car song) (- match (car song))))
				 	acc)))))))
(define (start-at-zero l)
	(let ((offset (car l)))
		(map (lambda (x) (- x offset)) l)))

(define (poll-keys)
	(if (null? (file-select '(0) '() 0))
		#f
		(begin
			(read-byte)
			#t)))
(define HZ 1000)
(define (read-samples n acc)
	(cond
		((<= n 0) (reverse acc))
		(else
			(if (= 0 (modulo n 500))
				(play-sample noise))
			(let ((sample (poll-keys)))
				(thread-sleep! (/ 1 HZ))
				(read-samples (sub1 n) (cons sample acc))))))

(define (samples->ts samples acc counter)
	(cond
		((null? samples) (reverse acc))
		((car samples)
			(samples->ts (cdr samples)
				     (cons counter acc) (add1 counter)))
		(else (samples->ts (cdr samples) acc (add1 counter)))))
	
	
;(print (read-samples (* HZ 1) '()))
(define song '(500 1000 1500))
;(play (ts->deltas song))
;(thread-sleep! .5)
(print (analyze song (samples->ts (read-samples (* HZ 1.6) '()) '() 0) '()))

(quit)
(define (main-loop)
	(let ((input (start-at-zero (read-pattern (ts->deltas song)))))
		(print 'song: song)
		(print 'input: input)
		(print 'error-report: (analyze song input '()))))

(main-loop)
