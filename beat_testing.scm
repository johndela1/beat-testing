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

(define (play pattern)
	(if (null? pattern)
		't
		(begin
			(thread-sleep! (millis->secs (car pattern)))
			(print 'sleep-for (car pattern))
			(play-sample noise)
			;(print 'note)
			(play (cdr pattern)))))
		
(define (analyze pattern input)
	(define (deltas->ts deltas)
		(define (loop deltas acc-time)
			(if (null? deltas)
				'()
				(let ((ts (+ acc-time (car deltas))))
				(cons ts (loop (cdr deltas) ts)))))
		(loop deltas 0))
	(define (close-enough t1 t2)
		(<= (abs (- t1 t2)) TOLER))
	(define (remove i l)
		(cond
			((null? l) '())
			((eqv? (car l) i) (cdr l))
			(else (cons (car l) (remove i (cdr l))))))
	(define (find-match ts l)
	(print 'find-match l)

		(cond
			((null? l) '())
			((close-enough (car l) ts) (car l))
			(else (find-match ts (cdr l)))))
	(define (loop pattern input acc)
		(cond
		((null? pattern) (list (reverse acc) input))
		(else (let ((match (find-match (car pattern) input)))
			(if (null? match)
			(loop (cdr pattern) input
				(cons (cons (car pattern) 'missed) acc))
			(loop (cdr pattern)
			 	 (remove match input)
				 (cons (cons 'err_tup
				 	(cons (car pattern) (- match (car pattern))))
				 	acc)))))))
	(loop (deltas->ts pattern) (deltas->ts input) '()))

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

(define (read-samples n)
	(define (make-deltas samples i prev)
		(cond
		((null? samples) '()) ((car samples)
			(cons (- i prev)
				(make-deltas (cdr samples) (add1 i) i)))
		(else (make-deltas (cdr samples) (add1 i) prev))))

	(define (loop n)
	(cond
		((<= n 0) '())
		(else
			(if (= 0 (modulo n 500)) (play-sample noise))

			(let ((sample (poll-keys)))
				(thread-sleep! (/ 1 HZ))
				(cons sample (loop (sub1 n)))))))
	(make-deltas (loop n) 0 0))
	
(define (loop-pattern pattern n)
	(if (= n 0)
		'()
		(append pattern (loop-pattern pattern (sub1 n)))))
(define pattern
	(loop-pattern '(b b 0 b b 0 b 0 b b 0 b b 0 0 b) 2));honky tonk cowbell

(define pattern '(0 500 500 500))

(play pattern)
(thread-sleep! .5)
(let ((s (read-samples (* HZ 2))))
(print (analyze pattern s))
(print 'p: pattern)
(print 'i: s))
