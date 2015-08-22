#!/usr/bin/csi -s

(use posix)
(use srfi-1)
(use srfi-18)
(use sdl-mixer)

(define HZ 1000)
(define TOLER 300)

(open-audio)

(define noise (load-sample "boom.vorbis"))

(define (secs->millis t)
  (* t 1000))

(define (millis->secs t)
  (/ t 1000))

(define (play pattern duration)
  (let ((deltas (pattern->deltas pattern duration)))
    (define (loop deltas)
      (if (null? deltas)
	  't
	  (begin
	    (thread-sleep! (millis->secs (car deltas)))
					;(play-sample noise)
	    (print 'note)
	    (loop (cdr deltas)))))
    (loop deltas)))


(define (analyze pattern input duration)
  (define (deltas->tss deltas)
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
    (cond
     ((null? l) '())
     ((close-enough (car l) ts) (car l))
     (else (find-match ts (cdr l)))))

  (define (loop ref input)
    (if (null? ref)
	input
	(let ((match (find-match (car ref) input))
	      (head (car ref))
	      (tail (cdr ref)))
	  (if (null? match)
	      (cons (cons head 'missed)
		    (loop tail input))
	      (cons (cons head (- match head))
		    (loop tail
			  (remove match input)))))))
  (loop (deltas->tss (pattern->deltas pattern duration))
	(deltas->tss input)))

(define (start-at-zero l)
  (let ((offset (car l)))
    (map (lambda (x) (- x offset)) l)))

(define (poll-keys)
  (if (null? (file-select '(0) '() 0))
      #f
      (begin
	(read-byte)
	#t)))

(define (read-samples n)
  ;; XXX early beat is not punished but rather set to perfect score
  ;; because the early keypress is waiting on the keyboard buffer
  ;; and when the programs reads it gets it at T0 and when
  ;; the song starts at T0 it is seen as a perfect match
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
      ;; (if (= 0 (modulo n 500)) (play-sample noise))
      (let ((sample (poll-keys)))
	(thread-sleep! (/ 1 HZ))
	(cons sample (loop (sub1 n)))))))
  (make-deltas (loop (* HZ n)) 0 0))

(define (loop-pattern pattern n)
  (if (= n 0)
      '()
      (append pattern
	      (loop-pattern pattern (sub1 n)))))

(define (pattern->deltas pattern duration)
  (let ((time (quotient (secs->millis duration) (length pattern))))
    (define (convert pattern acc)
      (cond
       ((null? pattern) '())
       ((= 0 (car pattern)) (convert (cdr pattern) (+ acc time)))
       (else (cons acc (convert (cdr pattern) time)))))
    (convert pattern 0)))
;; maybe tolerance should be based on tempo

(define easy '(1 1 1 1))
(define arpeg '(1 0 0 1 1 0 1))
(define honky-tonk '(1 1 0 1 1 0 1 0 1 1 0 1 1 0 0 1))
(define honky-tonk-2 (loop-pattern '(1 1 0 1 1 0 1 0 1 1 0 1 1 0 0 1) 2))
(define syncopate '(1 0 1 0 0 1 0 1))
(define calib '(1 1))

(define (trial pattern)
 (define len-in-secs (quotient (length pattern) 2))
 (thread-sleep! (/ len-in-secs (length pattern)))
 (play pattern len-in-secs)
 (thread-sleep! (/ len-in-secs (length pattern)))
 (let ((samples  (read-samples len-in-secs)))
   (print 'score--------- (analyze pattern samples len-in-secs))
   (print 'ref-deltas---- (pattern->deltas pattern len-in-secs))
   (print 'sample-deltas- samples)))

(define pattern-name (cadddr (argv)))

(define result
  (with-exception-handler
   (lambda (exn)
     (print 'use-valid-pattern) (quit))
   (lambda ()
     (trial (eval (with-input-from-string pattern-name read))))))
