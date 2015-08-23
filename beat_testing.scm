#!/usr/bin/csi -s

(use posix)
(use srfi-1)
(use srfi-18)
(use sdl-mixer)

(define BPM 80)
(define HZ 1000)
(define TOLER 300)

;; (open-audio)

;; (define noise (load-sample "boom.vorbis"))
(define (defined? v)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (exn)
	(k #f))
      (lambda () (eval (with-input-from-string v read)))))))

(define (secs->millis t)
  (* t 1000))

(define (millis->secs t)
  (/ t 1000))

(define (play pattern)
  (let ((deltas (pattern->deltas pattern)))
    (define (loop deltas)
      (if (not (null? deltas))
	  (begin
	    (thread-sleep! (millis->secs (car deltas)))
	    ;; (play-sample noise)
	    (print 'note)
	    (loop (cdr deltas)))))
    (loop deltas))
  (let* ((note-div (car pattern)) (notes (cadr pattern))
	 (delay-unit (/ (/ (length notes) note-div) (/ BPM 60))))
    ;; XXX need to account for more than one 0
    (thread-sleep!
	   (if (= 0 (last (cadr pattern)))
	       (* delay-unit 2)
	       delay-unit))))

(define (analyze pattern input)
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
  (let ((results (loop (deltas->tss (pattern->deltas pattern))
		    (deltas->tss input))))
    (define (passed? res)
      (cond
       ((null? res) 't)
       ((and
	 (not (atom? (car res)))
	 (integer? (cdar res)))
	(passed? (cdr res)))
       (else #f)))
    (cons (passed? results) results))) 

(define (poll-keys)
  (if (null? (file-select '(0) '() 0))
      #f
      (begin
	(read-byte)
	#t)))

(define (read-samples pattern)
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
  (define (sample-count pattern)
    (let ((note-div (car pattern)) (notes (cadr pattern)))
      	 (* (/ (/ (length notes) (/ note-div 4)) (/ BPM 60)) HZ)))
  (define (loop n)
    (cond
     ((<= n 0) '())
     (else
      ;; (if (= 0 (modulo n 500)) (play-sample noise))
      (let ((sample (poll-keys)))
	(thread-sleep! (/ 1 HZ))
	(cons sample (loop (sub1 n)))))))
  (make-deltas (loop (sample-count pattern)) 0 0))

(define (loop-pattern pattern n)
  (if (= n 0)
      '()
      (append pattern
	      (loop-pattern pattern (sub1 n)))))

(define (pattern->deltas pattern)
  (let* ((beat-div (car pattern)) (notes (cadr pattern))
    	 (time (secs->millis (/ (/ 4 beat-div) (/ BPM 60)))))
    (define (convert pattern acc)
      (cond
       ((null? pattern) '())
       ((= 0 (car pattern)) (convert (cdr pattern) (+ acc time)))
       (else (cons acc (convert (cdr pattern) time)))))
    (convert notes 0)))
;; maybe tolerance should be based on tempo

(define easy-4 '(4 (1 1 1 1)))
(define easy-8 '(8 (1 1 1 1)))
(define honky-tonk '(8 (1 1 0 1 1 0 1 0 1 1 0 1 1 0 0 1)))
(define honky-tonk-2 (loop-pattern honky-tonk 2))
(define syncopate '(8 (1 0 1 0 0 1 0 1)))
(define honky-tonk-4 '(4 (1 1 0 1 1 0 1 0 1 1 0 1 1 0 0 1)))
(define honky-tonk-8 '(8 (1 1 0 1 1 0 1 0 1 1 0 1 1 0 0 1)))

(define (trial pattern)
  (play pattern)
  (print 'done-play)
  (let* ((samples  (read-samples pattern))
	 (results (analyze pattern samples))
	 (passed (car results)))
    (print 'results: results)
    (if passed
	(print 'passed: (apply + (map (lambda (x) (abs (cdr x)))
				      (cdr results))))
	(print 'failed))
    (print 'ref-deltas: (pattern->deltas pattern))
    (print 'sample-deltas- samples)))

(define pattern-name (cadddr (argv)))

(if (defined? pattern-name)
    (trial (eval (with-input-from-string pattern-name read)))
    (print 'use-valid-pattern-name))
