#!/usr/bin/csi -s

(use srfi-18)

(define *SAMPLE-RATE* 8000)

(define (sound freq duration)
  (oscilate (* *SAMPLE-RATE* duration)
            (/ *SAMPLE-RATE* (* 2 freq))
            0))


(define (oscilate len half_wlen amplitude)
  (if (= len 0)
      (quote ())
      (if (= 0 (modulo (- 1 len) half_wlen))
          (cons amplitude (oscilate (sub1 len) half_wlen
                                    (bitwise-xor amplitude 255)))
          (cons amplitude (oscilate (sub1 len) half_wlen amplitude)))))



(define (loop freq)
(map write-byte (sound freq 1))
(thread-sleep! 1)
(map write-byte (sound freq 1))
(thread-sleep! 1)
(map write-byte (sound freq 1))
(thread-sleep! 1)
(map write-byte (sound freq 1))
(thread-sleep! 2)
(loop freq))

(define (output bytes)
	(if (null? bytes) 0
			  (when
				  (write-byte (car bytes))
				(output (cdr bytes)))))

(loop 100)
;(output (sound 1000 .01))
