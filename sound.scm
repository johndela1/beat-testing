#!/usr/bin/csi -s

(use srfi-18)

(define *sample-rate* 8000)

(define (sound freq duration)
  (let* ((half_wavelength (/ *sample-rate* (* 2 freq)))
         (len (* *sample-rate* duration)))
    (oscilate len 255 half_wavelength)))


(define (oscilate len amplitude half_wlen)
  (if (= len 0) (quote ())
		(if (= 0 (modulo len half_wlen))
			(cons amplitude (oscilate (sub1 len) (bitwise-xor amplitude 255) half_wlen))
			(cons amplitude (oscilate (sub1 len) amplitude half_wlen)))))



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
