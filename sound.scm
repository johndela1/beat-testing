#!/usr/bin/csi -s

(use srfi-18)
(use posix)

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

(define (play sound-func)
  (with-output-to-file "/dev/dsp" (lambda () (sound-func))))

(define (get-boom)
  (output (sound 500 .05)))

(define (get-chick)
  (output (sound 10000 .05)))

(define (output bytes)
  (if (null? bytes)
      0
      (when 't
       (write-byte (car bytes))
       (output (cdr bytes)))))

(define (loop)
  (if (null? (file-select '(0) '() 1))
      'f
      (read))
  (play get-boom)
  (thread-sleep! .1)
  (play get-chick)
  (thread-sleep! .1)
  (loop))
(loop)
