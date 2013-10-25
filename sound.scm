#!/usr/bin/csi -s

(use srfi-18)
(use posix)

(define *SAMPLE-RATE* 8000)
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

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
      (begin
       (write-byte (car bytes))
       (output (cdr bytes)))))

(define (poll-keys)
  (if (null? (file-select '(0) '() 0))
      ""
      (read-byte)))

(define (check-clock counter)
  (= 0 (modulo counter 10)))

(define (get-note-index counter)
  (modulo (/ counter 10) 6))

(define song '(B _ B B B _))

(define (loop counter record)
  ;(print counter)
  (if (check-clock counter)
      (if (eq? 'B (list-ref song (get-note-index counter)))
          (begin
           (play get-boom)
           (print 'boom))))
  (print (poll-keys))
  (thread-sleep! .1)

  (if (> counter 0)
      (loop (sub1 counter) record)
      record))

(loop 100 '())
