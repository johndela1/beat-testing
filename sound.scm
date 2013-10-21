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

(define (poll-keys)
  (if (null? (file-select '(0) '() 0))
      ""
      (read-byte)))
 

(define (check-clock counter)
  (= 0 (modulo counter 10)))

(define (get-note-index counter)
  (modulo (/ counter 10) 6))

(define (nth l n)
  (define (iter l m)
    (if (= n m)
        (car l)
        (iter (cdr l) (add1 m))))
  (iter l 0))
   
(define song '(B _ B _ B _))
(define (loop counter)
  ;(print 'tick)
  (if (check-clock counter)
      (print (nth song (get-note-index counter)))
      "")
  (print (poll-keys))
  (thread-sleep! .05)
  (loop (add1 counter)))

(loop 0)
