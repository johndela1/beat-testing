#!/usr/bin/csi -s

(use srfi-18)
(use posix)

(define song '(B _ B B B _))
;(define song '(B _ _ _ _ _ B _ _ _ _ _ B _ _ _ _ _ B _ _ _ _ _ B _ _ _ _ _ B _ _ _ _ _ B _ _ _ _ _ B _ _ B _ _ _ _ _ _ _ _ _ _ _ _ _ _ B _ B _ B _ B _ _ _ _ _ B _ _ _ _ _ B _ _ _ _ _ B _ _ _ _ _ _ _ _ _ _ _))
;(define song '(B _ _ _ _ _ B _ _ B _ _ _ _ _ _ _ _ _ _ _ _ _ _ B _ B _ B _ B _ _ _ _ _ ))

(define *song-len* (length song))
(define *hz* 100)
(define *jiffies-per-note* 20)

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(define (sound freq duration)
  (oscilate (* 8000 duration)
            (/ 8000 (* 2 freq))
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
      #f
      (begin (write  '-) (read-byte))))

(define (beat? counter)
  (= 0 (modulo counter *jiffies-per-note*)))

(define (get-note-index counter)
  (modulo (/ counter *jiffies-per-note*) *song-len*))

(define (check-for-beat counter)
 (if (beat? counter)
   (if (eq? 'B (list-ref song (get-note-index counter)))
       (begin
         ;(play get-boom)
         (write 'boom))
       )))

(define (loop counter record)
  (when (= (modulo counter 10) 0)
        (print ""))
  (check-for-beat counter)
  (thread-sleep! (/ 1 *hz*))

  (if (> counter 0)
      (loop (sub1 counter)
            (cons (poll-keys) record))
      record))

(print (loop (* *hz* *song-len*) '()))


