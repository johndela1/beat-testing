#!/usr/bin/csi -s

(use srfi-18)
(use posix)

(define *hz* 10)
(define *jiffies-per-note* 10)

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(define (sound freq duration)
  (let ((*ihz* 8000))
  (oscilate (* *ihz* duration)
            (/ *ihz* (* 2 freq))
            0)))


(define (oscilate len half_wlen amplitude)
  (if (= len 0)
      (quote ())
      (if (= 0 (modulo (- 1 len) half_wlen))
          (cons amplitude (oscilate (sub1 len) half_wlen
                                    (bitwise-xor amplitude 255)))
          (cons amplitude (oscilate (sub1 len) half_wlen amplitude)))))

(define (play sound-func)
  (with-output-to-file "/dev/null" (lambda () (sound-func))))

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
      (read-byte)))

(define (beat? counter)
  (= 0 (modulo counter *jiffies-per-note*)))

(define (get-note-index counter)
  (modulo (/ counter *jiffies-per-note*) song_len))

(define song '(B _ B B B _))
(define song_len 6)

(define (beat counter)
 (if (eq? 'B (list-ref song (get-note-index counter)))
     (begin
      (play get-boom)
      (print 'boom))))

(define (loop counter record)
  (if (beat? counter)
      (beat counter))
  (thread-sleep! (/ 1 *hz*))

  (if (> counter 0)
      (loop (sub1 counter)
            (cons (poll-keys) record))
      record))

(print (loop 100 '()))


