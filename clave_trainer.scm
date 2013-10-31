#!/usr/bin/csi -s

(use srfi-18)
(use posix)
(use sdl-mixer)
(open-audio)


(define *hz* 45)
(define *jiffies-per-note* 10)

;(define *song* '(B _ B B B _));two  over three
;(define *song* '(B _ B _ _ B _ B _ B _ _)) ;rhumba clave
(define *song* '(B _ B _ B _ _ B _ B _ _))  ;son clave
;(define *song* '(B _ _ _ _ _ B _ _ B _ _ _ _ _ _ _ _ _ _ _ _ _ _ B _ B _ B _ B _ _ _ _ _ B _ _ _ _ _ B _ _ _ _ _ B _ _ _ _ _ _ _ _ _ _ _))
;(define *song* '(B _ _ _ _ _ B _ _ B _ _ _ _ _ _ _ _ _ _ _ _ _ _ B _ B _ B _ B _ _ _ _ _ );)
(define *song-len* (length *song*))
(define *boom* (load-sample "boom.vorbis"))



(define (poll-keys!)
  (if (null? (file-select '(0) '() 0))
      #f
      (begin (print  '-) (read-byte))))

(define (get-sub-division jiffies)
  (modulo (/ jiffies *jiffies-per-note*) *song-len*))

(define (check-to-play sub-div)
; (display ".")
  (if (= 0 sub-div) (display 'one))
  (eq? 'B (list-ref *song* sub-div)))

(define (sub-division? jiffies)
  (= 0 (modulo jiffies *jiffies-per-note*)))

(define (check-sub-division jiffies)
  (if (and (sub-division? jiffies)
      (check-to-play (get-sub-division jiffies)))
      (begin
        (play-sample *boom*)
        (write 'boom))))


(define (loop jiffies)
  (if (poll-keys!)
      0
      (print ""))
  (check-sub-division jiffies)
  (thread-sleep! (/ 1 *hz*))
  (loop (add1 jiffies)))

(loop 0)
(close-audio)
