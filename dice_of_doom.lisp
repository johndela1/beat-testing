#!/usr/bin/clisp

(defparameter *num-players* 2)
(defparameter *max-dice* 2)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defun board-array (lst)
	(make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
	(board-array
		(loop for n below *board-hexnum*
			collect (list (random *num-players*)
				      (1+ (random *max-dice*))))))

(defun player-letter (n)
	(code-char (+ 97 n)))

(defun draw-board (board)
	(loop for y below *board-size*
		do (progn (fresh-line)
			(loop repeat (- *board-size* y)
				do (princ " "))
			(loop for x below *board-size*
			      for hex = (aref board (+ x (* *board-size* y)))
			      do (format t "~a-~a "
				      (player-letter (first hex))
				      (second hex))))))
(defun neighbors (pos)
	(let ((up (- pos *board-size*))
	      (down (+ pos *board-size*)))
		(loop for p in (append (list up down)
			(unless (zerop (mod pos *board-size*))
			 (list (1- up) (1- down)))
			(unless (zerop (mod (1+ pos) *board-size*))
			 (list (1+ up) (1+ down))))
			when (and (>= p 0) (< p *board-hexnum*))
			collect p)))

;; (draw-board (gen-board))
;(draw-board #((0 3) (0 3) (1 3) (1 1)))
(print (neighbors 2))
		
;(print '(0 3))

;;   0 1
;;  2 3
