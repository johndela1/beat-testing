(use ezxdisp)

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (f-vect v1 v2 f)
  (cons (f (xcor-vect v1)
           (xcor-vect v2))
        (f (ycor-vect v1)
           (ycor-vect v2))))

(define (add-vect v1 v2)
  (f-vect v1 v2 +))

(define (sub-vect v1 v2)
  (f-vect v1 v2 -))

(define (scale-vect m v)
	(cons (* (xcor-vect v) m)
              (* (ycor-vect v) m)))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect
       (scale-vect (xcor-vect v) (edge1-frame frame))
       (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define f (make-frame  (make-vect 0 0) (make-vect 1000 0) (make-vect 0 1000) ))
(define ezx (ezx-init 1000 1000 "picture language"))
(ezx-set-background ezx (make-ezx-color 0 0 0))
(ezx-redraw ezx)

(define (draw-line p1 p2)
  (ezx-line-2d ezx (car p1)
                   (cdr p1)
                   (car p2)
                   (cdr p2)
                   (make-ezx-color 254 254 254)
                   10)
   (ezx-redraw ezx))

(define (for-each f l)
  (if (not (null? l))
    (begin
      (f (car l))
      (for-each f (cdr l)))))

(define (segments->painter segment-list)
  (lambda (frame)
   (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
       segment-list)))

((segments->painter (list
                     (make-segment (make-vect 0 0) (make-vect 0 1))
                     (make-segment (make-vect 0 0) (make-vect 1 0))
                     (make-segment (make-vect 1 1) (make-vect 0 1))
                     (make-segment (make-vect 1 1) (make-vect 0 1))))
		     f)

(define (loop) (loop))
(loop)

