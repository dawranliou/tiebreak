(in-package #:tiebreak)

(defstruct ball
  x y dx dy)

(defun init-ball (x y dx dy)
  (make-ball :x x :y y :dx dx :dy dy))

(defun update-ball (b)
  (incf (ball-x b) (ball-dx b))
  (incf (ball-y b) (ball-dy b)))

(defun draw-ball (b)
  (draw-circle (ball-x b) (ball-y b) 5.0 +red+))

(defun ball-out-of-bound (b)
  (let ((x (ball-x b))
        (y (ball-y b)))
    (or (< 800 x)
        (< x 0)
        (< 450 y)
        (< y 0))))
