(in-package #:tiebreak)

(defparameter +g-per-frame+ (/ 32.174 4000))

(defstruct ball
  x y z dx dy dz hit-p)

(defun init-ball (x z dx dz)
  (make-ball :x x :z z :y 5
             :dx dx :dz dz :dy 0
             :hit-p nil))

(defun update-ball (b)
  (with-slots (x y z dx dy dz) b
    (let ((y-next (+ y dy)))
      (setf (ball-y b) (abs y-next)
            (ball-x b) (+ x dx)
            (ball-z b) (+ z dz)
            (ball-dy b) (- (* dy (if (< y-next 0) -0.9 1))
                           +g-per-frame+)))))

(defun draw-ball (b)
  (with-slots (x y z) b
    (draw-sphere (make-vector3 :x x :y y :z z) 1.0 +red+)))

(defun ball-out-of-bound (b)
  (with-slots (x y z) b
    (or (< 50 x)
        (< x -50)
        (< 50 z)
        (< z -50)
        (< y -10))))

(defun ball-hit (b)
  (unless (ball-hit-p b)
    (setf (ball-hit-p b) t
          (ball-dx b) (* -1 (ball-dx b))
          (ball-dz b) (* -1 (ball-dz b)))))
