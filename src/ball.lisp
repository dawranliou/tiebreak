(in-package #:tiebreak)

(defparameter +g-per-frame+ 0.005)
(defparameter +ball-r+ 1.0)

(define-entity ball (loc velocity))

(defun init-ball (x z vx vz)
  (create-entity 'ball
                 :loc/x x
                 :loc/y 5
                 :loc/z z
                 :velocity/x vx
                 :velocity/y 0.0
                 :velocity/z vz))

;;(init-ball 0 0 0 0)

(defun update-ball (b)
  (with-slots (loc/x loc/y loc/z velocity/x velocity/y velocity/z) b
    (let ((y-next (+ loc/y velocity/y)))
      (setf (loc/y b) (abs y-next)
            (loc/x b) (+ loc/x velocity/x)
            (loc/z b) (+ loc/z velocity/z)
            (velocity/y b) (- (* velocity/y (if (< y-next +ball-r+) -0.9 1))
                              +g-per-frame+)))))

(defun draw-ball-3d (b)
  (with-slots (loc/x loc/y loc/z) b
    (draw-circle-3d (make-vector3 :x loc/x :y 0 :z loc/z)
                    (+ loc/y 1.0)
                    (make-vector3 :x 1.0 :y 0.0 :z 0.0)
                    90.0
                    +blue+)
    (draw-sphere (make-vector3 :x loc/x :y loc/y :z loc/z) +ball-r+ +red+)))

(defun ball-out-of-bound (b)
  (with-slots (loc/x loc/y loc/z) b
    (or (< 50 loc/x)
        (< loc/x -50)
        (< 50 loc/z)
        (< loc/z -50)
        (< loc/y -10))))

(defun ball-hit (b)
  (setf (velocity/x b) (* -1 (velocity/x b))
        (velocity/y b) 0.2
        (velocity/z b) (* -1 (velocity/z b))))
