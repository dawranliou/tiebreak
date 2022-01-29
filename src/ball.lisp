(in-package #:tiebreak)

(defparameter +g-per-frame+ 0.005)
(defparameter +ball-r+ 1.0)

(define-entity ball (loc velocity projection bound elastic drag))

(defun init-ball (x z vx vz)
  (create-entity 'ball
                 :loc/x x
                 :loc/y 5
                 :loc/z z
                 :bound/xmin -30.0
                 :bound/xmax 30.0
                 :bound/zmin -40.0
                 :bound/zmax 40.0
                 :elastic/damp 0.9
                 :drag/air 0.998
                 :velocity/x vx
                 :velocity/y 0.0
                 :velocity/z vz
                 :projection/color +blue+
                 :projection/r 1.0))

(defun update-ball (b)
  (with-slots (loc/x loc/y loc/z velocity/x velocity/y velocity/z drag/air) b
    (let ((y-next (+ loc/y velocity/y)))
      (setf (loc/y b) (abs y-next)
            (loc/x b) (+ loc/x velocity/x)
            (loc/z b) (+ loc/z velocity/z)
            (velocity/y b) (* (- (* velocity/y (if (< y-next +ball-r+)
                                                   (* -1 (elastic/damp b))
                                                   1))
                                 +g-per-frame+)
                              drag/air)
            (velocity/x b) (* velocity/x drag/air)
            (velocity/z b) (* velocity/z drag/air)))))

(defun draw-ball-3d (b)
  (with-slots (loc/x loc/y loc/z) b
    (draw-sphere (make-vector3 :x loc/x :y loc/y :z loc/z) +ball-r+ +red+)))

(defun ball-out-of-bound (b)
  (with-slots (loc/x loc/y loc/z) b
    (or (< 50 loc/x)
        (< loc/x -50)
        (< 50 loc/z)
        (< loc/z -50)
        (< loc/y -10))))

(defun ball-hit (b)
  (setf (velocity/x b) 5.0
        (velocity/y b) 0.2
        (velocity/z b) 5.0))
