(in-package #:tiebreak)

(defparameter +g-per-frame+ 0.005)
(defparameter +ball-r+ 1.0)

(define-entity ball (loc velocity projection bound elastic drag shape size))

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
                 :shape/type :sphere
                 :shape/color +red+
                 :size/w +ball-r+
                 :size/h +ball-r+
                 :projection/color +blue+
                 :projection/r 2.0))

(defun update-ball (b)
  (with-slots (loc/x loc/y loc/z velocity/x velocity/y velocity/z drag/air size/h) b
    (let ((y-next (+ loc/y velocity/y)))
      (setf (loc/y b) (abs y-next)
            (loc/x b) (+ loc/x velocity/x)
            (loc/z b) (+ loc/z velocity/z)
            (velocity/y b) (* (- (* velocity/y (if (< y-next (/ size/h 2))
                                                   (* -1 (elastic/damp b))
                                                   1))
                                 +g-per-frame+)
                              drag/air)
            (velocity/x b) (* velocity/x drag/air)
            (velocity/z b) (* velocity/z drag/air)))))


(defun ball-hit (b)
  (setf (velocity/x b) -1.0
        (velocity/y b) 0.25
        (velocity/z b) -1.0))
