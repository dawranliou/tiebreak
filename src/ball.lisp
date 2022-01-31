(in-package #:tiebreak)

(defparameter +gravity+ -30)

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
                 :elastic/damp 0.8
                 :drag/air 0.998
                 :velocity/x vx
                 :velocity/y 0.0
                 :velocity/z vz
                 :shape/type :sphere
                 :shape/color +red+
                 :size/w 2.0
                 :size/h 2.0
                 :projection/color +gray+
                 :projection/r 2.0))

(defun update-ball (b dt)
  (with-slots (loc/x loc/y loc/z velocity/x velocity/y velocity/z
               drag/air size/h elastic/damp)
      b
    (setf (loc/x b) (+ loc/x (* velocity/x dt))
          (loc/y b) (+ loc/y (* velocity/y dt))
          (loc/z b) (+ loc/z (* velocity/z dt))
          (velocity/x b) (if (< (abs velocity/x) 5)
                             0.0
                             (* velocity/x drag/air))
          (velocity/y b) (+ velocity/y (* +gravity+ dt))
          (velocity/z b) (if (< (abs velocity/z) 5)
                             0.0
                             (* velocity/z drag/air)))
    (when (< (loc/y b) (/ size/h 2))    ; Hit the ground?
      (setf (loc/y b) (/ size/h 2)
            (velocity/y b) (if (< (abs velocity/y) 4)
                               0
                               (* velocity/y -1 elastic/damp))))))


(defun ball-hit (b power)
  (setf (velocity/x b) (* -10 power)
        (velocity/y b) (* 5 power)
        (velocity/z b) (* -10 power)))
