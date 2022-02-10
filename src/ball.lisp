(in-package #:tiebreak)

(defparameter +gravity+ -30)

(b:define-entity ball (loc velocity projection bound elastic drag shape size))

(defun init-ball (x z vx vz)
  (b:create-entity 'ball
                   :loc/x x
                   :loc/y 5
                   :loc/z z
                   :bound/xmin -30.0
                   :bound/xmax 30.0
                   :bound/zmin -40.0
                   :bound/zmax 40.0
                   :elastic/damp 0.8
                   :drag/air 0.999
                   :drag/ground 0.9
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
               drag/air drag/ground size/h elastic/damp)
      b
    (setf (loc/x b) (+ loc/x (* velocity/x dt))
          (loc/y b) (+ loc/y (* velocity/y dt))
          (loc/z b) (+ loc/z (* velocity/z dt))
          (velocity/x b) (if (< (abs velocity/x) 1)
                             0.0
                             (* velocity/x drag/air))
          (velocity/y b) (+ velocity/y (* +gravity+ dt))
          (velocity/z b) (if (< (abs velocity/z) 1)
                             0.0
                             (* velocity/z drag/air)))
    (when (< (loc/y b) (/ size/h 2))    ; Hit the ground?
      (setf (loc/y b) (/ size/h 2)
            (velocity/x b) (* drag/ground (velocity/x b))
            (velocity/y b) (if (< (abs velocity/y) 5)
                               0.0
                               (* velocity/y -1 elastic/damp))
            (velocity/z b) (* drag/ground (velocity/z b))))))


(defun ball-hit (b hit-box)
  (destructuring-bind (hx hz r p) hit-box
    (with-slots ((bx loc/x) (bz loc/z) (br projection/r)) *b*
      (when (checkcollisionpointcircle (make-vector2 :x hx :y hz)
                                       (make-vector2 :x bx :y bz)
                                       (+ r br))
        (let* ((dz (- bz hz))
               (dx (- bx hx))
               (distance (sqrt (+ (* dz dz) (* dx dx))))
               (dz-norm (/ dz distance))
               (dx-norm (/ dx distance))
               (vz (* 20 p dz-norm))
               (vx (* 20 p dx-norm)))
          (setf (velocity/x b) vx
                (velocity/y b) (* 5 p)
                (velocity/z b) vz))))))
