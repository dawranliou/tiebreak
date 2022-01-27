(in-package #:tiebreak)


(define-system draw-projection ((entity loc projection))
  (with-slots (loc/x loc/y loc/z projection/color projection/r) entity
    (draw-circle-3d (make-vector3 :x loc/x :y 0 :z loc/z)
                    (+ loc/y projection/r)
                    (make-vector3 :x 1.0 :y 0.0 :z 0.0)
                    90.0
                    projection/color)))
