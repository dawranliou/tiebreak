(in-package #:tiebreak)


(define-system draw-projection ((entity loc projection))
  (with-slots (loc/x loc/y loc/z projection/color projection/r) entity
    (draw-circle-3d (make-vector3 :x loc/x :y 0 :z loc/z)
                    (+ loc/y projection/r)
                    (make-vector3 :x 1.0 :y 0.0 :z 0.0)
                    90.0
                    projection/color)))


(define-system draw-sprite ((entity loc sprite size))
  (with-slots (loc/x loc/y loc/z
               sprite/w sprite/h sprite/row sprite/col sprite/texture
               size/w size/h)
      entity
    (let ((src-rec (make-rectangle :x (* sprite/h sprite/col)
                                   :y (* sprite/w sprite/row)
                                   :width sprite/w
                                   :height sprite/h)))
      (draw-billboard-rec *camera*
                          sprite/texture
                          src-rec
                          (make-vector3 :x loc/x :y loc/y :z loc/z)
                          (make-vector2 :x size/w :y size/h)
                          +green+))))
