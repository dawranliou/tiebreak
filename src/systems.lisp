(in-package #:tiebreak)


(define-system draw-projection ((entity loc projection))
  (with-slots (loc/x loc/y loc/z projection/color projection/r) entity
    (drawcircle3d (make-vector3 :x loc/x :y 0 :z loc/z)
                    projection/r
                    (make-vector3 :x 1.0 :y 0.0 :z 0.0)
                    90.0
                    projection/color)
    (when (stroke? entity)
      (with-slots (stroke/power stroke/reach dir/dir) entity
        (drawcircle3d (make-vector3 :x (+ loc/x
                                            (* stroke/reach
                                               (if (eql dir/dir :right) 1 -1)))
                                      :y 0
                                      :z loc/z)
                        (* projection/r stroke/power 1/3)
                        (make-vector3 :x 1.0 :y 0.0 :z 0.0)
                        90.0
                        (cond
                          ((< 3 stroke/power) +red+)
                          ((< 2 stroke/power) +orange+)
                          ((< 1 stroke/power) +yellow+)
                          (t +gray+)))))))


(define-system draw-sprite ((entity loc sprite size))
  (with-slots (loc/x loc/y loc/z
               sprite/w sprite/h sprite/row sprite/col sprite/texture
               size/w size/h)
      entity
    (let ((src-rec (make-rectangle :x (* sprite/h sprite/col)
                                   :y (* sprite/w sprite/row)
                                   :width sprite/w
                                   :height sprite/h)))
      (drawbillboardrec *camera*
                          sprite/texture
                          src-rec
                          (make-vector3 :x loc/x :y loc/y :z loc/z)
                          (make-vector2 :x size/w :y size/h)
                          +green+))))


(b:define-system draw-shape ((entity loc shape size))
  (with-slots (loc/x loc/y loc/z shape/type shape/color size/w size/h) entity
    (ecase shape/type
      (:sphere (drawsphere (make-vector3 :x loc/x :y (+ loc/y (/ size/h 2)) :z loc/z)
                            size/h
                            shape/color))
      (:cone (drawcylinder (make-vector3 :x loc/x :y 0 :z loc/z)
                            0.0 size/w size/h 10 shape/color)))))


(define-system grounded ((entity loc bound))
  (with-slots (loc/x loc/z bound/xmin bound/xmax bound/zmin bound/zmax)
      entity
    (if (elastic? entity)
        (with-slots (elastic/damp velocity/x velocity/z) entity
          (cond
            ((< loc/x bound/xmin) (setf (loc/x entity) bound/xmin
                                        (velocity/x entity) (* -1
                                                               elastic/damp
                                                               velocity/x)))
            ((< bound/xmax loc/x) (setf (loc/x entity) bound/xmax
                                        (velocity/x entity) (* -1
                                                               elastic/damp
                                                               velocity/x))))
          (cond
            ((< loc/z bound/zmin) (setf (loc/z entity) bound/zmin
                                        (velocity/z entity) (* -1
                                                               elastic/damp
                                                               velocity/z)))
            ((< bound/zmax loc/z) (setf (loc/z entity) bound/zmax
                                        (velocity/z entity) (* -1
                                                               elastic/damp
                                                               velocity/z)))))
        (progn
          (cond
            ((< loc/x bound/xmin) (setf (loc/x entity) bound/xmin))
            ((< bound/xmax loc/x) (setf (loc/x entity) bound/xmax)))
          (cond
            ((< loc/z bound/zmin) (setf (loc/z entity) bound/zmin))
            ((< bound/zmax loc/z) (setf (loc/z entity) bound/zmax)))))))
