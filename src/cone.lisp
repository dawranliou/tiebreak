(in-package #:tiebreak)

(b:define-entity cone (health loc shape size))

(defun make-cone (x z hp)
  (b:create-entity 'cone
                   :loc/x x
                   :loc/y 4
                   :loc/z z
                   :health/hitpoint hp
                   :shape/type :cone
                   :shape/color +orange+
                   :size/w 2.0
                   :size/h 4.0))

;;(defparameter cone1 (make-cone 0 -30 10))
;;(setf (loc/z cone1) -30)
;;(b:destroy-entity cone1)

(defun hit-cone (c)
  (decf (health/hitpoint c)))
