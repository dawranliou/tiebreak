(in-package #:tiebreak)

(b:define-aspect loc
  (x :initform 0.0 :type :single-float)
  (y :initform 0.0 :type :single-float)
  (z :initform 0.0 :type :single-float))

(b:define-aspect bound
  (xmin :type :single-float)
  (xmax :type :single-float)
  (zmin :type :single-float)
  (zmax :type :single-float))

(b:define-aspect elastic
  (damp :type single-float))

(b:define-aspect drag
  (air :initform 0.0 :type single-float)
  (ground :initform 0.0 :type single-float))

(b:define-aspect velocity
  (x :initform 0.0 :type :single-float)
  (y :initform 0.0 :type :single-float)
  (z :initform 0.0 :type :single-float))

(b:define-aspect projection
  color
  (r :initform 0.0 :type :single-float))

(b:define-aspect sprite
  (w :initform 16 :type :integer)
  (h :initform 16 :type :integer)
  (row :initform 0 :type :integer)
  (col :initform 0 :type :integer)
  texture)

(b:define-aspect shape
  (type :type :keyword)
  color)

(b:define-aspect animation
  (dt :initform 0.0 :type :single-float))

(b:define-aspect size
  (w :initform 0.0 :type :single-float)
  (h :initform 0.0 :type :single-float))

(b:define-aspect fsm
  (state :type :keyword))

(b:define-aspect dir
  (dir :initform :right :type :keyword))

(b:define-aspect stroke
  (power :initform 1.0 :type :single-float)
  (reach :initform 2.0 :type :single-float))

(b:define-aspect health
  (hitpoint :initform 7 :type :integer))
