(in-package #:tiebreak)

(define-aspect loc
  (x :initform 0.0 :type :single-float)
  (y :initform 0.0 :type :single-float)
  (z :initform 0.0 :type :single-float))

(define-aspect bound
  (xmin :type :single-float)
  (xmax :type :single-float)
  (zmin :type :single-float)
  (zmax :type :single-float))

(define-aspect elastic
  (damp :type single-float))

(define-aspect drag
  (air :initform 0.0 :type single-float))

(define-aspect velocity
  (x :initform 0.0 :type :single-float)
  (y :initform 0.0 :type :single-float)
  (z :initform 0.0 :type :single-float))

(define-aspect projection
  color
  (r :initform 0.0 :type :single-float))

(define-aspect sprite
  (w :initform 16 :type :integer)
  (h :initform 16 :type :integer)
  (row :initform 0 :type :integer)
  (col :initform 0 :type :integer)
  texture)

(define-aspect shape
  (type :type :keyword)
  color)

(define-aspect animation
  (dt :initform 0.0 :type :single-float))

(define-aspect size
  (w :initform 0.0 :type :single-float)
  (h :initform 0.0 :type :single-float))

(define-aspect fsm
  (state :type :keyword))

(define-aspect dir
  (dir :initform :right :type :keyword))
