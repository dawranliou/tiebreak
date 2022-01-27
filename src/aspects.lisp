(in-package #:tiebreak)

(define-aspect loc
  (x :initform 0.0 :type :single-float)
  (y :initform 0.0 :type :single-float)
  (z :initform 0.0 :type :single-float))

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

(define-aspect size
  (w :initform 0.0 :type :single-float)
  (h :initform 0.0 :type :single-float))
