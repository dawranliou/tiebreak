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
  (r :initform 0.0 :type single-float))

(define-aspect sprite
  w h row col texture)
