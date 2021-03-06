(in-package #:cl-user)

(defpackage #:tiebreak-asd
  (:use :cl :asdf))

(in-package #:tiebreak-asd)

(asdf:defsystem #:tiebreak
  :description "A tennis game."
  :author "Daw-Ran Liou <hi@dawranliou.com>"
  :license "MIT"
  :version "0.0.1"

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "tiebreak"
  :entry-point "tiebreak:main"

  :depends-on (#:cffi
               #:cffi-libffi
               :beast)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "raylib")
               (:file "main")))
