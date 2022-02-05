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

  :depends-on (:cl-raylib
               :beast)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "aspects")
                             (:file "systems")
                             (:file "player")
                             (:file "ball")
                             (:file "cone")
                             (:file "screens")
                             (:file "game")))))
