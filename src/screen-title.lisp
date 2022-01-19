(in-package #:tiebreak)

(defvar *frame-counter* 0)
(defvar *finish-screen* 0)

(defun init-title-screen ()
  (setq *frame-counter* 0
        *finish-screen* 0))

(defun update-title-screen ()
  (when (is-key-pressed +key-enter+)
    (setq *finish-screen* 2)))

(defun draw-title-screen ()
  (clear-background +black+)
  (draw-text "TIEBREAK" 120 220 60 +raywhite+)
  (draw-text "PRESS ENTER to START" 120 280 20 +raywhite+))

(defun unload-title-screen ())

(defun finish-title-screen ()
  *finish-screen*)
