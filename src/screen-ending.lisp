(in-package #:tiebreak)

(defvar *frame-counter* 0)
(defvar *finish-screen* 0)

(defun init-ending-screen ()
  (setq *frame-counter* 0
        *finish-screen* 0))

(defun update-ending-screen ()
  (when (is-key-pressed +key-enter+)
    (setq *finish-screen* 1)))

(defun draw-ending-screen ()
  (clear-background +black+)
  (draw-text "GAME OVER" 120 220 20 +raywhite+))

(defun unload-ending-screen ())

(defun finish-ending-screen ()
  *finish-screen*)
