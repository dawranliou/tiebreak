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
  (draw-rectangle 0 0 (get-screen-width) (get-screen-height) +blue+)
  (draw-text "ENDING SCREEN" 20 10 20 +darkblue+)
  (draw-text "PRESS ENTER or TAP to JUMP to TITLE SCREEN" 120 220 20 +darkblue+))

(defun unload-ending-screen ())

(defun finish-ending-screen ()
  *finish-screen*)
