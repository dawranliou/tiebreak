(in-package #:tiebreak)

(defvar *frame-counter* 0)
(defvar *finish-screen* 0)

(defvar +pos-err+ 5)
(defvar +player-w+ 20)
(defvar +player-h+ 100)
(defvar +player-step-x+ 200)
(defvar +player-step-y+ 150)

(defvar *player-walking-p* nil)
(defvar *target-x* 0)
(defvar *target-y* 0)
(defvar *player-x* 0)
(defvar *player-y* 0)
(defvar *player-dx* 0)
(defvar *player-dy* 0)
(defvar *player-dir* +1)                ; +1 right; -1 left

(defun init-gameplay-screen ()
  (setq *frame-counter* 0
        *finish-screen* 0
        *player-walking-p* nil
        *target-x* 0
        *target-y* 0
        *player-x* 100
        *player-y* 100
        *player-dx* 0
        *player-dy* 0
        *player-dir* +1))

#+nil
(init-gameplay-screen)

(defun update-gameplay-screen ()
  (when (is-key-pressed +key-enter+)
    (setq *finish-screen* 1))

  (when (is-mouse-button-pressed +mouse-left-button+)
    (let* ((target-x (get-mouse-x))
           (target-y (get-mouse-y))
           (dir-x (if (< *player-x* target-x) +1 -1))
           (dir-y (if (< *player-y* target-y) +1 -1)))
      (setq *player-walking-p* t
            *target-x* target-x
            *target-y* target-y
            *player-dx* (* dir-x (round (* (get-frame-time) +player-step-x+)))
            *player-dy* (* dir-y (round (* (get-frame-time) +player-step-y+)))
            *player-dir* dir-x)))

  (when (< (abs (- *player-x* *target-x*)) +pos-err+)
    (setq *player-dx* 0))

  (when (< (abs (- *player-y* *target-y*)) +pos-err+)
    (setq *player-dy* 0))

  (when (or (and (zerop *player-dx*) (zerop *player-dy*))
            (< *player-x* 0)
            (< (get-screen-width) *player-x*)
            (< *player-y* 0)
            (< (get-screen-height) *player-y*))
    (setq *player-walking-p* nil))

  (when *player-walking-p*
    (incf *player-x* *player-dx*)
    (incf *player-y* *player-dy*)))

(defun draw-player ()
  (let ((x (round (- *player-x* (* +player-w+ 1/2))))
        (y (round (- *player-y* (* +player-h+ 1/3)))))
    (draw-rectangle-lines x y +player-w+ +player-h+ +blue+)))

(defun draw-gameplay-screen ()
  ;; Background
  (draw-rectangle 0 0 (get-screen-width) (get-screen-height) +purple+)

  ;; Player
  (draw-player)

  ;; Heads-up display
  (draw-text "GAMEPLAY SCREEN" 20 10 20 +maroon+)
  (draw-text (format nil "Player: (~S, ~S)" *player-x* *player-y*) 120 220 20 +maroon+)
  (draw-text (format nil "Speed: (~S, ~S)" *player-dx* *player-dy*) 120 240 20 +maroon+)
  (draw-text (format nil "Target: (~S, ~S)" *target-x* *target-y*) 120 260 20 +maroon+))

(defun unload-gameplay-screen ())

(defun finish-gameplay-screen ()
  *finish-screen*)
