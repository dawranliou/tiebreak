(in-package #:tiebreak)

(defvar *frame-counter* 0)
(defvar *finish-screen* 0)

(defvar *player-texture*)
(defvar *player-frame* 0)
(defvar *player-frame-rec*)

(defvar *player-score* 0)
(defvar *opponent-score* 0)

(defvar +pos-err+ 5)
(defvar +player-w+ 64)
(defvar +player-h+ 64)
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
  (let ((player-texture (load-texture (namestring (merge-pathnames "player.png" *assets-path*)))))
    (setq *frame-counter* 0
          *finish-screen* 0
          *player-walking-p* nil
          *target-x* 0
          *target-y* 0
          *player-frame* 0
          *player-frame-rec* (make-rectangle :x 0.0 :y 0.0
                                             :width (/ (texture-width player-texture) 4)
                                             :height (texture-height player-texture))
          *player-x* 100
          *player-y* 100
          *player-dx* 0
          *player-dy* 0
          *player-dir* +1
          *player-texture* player-texture)))

#+nil
(init-gameplay-screen)

(defun update-gameplay-screen ()
  (incf *frame-counter*)
  (when (<= 10 *frame-counter*)
    (setq *frame-counter* 0)
    (incf *player-frame*)
    (when (<= 4 *player-frame*)
      (setq *player-frame* 0))
    (setf (rectangle-x *player-frame-rec*) (* *player-frame*
                                              (texture-width *player-texture*)
                                              1/4)))

  (when (is-key-pressed +key-enter+)
    (setq *finish-screen* 1))

  (when (is-key-down +key-right+)
    (incf *player-x* 3))

  (when (is-key-down +key-left+)
    (decf *player-x* 3))

  (when (is-key-down +key-up+)
    (decf *player-y* 3))

  (when (is-key-down +key-down+)
    (incf *player-y* 3)))

(defun draw-player ()
  (draw-rectangle-lines *player-x* *player-y* +player-w+ +player-h+ +blue+)
  (draw-texture-rec *player-texture*
                    *player-frame-rec*
                    (make-vector2 :x *player-x* :y *player-y*)
                    +white+))

(defun draw-gameplay-screen ()
  ;; Background
  (draw-rectangle 0 0 (get-screen-width) (get-screen-height) +black+)

  ;; Player
  (draw-player)

  ;; Heads-up display
  (draw-text (format nil "~S - ~S" *player-score* *opponent-score*) 120 10 20 +raywhite+))

(defun unload-gameplay-screen ()
  (unload-texture *player-texture*))

(defun finish-gameplay-screen ()
  *finish-screen*)
