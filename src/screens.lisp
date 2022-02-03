(in-package #:tiebreak)


;; Screen transition states

(defvar *trans-alpha* 0.0)
(defvar *on-transition-p* nil)
(defvar *trans-fade-out-p* nil)
(defvar *trans-from-screen* nil)
(defvar *trans-to-screen* nil)
(defvar *finish-screen* nil)


(defgeneric init-screen (screen))

(defmethod init-screen (screen)
  (setf *finish-screen* nil))

(defgeneric update-screen (screen dt))

(defmethod update-screen (screen dt)
  nil)

(defgeneric draw-screen (screen))

(defmethod draw-screen (screen)
  nil)

(defgeneric unload-screen (screen))

(defmethod unload-screen (screen)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; title screen

(defmethod update-screen ((screen (eql :title)) dt)
  (when (is-key-pressed +key-enter+)
    (setf *finish-screen* :gameplay)))

(defmethod draw-screen ((screen (eql :title)))
  (clear-background +black+)
  (draw-text "TIEBREAK" 120 220 60 +raywhite+)
  (draw-text "PRESS ENTER to START" 120 280 20 +raywhite+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gameplay screen

(defmethod init-screen :after ((screen (eql :gameplay)))
  (setf *p* (init-player 18 40)
        *b* (init-ball 0 0 30 30)))

(defmethod update-screen ((screen (eql :gameplay)) dt)
  (when (is-key-pressed +key-enter+)
    (setq *finish-screen* :ending))

  (update-player *p* dt)

  (update-ball *b* dt)

  (let ((hit-box (player-hit-box *p*)))
    (when hit-box
      (ball-hit *b* hit-box)))

  (run-grounded))

(defun draw-court ()
  (draw-plane (make-vector3 :x 0 :y -0.1 :z -20)
              (make-vector2 :x 60 :y 40)
              +lightgray+)
  (draw-plane (make-vector3 :x 0 :y -0.1 :z 20)
              (make-vector2 :x 60 :y 40)
              +darkgray+))

(defun draw-heads-up-display ()
  (draw-fps 10 10)
  (draw-text (format nil "~S - ~S" *player-score* *opponent-score*)
             375 10 20 +raywhite+))

(defmethod draw-screen ((screen (eql :gameplay)))
  (clear-background +black+)

  (with-slots (loc/x loc/z) *p*
    (let ((camera-pos (make-vector3 :x loc/x :y 50 :z 100))
          (camera-target (make-vector3 :x loc/x :y 0 :z 0))
          (camera-up (make-vector3 :x 0.0 :y 1.0 :z 0)))
      (setf *camera* (make-camera3d :position camera-pos
                                    :target camera-target
                                    :up camera-up
                                    :fovy 30.0
                                    :type +camera-perspective+))
      (with-mode-3d (*camera*)
        (draw-court)
        (run-draw-shape)
        (run-draw-sprite)
        (run-draw-projection))))

  ;; Heads-up display
  (draw-heads-up-display))

(defmethod unload-screen ((screen (eql :gameplay)))
  (clear-entities))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ending screen

(defmethod update-screen ((screen (eql :ending)) dt)
  (when (is-key-pressed +key-enter+)
    (setq *finish-screen* :title)))

(defmethod draw-screen ((screen (eql :ending)))
  (clear-background +black+)
  (draw-text "GAME OVER" 120 220 20 +raywhite+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transition

(defun transition-to-screen (screen)
  (setq *on-transition-p* t
        *trans-fade-out-p* nil
        *trans-from-screen* *current-screen*
        *trans-to-screen* screen
        *trans-alpha* 0.0))


(defun update-transition ()
  (if (not *trans-fade-out-p*)
      ;; Fade in
      (progn
        (incf *trans-alpha* 0.05)
        (when (< 1.01 *trans-alpha*)
          (setq *trans-alpha* 1.0)
          (unload-screen *trans-from-screen*)
          (init-screen *trans-to-screen*)
          (setq *current-screen* *trans-to-screen*
                *trans-fade-out-p* t)))
      ;; Fade out
      (progn
        (decf *trans-alpha* 0.02)
        (when (< *trans-alpha* -0.01)
          (setq *trans-alpha* 0.0
                *trans-fade-out-p* nil
                *on-transition-p* nil
                *trans-from-screen* -1
                *trans-to-screen* -1)))))

(defun draw-transition ()
  (draw-rectangle 0
                  0
                  (get-screen-width)
                  (get-screen-height)
                  (fade +black+ *trans-alpha*)))

(defmethod update-screen :around (screen dt)
  (if *on-transition-p*
      (update-transition)
      (progn
        (call-next-method)
        (when *finish-screen*
          (transition-to-screen *finish-screen*)))))

(defmethod draw-screen :after (screen)
  (when *on-transition-p*
    (draw-transition)))
