(in-package #:tiebreak)


(defvar *current-screen* nil)


(defvar *trans-alpha* 0.0)
(defvar *on-transition-p* nil)
(defvar *trans-fade-out-p* nil)
(defvar *trans-from-screen* nil)
(defvar *trans-to-screen* nil)


(defvar *frame-counter* 0)
(defvar *finish-screen* nil)


(defgeneric init-screen (screen))

(defgeneric update-screen (screen))

(defgeneric draw-screen (screen))

(defgeneric unload-screen (screen))

(defmethod unload-screen (screen))

;; title screen
(defmethod init-screen ((screen (eql :title)))
  (setf *frame-counter* 0
        *finish-screen* nil))

(defmethod update-screen ((screen (eql :title)))
  (when (is-key-pressed +key-enter+)
    (setf *finish-screen* :gameplay)))

(defmethod draw-screen ((screen (eql :title)))
  (clear-background +black+)
  (draw-text "TIEBREAK" 120 220 60 +raywhite+)
  (draw-text "PRESS ENTER to START" 120 280 20 +raywhite+))

;; gameplay screen

(defvar *player-frame-counter* 0)
(defvar *player-score* 0)
(defvar *opponent-score* 0)
(defvar *player-texture* nil)
(defvar *p* nil)
(defvar *b* nil)

(defmethod init-screen ((screen (eql :gameplay)))
  (setf *frame-counter* 0
        *finish-screen* nil)
  (unless *player-texture*
    (setf *player-texture*
          (load-texture
           (namestring (merge-pathnames "player.png" *assets-path*)))))
  (setf *p* (init-player 18 40)
        *b* (init-ball 0 0 0.2 0.5)))

(defmethod update-screen ((screen (eql :gameplay)))
  (when (is-key-pressed +key-enter+)
    (setq *finish-screen* :ending))

  (incf *frame-counter*)
  (when (<= 60 *frame-counter*)
    (setq *frame-counter* 0))

  (update-player *p*)

  (let ((hit-box (player-hit-box *p*)))
    (when hit-box
      (destructuring-bind (x y z r) hit-box
        (with-slots ((bx loc/x) (by loc/y) (bz loc/z)) *b*
          (when (check-collision-spheres (make-vector3 :x bx :y by :z bz) r
                                         (make-vector3 :x x :y y :z z) r)
            (ball-hit *b*))))))

  (if (ball-out-of-bound *b*)
      (progn
        (destroy-entity *b*)
        (setf *b* (init-ball -10 -30 0.2 0.5)))
      (update-ball *b*)))

(defun draw-court ()
  (draw-plane (make-vector3 :x 0 :y -0.1 :z 20)
              (make-vector2 :x 60 :y 40)
              +darkgray+))

(defun draw-heads-up-display ()
  (draw-fps 10 10)
  (draw-text (format nil "~S - ~S" *player-score* *opponent-score*) 375 10 20 +raywhite+))

(defmethod draw-screen ((screen (eql :gameplay)))
  ;; Background
  (clear-background +black+)

  (with-slots (x z) *p*
    (let* ((camera-pos (make-vector3 :x x :y 50 :z 100))
           (camera-target (make-vector3 :x x :y 0 :z 0))
           (camera-up (make-vector3 :x 0.0 :y 1.0 :z 0))
           (camera (make-camera3d :position camera-pos
                                  :target camera-target
                                  :up camera-up
                                  :fovy 30.0
                                  :type +camera-perspective+)))
      (with-mode-3d (camera)
        ;; Court
        (draw-court)
        ;; ball
        (draw-ball-3d *b*)
        ;; Player
        (draw-player-3d camera *p*)
        (run-draw-projection))))

  ;; Heads-up display
  (draw-heads-up-display))

(defmethod unload-screen ((screen (eql :gameplay)))
  (destroy-entity *b*)
  (when *player-texture*
    (unload-texture *player-texture*)
    (setf *player-texture* nil)))

;; ending screen

(defmethod init-screen ((screen (eql :ending)))
  (setf *frame-counter* 0
        *finish-screen* nil))

(defmethod update-screen ((screen (eql :ending)))
  (when (is-key-pressed +key-enter+)
    (setq *finish-screen* :title)))

(defmethod draw-screen ((screen (eql :ending)))
  (clear-background +black+)
  (draw-text "GAME OVER" 120 220 20 +raywhite+))

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
