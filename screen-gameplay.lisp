(in-package #:tiebreak)

(defvar *frame-counter* 0)
(defvar *player-frame-counter* 0)
(defvar *finish-screen* 0)
(defvar *player-score* 0)
(defvar *opponent-score* 0)

;; player instance
(defvar *player-texture* nil)
(defvar *p* nil)
(defvar *b* nil)

(defun init-gameplay-screen ()
  (setf *p* (init-player 18 40))
  (setf *b* (init-ball 0 0 0.2 0.5)))

(defun update-gameplay-screen ()
  (when (is-key-pressed +key-enter+)
    (setq *finish-screen* 1))

  (incf *frame-counter*)
  (when (<= 60 *frame-counter*)
    (setq *frame-counter* 0))

  (update-player *p*)

  (let ((hit-box (player-hit-box *p*)))
    (when hit-box
      (destructuring-bind (x y z r) hit-box
        (with-slots ((bx x) (by y) (bz z)) *b*
          (when (check-collision-spheres (make-vector3 :x bx :y by :z bz) r
                                         (make-vector3 :x x :y y :z z) r)
            (ball-hit *b*))))))

  (if (ball-out-of-bound *b*)
      (let ((init-x (get-random-value 200 600)))
        (setf *b* (init-ball -10 -30 0.2 0.5)))
      (update-ball *b*)))

(defun draw-court ()
  (draw-plane (make-vector3 :x 0 :y -0.1 :z 20)
              (make-vector2 :x 60 :y 40)
              +darkgray+))

(defun draw-heads-up-display ()
  (draw-fps 10 10)
  (draw-text (format nil "~S - ~S" *player-score* *opponent-score*) 375 10 20 +raywhite+))

(defun draw-gameplay-screen ()
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
        (draw-player-3d camera *p*))))

  ;; Heads-up display
  (draw-heads-up-display))

(defun unload-gameplay-screen ()
  (unload-player))

(defun finish-gameplay-screen ()
  *finish-screen*)
