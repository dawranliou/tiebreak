(in-package #:tiebreak)

(defvar *frame-counter* 0)
(defvar *player-frame-counter* 0)
(defvar *finish-screen* 0)
(defvar *player-score* 0)
(defvar *opponent-score* 0)

;; player instance
(defvar *player-texture* nil)
(defvar *p*)

(defun init-gameplay-screen ()
  (setf *p* (init-player 600 330)))

(defun update-gameplay-screen ()
  (when (is-key-pressed +key-enter+)
    (setq *finish-screen* 1))

  (incf *frame-counter*)
  (when (<= 60 *frame-counter*)
    (setq *frame-counter* 0))

  (update-player *p*))

(defun draw-court ()
  ;; Outter bound
  (let ((p1 (make-vector3 :z 0 :x -36/2 :y -39))
        (p2 (make-vector3 :z 0 :x 36/2 :y -39))
        (p3 (make-vector3 :z 0 :x 36/2 :y 39))
        (p4 (make-vector3 :z 0 :x -36/2 :y 39)))
    (draw-line-3d p1 p2 +gray+)
    (draw-line-3d p2 p3 +gray+)
    (draw-line-3d p3 p4 +gray+)
    (draw-line-3d p4 p1 +gray+))
  ;; Single's line
  (let ((p1 (make-vector3 :z 0 :x -27/2 :y -39))
        (p2 (make-vector3 :z 0 :x 27/2 :y -39))
        (p3 (make-vector3 :z 0 :x 27/2 :y 39))
        (p4 (make-vector3 :z 0 :x -27/2 :y 39)))
    (draw-line-3d p1 p4 +gray+)
    (draw-line-3d p2 p3 +gray+))
  ;; Net
  #+nil
  (draw-plane (make-vector3 :x 0 :y 0 :z 7/4)
              (make-vector2 :x 40 :y 3.5)
              +gray+)
  (let ((p1 (make-vector3 :z 0 :x -20 :y 0))
        (p2 (make-vector3 :z 0 :x 20 :y 0))
        (p3 (make-vector3 :z 3.5 :x 20 :y 0))
        (p4 (make-vector3 :z 3.5 :x -20 :y 0)))
    ;;(draw-line-3d p1 p2 +gray+)
    (draw-line-3d p2 p3 +gray+)
    (draw-line-3d p3 p4 +gray+)
    (draw-line-3d p4 p1 +gray+))
  ;; Service line
  (let ((p1 (make-vector3 :z 0 :x -27/2 :y -21))
        (p2 (make-vector3 :z 0 :x 27/2 :y -21))
        (p3 (make-vector3 :z 0 :x 27/2 :y 21))
        (p4 (make-vector3 :z 0 :x -27/2 :y 21))
        (p5 (make-vector3 :z 0 :x 0 :y -21))
        (p6 (make-vector3 :z 0 :x 0 :y 21)))
    (draw-line-3d p1 p2 +gray+)
    (draw-line-3d p3 p4 +gray+)
    (draw-line-3d p5 p6 +gray+)))

(defun draw-heads-up-display ()
  (draw-fps 10 10)
  (draw-text (format nil "~S - ~S" *player-score* *opponent-score*) 375 10 20 +raywhite+))

(defun draw-gameplay-screen ()
  ;; Background
  (clear-background +black+)

  (let* ((camera-pos (make-vector3 :x 0 :y 100 :z 30))
         (camera-target (make-vector3 :x 0.0 :y 15.0 :z 0.0))
         (camera-up (make-vector3 :x 0.0 :y -1.0 :z 0))
         (camera (make-camera3d :position camera-pos
                                :target camera-target
                                :up camera-up
                                :fovy 20.0
                                :type +camera-perspective+)))
    (with-mode-3d (camera)
      ;; Court
      (draw-court)))

  ;; Player
  (draw-player *p*)

  ;; Heads-up display
  (draw-heads-up-display))

(defun unload-gameplay-screen ()
  (unload-player))

(defun finish-gameplay-screen ()
  *finish-screen*)
