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
  (let ((p1 (make-vector2 :x 100 :y 350))
        (p2 (make-vector2 :x 700 :y 350))
        (p3 (make-vector2 :x 550 :y 100))
        (p4 (make-vector2 :x 250 :y 100)))
    (draw-line-v p1 p2 +gray+)
    (draw-line-v p2 p3 +gray+)
    (draw-line-v p3 p4 +gray+)
    (draw-line-v p4 p1 +gray+))
  ;; Single's line
  (let ((p1 (make-vector2 :x 150 :y 350))
        (p2 (make-vector2 :x 650 :y 350))
        (p3 (make-vector2 :x 525 :y 100))
        (p4 (make-vector2 :x 275 :y 100)))
    (draw-line-v p1 p4 +gray+)
    (draw-line-v p2 p3 +gray+))
  ;; Net
  (let ((p1 (make-vector2 :x 150 :y 185))
        (p2 (make-vector2 :x 650 :y 185))
        (p3 (make-vector2 :x 650 :y 160))
        (p4 (make-vector2 :x 150 :y 160)))
    ;;(draw-line-v p1 p2 +gray+)
    (draw-line-v p2 p3 +gray+)
    (draw-line-v p3 p4 +gray+)
    (draw-line-v p4 p1 +gray+))
  ;; Service line
  (let ((p1 (make-vector2 :x 200 :y 250))
        (p2 (make-vector2 :x 600 :y 250))
        (p3 (make-vector2 :x 540 :y 130))
        (p4 (make-vector2 :x 260 :y 130))
        (p5 (make-vector2 :x 400 :y 250))
        (p6 (make-vector2 :x 400 :y 130)))
    (draw-line-v p1 p2 +gray+)
    (draw-line-v p3 p4 +gray+)
    (draw-line-v p5 p6 +gray+)))

(defun draw-heads-up-display ()
  (draw-text (format nil "(~S, ~S)" (player-x *p*) (player-y *p*))
             (- (player-x *p*) 0)
             (- (player-y *p*) 30) 16 +gray+)
  (draw-fps 10 10)
  (draw-text (format nil "~S - ~S" *player-score* *opponent-score*) 375 10 20 +raywhite+))

(defun draw-gameplay-screen ()
  ;; Background
  (draw-rectangle 0 0 (get-screen-width) (get-screen-height) +black+)

  ;; Court
  (draw-court)

  ;; Player
  (draw-player *p*)

  ;; Heads-up display
  (draw-heads-up-display))

(defun unload-gameplay-screen ()
  (unload-player))

(defun finish-gameplay-screen ()
  *finish-screen*)
