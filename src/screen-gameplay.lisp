(in-package #:tiebreak)

(defvar *frame-counter* 0)
(defvar *player-frame-counter* 0)
(defvar *finish-screen* 0)

(defvar *player-texture*)
(defvar *player-frame-row* 0)
(defvar *player-frame-col* 0)
(defvar *player-frame-rec*)

(defvar *player-score* 0)
(defvar *opponent-score* 0)

(defvar +pos-err+ 5)
(defvar +player-w+ 64)
(defvar +player-h+ 64)
(defvar +player-step-x+ 200)
(defvar +player-step-y+ 150)

(defvar *player-state* :idle)

(defvar +player-idle-frame-index+ 0)
(defvar +player-swing-fh-frame-index+ 1)
(defvar +player-swing-bh-frame-index+ 2)
(defvar +player-run-right-frame-index+ 3)
(defvar +player-run-left-frame-index+ 4)

(defvar *player-swing-phase*)

(defvar *player-x* 0)
(defvar *player-y* 0)
(defvar *player-dx* 3)
(defvar *player-dy* 3)
(defvar *player-dir* :right)
(defvar *player-off-balance* nil)

(defun init-gameplay-screen ()
  (let ((player-texture (load-texture (namestring (merge-pathnames "player.png" *assets-path*)))))
    (setq *frame-counter* 0
          *finish-screen* 0
          *player-frame-counter* 0
          *player-state* :idle
          *player-swing-phase* nil
          *player-dir* :right
          *player-off-balance* nil
          *player-frame-row* 0
          *player-frame-col* 0
          *player-frame-rec* (make-rectangle :x (* 64 *player-frame-col*)
                                             :y (* 64 +player-idle-frame-index+)
                                             :width 64
                                             :height 64)
          *player-x* 600
          *player-y* 330
          *player-dx* 3
          *player-dy* 3
          *player-texture* player-texture)))

#+nil
(init-gameplay-screen)

(defun update-gameplay-screen ()
  (incf *frame-counter*)
  (when (<= 60 *frame-counter*)
    (setq *frame-counter* 0))

  (incf *player-frame-counter*)

  ;; input

  (when (is-key-pressed +key-enter+)
    (setq *finish-screen* 1))

  (when (is-key-down +key-right+)
    (setq *player-dir* :right)
    (if (eq :swing *player-state*)
        (setq *player-off-balance* t)
        (setq *player-state* :run
              *player-frame-row* +player-run-right-frame-index+))
    (incf *player-x* *player-dx*))

  (when (is-key-down +key-left+)
    (setq *player-dir* :left)
    (if (eq :swing *player-state*)
        (setq *player-off-balance* t)
        (setq *player-state* :run
              *player-frame-row* +player-run-left-frame-index+))
    (decf *player-x* *player-dx*))

  (when (is-key-down +key-up+)
    (if (eq :swing *player-state*)
        (setq *player-off-balance* t)
        (setq *player-state* :run
              *player-frame-row* +player-run-right-frame-index+))
    (decf *player-y* *player-dy*))

  (when (is-key-down +key-down+)
    (if (eq :swing *player-state*)
        (setq *player-off-balance* t)
        (setq *player-state* :run
              *player-frame-row* +player-run-right-frame-index+))
    (incf *player-y* *player-dy*))

  (when (and (eq *player-state* :run)
             (not (is-key-down +key-right+))
             (not (is-key-down +key-left+))
             (not (is-key-down +key-up+))
             (not (is-key-down +key-down+)))
    (setq *player-state* :idle
          *player-frame-row* +player-idle-frame-index+))

  (when (is-key-down +key-z+)
    (setq *player-state* :swing
          *player-swing-phase* :enter))

  (case *player-state*
    (:idle (when (<= 10 *player-frame-counter*)
             (setq *player-frame-counter* 0)
             (incf *player-frame-col*)
             (when (<= 4 *player-frame-col*)
               (setq *player-frame-col* 0))
             (setf (rectangle-x *player-frame-rec*) (* *player-frame-col* 64))))
    (:run (when (<= 10 *player-frame-counter*)
             (setq *player-frame-counter* 0)
             (incf *player-frame-col*)
             (when (<= 4 *player-frame-col*)
               (setq *player-frame-col* 0))
             (setf (rectangle-x *player-frame-rec*) (* *player-frame-col* 64))))
    (:swing (case *player-swing-phase*
              (:enter (progn (setq *player-swing-phase* :unit-turn
                                   *player-dx* 0
                                   *player-dy* 0
                                   *player-frame-row* (if (eq :right *player-dir*)
                                                          +player-swing-fh-frame-index+
                                                          +player-swing-bh-frame-index+)
                                   *player-frame-col* 0)
                               (setf (rectangle-y *player-frame-rec*) (* *player-frame-row* 64))
                               (setf (rectangle-x *player-frame-rec*) 0)))
               (:unit-turn (when (is-key-up +key-z+)
                               (setq *player-swing-phase* :swing
                                     *player-dx* 0
                                     *player-dy* 0
                                     *player-frame-col* 1
                                     *player-frame-counter* 0)))
               (:swing (progn (when (<= 8 *player-frame-counter*)
                                (setq *player-frame-counter* 0
                                      *player-dx* 0
                                      *player-dy* 0)
                                (incf *player-frame-col*)
                                (when (<= 4 *player-frame-col*)
                                  (setq *player-swing-phase* :exit
                                        *player-frame-counter* 0
                                        *player-frame-col* 0
                                        *player-frame-row* 0)
                                  (setf (rectangle-y *player-frame-rec*) (* *player-frame-row* 64))))
                                (setf (rectangle-x *player-frame-rec*) (* *player-frame-col* 64))))
               (:exit
                (setf (rectangle-y *player-frame-rec*) (* *player-frame-row* 64))
                (setq *player-swing-phase* nil
                      *player-state* :idle
                      *player-dx* 3
                      *player-dy* 3)))))

  (setf (rectangle-y *player-frame-rec*) (* *player-frame-row* 64)))

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

(defun draw-player ()
  ;;(draw-rectangle-lines *player-x* *player-y* +player-w+ +player-h+ +blue+)
  (draw-texture-rec *player-texture*
                    *player-frame-rec*
                    (make-vector2 :x *player-x* :y *player-y*)
                    +white+))

(defun draw-heads-up-display ()
  ;; debugging stuff
  ;;(draw-text (format nil "Global Frame ~S" *frame-counter*) 220 160 20 +raywhite+)
  ;;(draw-text (format nil "Player Frame ~S" *player-frame-counter*) 220 180 20 +raywhite+)
  ;;(draw-text (format nil "State ~S" *player-state*) 220 200 20 +raywhite+)
  ;;(draw-text (format nil "Swing ~S" *player-swing-phase*) 220 220 20 +raywhite+)
  ;;(draw-text (format nil "Frame (~S, ~S)" *player-frame-row* *player-frame-col*) 220 240 20 +raywhite+)
  (draw-text (format nil "(~S, ~S)" *player-x* *player-y*)
             (- *player-x* 0)
             (- *player-y* 30) 16 +gray+)
  ;;(draw-fps 10 10)
  (draw-text (format nil "~S - ~S" *player-score* *opponent-score*) 375 10 20 +raywhite+))

(defun draw-gameplay-screen ()
  ;; Background
  (draw-rectangle 0 0 (get-screen-width) (get-screen-height) +black+)

  ;; Court
  (draw-court)

  ;; Player
  (draw-player)

  ;; Heads-up display
  (draw-heads-up-display))

(defun unload-gameplay-screen ()
  (unload-texture *player-texture*))

(defun finish-gameplay-screen ()
  *finish-screen*)
