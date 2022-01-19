(in-package #:tiebreak)

(defparameter +sprite-idle+ 0)
(defparameter +sprite-fh-swing+ 1)
(defparameter +sprite-bh-swing+ 2)
(defparameter +sprite-run-right+ 3)
(defparameter +sprite-run-left+ 4)
(defparameter +dx+ 3)
(defparameter +dy+ 3)

(defstruct player
  x y dir state frame-counter)

(defun init-player (x y)
  (unless *player-texture*
    (setf *player-texture* (load-texture
                     (namestring (merge-pathnames "player.png" *assets-path*)))))
  (make-player :x x
               :y y
               :dir :right
               :state :idle
               :frame-counter 0))

(defun player-into-state (p new-state)
  (setf (player-state p) new-state
        (player-frame-counter p) 0))

(defun update-player (p)
  ;; state + input = new state
  (let* ((current-x (player-x p))
         (current-y (player-y p))
         (current-frame (player-frame-counter p))
         (right-key-down (is-key-down +key-right+))
         (left-key-down (is-key-down +key-left+))
         (up-key-down (is-key-down +key-up+))
         (down-key-down (is-key-down +key-down+))
         (move-p (or right-key-down left-key-down up-key-down down-key-down))
         (z-key-down (is-key-down +key-z+))
         (z-key-released (is-key-released +key-z+)))
    (case (player-state p)
      (:idle (cond
               (z-key-down (setf (player-state p) :load
                                 (player-frame-counter p) 0))
               (move-p (setf (player-state p) :move
                             (player-frame-counter p) 0))
               (t (setf (player-frame-counter p) (if (< current-frame 40)
                                                     (1+ current-frame)
                                                     0)))))
      (:move (cond
               (z-key-down (setf (player-state p) :load
                                 (player-frame-counter p) 0))
               (move-p (setf (player-frame-counter p) (if (< current-frame 40)
                                                          (1+ current-frame)
                                                          0)
                             (player-dir p) (cond
                                              (right-key-down :right)
                                              (left-key-down :left)
                                              (up-key-down :right)
                                              (down-key-down :left))
                             (player-x p) (+ (* +dx+
                                                (+ (if right-key-down 1 0)
                                                   (if left-key-down -1 0)))
                                             current-x)
                             (player-y p) (+ (* +dy+
                                                (+ (if up-key-down -1 0)
                                                   (if down-key-down 1 0)))
                                             current-y)))
               (t (setf (player-state p) :idle
                        (player-frame-counter p) 0))))
      (:load (cond
               (z-key-released (setf (player-state p) :swing
                                     (player-frame-counter p) 0))
               (right-key-down (setf (player-dir p) :right))
               (left-key-down (setf (player-dir p) :left))
               (z-key-down t)))
      (:swing (if (< current-frame 24)
                  (setf (player-frame-counter p) (1+ current-frame))
                  (setf (player-state p) :idle
                        (player-frame-counter p) 0))))))

(defun player-frame-rec (row col)
  (make-rectangle :x (* 64 row) :y (* 64 col) :width 64 :height 64))

(defun draw-player (p)
  (let ((x (player-x p))
        (y (player-y p))
        (face-right-p (equal :right (player-dir p)))
        (current-frame (player-frame-counter p)))
    (case (player-state p)
      (:idle (draw-texture-rec *player-texture*
                               (player-frame-rec (floor current-frame 10)
                                                 +sprite-idle+)
                               (make-vector2 :x x :y y)
                               +green+))
      (:move (draw-texture-rec *player-texture*
                               (player-frame-rec (floor current-frame 10)
                                                 (if face-right-p
                                                     +sprite-run-right+
                                                     +sprite-run-left+))
                               (make-vector2 :x x :y y)
                               +green+))
      (:load (draw-texture-rec *player-texture*
                               (player-frame-rec 0
                                                 (if face-right-p
                                                     +sprite-fh-swing+
                                                     +sprite-bh-swing+))
                               (make-vector2 :x x :y y)
                               +green+))
      (:swing (draw-texture-rec *player-texture*
                                (player-frame-rec (1+ (floor current-frame 10))
                                                  (if face-right-p
                                                      +sprite-fh-swing+
                                                      +sprite-bh-swing+))
                                (make-vector2 :x x :y y)
                                +green+)))))

(defun unload-player ()
  (when *player-texture*
    (unload-texture *player-texture*)
    (setf *player-texture* nil)))
