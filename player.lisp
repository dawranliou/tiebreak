(in-package #:tiebreak)

(defparameter +sprite-idle+ 0)
(defparameter +sprite-fh-swing+ 1)
(defparameter +sprite-bh-swing+ 2)
(defparameter +sprite-run-right+ 3)
(defparameter +sprite-run-left+ 4)
(defparameter +dx+ 0.4)
(defparameter +dz+ 0.5)

(defstruct player
  x z dir state frame-counter)

(defun init-player (x z)
  (unless *player-texture*
    (setf *player-texture*
          (load-texture
           (namestring (merge-pathnames "player.png" *assets-path*)))))
  (make-player :x x
               :z z
               :dir :right
               :state :idle
               :frame-counter 0))

(defun player-into-state (p new-state)
  (setf (player-state p) new-state
        (player-frame-counter p) 0))

(defun update-player (p)
  ;; state + input = new state
  (let* ((current-x (player-x p))
         (current-z (player-z p))
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
                             (player-z p) (+ (* +dz+
                                                (+ (if up-key-down -1 0)
                                                   (if down-key-down 1 0)))
                                             current-z)))
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
  (make-rectangle :x (* 16 row) :y (* 16 col) :width 16 :height 16))

(defun player-hit-box (p)
  (let ((x (player-x p))
        (z (player-z p))
        (state (player-state p))
        (face-right-p (equal :right (player-dir p)))
        (current-frame (player-frame-counter p)))
    (case state
      (:load (list (+ x (if face-right-p 2 -1)) 2 z 1.0))
      (:swing (list (+ x (if face-right-p 2 -1)) 2 z 1.5))
      (t nil))))

(defun draw-player-3d (camera p)
  (let ((x (player-x p))
        (z (player-z p))
        (state (player-state p))
        (face-right-p (equal :right (player-dir p)))
        (current-frame (player-frame-counter p)))
    (let ((src-rec (case state
                     (:idle (player-frame-rec (floor current-frame 10)
                                              +sprite-idle+))
                     (:move (player-frame-rec (floor current-frame 10)
                                              (if face-right-p
                                                  +sprite-run-right+
                                                  +sprite-run-left+)))
                     (:load (player-frame-rec 0
                                              (if face-right-p
                                                  +sprite-fh-swing+
                                                  +sprite-bh-swing+)))
                     (:swing (player-frame-rec (1+ (floor current-frame 8))
                                               (if face-right-p
                                                   +sprite-fh-swing+
                                                   +sprite-bh-swing+)))))
          (dst-rec (if (and (find state '(:load :swing)) face-right-p)
                     (make-rectangle :x (+ 12 x) :y z :width 64 :height 64)
                     (make-rectangle :x x :y z :width 64 :height 64))))
      (draw-circle-3d (make-vector3 :x x :y 0 :z z)
                    4.0
                    (make-vector3 :x 1.0 :y 0.0 :z 0.0)
                    90.0
                    +blue+)
      (draw-billboard-rec camera
                          *player-texture*
                          src-rec
                          (make-vector3 :x x :y 4 :z z)
                          (make-vector2 :x 8 :y 8.0)
                          +green+))))

(defun unload-player ()
  (when *player-texture*
    (unload-texture *player-texture*)
    (setf *player-texture* nil)))
