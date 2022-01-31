(in-package #:tiebreak)

(defparameter +sprite-idle+ 0)
(defparameter +sprite-fh-swing+ 1)
(defparameter +sprite-bh-swing+ 2)
(defparameter +sprite-run-right+ 3)
(defparameter +sprite-run-left+ 4)


(define-entity player
    (loc bound velocity projection sprite size dir animation fsm stroke))


(defun init-player (x z)
  (create-entity 'player
                 :loc/x x
                 :loc/y 4.0
                 :loc/z z
                 :bound/xmin -30.0
                 :bound/xmax 30.0
                 :bound/zmin -40.0
                 :bound/zmax 40.0
                 :velocity/x 30.0
                 :velocity/z 30.0
                 :projection/color +gray+
                 :projection/r 5.0
                 :sprite/w 16
                 :sprite/h 16
                 :sprite/row 0
                 :sprite/col 1
                 :sprite/texture *player-texture*
                 :size/w 8.0
                 :size/h 8.0
                 :dir/dir :right
                 :fsm/state :idle))


(defun update-player (p dt)
  ;; Update state
  (with-slots (loc/x loc/z animation/dt stroke/power) p
    (let ((right-key-down (is-key-down +key-right+))
          (left-key-down (is-key-down +key-left+))
          (up-key-down (is-key-down +key-up+))
          (down-key-down (is-key-down +key-down+))
          (z-key-down (is-key-down +key-z+))
          (z-key-released (is-key-released +key-z+)))
      (let ((move-p (or right-key-down left-key-down up-key-down down-key-down)))
        (case (fsm/state p)
          (:idle (cond
                   (z-key-down (setf (fsm/state p) :load
                                     (animation/dt p) 0.0))
                   (move-p (setf (fsm/state p) :move
                                 (animation/dt p) 0.0))
                   (t (setf (animation/dt p) (if (< animation/dt 0.8)
                                                 (+ animation/dt dt)
                                                 0.0)))))
          (:move (cond
                   (z-key-down (setf (fsm/state p) :load
                                     (animation/dt p) 0.0))
                   (move-p (setf (animation/dt p) (if (< animation/dt 0.8)
                                                      (+ animation/dt dt)
                                                      0.0)
                                 (dir/dir p) (cond
                                               (right-key-down :right)
                                               (left-key-down :left)
                                               (up-key-down :right)
                                               (down-key-down :left))
                                 (loc/x p) (+ (* (velocity/x p)
                                                 dt
                                                 (+ (if right-key-down 1 0)
                                                    (if left-key-down -1 0)))
                                              loc/x)
                                 (loc/z p) (+ (* (velocity/z p)
                                                 dt
                                                 (+ (if up-key-down -1 0)
                                                    (if down-key-down 1 0)))
                                              loc/z)))
                   (t (setf (fsm/state p) :idle
                            (animation/dt p) 0.0))))
          (:load (progn
                   (when right-key-down (setf (dir/dir p) :right))
                   (when left-key-down (setf (dir/dir p) :left))
                   (cond
                     (z-key-released (setf (fsm/state p) :swing
                                           (animation/dt p) 0.0))
                     (z-key-down (setf (stroke/power p) (min (+ stroke/power (* dt 5))
                                                             3.0))))))
          (:swing (if (< animation/dt 0.4)
                      (setf (animation/dt p) (+ animation/dt dt))
                      (setf (fsm/state p) :idle
                            (stroke/power p) 1.0
                            (animation/dt p) 0.0)))))))

  ;; update animation
  (let ((face-right-p (equal :right (dir/dir p))))
    (case (fsm/state p)
      (:idle (setf (sprite/row p) +sprite-idle+
                   (sprite/col p) (floor (animation/dt p) 0.2)))
      (:move (setf (sprite/row p) (if face-right-p
                                      +sprite-run-right+
                                      +sprite-run-left+)
                   (sprite/col p) (floor (animation/dt p) 0.2)))
      (:load (setf (sprite/row p) (if face-right-p
                                      +sprite-fh-swing+
                                      +sprite-bh-swing+)
                   (sprite/col p) 0))
      (:swing (setf (sprite/row p) (if face-right-p
                                       +sprite-fh-swing+
                                       +sprite-bh-swing+)
                    (sprite/col p) (1+ (floor (animation/dt p) 0.1)))))))


(defun player-hit-box (p)
  (with-slots ((x loc/x) (z loc/z) (r projection/r) stroke/power) p
    (case (fsm/state p)
      ;;(:load (list x z r))
      (:swing (list x z r stroke/power))
      (t nil))))
