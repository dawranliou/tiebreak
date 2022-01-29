(in-package #:tiebreak)

(defparameter +sprite-idle+ 0)
(defparameter +sprite-fh-swing+ 1)
(defparameter +sprite-bh-swing+ 2)
(defparameter +sprite-run-right+ 3)
(defparameter +sprite-run-left+ 4)
(defparameter +player-speed+ 0.5)


(define-entity player
    (loc bound velocity projection sprite size dir animation fsm))


(defun init-player (x z)
  (create-entity 'player
                 :loc/x x
                 :loc/y 4.0
                 :loc/z z
                 :bound/xmin -30.0
                 :bound/xmax 30.0
                 :bound/zmin -40.0
                 :bound/zmax 40.0
                 :velocity/x +player-speed+
                 :velocity/z +player-speed+
                 :projection/color +blue+
                 :projection/r 2.0
                 :sprite/w 16
                 :sprite/h 16
                 :sprite/row 0
                 :sprite/col 1
                 :sprite/texture *player-texture*
                 :size/w 8.0
                 :size/h 8.0
                 :animation/frame 0
                 :dir/dir :right
                 :fsm/state :idle))


(defun update-player (p)
  ;; state + input = new state
  (let* ((current-x (loc/x p))
         (current-z (loc/z p))
         (current-frame (animation/frame p))
         (right-key-down (is-key-down +key-right+))
         (left-key-down (is-key-down +key-left+))
         (up-key-down (is-key-down +key-up+))
         (down-key-down (is-key-down +key-down+))
         (move-p (or right-key-down left-key-down up-key-down down-key-down))
         (z-key-down (is-key-down +key-z+))
         (z-key-released (is-key-released +key-z+)))
    (case (fsm/state p)
      (:idle (cond
               (z-key-down (setf (fsm/state p) :load
                                 (animation/frame p) 0))
               (move-p (setf (fsm/state p) :move
                             (animation/frame p) 0))
               (t (setf (animation/frame p) (if (< current-frame 40)
                                                (1+ current-frame)
                                                0)))))
      (:move (cond
               (z-key-down (setf (fsm/state p) :load
                                 (animation/frame p) 0))
               (move-p (setf (animation/frame p) (if (< current-frame 40)
                                                     (1+ current-frame)
                                                     0)
                             (dir/dir p) (cond
                                           (right-key-down :right)
                                           (left-key-down :left)
                                           (up-key-down :right)
                                           (down-key-down :left))
                             (loc/x p) (+ (* (velocity/x p)
                                             (+ (if right-key-down 1 0)
                                                (if left-key-down -1 0)))
                                          current-x)
                             (loc/z p) (+ (* (velocity/z p)
                                             (+ (if up-key-down -1 0)
                                                (if down-key-down 1 0)))
                                          current-z)))
               (t (setf (fsm/state p) :idle
                        (animation/frame p) 0))))
      (:load (cond
               (z-key-released (setf (fsm/state p) :swing
                                     (animation/frame p) 0))
               (right-key-down (setf (dir/dir p) :right))
               (left-key-down (setf (dir/dir p) :left))
               (z-key-down t)))
      (:swing (if (< current-frame 24)
                  (setf (animation/frame p) (1+ current-frame))
                  (setf (fsm/state p) :idle
                        (animation/frame p) 0))))))


(defun player-hit-box (p)
  (let ((x (loc/x p))
        (z (loc/z p))
        (face-right-p (equal :right (dir/dir p))))
    (case (fsm/state p)
      (:load (list (+ x (if face-right-p 2 -1)) 2 z 1.0))
      (:swing (list (+ x (if face-right-p 2 -1)) 2 z 1.5))
      (t nil))))


(defun update-player-animation (p)
  (let ((face-right-p (equal :right (dir/dir p)))
        (current-frame (animation/frame p)))
    (case (fsm/state p)
      (:idle (setf (sprite/row p) +sprite-idle+
                   (sprite/col p) (floor current-frame 10)))
      (:move (setf (sprite/row p) (if face-right-p
                                      +sprite-run-right+
                                      +sprite-run-left+)
                   (sprite/col p) (floor current-frame 10)))
      (:load (setf (sprite/row p) (if face-right-p
                                      +sprite-fh-swing+
                                      +sprite-bh-swing+)
                   (sprite/col p) 0))
      (:swing (setf (sprite/row p) (if face-right-p
                                       +sprite-fh-swing+
                                       +sprite-bh-swing+)
                    (sprite/col p) (1+ (floor current-frame 8)))))))
