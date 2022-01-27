(in-package #:tiebreak)

(defparameter +sprite-idle+ 0)
(defparameter +sprite-fh-swing+ 1)
(defparameter +sprite-bh-swing+ 2)
(defparameter +sprite-run-right+ 3)
(defparameter +sprite-run-left+ 4)
(defparameter +player-speed+ 0.5)

(defvar *player-dir* :right)
(defvar *player-state* :idle)
(defvar *player-frame-counter* 0)


(define-entity player (loc velocity projection sprite))


(defun init-player (x z)
  (create-entity 'player
                 :loc/x x
                 :loc/y 4.0
                 :loc/z z
                 :velocity/x +player-speed+
                 :velocity/z +player-speed+
                 :projection/color +blue+
                 :projection/r 2.0))

(defun player-into-state (p new-state)
  (setf *player-state* new-state
        *player-frame-counter* 0))

(defun update-player (p)
  ;; state + input = new state
  (let* ((current-x (loc/x p))
         (current-z (loc/z p))
         (current-frame *player-frame-counter*)
         (right-key-down (is-key-down +key-right+))
         (left-key-down (is-key-down +key-left+))
         (up-key-down (is-key-down +key-up+))
         (down-key-down (is-key-down +key-down+))
         (move-p (or right-key-down left-key-down up-key-down down-key-down))
         (z-key-down (is-key-down +key-z+))
         (z-key-released (is-key-released +key-z+)))
    (case *player-state*
      (:idle (cond
               (z-key-down (setf *player-state* :load
                                 *player-frame-counter* 0))
               (move-p (setf *player-state* :move
                             *player-frame-counter* 0))
               (t (setf *player-frame-counter* (if (< current-frame 40)
                                                   (1+ current-frame)
                                                   0)))))
      (:move (cond
               (z-key-down (setf *player-state* :load
                                 *player-frame-counter* 0))
               (move-p (setf *player-frame-counter* (if (< current-frame 40)
                                                        (1+ current-frame)
                                                        0)
                             *player-dir* (cond
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
               (t (setf *player-state* :idle
                        *player-frame-counter* 0))))
      (:load (cond
               (z-key-released (setf *player-state* :swing
                                     *player-frame-counter* 0))
               (right-key-down (setf *player-dir* :right))
               (left-key-down (setf *player-dir* :left))
               (z-key-down t)))
      (:swing (if (< current-frame 24)
                  (setf *player-frame-counter* (1+ current-frame))
                  (setf *player-state* :idle
                        *player-frame-counter* 0))))))

(defun player-frame-rec (row col)
  (make-rectangle :x (* 16 row) :y (* 16 col) :width 16 :height 16))

(defun player-hit-box (p)
  (let ((x (loc/x p))
        (z (loc/z p))
        (face-right-p (equal :right *player-dir*)))
    (case *player-state*
      (:load (list (+ x (if face-right-p 2 -1)) 2 z 1.0))
      (:swing (list (+ x (if face-right-p 2 -1)) 2 z 1.5))
      (t nil))))

(defun draw-player-3d (camera p)
  (let ((x (loc/x p))
        (z (loc/z p))
        (face-right-p (equal :right *player-dir*))
        (current-frame *player-frame-counter*))
    (let ((src-rec (case *player-state*
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
                                                   +sprite-bh-swing+))))))
      (draw-billboard-rec camera
                          *player-texture*
                          src-rec
                          (make-vector3 :x x :y 4 :z z)
                          (make-vector2 :x 8 :y 8.0)
                          +green+))))
