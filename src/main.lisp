(in-package #:tiebreak)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS
(defparameter +screen-width+ 600)
(defparameter +screen-height+ 800)
(defparameter +court-min-x+ 100)
(defparameter +court-min-y+ 100)
(defparameter +court-max-x+ 500)
(defparameter +court-max-y+ 700)
(defparameter +court-width+ 400)
(defparameter +court-height+ 600)
(defparameter +player-v+ 200)
(defparameter +drag-air+ 0.999)
(defparameter +drag-ground+ 0.9)
(defparameter +elastic-damp+ 0.8)
(defparameter +gravity+ -300)

(defparameter +sprite-idle+ 0)
(defparameter +sprite-fh-swing+ 1)
(defparameter +sprite-bh-swing+ 2)
(defparameter +sprite-run-right+ 3)
(defparameter +sprite-run-left+ 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBALS
(defvar *trans-alpha* 0.0)
(defvar *on-transition-p* nil)
(defvar *trans-fade-out-p* nil)
(defvar *trans-from-screen* nil)
(defvar *trans-to-screen* nil)
(defvar *finish-screen* nil)
(defvar *player-score* 0)
(defvar *opponent-score* 0)
(defvar *player-texture* nil)
(defvar *p* nil)
(defvar *b* nil)
(defvar *current-screen* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MACROS
(defmacro -<> (expr &rest forms)
  "Thread the given forms, with `<>` as a placeholder."
  `(let* ((<> ,expr)
          ,@(mapcar (lambda (form)
                      (if (symbolp form)
                        `(<> (,form <>))
                        `(<> ,form)))
                    forms))
     <>))

(defmacro if-let (bindings &body (then-form &optional else-form))
  "Creates new variable bindings, and conditionally executes either THEN-FORM or
ELSE-FORM. ELSE-FORM defaults to NIL.  BINDINGS can be a single binding list or
a list of bindings."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (if (and ,@variables)
           ,then-form
           ,else-form))))

(defmacro when-let (bindings &body body)
  "Creates new variable bindings, and conditionally executes BODY."
  (let* ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                           (list bindings)
                           bindings))
         (variables (mapcar #'car binding-list)))
    `(let ,binding-list
       (when (and ,@variables)
         ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITY FUNCTIONS
(defun clamp (min max value)
  (cond ((<= value min) min)
        ((<= max value) max)
        (t value)))

(defun in-court-x (value)
  (clamp +court-min-x+ +court-max-x+ value))

(defun in-court-y (value)
  (clamp +court-min-y+ +court-max-y+ value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERICS
(defgeneric init (thing))
(defgeneric unload (thing))
(defgeneric update (thing dt))
(defgeneric render (thing))

(defmethod init (thing)
  (setf *finish-screen* nil))

(defmethod unload (thing))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASPECTS
(b:define-aspect loc
  (x :initform 0.0 :type :single-float)
  (y :initform 0.0 :type :single-float)
  (z :initform 0.0 :type :single-float))

(b:define-aspect v
  (x :initform 0.0 :type :single-float)
  (y :initform 0.0 :type :single-float)
  (z :initform 0.0 :type :single-float))

(b:define-aspect size
  (w :initform 0.0 :type :single-float)
  (h :initform 0.0 :type :single-float))

(b:define-system draw-shadow ((entity loc size))
  (with-slots (loc/x loc/y size/w size/h) entity
    (drawellipse (floor loc/x) (floor loc/y) (/ size/w 2) 4.0 +gray+)))

(b:define-aspect renderable)

(b:define-system draw-renderable ((entity renderable))
  (render entity))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PLAYER ENTITY
(b:define-entity player (renderable loc size)
  (timer :initform 0)
  (power :initform 0.0)
  (state :initform :idle)
  (dir :initform :right)
  (sprite-row :initform 0)
  (sprite-col :initform 0))

(defun init-player (x y)
  (b:create-entity 'player
                   :loc/x x :loc/y y :loc/z 0.0
                   :size/w 64.0 :size/h 64.0))

(defmethod render ((p player))
  (with-slots (loc/x loc/y sprite-row sprite-col size/w size/h) p
    (let ((src-rec (make-rectangle :x (* 16 sprite-col)
                                   :y (* 16 sprite-row)
                                   :width 16
                                   :height 16))
          (r (* size/w 0.5)))
      (drawtexturepro *player-texture*
                      src-rec
                      (make-rectangle :x (- loc/x r 5)
                                      :y (- loc/y size/h)
                                      :width size/w
                                      :height size/h)
                      (make-vector2 :x 0.0 :y 0.0)
                      0.0
                      +raywhite+))))

(defmethod update ((p player) dt)
  ;; Update state
  (with-slots (loc/x loc/y timer power state dir) p
    (let ((right-key-down (iskeydown +key-right+))
          (left-key-down (iskeydown +key-left+))
          (up-key-down (iskeydown +key-up+))
          (down-key-down (iskeydown +key-down+))
          (z-key-down (iskeydown +key-z+))
          (z-key-released (iskeyreleased +key-z+)))
      (let ((move-p (or right-key-down left-key-down up-key-down down-key-down)))
        (case state
          (:idle (cond
                   (z-key-down (setf state :load
                                     timer 0.0))
                   (move-p (setf state :move
                                 timer 0.0))
                   (t (setf timer (if (< dt 0.8)
                                   (+ timer dt)
                                   0.0)))))
          (:move (cond
                   (z-key-down (setf state :load
                                     timer 0.0))
                   (move-p (setf timer (if (< timer 0.8)
                                           (+ timer dt)
                                           0.0)
                                 dir (cond
                                       (right-key-down :right)
                                       (left-key-down :left)
                                       (up-key-down :right)
                                       (down-key-down :left))
                                 loc/x (+ (* +player-v+
                                             dt
                                             (+ (if right-key-down 1 0)
                                                (if left-key-down -1 0)))
                                          loc/x)
                                 loc/y (+ (* +player-v+
                                             dt
                                             (+ (if up-key-down -1 0)
                                                (if down-key-down 1 0)))
                                          loc/y)))
                   (t (setf state :idle
                            timer 0.0))))
          (:load (progn
                   (when right-key-down (setf dir :right))
                   (when left-key-down (setf dir :left))
                   (cond
                     (z-key-released (setf state :swing
                                           timer 0.0))
                     (z-key-down (setf power (min (+ power (* dt 1000))
                                                  300.0))))))
          (:swing (if (< timer 0.4)
                      (setf timer (+ timer dt))
                      (setf state :idle
                            power 1.0
                            timer 0.0)))))))

  ;; update animation
  (with-slots (dir state sprite-row sprite-col timer) p
    (let ((face-right-p (equal :right dir)))
      (case state
        (:idle (setf sprite-row +sprite-idle+
                     sprite-col (floor timer 0.2)))
        (:move (setf sprite-row (if face-right-p
                                    +sprite-run-right+
                                    +sprite-run-left+)
                     sprite-col (floor timer 0.2)))
        (:load (setf sprite-row (if face-right-p
                                    +sprite-fh-swing+
                                    +sprite-bh-swing+)
                     sprite-col 0))
        (:swing (setf sprite-row (if face-right-p
                                     +sprite-fh-swing+
                                     +sprite-bh-swing+)
                      sprite-col (1+ (floor timer 0.1))))))))

(defun player-hit-box (p)
  (with-slots (loc/x loc/y size/w power state) p
    (case state
      (:swing (list loc/x loc/y size/w power))
      (t nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BALL ENTITY
(b:define-entity ball (renderable loc v size)
  (g :initform +gravity+))

(defun init-ball (x y vx vy)
  (b:create-entity 'ball
                   :loc/x x :loc/y y :loc/z 30.0
                   :v/x vx :v/y vy :v/z 200.0
                   :size/w 20.0 :size/h 20.0))

(defmethod render ((b ball))
  (with-slots (loc/x loc/y loc/z size/w) b
    (let ((r (* size/w 0.5)))
    (drawcircle (floor loc/x) (floor (- loc/y r loc/z))
                r +raywhite+))))

(defmethod update ((b ball) dt)
  (with-slots (loc/x loc/y loc/z v/x v/y v/z g) b
    ;;(drawtext (format nil "V/X ~S" v/x) 100 100 20 +raywhite+)
    (let* ((target-x (+ loc/x (* v/x dt)))
           (target-y (+ loc/y (* v/y dt)))
           (target-z (+ loc/z (* v/z dt)))
           (target-vx (* v/x +drag-air+))
           (target-vy (* v/y +drag-air+))
           (target-vz (+ v/z (* g dt)))
           (bounce-x? (or (<= target-x +court-min-x+)
                          (<= +court-max-x+ target-y)))
           (bounce-y? (or (<= target-y +court-min-y+)
                          (<= +court-max-y+ target-y)))
           (bounce-z? (<= loc/z 0.1))
           (stopped-x? (< (abs v/x) 0.1))
           (stopped-y? (< (abs v/y) 0.1))
           (stopped-z? (and bounce-z? (< (abs target-vz) 1))))
      (drawtext (format nil "BOUNCE ~S" bounce-z?) 100 110 20 +raywhite+)
      (drawtext (format nil "STOPPED ~S" stopped-z?) 100 130 20 +raywhite+)
      (setf loc/x (in-court-x target-x)
            loc/y (in-court-y target-y)
            loc/z (if stopped-z? 0.0 target-z)
            v/x (cond (stopped-x? 0.0)
                      (bounce-x? (* -1 target-vx +elastic-damp+))
                      (t (if stopped-z? (* target-vx +drag-ground+) target-vx)))
            v/y (cond (stopped-y? 0.0)
                      (bounce-y? (* -1 target-vy +elastic-damp+))
                      (t (if stopped-z? (* target-vy +drag-ground+) target-vy)))
            v/z (cond (stopped-z? 0.0)
                      (bounce-z? (* -1 target-vz +elastic-damp+))
                      (t target-vz))
            g (if stopped-z? 0.0 +gravity+)))))

(defun ball-hit (b hit-box)
  (destructuring-bind (hx hy hr p) hit-box
    (with-slots ((bx loc/x) (by loc/y) (br size/w) g) *b*
      (when (checkcollisionpointcircle (make-vector2 :x hx :y hy)
                                       (make-vector2 :x bx :y by)
                                       (+ hr br))
        (let* ((dy (- by hy))
               (dx (- bx hx))
               (distance (sqrt (+ (* dx dx) (* dy dy))))
               (dy-norm (/ dy distance))
               (dx-norm (/ dx distance))
               (vy (* p dy-norm))
               (vx (* p dx-norm)))
          (setf (v/x b) vx
                (v/y b) vy
                (v/z b) 100.0
                g +gravity+))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASSETS
(defun asset-path (filename)
  (namestring (merge-pathnames filename
                               (asdf:system-relative-pathname :tiebreak
                                                              #p"assets/"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GAME
(defmethod init ((thing (eql :game)))
  (setf *current-screen* :title-screen
        ;; *trans-alpha* 0.0
        ;; *on-transition-p* nil
        ;; *trans-fade-out-p* nil
        ;; *trans-from-screen* nil
        ;; *trans-to-screen* nil
        *player-texture* (loadtexture (asset-path "player.png"))))

(defmethod unload ((thing (eql :game)))
  (unloadtexture *player-texture*)
  (setf *player-texture* nil)
  (b:clear-entities))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TITLE SCREEN
(defmethod update ((thing (eql :title-screen)) dt)
  (when (iskeypressed +key-enter+)
    (setf *finish-screen* :gameplay-screen)))

(defmethod render ((thing (eql :title-screen)))
  (clearbackground +black+)
  (drawtext "TIEBREAK" 120 220 60 +raywhite+)
  (drawtext "PRESS ENTER to START" 120 280 20 +raywhite+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GAMEPLAY SCREEN
(defmethod init :after ((screen (eql :gameplay-screen)))
  (setf *p* (init-player 450 650))
  (setf *b* (init-ball 100 100 #+nil 440 #+nil 620 200.0 200.0)))

(defmethod update ((screen (eql :gameplay-screen)) dt)
  (when (iskeypressed +key-enter+)
    (setq *finish-screen* :title-screen))
  (when-let ((hit-box (player-hit-box *p*)))
    (ball-hit *b* hit-box))
  (update *p* dt)
  (update *b* dt))

(defun draw-court ()
  (let ((half-height (floor (* 1/2 +court-height+))))
    (drawrectanglelines +court-min-x+ +court-min-y+
                        +court-width+ half-height
                        +lightgray+)
    (drawrectanglelines +court-min-x+ (+ +court-min-y+ half-height)
                        +court-width+ half-height
                        +lightgray+)))

(defun draw-heads-up-display ()
  (drawfps 520 10)
  (drawtext (format nil "~S - ~S" *player-score* *opponent-score*)
            10 10 20 +raywhite+))

(defmethod render ((screen (eql :gameplay-screen)))
  (clearbackground +black+)
  (draw-court)
  (run-draw-shadow)
  (run-draw-renderable)
  (draw-heads-up-display))

(defmethod unload ((screen (eql :gameplay-screen)))
  (b:clear-entities))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ENDING
(defmethod update ((screen (eql :ending-screen)) dt)
  (when (iskeypressed +key-enter+)
    (setq *finish-screen* :title-screen)))

(defmethod render ((screen (eql :ending-screen)))
  (clearbackground +black+)
  (drawtext "GAME OVER" 120 220 20 +raywhite+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRANSITION
(defun transition-to-screen (screen)
  (setq *on-transition-p* t
        *trans-fade-out-p* nil
        *trans-from-screen* *current-screen*
        *trans-to-screen* screen
        *trans-alpha* 0.0))

(defun update-transition ()
  (if *trans-fade-out-p*
      ;; Fade out
      (progn
        (decf *trans-alpha* 0.02)
        (when (< *trans-alpha* -0.01)
          (setq *trans-alpha* 0.0
                *trans-fade-out-p* nil
                *on-transition-p* nil
                *trans-from-screen* -1
                *trans-to-screen* -1)))
      ;; Fade in
      (progn
        (incf *trans-alpha* 0.05)
        (when (< 1.01 *trans-alpha*)
          (setq *trans-alpha* 1.0)
          (unload *trans-from-screen*)
          (init *trans-to-screen*)
          (setq *current-screen* *trans-to-screen*
                *trans-fade-out-p* t)))))

(defun render-transition ()
  (drawrectangle 0
                 0
                 +screen-width+
                 +screen-height+
                 (fade +black+ *trans-alpha*)))

(defmethod update :around (screen dt)
  (if *on-transition-p*
      (update-transition)
      (progn
        (call-next-method)
        (when *finish-screen*
          (transition-to-screen *finish-screen*)))))

(defmethod render :after (screen)
  (when *on-transition-p*
    (render-transition)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAIN GAME LOOP
(defun main ()
  (with-window (+screen-width+ +screen-height+ "Tiebreak!")
    (init :game)
    (settargetfps 60)
    (setexitkey 0)                      ; Don't quit on ESC
    (unwind-protect
         (loop
           (if (windowshouldclose) (return))
           (update *current-screen* (getframetime))
           (with-drawing
             (render *current-screen*)))
      (unload :game))))
