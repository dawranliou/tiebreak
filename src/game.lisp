(in-package #:tiebreak)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS
(defparameter +screen-width+ 800)
(defparameter +screen-height+ 450)


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
(defvar *camera* nil)


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
  (setf *p* (init-player 18 40)
        *b* (init-ball 0 0 30 30))
  (let ((camera-pos (make-vector3 :x 18 :y 50 :z 100))
        (camera-target (make-vector3 :x 18 :y 0 :z 0))
        (camera-up (make-vector3 :x 0.0 :y 1.0 :z 0)))
    (setf *camera* (make-camera3d :position camera-pos
                                  :target camera-target
                                  :up camera-up
                                  :fovy 30.0
                                  :projection +camera-perspective+))))

(defmethod update ((screen (eql :gameplay-screen)) dt)
  (when (iskeypressed +key-enter+)
    (setq *finish-screen* :ending-screen))
  (with-slots (loc/x) *p*
    (setf (vector3-x (camera3d-position *camera*)) loc/x
          (vector3-x (camera3d-target *camera*)) loc/x))
  (update-player *p* dt)
  (update-ball *b* dt)
  (let ((hit-box (player-hit-box *p*)))
    (when hit-box
      (ball-hit *b* hit-box)))
  (run-grounded))

(defun draw-court ()
  (drawplane (make-vector3 :x 0 :y -0.1 :z -20)
             (make-vector2 :x 60 :y 40)
             +lightgray+)
  (drawplane (make-vector3 :x 0 :y -0.1 :z 20)
             (make-vector2 :x 60 :y 40)
             +darkgray+))

(defun draw-heads-up-display ()
  (drawfps 10 10)
  (drawtext (format nil "~S - ~S" *player-score* *opponent-score*)
            375 10 20 +raywhite+))

(defmethod render ((screen (eql :gameplay-screen)))
  (clearbackground +black+)
  (with-mode-3d (*camera*)
    (draw-court)
    (run-draw-shape)
    (run-draw-sprite)
    (run-draw-projection))
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
