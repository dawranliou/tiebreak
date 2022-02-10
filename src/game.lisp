(in-package #:tiebreak)


(defparameter +screen-width+ 800)
(defparameter +screen-height+ 450)


;; Global states

(defvar *assets-path* (asdf:system-relative-pathname :tiebreak #p"assets/"))
(defvar *player-score* 0)
(defvar *opponent-score* 0)
(defvar *player-texture* nil)
(defvar *p* nil)
(defvar *b* nil)
(defvar *current-screen* nil)
(defvar *camera* nil)


(defun init-game ()
  (setf *current-screen* :title)
  ;; Load assets
  (setf *player-texture*
        (loadtexture
         (namestring (merge-pathnames "player.png" *assets-path*)))))


(defun unload-game ()
  (when *player-texture*
    (unloadtexture *player-texture*)
    (setf *player-texture* nil)))


(defun main ()
  (with-window (+screen-width+ +screen-height+ "Tiebreak!")
    (init-game)
    (init-screen *current-screen*)
    (settargetfps 60)
    (setexitkey 0)                    ; Don't quit on ESC

    (loop
      (if (windowshouldclose) (return))
      (update-screen *current-screen* (getframetime))
      (with-drawing
        (draw-screen *current-screen*)))

    (unload-screen *current-screen*)
    (unload-game)))
