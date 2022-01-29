(in-package #:tiebreak)


(defparameter +screen-width+ 800)
(defparameter +screen-height+ 450)


(defvar *assets-path* (asdf:system-relative-pathname :tiebreak #p"assets/"))
(defvar *player-score* 0)
(defvar *opponent-score* 0)
(defvar *player-texture* nil)
(defvar *p* nil)
(defvar *b* nil)


(defun init-game ()
  (setf *current-screen* :title)
  (setf *player-texture*
        (load-texture
         (namestring (merge-pathnames "player.png" *assets-path*)))))


(defun unload-game ()
  (when *player-texture*
    (unload-texture *player-texture*)
    (setf *player-texture* nil)))


(defun main ()
  (with-window (+screen-width+ +screen-height+ "Tiebreak!")
    (init-game)
    (init-screen *current-screen*)
    (set-target-fps 60)

    ;; Main game loop
    (loop
      (if (window-should-close) (return))
      (update-screen *current-screen*)
      (with-drawing
        (draw-screen *current-screen*)))

    (unload-screen *current-screen*)
    (unload-game)))
