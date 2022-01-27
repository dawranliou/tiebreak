(in-package #:tiebreak)


(defparameter *screen-width* 800)
(defparameter *screen-height* 450)


(defvar *assets-path* (asdf:system-relative-pathname :tiebreak #p"assets/"))


(defun init-game ()
  (setq *current-screen* :title
        *trans-alpha* 0.0
        *on-transition-p* nil
        *trans-fade-out-p* nil
        *trans-from-screen* nil
        *trans-to-screen* nil))


(defun update-draw-frame ()
  (if *on-transition-p*
      (update-transition)
      (progn
        (update-screen *current-screen*)
        (when *finish-screen*
          (transition-to-screen *finish-screen*))))

  (with-drawing
    (draw-screen *current-screen*)
    (when *on-transition-p*
      (draw-transition))))

(defun main ()
  (with-window (*screen-width* *screen-height* "Tiebreak!")
    (init-game)
    (init-screen *current-screen*)
    (set-target-fps 60)

    ;; Main game loop
    (loop
      (if (window-should-close) (return))
      (update-draw-frame))

    (unload-screen *current-screen*)))

#+nil
(main)

#+nil
(transition-to-screen :title)
