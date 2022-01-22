(in-package #:tiebreak)

(defvar *current-screen* :logo)

(defparameter *screen-width* 800)
(defparameter *screen-height* 450)

(defvar *trans-alpha* 0.0)
(defvar *on-transition-p* nil)
(defvar *trans-fade-out-p* nil)
(defvar *trans-from-screen* nil)
(defvar *trans-to-screen* nil)

(defvar *assets-path* (asdf:system-relative-pathname :tiebreak #p"assets/"))

(defun init-game ()
  (setq *current-screen* :title ;;:logo
        *trans-alpha* 0.0
        *on-transition-p* nil
        *trans-fade-out-p* nil
        *trans-from-screen* nil
        *trans-to-screen* nil))

(defun transition-to-screen (screen)
  (setq *on-transition-p* t
        *trans-fade-out-p* nil
        *trans-from-screen* *current-screen*
        *trans-to-screen* screen
        *trans-alpha* 0.0))

(defun update-transition ()
  (if (not *trans-fade-out-p*)
      ;; Fade in
      (progn
        (incf *trans-alpha* 0.05)
        (when (< 1.01 *trans-alpha*)
          (setq *trans-alpha* 1.0)
          (case *trans-from-screen*
            (:logo (unload-logo-screen))
            (:title (unload-title-screen))
            (:gameplay (unload-gameplay-screen))
            (:ending (unload-ending-screen)))
          (case *trans-to-screen*
            (:logo (init-logo-screen))
            (:title (init-title-screen))
            (:gameplay (init-gameplay-screen))
            (:ending (init-ending-screen)))
          (setq *current-screen* *trans-to-screen*
                *trans-fade-out-p* t)))
      ;; Fade out
      (progn
        (decf *trans-alpha* 0.02)
        (when (< *trans-alpha* -0.01)
          (setq *trans-alpha* 0.0
                *trans-fade-out-p* nil
                *on-transition-p* nil
                *trans-from-screen* -1
                *trans-to-screen* -1)))))

(defun draw-transition ()
  (draw-rectangle 0
                  0
                  (get-screen-width)
                  (get-screen-height)
                  (fade +black+ *trans-alpha*)))

(defun update-draw-frame ()
  (if *on-transition-p*
      (update-transition)
      (case *current-screen*
        (:logo
         (update-logo-screen)
         (when (finish-logo-screen-p)
           (transition-to-screen :title)))
        (:title
         (update-title-screen)
         (when (= (finish-title-screen) 1)
           (transition-to-screen :options))
         (when (= (finish-title-screen) 2)
           (transition-to-screen :gameplay)))
        (:gameplay
         (update-gameplay-screen)
         (when (= (finish-gameplay-screen) 1)
           (transition-to-screen :ending)))
        (:ending
         (update-ending-screen)
         (when (= (finish-gameplay-screen) 1)
           (transition-to-screen :title)))))

    (with-drawing
      (case *current-screen*
        (:logo (draw-logo-screen))
        (:title (draw-title-screen))
        (:gameplay (draw-gameplay-screen))
        (:ending (draw-ending-screen)))

      (when *on-transition-p* (draw-transition))))

(defun main ()
  (with-window (*screen-width* *screen-height* "Tiebreak!")
    (init-game)
    (init-logo-screen)
    (set-target-fps 60)

    ;; Main game loop
    (loop
      (if (window-should-close) (return))
      (update-draw-frame))

    (case *current-screen*
      (:logo (unload-logo-screen))
      (:title (unload-title-screen))
      (:gameplay (unload-gameplay-screen))
      (:ending (unload-ending-screen)))))

#+nil
(main)

#+nil
(transition-to-screen :title)
