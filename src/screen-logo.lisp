(in-package #:tiebreak)

(defvar *frame-counter* 0)
(defvar *finish-screen-p* nil)
(defvar *logo-position-x* 0)
(defvar *logo-position-y* 0)
(defvar *letters-count* 0)
(defvar *top-side-rec-width* 0)
(defvar *left-side-rec-width* 0)
(defvar *bottom-side-rec-width* 0)
(defvar *right-side-rec-height* 0)

(defvar *state* 0)
(defvar *alpha* 1.0)

(defun init-logo-screen ()
  (setq *finish-screen-p* nil
        *frame-counter* 0
        *letters-count* 0
        *logo-position-x* (- (/ (get-screen-width) 2) 128)
        *logo-position-y* (- (/ (get-screen-height) 2) 128)
        *top-side-rec-width* 16
        *left-side-rec-width* 16
        *bottom-side-rec-width* 16
        *right-side-rec-height* 16
        *state* 0
        *alpha* 1.0))

(defun update-logo-screen ()
  (case *state*
    (0
     (incf *frame-counter*)
     (when (= *frame-counter* 80)
       (setq *state* 1
             *frame-counter* 0)))
    (1
     (incf *top-side-rec-width* 8)
     (incf *left-side-rec-width* 8)
     (when (= *top-side-rec-width* 256)
       (setq *state* 2)))
    (2
     (incf *bottom-side-rec-width* 8)
     (incf *right-side-rec-height* 8)
     (when (= *bottom-side-rec-width* 256)
       (setq *state* 3)))
    (3
     (incf *frame-counter*)
     (if (< *letters-count* 10)
         (when (< 12 *frame-counter*)
           (incf *letters-count*)
           (setq *frame-counter* 0))
         (when (< 200 *frame-counter*)
           (decf *alpha* 0.02)
           (when (< *alpha* 0.0)
             (setq *alpha* 0.0
                   *finish-screen-p* t)))))))

(defun draw-logo-screen ()
  (case *state*
    (0 (unless (zerop (mod (floor *frame-counter* 10) 2))
         (draw-rectangle *logo-position-x* *logo-position-y*
                         16 16
                         +black+)))
    (1
     (draw-rectangle *logo-position-x* *logo-position-y*
                     *top-side-rec-width* 16
                     +black+)
     (draw-rectangle *logo-position-x* *logo-position-y*
                     16 *left-side-rec-width*
                     +black+))
    (2
     (draw-rectangle *logo-position-x* *logo-position-y*
                     *top-side-rec-width* 16
                     +black+)
     (draw-rectangle *logo-position-x* *logo-position-y*
                     16 *left-side-rec-width*
                     +black+)
     (draw-rectangle (+ *logo-position-x* 240) *logo-position-y*
                     16 *right-side-rec-height*
                     +black+)
     (draw-rectangle *logo-position-x* (+ *logo-position-y* 240)
                     *bottom-side-rec-width* 16
                     +black+))
    (3
     (draw-rectangle *logo-position-x* *logo-position-y*
                     *top-side-rec-width* 16
                     +black+)
     (draw-rectangle *logo-position-x* *logo-position-y*
                     16 *left-side-rec-width*
                     +black+)
     (draw-rectangle (+ *logo-position-x* 240) *logo-position-y*
                     16 *right-side-rec-height*
                     +black+)
     (draw-rectangle *logo-position-x* (+ *logo-position-y* 240)
                     *bottom-side-rec-width* 16
                     +black+)
     (draw-rectangle (- (/ (get-screen-width) 2) 112)
                     (- (/ (get-screen-height) 2) 112)
                     224 224
                     (fade +raywhite+ *alpha*))
     (draw-text (text-subtext "raylib" 0 *letters-count*)
                (- (/ (get-screen-width) 2) 44)
                (+ (/ (get-screen-height) 2) 48)
                50 (fade +black+ *alpha*))
     (when (< 20 *frame-counter*)
       (draw-text "powered by" *logo-position-x* (- *logo-position-y* 27)
                  20 (fade +darkgray+ *alpha*))))))

(defun unload-logo-screen ())
(defun finish-logo-screen-p ()
  *finish-screen-p*)
