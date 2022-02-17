(in-package #:tiebreak)

(defclass game-window (kit.sdl2:window)
  ((renderer :accessor renderer)
   (refresh :initarg :refresh :reader refresh)))

(defmethod initialize-instance :after ((w game-window) &key &allow-other-keys)
  (setf (kit.sdl2:idle-render w) t)
  (with-slots (renderer kit.sdl2:sdl-window) w
    (let ((flags '(:accelerated :targettexture :presentvsync)))
      (setf renderer (sdl2:create-renderer kit.sdl2:sdl-window -1 flags)))))

(defmethod kit.sdl2:close-window ((w game-window))
  (with-slots (renderer) w
    (when (and (slot-boundp w 'renderer) renderer)
      (sdl2:destroy-renderer renderer)))
  (call-next-method))

(defmethod kit.sdl2:render :before ((w game-window))
  (with-slots (renderer) w
    (sdl2:set-render-draw-color renderer 1 0 0 255)
    (sdl2:render-clear renderer)))

(defmethod kit.sdl2:render :after ((w game-window))
  (with-slots (renderer) w
    (sdl2:render-present renderer)))

(defmethod kit.sdl2:render ((w game-window))
  (with-slots (renderer) w
    (sdl2:render-draw-rect renderer (sdl2:make-rect 400 400 35 35))))

(defmethod kit.sdl2:textinput-event :after ((w game-window) ts text)
  (when (string= "Q" (string-upcase text))
    (kit.sdl2:close-window w)))

(defmethod kit.sdl2:keyboard-event :after ((w game-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-escape scancode)
      (kit.sdl2:close-window w))))

(kit.sdl2:define-start-function run (&key (w 1280) (h 720))
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  (sdl2:gl-set-attr :context-profile-mask 1)
  (sdl2:gl-set-attr :stencil-size 8)
  (make-instance 'game-window :w w :h h))

;; (run)
