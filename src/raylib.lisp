(in-package #:tiebreak)

;; library

(c:define-foreign-library libraylib
  (:darwin "libraylib.dylib")
  (:unix "libraylib.so")
  (:windows "raylib.dll")
  (t (:default "libraylib")))

(c:use-foreign-library libraylib)

;; structs

(c:defcstruct (%color :class color-type)
  "Color type, RGBA (32bit)"
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(defmethod c:translate-from-foreign (ptr (type color-type))
  (c:with-foreign-slots ((r g b a) ptr (:struct %color))
    (list r g b a)))

(defmethod c:translate-into-foreign-memory (val (type color-type) ptr)
  (c:with-foreign-slots ((r g b a) ptr (:struct %color))
    (setf r (nth 0 val)
          g (nth 1 val)
          b (nth 2 val)
          a (nth 3 val))))

(c:defcstruct (%texture :class texture-type)
  "Texture type"
  (id :unsigned-int)
  (width :int)
  (height :int)
  (mipmaps :int)
  (format :int))

(defstruct texture
  id width height mipmaps format)

(defmethod c:translate-into-foreign-memory (object (type texture-type) pointer)
  (c:with-foreign-slots ((id width height mipmaps format) pointer (:struct %texture))
    (setf id (texture-id object)
          width (texture-width object)
          height (texture-height object)
          mipmaps (texture-mipmaps object)
          format (texture-format object))))

(defmethod c:translate-from-foreign (pointer (type texture-type))
  (c:with-foreign-slots ((id width height mipmaps format) pointer (:struct %texture))
    (make-texture :id id :width width :height height :mipmaps mipmaps :format format)))

(c:defcstruct (%vector2 :class vector2-type)
  "Vector2 type"
  (x :float)
  (y :float))

(defstruct vector2
  x y)

(defmethod c:translate-into-foreign-memory (object (type vector2-type) pointer)
  (c:with-foreign-slots ((x y) pointer (:struct %vector2))
    (setf x (coerce (vector2-x object) 'float))
    (setf y (coerce (vector2-y object) 'float))))

(defmethod c:translate-from-foreign (pointer (type vector2-type))
  (c:with-foreign-slots ((x y) pointer (:struct %vector2))
    (make-vector2 :x x :y y)))

(c:defcstruct (%vector3 :class vector3-type)
  "Vector3 type"
  (x :float)
  (y :float)
  (z :float))

(defstruct vector3
  x y z)

(defmethod c:translate-into-foreign-memory (object (type vector3-type) pointer)
  (c:with-foreign-slots ((x y z) pointer (:struct %vector3))
    (setf x (coerce (vector3-x object) 'float))
    (setf y (coerce (vector3-y object) 'float))
    (setf z (coerce (vector3-z object) 'float))))

(defmethod c:translate-from-foreign (pointer (type vector3-type))
  (c:with-foreign-slots ((x y z) pointer (:struct %vector3))
    (make-vector3 :x x :y y :z z)))

(c:defcstruct (%camera3d :class camera3d-type)
  "Camera type, defines a camera position/orientation in 3d space"
  (position (:struct %vector3))
  (target (:struct %vector3))
  (up (:struct %vector3))
  (fovy :float)
  (projection :int))

(defstruct camera3d
  position target up fovy projection)

(defmethod c:translate-into-foreign-memory (object (type camera3d-type) pointer)
  (c:with-foreign-slots ((fovy projection) pointer (:struct %camera3d))
    (c:convert-into-foreign-memory (camera3d-position object)
                                   '(:struct %vector3)
                                   (c:foreign-slot-pointer pointer
                                                           '(:struct %camera3d)
                                                           'position))
    (c:convert-into-foreign-memory (camera3d-target object)
                                   '(:struct %vector3)
                                   (c:foreign-slot-pointer pointer
                                                           '(:struct %camera3d)
                                                           'target))
    (c:convert-into-foreign-memory (camera3d-up object)
                                   '(:struct %vector3)
                                   (c:foreign-slot-pointer pointer
                                                           '(:struct %camera3d)
                                                           'up))
    (setf fovy (coerce (camera3d-fovy object) 'float)
          projection (camera3d-projection object))))

(defmethod c:translate-from-foreign (pointer (type camera3d-type))
  (c:with-foreign-slots ((position target up fovy projection) pointer (:struct %camera3d))
    (let ((px (c:foreign-slot-value position '(:struct %vector3) 'x))
          (py (c:foreign-slot-value position '(:struct %vector3) 'y))
          (pz (c:foreign-slot-value position '(:struct %vector3) 'z))
          (tx (c:foreign-slot-value target '(:struct %vector3) 'x))
          (ty (c:foreign-slot-value target '(:struct %vector3) 'y))
          (tz (c:foreign-slot-value target '(:struct %vector3) 'z))
          (ux (c:foreign-slot-value up '(:struct %vector3) 'x))
          (uy (c:foreign-slot-value up '(:struct %vector3) 'y))
          (uz (c:foreign-slot-value up '(:struct %vector3) 'z)))
      (make-camera3d :position (make-vector3 :x px :y py :z pz)
                     :target (make-vector3 :x tx :y ty :z tz)
                     :up (make-vector3 :x ux :y uy :z uz)
                     :fovy fovy
                     :projection projection))))

(c:defcstruct (%rectangle :class rectangle-type)
  "Rectangle type"
  (x :float)
  (y :float)
  (width :float)
  (height :float))

(defstruct rectangle
  x y width height)

(defmethod c:translate-into-foreign-memory (object (type rectangle-type) pointer)
  (c:with-foreign-slots ((x y width height) pointer (:struct %rectangle))
    (setf x (coerce (rectangle-x object) 'float))
    (setf y (coerce (rectangle-y object) 'float))
    (setf width (coerce (rectangle-width object) 'float))
    (setf height (coerce (rectangle-height object) 'float))))

(defmethod c:translate-from-foreign (pointer (type rectangle-type))
  (c:with-foreign-slots ((x y width height) pointer (:struct %rectangle))
    (make-rectangle :x x :y y :width width :height height)))

;; functions

(c:defcfun "InitWindow" :void
  "Initialize window and OpenGL context"
  (width :int)
  (height :int)
  (title :string))

(c:defcfun "CloseWindow" :void
  "Close window and unload OpenGL context")

(c:defcfun "WindowShouldClose" :bool
  "Check if KEY_ESCAPE pressed or Close icon pressed")

(c:defcfun "BeginDrawing" :void
  "Setup canvas (framebuffer) to start drawing")

(c:defcfun "EndDrawing" :void
  "End canvas drawing and swap buffers (double buffering)")

(c:defcfun "ClearBackground" :void
  "Set background color (framebuffer clear color)"
  (color (:struct %color)))

(c:defcfun "DrawText" :void
  "Draw text (using default font)"
  (text :string)
  (pos-x :int)
  (pos-y :int)
  (font-size :int)
  (color (:struct %color)))

(c:defcfun "LoadTexture" (:struct %texture)
  "Load texture from file into GPU memory (VRAM)"
  (file-name :string))

(c:defcfun "UnloadTexture" :void
  "Unload texture from GPU memory (VRAM)"
  (texture (:struct %texture)))

(c:defcfun "SetTargetFPS" :void
  "Set target FPS (maximum)"
  (fps :int))

(c:defcfun "SetExitKey" :void
  "Set a custom key to exit program (default is ESC)"
  (key :int))

(c:defcfun "GetFrameTime" :float
  "Returns time in seconds for last frame drawn")

(c:defcfun "IsKeyPressed" :bool
  "Detect if a key has been pressed once"
  (key :int))

(c:defcfun "BeginMode3D" :void
  "Initializes 3D mode with custom camera (3D)"
  (camera (:struct %camera3d)))

(c:defcfun "EndMode3D" :void
  "Ends 3D mode and returns to default 2D orthographic mode")

(c:defcfun "DrawFPS" :void
  "Shows current FPS"
  (pos-x :int)
  (pos-y :int))

(c:defcfun "DrawPlane" :void
  (current-pos (:struct %vector3))
  (size  (:struct %vector2))
  (color (:struct %color)))

(c:defcfun "GetScreenWidth" :int
  "Get current screen width")

(c:defcfun "GetScreenHeight" :int
  "Get current screen height")

(c:defcfun "DrawRectangle" :void
  "Draw a color-filled rectangle"
  (pos-x :int)
  (pos-y :int)
  (width :int)
  (height :int)
  (color (:struct %color)))

(c:defcfun "Fade" (:struct %color)
  "Color fade-in or fade-out, alpha goes from 0.0f to 1.0f"
  (color (:struct %color))
  (alpha :float))

(c:defcfun "CheckCollisionPointCircle" :bool
  (point (:struct %vector2))
  (center (:struct %vector2))
  (radius :float))

(c:defcfun "IsKeyDown" :bool
  "Detect if a key is being pressed"
  (key :int))

(c:defcfun "IsKeyReleased" :bool
  "Detect if a key has been released once"
  (key :int))

(c:defcfun "DrawCircle3D" :void
  (position (:struct %vector3))
  (radius :float)
  (rotation-axis (:struct %vector3))
  (rotation-angle :float)
  (color (:struct %color)))

(c:defcfun "DrawBillboardRec" :void
  (camera (:struct %camera3d))
  (texture (:struct %texture))
  (source-rec (:struct %rectangle))
  (center (:struct %vector3))
  (size (:struct %vector2))
  (tint (:struct %color)))

(c:defcfun "DrawSphere" :void
  "Draw sphere"
  (center-pos (:struct %vector3))
  (radius :float)
  (color (:struct %color)))

(c:defcfun "DrawCylinder" :void
  (position (:struct %vector3))
  (radius-top :float)
  (radius-bottom :float)
  (height :float)
  (slices :int)
  (color (:struct %color)))


;; macros

(defmacro with-window ((width height title) &body body)
  `(progn (initwindow ,width ,height ,title)
          (unwind-protect (progn ,@body)
            (closewindow))))

(defmacro with-drawing (&body body)
  `(progn (begindrawing)
          (unwind-protect (progn ,@body)
            (enddrawing))))

(defmacro with-mode-3d ((camera) &body body)
  `(progn (beginmode3d ,camera)
          (unwind-protect (progn ,@body)
            (endmode3d))))

;; constants

(defparameter +black+ '(0 0 0 255))
(defparameter +darkgray+ '(80 80 80 255))
(defparameter +gray+ '(130 130 130 255))
(defparameter +lightgray+ '(200 200 200 255))
(defparameter +raywhite+ '(245 245 245 255))
(defparameter +red+ '(230 41 55 255 ))
(defparameter +orange+ '(255 161 0 255 ))
(defparameter +yellow+ '(253 249 0 255))
(defparameter +green+ '(0 228 48 255))
(defparameter +key-enter+ 257)
(defparameter +camera-perspective+ 0)
(defparameter +key-right+ 262)
(defparameter +key-left+ 263)
(defparameter +key-down+ 264)
(defparameter +key-up+ 265)
(defparameter +key-z+ 90)
