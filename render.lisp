(in-package #:gravity)

(defvar *screen-size*)
(defvar *fullscreen* NIL)
(defvar *clear-screen* t)

(defgeneric render (obj loc size))

(defun init-screen ()
  (setf *screen-size* (sdl:video-dimensions))
  (if (not *fullscreen*) (setf *screen-size* #(800 480)))
  (sdl:window
    (aref *screen-size* 0) (aref *screen-size* 1)
    :title-caption "Gravity"
    :double-buffer T
    :fullscreen *fullscreen*
    :resizable t
    :SW NIL)
  (setf (sdl:frame-rate) 60))

(defun render-world ()
  (setf *screen-size* (sdl:video-dimensions))
  (dolist (e (world-entities *world*))
    (sdl:draw-filled-circle-* (floor (car (entity-location e)))
                       (floor (cdr (entity-location e)))
                       (entity-radius e)
                       :color (entity-color e))))

(defun render-game ()
  (if *clear-screen* (sdl:clear-display sdl:*black*))
  (render-world)
  (sdl:update-display))
