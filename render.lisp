(in-package #:gravity)

(defvar *screen-size*)
(defvar *fullscreen* NIL)
(defvar *clear-screen* t)
(defvar *draw-preview* t)

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
    (let ((x (floor (car (entity-location e))))
          (y (floor (cdr (entity-location e))))
          (radius (entity-radius e))
          (screen-width (aref *screen-size* 0))
          (screen-height (aref *screen-size* 1)))
      ;; Prevent planets on the loose from causing trouble because they're drawn
      ;; far outside of the window
      (unless (or (> 0 (+ x radius))
                  (> 0 (+ y radius))
                  (< screen-width (- x radius))
                  (< screen-height (- y radius)))
        (sdl:draw-filled-circle-* x
                                  y
                                  (entity-radius e)
                                  :color (entity-color e)))))
  ;; Preview for new planet
  (when *draw-preview*
    (sdl:draw-circle-* (car *new-coords*)
                       (cdr *new-coords*)
                       *new-radius*
                       :color (peek-color))
    ;; Line for new planet movement
    (let ((movement-vector (vector+ *new-coords*
                                    (vector-scale *new-vector*
                                                  10))))
      (sdl:draw-line-* (car *new-coords*)
                       (cdr *new-coords*)
                       (round (car movement-vector))
                       (round (cdr movement-vector))))))

(defun render-game ()
  (if *clear-screen* (sdl:clear-display sdl:*black*))
  (render-world)
  (sdl:update-display))
