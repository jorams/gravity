(in-package #:gravity)

(defvar *paused* nil)

(defun tick (&optional override-paused?)
  (unless (and *paused* (not override-paused?))
    (update-world)
    (render-game)))

(defun handle-key (key mod type)
  (declare (ignore mod))
  (when (eq type :down)
    (case key
      (:sdl-key-q
       (when *paused*
         (sdl:push-quit-event)))
      (:sdl-key-escape
       (setf *paused* (not *paused*)))
      (:sdl-key-n
       (tick t))
      (:sdl-key-d
       (setf *clear-screen* (not *clear-screen*)))
      (:sdl-key-r
       (setf *new-vector* (cons 0 0)))
      (:sdl-key-p
       (setf *draw-preview* (not *draw-preview*))))))

(defun game ()
  (sdl:with-init (sdl:sdl-init-video)
    (init-screen)
    (sdl:enable-key-repeat 100 100)
    (with-world ()
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event
         (:key key :mod-key mod)
         (handle-key key mod :down))
        (:video-resize-event
         (:w width :h height)
         (setf *screen-size* (vector width height))
         (sdl:resize-window width height))
        (:mouse-button-down-event
         (:button button :x x :y y)
         (case button
           ((sdl:sdl-button-left sdl:sdl-button-right)
            (setf *new-coords* (vector x y)))))
        (:mouse-motion-event
         (:x x :y y)
         (cond
           ;; Update radius
           ((sdl:mouse-right-p)
            (setf *new-radius*
                  (round (vector-distance
                          (cons x y)
                          *new-coords*))))
           ;; Update movement vector
           ((sdl:mouse-left-p)
            (setf *new-vector*
                  (vector-scale (distance-vector
                                 (cons x y)
                                 *new-coords*)
                                0.1)))))
        (:mouse-button-up-event
         (:button button)
         (when (= button sdl:sdl-button-left)
           (create-entity)))
        (:idle () (tick))))))

(defun start ()
  (bt:make-thread #'game :name "Gravity"))
