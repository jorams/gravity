(in-package #:gravity)

(defvar *paused* nil)

(defun tick (&optional override-paused?)
  (unless (and *paused* (not override-paused?))
    (format t "~S~%" (sdl:video-dimensions))
    (update-world)
    (render-game)))

(defun handle-key (key mod type)
  (declare (ignore mod))
  (cond ((and (eq type :down) (eq key :sdl-key-q) *paused*)
         (sdl:push-quit-event))
        ((and (eq type :down) (eq key :sdl-key-escape))
         (setf *paused* (not *paused*)))
        ((and (eq type :down) (eq key :sdl-key-n))
         (tick t))
        ((and (eq type :down) (eq key :sdl-key-d))
         (setf *clear-screen* (not *clear-screen*)))))

(defun game ()
  (sdl:with-init (sdl:sdl-init-video)
    (init-screen)
    (init-world)
    (sdl:enable-key-repeat 100 100)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event
       (:key key :mod-key mod)
       (handle-key key mod :down))
      (:video-resize-event
       (:w width :h height)
       (sdl:resize-window width height))
      (:idle () (tick)))))

(defun start ()
  (bt:make-thread #'game :name "Gravity"))
