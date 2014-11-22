(in-package #:gravity)

(defvar *world*)
(defvar *new-coords* (vec 0 0))
(defvar *new-radius* 20)
(defvar *new-vector* (vec 0 0))

(defclass world ()
  ((entities
    :initarg :entities
    :accessor world-entities)))

(defclass entity ()
  ((location
    :initarg :location
    :initform (vec 100 100)
    :accessor entity-location)
   (radius
    :initarg :radius
    :initform 20
    :accessor entity-radius)
   (vector
    :initarg :vector
    :initform (vec 0 0)
    :accessor entity-vector)
   (mass
    :initarg :mass
    :initform 10
    :accessor entity-mass)
   (color
    :type sdl:color
    :initarg :color
    :initform sdl:*white*
    :accessor entity-color)
   (solid?
    :initarg :solid?
    :initform nil
    :accessor entity-solid?)))

(defmacro with-world ((&optional (world '(make-instance 'world
                                          :entities ())))
                      &body body)
  `(let ((*world* ,world)
         (*new-coords* (vec 0 0))
         (*new-radius* 20)
         (*new-vector* (vec 0 0)))
     ,@body))

(defun create-entity (&key
                        (radius *new-radius*)
                        (location *new-coords*)
                        (vector (vec* *new-vector* (vec -1 -1)))
                        (color (pop-color))
                        (mass *new-radius*))
  (push (make-instance 'entity
                       :radius radius
                       :location location
                       :vector vector
                       :color color
                       :mass mass)
        (world-entities *world*)))

(defun update-entity (entity)
  (if (entity-solid? entity)
      (setf (entity-vector entity) (vec 0 0))
      (let ((self (entity-location entity)))
        (dolist (e (world-entities *world*))
          (unless (eq e entity)
            (let* ((other (entity-location e))
                   (force (/ (entity-mass e) (vec-distance self other) 1000))
                   (gravity (vec-scale (distance-vec self other) force)))
              (setf (entity-vector entity)
                    (vec+ (entity-vector entity) gravity)))))))
  entity)

(defun move-entity (entity)
  (setf (entity-location entity)
        (vec- (entity-location entity)
              (entity-vector entity)))
  entity)

(defun update-world ()
  (unless (or (sdl:mouse-left-p) (sdl:mouse-right-p))
    (setf *new-coords* (vec (sdl:mouse-x)
                            (sdl:mouse-y))))
  (setf (world-entities *world*)
        (mapcar #'move-entity
                (mapcar #'update-entity (world-entities *world*)))))

