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

(defun merge-entities (e1 e2)
  (let ((l1 (entity-location e1))
        (l2 (entity-location e2))
        (r1 (entity-radius e1))
        (r2 (entity-radius e2))
        (m1 (entity-mass e1))
        (m2 (entity-mass e2))
        (v1 (entity-vector e1))
        (v2 (entity-vector e2))
        (c1 (entity-color e1))
        (c2 (entity-color e2)))
    (make-instance 'entity
                   :location (vec-average l1 l2)
                   :radius (+ r1 r2)
                   :mass (+ m1 m2)
                   :vector (vec-average v1 v2)
                   :color (color-blend c1 c2))))

(defun update-entity (entity)
  (dolist (e (world-entities *world*))
    (unless (eq e entity)
      (let* ((self (entity-location entity))
             (other (entity-location e))
             (distance (vec-distance self other))
             (force (if (zerop distance)
                        0
                        (/ (entity-mass e) distance 1000)))
             (gravity (vec-scale (distance-vec self other) force)))
        (if (> distance (+ (entity-radius entity)
                           (entity-radius e)))
            (setf (entity-vector entity)
                  (vec+ (entity-vector entity) gravity))
            (progn
              (return-from update-entity
                (values (merge-entities entity e)
                        e)))))))
  entity)

(defun move-entity (entity)
  (setf (entity-location entity)
        (vec- (entity-location entity)
              (entity-vector entity))))

(defun update-world ()
  (unless (or (sdl:mouse-left-p) (sdl:mouse-right-p))
    (setf *new-coords* (vec (sdl:mouse-x)
                            (sdl:mouse-y))))
  (setf (world-entities *world*)
        (loop for entity in (world-entities *world*)
              for (new-entity . new-entities-to-ignore)
                = (multiple-value-list
                   (unless (find entity entities-to-ignore)
                     (update-entity entity)))
              when new-entity
                do (move-entity new-entity)
                and collect new-entity
              append new-entities-to-ignore into entities-to-ignore)))

