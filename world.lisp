(in-package #:gravity)

;; Note: Should actually be 6.67384e-11
(defparameter +g+ 6.67384e-8)
(defparameter +force-multiplier+ 600)

(defvar *world*)
(defvar *new-coords* (vec 0 0))
(defvar *new-radius* 20)
(defvar *new-vector* (vec 0 0))

(defvar *bouncep* nil)

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
                        (mass (* 4/3 pi (expt radius 3))))
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
        (m1 (entity-mass e1))
        (m2 (entity-mass e2))
        (v1 (entity-vector e1))
        (v2 (entity-vector e2))
        (c1 (entity-color e1))
        (c2 (entity-color e2)))
    (make-instance 'entity
                   :location (vec-weighted-average l1 m1 l2 m2)
                   :radius (expt (/ (+ m1 m2) pi 4/3) 1/3)
                   :mass (+ m1 m2)
                   :vector (vec-weighted-average v1 m1 v2 m2)
                   :color (weighted-color-blend c1 m1 c2 m2))))

(defun update-entity (e1 entities)
  (dolist (e2 entities)
    (unless (eq e1 e2)
      (let* ((l1 (entity-location e1))
             (l2 (entity-location e2))
             (m1 (entity-mass e1))
             (m2 (entity-mass e2))
             (distance (vec-distance l1 l2))
             (force (if (>= 0 distance)
                        0
                        (* +force-multiplier+ +g+
                           (/ (* m1 m2)
                              (expt distance 2)))))
             (gravity (vec-scale (vec-normalize (distance-vec l1 l2))
                                 (/ force m1))))
        (if (> distance (+ (entity-radius e1)
                           (entity-radius e2)))
            (setf (entity-vector e1)
                  (vec+ (entity-vector e1) gravity))
            (progn
              (return-from update-entity
                (values (merge-entities e1 e2)
                        e2)))))))
  e1)

(defun move-entity (entity)
  (with-accessors ((location entity-location)
                   (vector entity-vector))
      entity
    (case *bouncep*
      ((nil)
       (setf location (vec- location
                            vector)))
      (t
       (setf location
             (vec-max (vec 0 0)
                      (vec-min (vec- location
                                     vector)
                               (vec (aref *screen-size* 0)
                                    (aref *screen-size* 1)))))
       (setf vector
             (vec (if (or (zerop (vec-x location))
                          (= (vec-x location)
                             (aref *screen-size* 0)))
                      (* -1 (vec-x vector))
                      (vec-x vector))
                  (if (or (zerop (vec-y location))
                          (= (vec-y location)
                             (aref *screen-size* 1)))
                      (* -1 (vec-y vector))
                      (vec-y vector))))))))

(defun update-world ()
  (setf (world-entities *world*)
        (loop for list = (world-entities *world*) then (rest list)
              for entity = (first list)
              while entity
              for (new-entity to-ignore)
                = (multiple-value-list (update-entity entity list))
              do (move-entity new-entity)
              collect new-entity
              do (alexandria:removef list to-ignore)
              sum 1 into count)))

