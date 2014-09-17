(in-package #:gravity)

(defvar *world*)

(defclass world ()
  ((entities
     :initarg :entities
     :accessor world-entities)))

(defclass entity ()
  ((location
     :initarg :location
     :initform '(100 . 100)
     :accessor entity-location)
   (radius
     :initarg :radius
     :initform 20
     :accessor entity-radius)
   (vector
     :initarg :vector
     :initform '(0 . 0)
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
     :initform NIL
     :accessor entity-solid?)))

(defun init-world ()
  (setf *world* (make-instance 'world)
        (world-entities *world*)
        (list (make-instance 'entity
                             :radius 20
                             :location (cons 1500 800)
                             :color sdl:*blue*
                             :mass 10)
              (make-instance 'entity
                             :radius 10
                             :location (cons 300 200)
                             :mass 1
                             :vector '(0 . 1))
              (make-instance 'entity
                             :radius 30
                             :location (cons 1000 100)
                             :color sdl:*green*
                             :mass 50)
              (make-instance 'entity
                             :radius 30
                             :location (cons 400 200)
                             :color sdl:*red*
                             :mass 150))))

(defun update-entity (entity)
  (if (entity-solid? entity)
    (setf (entity-vector entity) (cons 0 0))
    (let ((self (entity-location entity)))
      (dolist (e (world-entities *world*))
        (unless (eq e entity)
          (let* ((other (entity-location e))
                 (force (/ (entity-mass e) (vector-distance self other) 1000))
                 (gravity (vector-scale (distance-vector self other) force)))
            ;(format t "~S~%" gravity)
            (setf (entity-vector entity)
                  (vector+ (entity-vector entity) gravity)))))))
  entity)

(defun move-entity (entity)
  (if (eq (entity-color entity) sdl:*red*)
    (setf (entity-location entity)
          (cons (sdl:mouse-x) (sdl:mouse-y)))
    (setf (entity-location entity)
          (vector- (entity-location entity)
                   (entity-vector entity))))
  #+NIL (setf (entity-location entity)
	(vector- (entity-location entity)
		 (entity-vector entity)))
  entity)

(defun update-world ()
  (setf (world-entities *world*)
        (mapcar #'move-entity
                (mapcar #'update-entity (world-entities *world*)))))

