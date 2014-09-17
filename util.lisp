(in-package #:gravity)

(defun vector+ (v1 v2)
  (cons (+ (car v1) (car v2))
        (+ (cdr v1) (cdr v2))))

(defun vector- (v1 v2)
  (cons (- (car v1) (car v2))
        (- (cdr v1) (cdr v2))))

(defun vector* (v1 v2)
  (cons (* (car v1) (car v2))
        (* (cdr v1) (cdr v2))))

(defun vector-scale (v scale)
  (vector* v (cons scale scale)))

(defun vector-distance (v1 v2)
  (sqrt (+ (expt (- (car v2) (car v1))
                 2)
           (expt (- (cdr v2) (cdr v1))
                 2))))

(defun distance-vector (v1 v2)
  (let ((dist-x (- (car v1) (car v2)))
        (dist-y (- (cdr v1) (cdr v2))))
    (cons dist-x dist-y)))

(defun vector-angle (v1 v2)
  (atan (- (cdr v2) (cdr v1))
        (- (car v2) (car v1))))

(defun vector-size (vector)
  (sqrt (+ (expt (car vector) 2)
           (expt (cdr vector) 2))))

(defun vector-normalize (vector)
  (let ((size (vector-size vector))
        (x (car vector))
        (y (cdr vector)))
    (cons (if (zerop x) x (/ x size))
          (if (zerop y) y (/ y size)))))
