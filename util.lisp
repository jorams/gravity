(in-package #:gravity)

(defun average (x1 x2)
  (/ (+ x1 x2) 2))

(defun weighted-average (x1 w1 x2 w2)
  (/ (+ (* x1 w1) (* x2 w2))
     (+ w1 w2)))

;;; Colors

(defun random-color ()
  (sdl:color
   :r (+ 6 (random 250))
   :g (+ 6 (random 250))
   :b (+ 6 (random 250))))

(defvar *next-color* (random-color))

(defun pop-color ()
  (prog1 *next-color*
    (setf *next-color* (random-color))))

(defun peek-color () *next-color*)

(defun weighted-color-blend (c1 w1 c2 w2)
  (sdl:color :r (weighted-average (sdl:r c1) w1
                                  (sdl:r c2) w2)
             :g (weighted-average (sdl:g c1) w1
                                  (sdl:g c2) w2)
             :b (weighted-average (sdl:b c1) w1
                                  (sdl:b c2) w2)))

(defun color-blend (c1 c2)
  (weighted-color-blend c1 1 c2 1))

;;; Vectors

(deftype vec ()
  '(cons number number))

(defun vec (x y)
  (cons x y))

(defun vec-x (vec)
  (car vec))

(defun vec-y (vec)
  (cdr vec))

(defun vec+ (v1 v2)
  (vec (+ (vec-x v1) (vec-x v2))
       (+ (vec-y v1) (vec-y v2))))

(defun vec- (v1 v2)
  (vec (- (vec-x v1) (vec-x v2))
       (- (vec-y v1) (vec-y v2))))

(defun vec* (v1 v2)
  (vec (* (vec-x v1) (vec-x v2))
       (* (vec-y v1) (vec-y v2))))

(defun vec-scale (v scale)
  (vec* v (vec scale scale)))

(defun vec-distance (v1 v2)
  (sqrt (+ (expt (- (vec-x v2) (vec-x v1))
                 2)
           (expt (- (vec-y v2) (vec-y v1))
                 2))))

(defun distance-vec (v1 v2)
  (vec- v1 v2))

(defun vec-average (v1 v2)
  (vec-scale (vec+ v1 v2)
             1/2))

(defun vec-weighted-average (v1 w1 v2 w2)
  (vec-scale (vec+ (vec* v1 (vec w1 w1))
                   (vec* v2 (vec w2 w2)))
             (/ 1 (+ w1 w2))))

(defun vec-min (v1 v2)
  (vec (min (vec-x v1)
            (vec-x v2))
       (min (vec-y v1)
            (vec-y v2))))

(defun vec-max (v1 v2)
  (vec (max (vec-x v1)
            (vec-x v2))
       (max (vec-y v1)
            (vec-y v2))))

(defun vec-angle (v1 v2)
  (atan (- (vec-y v2) (vec-y v1))
        (- (vec-x v2) (vec-x v1))))

(defun vec-size (vector)
  (sqrt (+ (expt (vec-x vector) 2)
           (expt (vec-y vector) 2))))

(defun vec-normalize (vector)
  (let ((size (vec-size vector))
        (x (vec-x vector))
        (y (vec-y vector)))
    (vec (if (zerop x) x (/ x size))
         (if (zerop y) y (/ y size)))))
