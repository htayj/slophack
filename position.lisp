(in-package :flaghack)

(defstruct (pos (:constructor make-pos (x y z))
                (:copier nil))
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum))

(defun pos-shift (p delta)
  (make-pos (+ (pos-x p) (pos-x delta))
            (+ (pos-y p) (pos-y delta))
            (+ (pos-z p) (pos-z delta))))

(defun pos-equal (a b)
  (and (= (pos-x a) (pos-x b))
       (= (pos-y a) (pos-y b))))

(defun collide-p (pos-a container-a pos-b container-b)
  (and (string= container-a container-b)
       (pos-equal pos-a pos-b)))

;; Direction vectors
(defparameter +up+         (make-pos  0 -1 0))
(defparameter +down+       (make-pos  0  1 0))
(defparameter +left+       (make-pos -1  0 0))
(defparameter +right+      (make-pos  1  0 0))
(defparameter +up-left+    (make-pos -1 -1 0))
(defparameter +up-right+   (make-pos  1 -1 0))
(defparameter +down-left+  (make-pos -1  1 0))
(defparameter +down-right+ (make-pos  1  1 0))

(defun direction-to-delta (dir)
  (ecase dir
    (:n  +up+)
    (:s  +down+)
    (:w  +left+)
    (:e  +right+)
    (:nw +up-left+)
    (:ne +up-right+)
    (:sw +down-left+)
    (:se +down-right+)))
