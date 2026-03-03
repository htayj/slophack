(in-package :flaghack)

(defun hippie-ai (gs entity)
  "Simple rectangular patrol pattern."
  (declare (ignore gs))
  (let ((x (pos-x (entity-at entity)))
        (y (pos-y (entity-at entity))))
    (cond
      ((and (<= x 50) (< y 15)) (list :type :move :dir :s))
      ((and (< x 70) (>= y 15)) (list :type :move :dir :e))
      ((and (>= x 70) (> y 5))  (list :type :move :dir :n))
      ((and (> x 50) (<= y 5))  (list :type :move :dir :w))
      (t (list :type :noop)))))

(defun acid-kop-ai (gs entity)
  "Same patrol as hippie for now."
  (hippie-ai gs entity))

(defun plan-ai (gs entity)
  "Get an action for a creature based on its type."
  (case (entity-tag entity)
    (:hippie  (hippie-ai gs entity))
    (:acidcop (acid-kop-ai gs entity))
    (otherwise (list :type :noop))))

(defun plan-all-ai (gs)
  "Plan actions for all non-player creatures. Returns list of (entity . action)."
  (let (plans)
    (dolist (creature (get-creatures gs))
      (unless (eq (entity-tag creature) :player)
        (let ((action (plan-ai gs creature)))
          (unless (eq (getf action :type) :noop)
            (push (cons creature action) plans)))))
    plans))
