(in-package :flaghack)

;;; Distance helpers

(defun manhattan-distance (a b)
  (+ (abs (- (pos-x a) (pos-x b)))
     (abs (- (pos-y a) (pos-y b)))))

(defun direction-toward (from to)
  "Return a direction keyword to move FROM toward TO."
  (let ((dx (- (pos-x to) (pos-x from)))
        (dy (- (pos-y to) (pos-y from))))
    (cond
      ((and (> dx 0) (< dy 0)) :ne)
      ((and (< dx 0) (< dy 0)) :nw)
      ((and (> dx 0) (> dy 0)) :se)
      ((and (< dx 0) (> dy 0)) :sw)
      ((> dx 0) :e)
      ((< dx 0) :w)
      ((< dy 0) :n)
      ((> dy 0) :s)
      (t nil))))

;;; Patrol AI — rectangular loop

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

;;; Pursuit AI — chase the player when in range

(defun pursue-player-ai (gs entity range)
  "Chase the player if within RANGE tiles, otherwise wander."
  (let ((player (get-player gs)))
    (if (and player
             (<= (manhattan-distance (entity-at entity) (entity-at player))
                 range))
        ;; Pursue
        (let ((dir (direction-toward (entity-at entity) (entity-at player))))
          (if dir
              (list :type :move :dir dir)
              (list :type :noop)))
        ;; Wander randomly
        (let ((dirs '(:n :s :e :w :ne :nw :se :sw)))
          (if (zerop (random 3))  ; only move 1/3 of the time when idle
              (list :type :move :dir (nth (random (length dirs)) dirs))
              (list :type :noop))))))

;;; Creature-specific AI

(defun acid-kop-ai (gs entity)
  "AcidKop pursues aggressively within 15 tiles."
  (pursue-player-ai gs entity 15))

(defun ranger-ai (gs entity)
  "Rangers patrol and pursue within 10 tiles."
  (pursue-player-ai gs entity 10))

(defun wook-ai (gs entity)
  "Wooks wander and pursue within 8 tiles."
  (pursue-player-ai gs entity 8))

(defun egregore-ai (gs entity)
  "Egregores pursue within 12 tiles."
  (pursue-player-ai gs entity 12))

;;; Dispatch

(defun plan-ai (gs entity)
  "Get an action for a creature based on its type."
  (case (entity-tag entity)
    (:hippie             (hippie-ai gs entity))
    (:acidcop            (acid-kop-ai gs entity))
    (:ranger             (ranger-ai gs entity))
    (:wook               (wook-ai gs entity))
    (:lesser-egregore    (egregore-ai gs entity))
    (:greater-egregore   (egregore-ai gs entity))
    (:collective-egregore (egregore-ai gs entity))
    (otherwise           (list :type :noop))))

(defun plan-all-ai (gs)
  "Plan actions for all non-player creatures. Returns list of (entity . action)."
  (let (plans)
    (dolist (creature (get-creatures gs))
      (unless (eq (entity-tag creature) :player)
        (let ((action (plan-ai gs creature)))
          (unless (eq (getf action :type) :noop)
            (push (cons creature action) plans)))))
    plans))
