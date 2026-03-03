(in-package :flaghack)

;;; Movement with collision detection

(defun move-entity (gs entity delta)
  "Move entity by delta, checking for collisions. Returns t if moved."
  (let* ((new-pos (pos-shift (entity-at entity) delta))
         (entities-at-new (get-entities-at gs new-pos)))
    ;; Check for impassable terrain or creatures blocking
    (unless (some (lambda (e)
                    (or (impassable-p e)
                        (and (creature-p e)
                             (not (string= (entity-key e) (entity-key entity))))))
                  entities-at-new)
      ;; Move the entity
      (setf (entity-at entity) new-pos)
      t)))

;;; Pickup / Drop

(defun pickup-item (gs creature item-key)
  "Pick up item by key, putting it in creature's inventory."
  (let ((item (world-get gs item-key)))
    (when (and item (item-p item))
      (setf (entity-container item) (entity-key creature))
      (setf (entity-at item) (make-pos 0 0 0))
      (gs-log gs (format nil "Picked up ~a." (entity-tag item)))
      t)))

(defun drop-item (gs creature item-key)
  "Drop item from creature's inventory to the ground."
  (let ((item (world-get gs item-key)))
    (when (and item
               (string= (entity-container item) (entity-key creature)))
      (setf (entity-container item) "world")
      (setf (entity-at item) (copy-pos (entity-at creature)))
      (gs-log gs (format nil "Dropped ~a." (entity-tag item)))
      t)))

(defun copy-pos (p)
  (make-pos (pos-x p) (pos-y p) (pos-z p)))

;;; Action execution

(defun execute-action (gs entity action)
  "Execute an action for an entity. Action is a plist (:type <type> ...)."
  (case (getf action :type)
    (:move
     (let ((delta (direction-to-delta (getf action :dir))))
       (move-entity gs entity delta)))
    (:pickup
     (let ((item-key (getf action :key)))
       (pickup-item gs entity item-key)))
    (:pickup-all
     (let* ((items (get-items-at gs (entity-at entity)))
            (items (remove-if (lambda (e)
                                (string= (entity-key e) (entity-key entity)))
                              items)))
       (dolist (item items)
         (pickup-item gs entity (entity-key item)))))
    (:drop
     (let ((item-key (getf action :key)))
       (drop-item gs entity item-key)))
    (:drop-all
     (let ((inventory (get-inventory gs (entity-key entity))))
       (dolist (item inventory)
         (drop-item gs entity (entity-key item)))))
    (:noop nil)
    (:apply nil)))
