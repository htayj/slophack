(in-package :flaghack)

(defstruct game-state
  (world (make-hash-table :test 'equal) :type hash-table)
  (log nil :type list))

(defun gs-log (gs message)
  (push message (game-state-log gs)))

(defun world-get (gs key)
  (gethash key (game-state-world gs)))

(defun world-set (gs entity)
  (setf (gethash (entity-key entity) (game-state-world gs)) entity))

(defun world-remove (gs key)
  (remhash key (game-state-world gs)))

(defun get-player (gs)
  (world-get gs "player"))

(defun get-entities-at (gs p)
  (let (result)
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (and (string= (entity-container v) "world")
                          (pos-equal (entity-at v) p))
                 (push v result)))
             (game-state-world gs))
    result))

(defun get-items-at (gs p)
  (remove-if-not #'item-p (get-entities-at gs p)))

(defun get-inventory (gs creature-key)
  (let (result)
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (string= (entity-container v) creature-key)
                 (push v result)))
             (game-state-world gs))
    result))

(defun get-creatures (gs)
  (let (result)
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (creature-p v)
                 (push v result)))
             (game-state-world gs))
    result))

(defun add-entities (gs entities)
  (dolist (e entities)
    (world-set gs e)))
