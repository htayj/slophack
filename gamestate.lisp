(in-package :flaghack)

(defstruct game-state
  (world (make-hash-table :test 'equal) :type hash-table)
  (log nil :type list)
  (depth 0 :type fixnum)            ; current dungeon depth (0=campground)
  (levels (make-hash-table) :type hash-table)  ; depth → saved game-state-world
  (heat 80 :type fixnum))           ; environmental heat for current level

(defun level-heat (depth)
  "Return the base heat value for a given dungeon depth."
  (cond
    ((= depth 0) 80)          ; campground: hot summer festival
    ((= depth 1) 60)          ; shallow: still warm
    ((= depth 2) 45)          ; moderate
    (t (max 20 (- 80 (* depth 15))))))

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

;;; Level transition helpers

(defun save-current-level (gs)
  "Save the current level's world hash-table (minus the player)."
  (let ((saved (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (unless (string= k "player")
                 (setf (gethash k saved) v)))
             (game-state-world gs))
    (setf (gethash (game-state-depth gs) (game-state-levels gs)) saved)))

(defun load-level (gs depth)
  "Load a previously saved level, or generate a new BSP dungeon."
  (let ((saved (gethash depth (game-state-levels gs))))
    (if saved
        ;; Restore saved level
        (let ((new-world (make-hash-table :test 'equal)))
          (maphash (lambda (k v) (setf (gethash k new-world) v)) saved)
          (setf (game-state-world gs) new-world))
        ;; Generate new level
        (let ((new-gs (bsp-gen-level (random 1000000) depth depth)))
          (setf (game-state-world gs) (game-state-world new-gs)))))
  (setf (game-state-depth gs) depth)
  (setf (game-state-heat gs) (level-heat depth)))
