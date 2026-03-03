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
     (let* ((intended-dir (getf action :dir))
            ;; Drunk stumble: 1-in-3 chance of random direction
            (actual-dir (if (and (typep entity 'creature)
                                 (member (bac-status (creature-bac entity))
                                         '(:drunk :wasted :beyond-wasted)))
                            (if (zerop (random 3))
                                (let ((dirs '(:n :s :e :w :ne :nw :se :sw)))
                                  (nth (random 8) dirs))
                                intended-dir)
                            intended-dir))
            (delta (direction-to-delta actual-dir)))
       (when (and (not (eq actual-dir intended-dir))
                  (typep entity 'creature)
                  (string= (entity-key entity) "player"))
         (gs-log gs "You stumble."))
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
    (:descend
     (let* ((pos (entity-at entity))
            (entities (get-entities-at gs pos))
            (stairs (find-if (lambda (e) (eq (entity-tag e) :stairs-down)) entities)))
       (if stairs
           (let ((new-depth (1+ (game-state-depth gs))))
             (save-current-level gs)
             (load-level gs new-depth)
             ;; Place player on stairs-up of new level
             (let ((up-stairs nil))
               (maphash (lambda (k v)
                          (declare (ignore k))
                          (when (eq (entity-tag v) :stairs-up)
                            (setf up-stairs v)))
                        (game-state-world gs))
               (if up-stairs
                   (setf (entity-at entity) (copy-pos (entity-at up-stairs)))
                   (let ((fp (find-random-floor gs)))
                     (when fp (setf (entity-at entity) fp)))))
             (world-set gs entity)
             (gs-log gs (format nil "You descend to level ~a." new-depth)))
           (gs-log gs "There are no stairs down here."))))
    (:ascend
     (let* ((pos (entity-at entity))
            (entities (get-entities-at gs pos))
            (stairs (find-if (lambda (e) (eq (entity-tag e) :stairs-up)) entities)))
       (if stairs
           (let ((new-depth (1- (game-state-depth gs))))
             (if (< new-depth 0)
                 (gs-log gs "You can't go any higher.")
                 (progn
                   (save-current-level gs)
                   (load-level gs new-depth)
                   ;; Place player on stairs-down of previous level
                   (let ((down-stairs nil))
                     (maphash (lambda (k v)
                                (declare (ignore k))
                                (when (eq (entity-tag v) :stairs-down)
                                  (setf down-stairs v)))
                              (game-state-world gs))
                     (if down-stairs
                         (setf (entity-at entity) (copy-pos (entity-at down-stairs)))
                         (let ((fp (find-random-floor gs)))
                           (when fp (setf (entity-at entity) fp)))))
                   (world-set gs entity)
                   (gs-log gs (format nil "You ascend to level ~a." new-depth)))))
           (gs-log gs "There are no stairs up here."))))
    (:eat
     (let* ((item-key (getf action :key))
            (item (world-get gs item-key)))
       (when (and item (food-p item)
                  (string= (entity-container item) (entity-key entity)))
         (world-remove gs item-key)
         (let ((restore (case (entity-tag item)
                          (:poptart  150)
                          (:trailmix 200)
                          (:pancake  250)
                          (:bacon    300)
                          (:soup     200)
                          (otherwise 100))))
           (when (typep entity 'creature)
             (setf (creature-hunger entity)
                   (min (creature-max-hunger entity)
                        (+ (creature-hunger entity) restore)))
             (setf (creature-hp entity)
                   (min (creature-max-hp entity)
                        (+ (creature-hp entity) (floor restore 10)))))
           (gs-log gs (format nil "You eat the ~a. Delicious!" (entity-tag item)))))))
    (:quaff
     (let* ((item-key (getf action :key))
            (item (world-get gs item-key)))
       (when (and item (drink-p item)
                  (string= (entity-container item) (entity-key entity)))
         (world-remove gs item-key)
         (when (typep entity 'creature)
           (case (entity-tag item)
             (:water
              (setf (creature-hunger entity)
                    (min (creature-max-hunger entity)
                         (+ (creature-hunger entity) 50)))
              ;; Water can push hydration beyond max (overhydration)
              (incf (creature-hydration entity) 80)
              (setf (creature-bladder entity)
                    (min (creature-max-bladder entity)
                         (+ (creature-bladder entity) 25)))
              (if (> (creature-hydration entity)
                     (creature-max-hydration entity))
                  (gs-log gs "You drink the water. You feel waterlogged.")
                  (gs-log gs "You drink the water. Refreshing.")))
             (:booze
              (setf (creature-hp entity)
                    (min (creature-max-hp entity)
                         (+ (creature-hp entity) 2)))
              (setf (creature-hydration entity)
                    (min (creature-max-hydration entity)
                         (+ (creature-hydration entity) 30)))
              (incf (creature-bac entity) 25)
              (setf (creature-bladder entity)
                    (min (creature-max-bladder entity)
                         (+ (creature-bladder entity) 10)))
              (let ((level (bac-status (creature-bac entity))))
                (case level
                  (:tipsy      (gs-log gs "You drink the booze. You feel loose."))
                  (:plastered  (gs-log gs "You drink the booze. You feel happy."))
                  (:drunk      (gs-log gs "You drink the booze. You feel sloppy."))
                  (:wasted     (gs-log gs "You drink the booze. You feel confused."))
                  (:beyond-wasted (gs-log gs "You drink the booze. The room is spinning..."))
                  (otherwise   (gs-log gs "You drink the booze. Smooth.")))))
             (:milk
              (setf (creature-hp entity)
                    (min (creature-max-hp entity)
                         (+ (creature-hp entity) 5)))
              (setf (creature-hunger entity)
                    (min (creature-max-hunger entity)
                         (+ (creature-hunger entity) 100)))
              (setf (creature-hydration entity)
                    (min (creature-max-hydration entity)
                         (+ (creature-hydration entity) 60)))
              (setf (creature-bladder entity)
                    (min (creature-max-bladder entity)
                         (+ (creature-bladder entity) 20)))
              ;; Messages based on hidden milk properties
              (let ((source (when (typep item 'item-entity)
                              (getf (item-entity-props item) :source)))
                    (freshness (when (typep item 'item-entity)
                                 (getf (item-entity-props item) :freshness))))
                (case source
                  (:cow   (gs-log gs "You drink the milk. Your bones feel strong."))
                  (:human (gs-log gs "You drink the milk. Tastes nostalgic."))
                  (otherwise (gs-log gs "You drink the milk.")))
                (case freshness
                  (:chunky (gs-log gs (if (zerop (random 2))
                                          "Extra thick." "Chewy.")))
                  (:sour   (gs-log gs "Has a zing to it."))
                  (:fresh  nil))))
             (:acid
              (setf (creature-vril entity)
                    (min (creature-max-vril entity)
                         (+ (creature-vril entity) 5)))
              (add-status-effect entity :confused 20)
              (gs-log gs "You drink the acid. The world melts around you."))
             (otherwise
              (gs-log gs (format nil "You drink the ~a." (entity-tag item)))))))))
    (:piss
     (when (typep entity 'creature)
       (let ((target (getf action :target))
             (bladder-was (creature-bladder entity)))
         (setf (creature-bladder entity) 0)
         ;; Pissing depletes electrolytes proportional to how much was in bladder
         (let ((electrolyte-loss (max 1 (floor bladder-was 5))))
           (setf (creature-electrolytes entity)
                 (max 0 (- (creature-electrolytes entity) electrolyte-loss))))
         (if (zerop bladder-was)
             (gs-log gs "You strain but nothing comes out.")
             (case target
               (:self
                (gs-log gs "You piss on yourself. Your clothes are soaked."))
               (:ground
                (gs-log gs "You piss on the ground."))
               (:up
                (gs-log gs "You piss straight up. Gravity is unkind."))
               (:dir
                (let* ((dir (getf action :direction))
                       (delta (direction-to-delta dir))
                       (target-pos (pos-shift (entity-at entity) delta))
                       (entities-at (get-entities-at gs target-pos))
                       (creature-hit (find-if #'creature-p entities-at))
                       (items-hit (remove-if-not #'item-p entities-at)))
                  (dolist (item items-hit)
                    (when (typep item 'item-entity)
                      (setf (item-entity-wet item) t)))
                  (if creature-hit
                      (gs-log gs (format nil "You piss on ~a!"
                                         (creature-name creature-hit)))
                      (gs-log gs (format nil "You piss to the ~a." dir)))))
               (:inventory
                (let* ((item-key (getf action :item-key))
                       (item (world-get gs item-key)))
                  (when (and item (typep item 'item-entity))
                    (setf (item-entity-wet item) t)
                    (gs-log gs (format nil "You piss on your ~a. Why?"
                                       (entity-tag item))))))
               (otherwise
                (gs-log gs "You relieve yourself.")))))))
    (:noop nil)
    (:apply nil)))
