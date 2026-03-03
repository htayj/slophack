(in-package :flaghack)

;;; Run mode — move repeatedly until obstacle

(defun can-move-p (gs entity delta)
  "Check if entity can move by delta without collision."
  (let* ((new-pos (pos-shift (entity-at entity) delta))
         (entities-at-new (get-entities-at gs new-pos)))
    (not (some (lambda (e)
                 (or (impassable-p e)
                     (and (creature-p e)
                          (not (string= (entity-key e) (entity-key entity))))))
               entities-at-new))))

(defun adjacent-floor-count (gs pos)
  "Count passable adjacent tiles (for detecting corridor branches)."
  (let ((count 0))
    (dolist (dir '(:n :s :e :w :ne :nw :se :sw))
      (let* ((delta (direction-to-delta dir))
             (adj-pos (pos-shift pos delta))
             (entities (get-entities-at gs adj-pos)))
        (unless (some #'impassable-p entities)
          (incf count))))
    count))

(defun creature-visible-p (gs pos)
  "Check if any non-player creature is visible adjacent to pos."
  (dolist (dir '(:n :s :e :w :ne :nw :se :sw))
    (let* ((delta (direction-to-delta dir))
           (adj-pos (pos-shift pos delta))
           (entities (get-entities-at gs adj-pos)))
      (when (some (lambda (e) (and (creature-p e)
                                    (not (eq (entity-tag e) :player))))
                  entities)
        (return t)))))

;;; Per-turn stat ticking

(defun tick-player-stats (gs player)
  "Tick all per-turn player stats: hunger, hydration, BAC, bladder, heat effects."
  (let ((heat (game-state-heat gs)))

    ;; --- Hunger ---
    (when (> (creature-hunger player) 0)
      (decf (creature-hunger player)))
    (when (= (creature-hunger player) 150)
      (gs-log gs "You are getting hungry."))
    (when (= (creature-hunger player) 0)
      (gs-log gs "You are starving!")
      (decf (creature-hp player)))

    ;; --- Hydration drain (heat increases rate) ---
    ;; Overhydration: beyond max, drains fast and fills bladder rapidly
    (let* ((hydration (creature-hydration player))
           (max-hyd (creature-max-hydration player))
           (overhydrated (> hydration max-hyd))
           (drain (if overhydrated
                      5  ; overhydration drains rapidly
                      (cond
                        ((>= heat 76) 3)    ; sweltering: triple drain
                        ((>= heat 56) 2)    ; hot: double drain
                        (t             1))))) ; cool/warm: normal
      (setf (creature-hydration player)
            (max 0 (- (creature-hydration player) drain)))
      ;; Overhydration dumps excess into bladder fast
      (when overhydrated
        (setf (creature-bladder player)
              (min (creature-max-bladder player)
                   (+ (creature-bladder player) 3)))))

    ;; --- Heat + dehydration interaction ---
    (let ((hydration (creature-hydration player))
          (max-hyd (creature-max-hydration player)))
      ;; Heat stroke: hydration < 10% AND heat >= 56
      (when (and (>= heat 56)
                 (< hydration (floor max-hyd 10)))
        (gs-log gs "You are suffering heat stroke!")
        (decf (creature-hp player))
        (add-status-effect player :confused 3))
      ;; Heat exhaustion: hydration < 25% AND heat >= 56 (but not stroke)
      (when (and (>= heat 56)
                 (< hydration (floor max-hyd 4))
                 (>= hydration (floor max-hyd 10)))
        (when (zerop (random 5))
          (gs-log gs "You feel woozy from the heat."))
        (when (zerop (random 3))
          (decf (creature-hp player)))))

    ;; --- BAC decay ---
    (when (> (creature-bac player) 0)
      (decf (creature-bac player))
      (let ((bac (creature-bac player)))
        (case bac
          (10 (gs-log gs "You feel sober again."))
          (25 (gs-log gs "You feel less tipsy."))
          (45 (gs-log gs "You start to sober up."))
          (65 (gs-log gs "You feel less sloppy.")))))

    ;; --- BAC effects (per turn while above threshold) ---
    (let ((bac-level (bac-status (creature-bac player))))
      (case bac-level
        (:wasted
         (add-status-effect player :confused 2))
        (:beyond-wasted
         (add-status-effect player :confused 2)
         ;; Coma chance: (bac - 85)% per turn
         (when (< (random 100) (- (creature-bac player) 85))
           (gs-log gs "You pass out!")
           (add-status-effect player :unconscious 5)))))

    ;; --- Bladder filling ---
    (let* ((hydration (creature-hydration player))
           (max-hyd (creature-max-hydration player))
           (base-fill (cond
                        ((> hydration (floor max-hyd 2)) 2)
                        ((> hydration (floor max-hyd 4)) 1)
                        (t 0)))
           (heat-adjusted (if (>= heat 56)
                              (floor base-fill 2)
                              base-fill)))
      (setf (creature-bladder player)
            (min (creature-max-bladder player)
                 (+ (creature-bladder player) heat-adjusted))))
    (when (= (creature-bladder player) 80)
      (gs-log gs "You need to find a bathroom."))
    (when (>= (creature-bladder player) 95)
      (gs-log gs "You really need to go!"))

    ;; --- Electrolyte depletion effects ---
    (let ((elec (creature-electrolytes player))
          (max-elec (creature-max-electrolytes player)))
      (when (and (< elec (floor max-elec 4))
                 (> elec (floor max-elec 10))
                 (zerop (random 8)))
        (gs-log gs "You feel weak and crampy."))
      (when (and (<= elec (floor max-elec 10))
                 (> elec 0))
        (when (zerop (random 5))
          (gs-log gs "Your muscles seize up."))
        (when (zerop (random 4))
          (decf (creature-hp player))))
      (when (<= elec 0)
        (gs-log gs "Your heart stutters from electrolyte depletion!")
        (decf (creature-hp player) 2)))

    ;; --- Tick status effects ---
    (tick-status-effects player)))

(defun run-in-direction (gs scr entity dir)
  "Move entity repeatedly in DIR until wall, corridor branch, or creature spotted.
   Executes AI between each step. Returns number of steps taken."
  (let ((delta (direction-to-delta dir))
        (steps 0))
    (loop
      (unless (can-move-p gs entity delta)
        (return steps))
      ;; Move
      (move-entity gs entity delta)
      (incf steps)
      ;; Execute AI each step
      (let ((ai-plans (plan-all-ai gs)))
        (dolist (plan ai-plans)
          (execute-action gs (car plan) (cdr plan))))
      ;; Tick stats each step
      (when (eq (entity-tag entity) :player)
        (tick-player-stats gs entity))
      ;; Render each step
      (croatoan:clear scr)
      (render-messages scr gs)
      (render-world scr gs)
      (render-status scr gs)
      (croatoan:refresh scr)
      ;; Stop conditions: corridor branch or creature spotted
      (when (or (> (adjacent-floor-count gs (entity-at entity)) 3)
                (creature-visible-p gs (entity-at entity)))
        (return steps)))))

;;; Look mode — move cursor to inspect entities

(defun look-mode (scr gs)
  "Enter look mode: move cursor with hjkl to inspect entities. ESC to exit."
  (let* ((player (get-player gs))
         (cx (if player (pos-x (entity-at player)) 0))
         (cy (if player (pos-y (entity-at player)) 0)))
    (multiple-value-bind (cam-x cam-y) (compute-camera gs)
      (gs-log gs "Look mode — move cursor with hjkl, ESC to exit.")
      (loop
        ;; Render with cursor
        (croatoan:clear scr)
        (render-messages scr gs)
        (render-world scr gs)
        (render-status scr gs)
        ;; Show what's at cursor position (world coords)
        (let* ((cursor-pos (make-pos cx cy (game-state-depth gs)))
               (entities (get-entities-at gs cursor-pos))
               (desc (if entities
                         (format nil "~{~a~^, ~}"
                                 (mapcar (lambda (e)
                                           (cond
                                             ((creature-p e)
                                              (format nil "~a (~a)" (entity-tag e) (creature-name e)))
                                             ((typep e 'signpost)
                                              (format nil "sign: ~a" (signpost-label e)))
                                             (t (string-downcase (symbol-name (entity-tag e))))))
                                         entities))
                         "nothing")))
          (croatoan:add-string scr
            (format nil "Look: (~a,~a) ~a" cx cy desc)
            :y (+ +map-y-offset+ +screen-height+ 3) :x 0
            :fgcolor :cyan :bgcolor :black))
        ;; Draw cursor marker at screen coords
        (let ((screen-cx (- cx cam-x))
              (screen-cy (- cy cam-y)))
          (when (and (>= screen-cx 0) (< screen-cx +screen-width+)
                     (>= screen-cy 0) (< screen-cy +screen-height+))
            (croatoan:add-char scr #\X
                               :y (+ screen-cy +map-y-offset+) :x screen-cx
                               :fgcolor :yellow :bgcolor :black
                               :attributes '(:bold))))
        (croatoan:refresh scr)
        ;; Input — cursor clamped to viewport
        (let* ((event (croatoan:get-event scr))
               (key (when event (croatoan:event-key event))))
          (cond
            ((or (eql key #\Esc) (eql key #\;) (eql key #\q) (eql key #\Space))
             (return))
            ((or (eql key #\h) (eql key :left))  (when (> cx cam-x) (decf cx)))
            ((or (eql key #\l) (eql key :right)) (when (< cx (+ cam-x +screen-width+ -1)) (incf cx)))
            ((or (eql key #\k) (eql key :up))    (when (> cy cam-y) (decf cy)))
            ((or (eql key #\j) (eql key :down))  (when (< cy (+ cam-y +screen-height+ -1)) (incf cy)))
            ((eql key #\y) (when (and (> cx cam-x) (> cy cam-y)) (decf cx) (decf cy)))
            ((eql key #\u) (when (and (< cx (+ cam-x +screen-width+ -1)) (> cy cam-y)) (incf cx) (decf cy)))
            ((eql key #\b) (when (and (> cx cam-x) (< cy (+ cam-y +screen-height+ -1))) (decf cx) (incf cy)))
            ((eql key #\n) (when (and (< cx (+ cam-x +screen-width+ -1)) (< cy (+ cam-y +screen-height+ -1))) (incf cx) (incf cy)))))))))

;;; Key mapping

(defun key-to-action (key scr gs)
  "Map a keypress to a game action. SCR needed for interactive menus."
  (let ((player (get-player gs)))
    (cond
      ;; Vi-style movement
      ((eql key #\h) (list :type :move :dir :w))
      ((eql key #\j) (list :type :move :dir :s))
      ((eql key #\k) (list :type :move :dir :n))
      ((eql key #\l) (list :type :move :dir :e))
      ((eql key #\y) (list :type :move :dir :nw))
      ((eql key #\u) (list :type :move :dir :ne))
      ((eql key #\b) (list :type :move :dir :sw))
      ((eql key #\n) (list :type :move :dir :se))
      ;; Run mode — shift+vi keys
      ((eql key #\H) (when player (run-in-direction gs scr player :w))  nil)
      ((eql key #\J) (when player (run-in-direction gs scr player :s))  nil)
      ((eql key #\K) (when player (run-in-direction gs scr player :n))  nil)
      ((eql key #\L) (when player (run-in-direction gs scr player :e))  nil)
      ((eql key #\Y) (when player (run-in-direction gs scr player :nw)) nil)
      ((eql key #\U) (when player (run-in-direction gs scr player :ne)) nil)
      ((eql key #\B) (when player (run-in-direction gs scr player :sw)) nil)
      ((eql key #\N) (when player (run-in-direction gs scr player :se)) nil)
      ;; Arrow keys
      ((eql key :up)    (list :type :move :dir :n))
      ((eql key :down)  (list :type :move :dir :s))
      ((eql key :left)  (list :type :move :dir :w))
      ((eql key :right) (list :type :move :dir :e))
      ;; Look mode
      ((eql key #\;)
       (look-mode scr gs)
       nil)
      ;; Inventory screen
      ((eql key #\i)
       (show-inventory scr gs)
       nil)
      ;; Pickup — select if multiple items
      ((or (eql key #\g) (eql key #\,))
       (let ((items (when player (get-items-at gs (entity-at player)))))
         (cond
           ((null items)
            (gs-log gs "Nothing here to pick up.") nil)
           ((= (length items) 1)
            (list :type :pickup :key (entity-key (first items))))
           (t
            (let ((chosen (select-item scr "Pick up what?" items)))
              (if chosen
                  (list :type :pickup :key (entity-key chosen))
                  nil))))))
      ;; Drop — select from inventory
      ((eql key #\d)
       (let ((inventory (when player (get-inventory gs "player"))))
         (if (null inventory)
             (progn (gs-log gs "Nothing to drop.") nil)
             (let ((chosen (select-item scr "Drop what?" inventory)))
               (if chosen
                   (list :type :drop :key (entity-key chosen))
                   nil)))))
      ;; Drop multiple
      ((eql key #\D)
       (let ((inventory (when player (get-inventory gs "player"))))
         (if (null inventory)
             (progn (gs-log gs "Nothing to drop.") nil)
             (let ((chosen (select-multiple-items scr "Drop which items?" inventory)))
               (when chosen
                 (dolist (item chosen)
                   (drop-item gs player (entity-key item))))
               nil))))
      ;; Eat — select food from inventory
      ((eql key #\e)
       (let* ((inventory (when player (get-inventory gs "player")))
              (food (remove-if-not #'food-p inventory)))
         (if (null food)
             (progn (gs-log gs "You have nothing to eat.") nil)
             (let ((chosen (select-item scr "Eat what?" food)))
               (if chosen
                   (list :type :eat :key (entity-key chosen))
                   nil)))))
      ;; Quaff — select drink from inventory
      ((eql key #\q)
       (let* ((inventory (when player (get-inventory gs "player")))
              (drinks (remove-if-not #'drink-p inventory)))
         (if (null drinks)
             (progn (gs-log gs "You have nothing to drink.") nil)
             (let ((chosen (select-item scr "Drink what?" drinks)))
               (if chosen
                   (list :type :quaff :key (entity-key chosen))
                   nil)))))
      ;; Piss — directional prompt
      ((eql key #\p)
       (let ((dir-result (prompt-direction scr gs "Piss in what direction?")))
         (when dir-result
           (let ((target (getf dir-result :target)))
             (case target
               (:dir
                (list :type :piss :target :dir
                      :direction (getf dir-result :direction)))
               (:self
                (list :type :piss :target :self))
               (:ground
                (list :type :piss :target :ground))
               (:up
                (list :type :piss :target :up))
               (:inventory
                (list :type :piss :target :inventory
                      :item-key (getf dir-result :item-key))))))))
      ;; Stairs
      ((eql key #\>)
       (list :type :descend))
      ((eql key #\<)
       (list :type :ascend))
      ;; Wait
      ((or (eql key #\.) (eql key #\Space))
       (list :type :noop))
      ;; Extended commands
      ((eql key #\#)
       (let ((cmd (prompt-extended-command scr)))
         (case cmd
           (:quit :quit)
           (:eat (key-to-action #\e scr gs))
           (:quaff (key-to-action #\q scr gs))
           (:drop (key-to-action #\d scr gs))
           (:pickup (key-to-action #\, scr gs))
           (:look (look-mode scr gs) nil)
           (:piss (key-to-action #\p scr gs))
           (:help (show-help scr) nil)
           (:wait (list :type :noop))
           (:descend (list :type :descend))
           (:ascend (list :type :ascend))
           (:inventory (show-inventory scr gs) nil)
           (:messages (show-message-log scr gs) nil)
           (otherwise nil))))
      ;; Help
      ((eql key #\?)
       (show-help scr)
       nil)
      ;; Message log (C-p = code 16)
      ((and (characterp key) (= (char-code key) 16))
       (show-message-log scr gs)
       nil)
      ;; Quit (C-q = code 17)
      ((and (characterp key) (= (char-code key) 17)) :quit)
      ;; Unknown key
      (t nil))))

(defparameter *project-dir*
  (directory-namestring
   (asdf:system-source-directory :flaghack)))

(defparameter *reload-sentinel*
  (merge-pathnames ".reload" *project-dir*))

(defun check-reload (gs)
  "If .reload sentinel exists, reload the ASDF system and delete it."
  (when (probe-file *reload-sentinel*)
    (handler-case
        (progn
          (asdf:load-system :flaghack :force t)
          (delete-file *reload-sentinel*)
          (gs-log gs "Reloaded."))
      (error (c)
        (delete-file *reload-sentinel*)
        (gs-log gs (format nil "Reload error: ~a" c))))))

(defun player-dead-p (gs)
  "Check if player is dead (HP <= 0)."
  (let ((player (get-player gs)))
    (and player (<= (creature-hp player) 0))))

(defun show-death-screen (scr gs)
  "Display the death screen."
  (croatoan:clear scr)
  (let ((player (get-player gs)))
    (croatoan:add-string scr "You have died." :y 5 :x 30
                         :fgcolor :red :bgcolor :black :attributes '(:bold))
    (croatoan:add-string scr
      (format nil "You reached dungeon level ~a." (game-state-depth gs))
      :y 8 :x 25 :fgcolor :white :bgcolor :black)
    (when player
      (let ((cause (cond
                     ((<= (creature-hunger player) 0) "starvation")
                     ((and (>= (game-state-heat gs) 56)
                           (< (creature-hydration player)
                              (floor (creature-max-hydration player) 10)))
                      "heat stroke")
                     ((>= (creature-bac player) 86) "alcohol poisoning")
                     (t "your injuries"))))
        (croatoan:add-string scr
          (format nil "Cause of death: ~a" cause)
          :y 10 :x 25 :fgcolor :white :bgcolor :black)))
    (croatoan:add-string scr "(press any key to quit)" :y 14 :x 25
                         :fgcolor :white :bgcolor :black)
    (croatoan:refresh scr)
    (croatoan:get-event scr)))

(defun game-loop (scr gs)
  "Main game loop."
  (loop
    ;; Check for death
    (when (player-dead-p gs)
      (show-death-screen scr gs)
      (return))
    ;; Check for live reload
    (check-reload gs)
    ;; Render
    (croatoan:clear scr)
    (render-messages scr gs)
    (render-world scr gs)
    (render-status scr gs)
    (croatoan:refresh scr)
    ;; Check for unconscious — skip input, auto-noop
    (let ((player (get-player gs)))
      (if (and player (has-status-p player :unconscious))
          (progn
            (gs-log gs "You are unconscious...")
            (let ((ai-plans (plan-all-ai gs)))
              (dolist (plan ai-plans)
                (execute-action gs (car plan) (cdr plan))))
            (tick-player-stats gs player)
            ;; Brief pause so player sees the message
            (sleep 0.3))
          ;; Normal input
          (let* ((event (croatoan:get-event scr))
                 (key (when event (croatoan:event-key event))))
            (let ((action (key-to-action key scr gs)))
              (when (eq action :quit)
                (return))
              (when action
                ;; Execute AI
                (let ((ai-plans (plan-all-ai gs)))
                  (dolist (plan ai-plans)
                    (execute-action gs (car plan) (cdr plan))))
                ;; Execute player action and tick stats
                (when player
                  (execute-action gs player action)
                  (tick-player-stats gs player)))))))))

(defparameter *swank-port* 4005)

(defun start-swank ()
  "Start a Swank server for live development."
  (asdf:load-system :swank)
  (let ((start (find-symbol "CREATE-SERVER" :swank)))
    (when start
      (funcall start :port *swank-port* :dont-close t)
      (format t "~%Swank server started on port ~a~%" *swank-port*))))

(defun run (&key (swank t))
  "Entry point — start the game. With :swank t (default), starts a Swank server on port 4005."
  (when swank
    (start-swank))
  (croatoan:with-screen (scr :input-echoing nil
                             :input-blocking t
                             :enable-function-keys t
                             :cursor-visible nil)
    ;; Disable XON/XOFF flow control so C-q reaches the application
    (cffi:foreign-funcall "raw" :int)
    (let ((gs (burn-level)))
      ;; Place player on a road tile
      (let ((road-pos (find-random-road gs)))
        (when road-pos
          (let ((player (make-player (pos-x road-pos)
                                     (pos-y road-pos)
                                     (pos-z road-pos))))
            (world-set gs player))))
      (game-loop scr gs))))
