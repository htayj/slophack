(in-package :flaghack)

;;; Wall variant → box-drawing character
(defun wall-variant-char (variant)
  (case variant
    (:vertical     #\|)
    (:horizontal   #\-)
    (:top-left     #\+)
    (:top-right    #\+)
    (:bottom-left  #\+)
    (:bottom-right #\+)
    (:cross        #\+)
    (:t-up         #\+)
    (:t-down       #\+)
    (:t-left       #\+)
    (:t-right      #\+)
    (:none         #\#)
    (otherwise     #\#)))

;;; Entity → display tile
;;; Returns (values char fgcolor attributes)

(defun entity-tile (entity)
  "Returns (values char fgcolor attributes) for an entity."
  (let ((tag (entity-tag entity)))
    (case tag
      ;; Creatures
      (:player              (values #\@ :white  nil))
      (:ranger              (values #\@ :magenta nil))
      (:hippie              (values #\h :yellow nil))
      (:wook                (values #\h :cyan   nil))
      (:acidcop             (values #\K :magenta nil))
      (:lesser-egregore     (values #\e :green  nil))
      (:greater-egregore    (values #\E :green  nil))
      (:collective-egregore (values #\E :green  nil))
      ;; Items
      (:flag     (values #\F :yellow  '(:bold)))
      (:water    (values #\! :cyan    nil))
      (:booze    (values #\! :yellow  nil))
      (:milk     (values #\! :white   nil))
      (:acid     (values #\! :green   nil))
      (:bacon    (values #\% :red     '(:bold)))
      (:poptart  (values #\% :yellow  '(:bold)))
      (:trailmix (values #\% :yellow  nil))
      (:pancake  (values #\% :white   '(:bold)))
      (:soup     (values #\% :red     nil))
      ;; Terrain
      (:wall
       (let ((ch (wall-variant-char (if (has-variant-p entity)
                                        (terrain-variant entity)
                                        :none))))
         (values ch :white nil)))
      (:tentwall
       (let ((ch (wall-variant-char (if (has-variant-p entity)
                                        (terrain-variant entity)
                                        :none))))
         (values ch :blue nil)))
      (:tunnel      (values #\# :white nil))
      (:road        (values #\# :yellow nil))
      (:playa       (values #\. :white nil))
      (:camp-sign   (values #\* :cyan '(:bold)))
      (:stairs-down (values #\> :white '(:bold)))
      (:stairs-up   (values #\< :white '(:bold)))
      (:floor       (values #\. :white nil))
      ;; Fallback
      (otherwise (values #\? :white nil)))))

;;; Render priority: creatures > items > terrain
(defun entity-render-priority (entity)
  (cond
    ((creature-p entity)   4)
    ((item-p entity)       3)
    ((impassable-p entity) 2)
    ((terrain-tag-p entity) 1)
    (t                     0)))

;;; Layout constants
;;; Message box sits above the map, status bar below.
(defparameter +msg-height+ 3)     ; lines for message box
(defparameter +map-y-offset+ (+ +msg-height+ 1)) ; map starts after messages + border

;;; Viewport / camera

(defun compute-camera (gs)
  "Return camera offset (values cam-x cam-y) to center viewport on player."
  (let ((player (get-player gs)))
    (if player
        (values (- (pos-x (entity-at player)) (floor +screen-width+ 2))
                (- (pos-y (entity-at player)) (floor +screen-height+ 2)))
        (values 0 0))))

;;; Screen rendering with croatoan

(defun render-messages (scr gs)
  "Render the message box above the game board."
  (let ((msgs (game-state-log gs)))
    ;; Show up to +msg-height+ most recent messages
    (loop for i from 0 below +msg-height+
          for msg in msgs
          when msg do
            (croatoan:add-string scr
              (subseq msg 0 (min (length msg) (1- +screen-width+)))
              :y i :x 0
              :fgcolor :cyan :bgcolor :black))
    ;; Separator line
    (croatoan:add-string scr
      (make-string +screen-width+ :initial-element #\-)
      :y +msg-height+ :x 0
      :fgcolor :white :bgcolor :black)))

(defun render-world (scr gs)
  "Render all visible entities to the ncurses screen with viewport."
  (multiple-value-bind (cam-x cam-y) (compute-camera gs)
    ;; Build a 2D grid of the top-priority entity at each screen position
    (let ((grid (make-hash-table :test 'equal)))
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (when (string= (entity-container v) "world")
                   (let* ((p (entity-at v))
                          (sx (- (pos-x p) cam-x))
                          (sy (- (pos-y p) cam-y)))
                     ;; Only consider entities within viewport
                     (when (and (>= sx 0) (< sx +screen-width+)
                                (>= sy 0) (< sy +screen-height+))
                       (let* ((gk (cons sx sy))
                              (existing (gethash gk grid)))
                         (when (or (null existing)
                                   (> (entity-render-priority v)
                                      (entity-render-priority existing)))
                           (setf (gethash gk grid) v)))))))
               (game-state-world gs))
      ;; Draw each cell, offset by +map-y-offset+
      (maphash (lambda (gk entity)
                 (let ((sx (car gk))
                       (sy (cdr gk)))
                   (multiple-value-bind (ch fgcolor attrs) (entity-tile entity)
                     (croatoan:add-char scr ch
                                        :y (+ sy +map-y-offset+) :x sx
                                        :fgcolor fgcolor
                                        :bgcolor :black
                                        :attributes attrs))))
               grid))))

(defun render-status (scr gs)
  "Render status bar below the map with player stats."
  (let* ((player (get-player gs))
         (items-here (when player (get-items-at gs (entity-at player))))
         (status-y (+ +map-y-offset+ +screen-height+)))
    ;; Separator
    (croatoan:add-string scr
      (make-string +screen-width+ :initial-element #\-)
      :y status-y :x 0
      :fgcolor :white :bgcolor :black)
    (when player
      ;; Stats line: HP, Vril, Hunger, Heat, Depth
      (let* ((hunger-str (or (hunger-status player) ""))
             (heat-str (heat-status (game-state-heat gs)))
             (effects (creature-status-effects player))
             (effect-str (if effects
                             (format nil " [~{~a~^,~}]"
                                     (mapcar (lambda (e) (car e)) effects))
                             ""))
             (stats (format nil "HP:~a/~a Vril:~a/~a ~a ~a Dlvl:~a~a"
                            (creature-hp player) (creature-max-hp player)
                            (creature-vril player) (creature-max-vril player)
                            hunger-str
                            heat-str
                            (game-state-depth gs)
                            effect-str)))
        (croatoan:add-string scr
          (subseq stats 0 (min (length stats) (1- +screen-width+)))
          :y (1+ status-y) :x 0
          :fgcolor (if (< (creature-hp player) (floor (creature-max-hp player) 3))
                       :red :white)
          :bgcolor :black)))
    ;; Items on ground
    (when items-here
      (croatoan:add-string scr
        (format nil "Here: ~{~a~^, ~}"
                (mapcar (lambda (e) (entity-tag e)) items-here))
        :y (+ status-y 2) :x 0
        :fgcolor :yellow :bgcolor :black))))

;;; Item selection menu (nethack-style letter prompts)

(defun item-letter (index)
  "Map index 0-25 to a-z."
  (code-char (+ (char-code #\a) index)))

(defun letter-to-index (ch)
  "Map a-z to index 0-25. Returns nil for non-letter."
  (when (and (characterp ch)
             (char>= ch #\a) (char<= ch #\z))
    (- (char-code ch) (char-code #\a))))

(defun select-item (scr prompt items)
  "Show a list of items with letter labels. Returns selected entity or nil.
   ESC or space cancels."
  (when (null items) (return-from select-item nil))
  (croatoan:clear scr)
  (croatoan:add-string scr prompt :y 0 :x 0
                       :fgcolor :white :bgcolor :black)
  (loop for item in items
        for i from 0
        for letter = (item-letter i)
        do (multiple-value-bind (ch fgcolor attrs) (entity-tile item)
             (declare (ignore ch))
             (croatoan:add-string scr
               (format nil "~a - ~a" letter (entity-tag item))
               :y (+ 2 i) :x 2
               :fgcolor fgcolor :bgcolor :black
               :attributes attrs)))
  (croatoan:add-string scr "(ESC to cancel)" :y (+ 3 (length items)) :x 2
                       :fgcolor :white :bgcolor :black)
  (croatoan:refresh scr)
  ;; Wait for selection
  (loop
    (let* ((event (croatoan:get-event scr))
           (key (when event (croatoan:event-key event))))
      (cond
        ((or (eql key #\Esc) (eql key #\Space) (eql key #\q))
         (return nil))
        ((characterp key)
         (let ((idx (letter-to-index key)))
           (when (and idx (< idx (length items)))
             (return (nth idx items)))))))))

(defun select-multiple-items (scr prompt items)
  "Show items with letter labels. Toggle with letters, confirm with enter.
   Returns list of selected entities."
  (when (null items) (return-from select-multiple-items nil))
  (let ((selected (make-array (length items) :initial-element nil)))
    (loop
      (croatoan:clear scr)
      (croatoan:add-string scr prompt :y 0 :x 0
                           :fgcolor :white :bgcolor :black)
      (loop for item in items
            for i from 0
            for letter = (item-letter i)
            for sel = (aref selected i)
            do (multiple-value-bind (ch fgcolor attrs) (entity-tile item)
                 (declare (ignore ch))
                 (croatoan:add-string scr
                   (format nil "~a ~a ~a"
                           letter (if sel "+" "-") (entity-tag item))
                   :y (+ 2 i) :x 2
                   :fgcolor (if sel :green fgcolor) :bgcolor :black
                   :attributes attrs)))
      (croatoan:add-string scr "(toggle: a-z, confirm: Enter, cancel: ESC)"
                           :y (+ 3 (length items)) :x 2
                           :fgcolor :white :bgcolor :black)
      (croatoan:refresh scr)
      (let* ((event (croatoan:get-event scr))
             (key (when event (croatoan:event-key event))))
        (cond
          ((or (eql key #\Esc))
           (return nil))
          ((or (eql key #\Newline) (eql key #\Return))
           (return (loop for item in items
                         for i from 0
                         when (aref selected i) collect item)))
          ((characterp key)
           (let ((idx (letter-to-index key)))
             (when (and idx (< idx (length items)))
               (setf (aref selected idx) (not (aref selected idx)))))))))))

;;; Directional action prompt (reusable)

(defun prompt-direction (scr gs prompt)
  "Prompt for a direction/target. Returns a plist or nil on cancel.
   (:target :dir :direction <dir>)  — adjacent tile
   (:target :self)                  — on self
   (:target :ground)                — ground at current tile
   (:target :up)                    — up in the air
   (:target :inventory :item-key <key>) — on inventory item
   nil                              — cancelled"
  (let ((status-y (+ +map-y-offset+ +screen-height+)))
    (croatoan:add-string scr
      (format nil "~a [hjklyubn .=self >=ground <=up i=item ESC=cancel]" prompt)
      :y (+ status-y 3) :x 0
      :fgcolor :cyan :bgcolor :black)
    (croatoan:refresh scr)
    (loop
      (let* ((event (croatoan:get-event scr))
             (key (when event (croatoan:event-key event))))
        (cond
          ((or (eql key #\Esc) (eql key #\Space))
           (return nil))
          ((eql key #\h) (return (list :target :dir :direction :w)))
          ((eql key #\j) (return (list :target :dir :direction :s)))
          ((eql key #\k) (return (list :target :dir :direction :n)))
          ((eql key #\l) (return (list :target :dir :direction :e)))
          ((eql key #\y) (return (list :target :dir :direction :nw)))
          ((eql key #\u) (return (list :target :dir :direction :ne)))
          ((eql key #\b) (return (list :target :dir :direction :sw)))
          ((eql key #\n) (return (list :target :dir :direction :se)))
          ((eql key #\.) (return (list :target :self)))
          ((eql key #\>) (return (list :target :ground)))
          ((eql key #\<) (return (list :target :up)))
          ((eql key #\i)
           (let* ((inventory (get-inventory gs "player"))
                  (chosen (select-item scr "On which item?" inventory)))
             (if chosen
                 (return (list :target :inventory :item-key (entity-key chosen)))
                 (return nil)))))))))

;;; Extended command system

(defparameter +extended-commands+
  '(("quit"      . :quit)
    ("eat"       . :eat)
    ("quaff"     . :quaff)
    ("drink"     . :quaff)
    ("drop"      . :drop)
    ("pickup"    . :pickup)
    ("get"       . :pickup)
    ("look"      . :look)
    ("piss"      . :piss)
    ("help"      . :help)
    ("wait"      . :wait)
    ("descend"   . :descend)
    ("ascend"    . :ascend)
    ("inventory" . :inventory)
    ("messages"  . :messages)
    ("log"       . :messages)))

(defun prompt-extended-command (scr)
  "Prompt for an extended command name. Returns command keyword or nil.
   Tab completes partial input. ESC cancels."
  (let ((input "")
        (prompt-y (+ +map-y-offset+ +screen-height+ 3)))
    (loop
      ;; Clear prompt line and display
      (croatoan:add-string scr
        (format nil "# ~a~a" input
                (make-string (max 0 (- +screen-width+ (length input) 2))
                             :initial-element #\Space))
        :y prompt-y :x 0
        :fgcolor :cyan :bgcolor :black)
      (croatoan:refresh scr)
      ;; Read key
      (let* ((event (croatoan:get-event scr))
             (key (when event (croatoan:event-key event))))
        (cond
          ((eql key #\Esc)
           (return nil))
          ((or (eql key #\Newline) (eql key #\Return))
           (if (= (length input) 0)
               (return nil)
               (let ((match (assoc input +extended-commands+
                                   :test #'string-equal)))
                 (return (if match (cdr match) nil)))))
          ((or (eql key #\Backspace) (eql key #\Rubout) (eql key :backspace))
           (when (> (length input) 0)
             (setf input (subseq input 0 (1- (length input))))))
          ((eql key #\Tab)
           ;; Tab completion: if exactly one match, complete it
           (let ((matches (remove-if-not
                           (lambda (cmd)
                             (and (>= (length (car cmd)) (length input))
                                  (> (length input) 0)
                                  (string-equal input (car cmd)
                                                :end2 (length input))))
                           +extended-commands+)))
             (cond
               ((= (length matches) 1)
                (setf input (car (first matches))))
               ((> (length matches) 1)
                ;; Show matching options on the line below
                (croatoan:add-string scr
                  (format nil "~{~a~^ ~}"
                          (remove-duplicates
                           (mapcar #'car matches) :test #'string=))
                  :y (1+ prompt-y) :x 2
                  :fgcolor :white :bgcolor :black)
                (croatoan:refresh scr)))))
          ((and (characterp key) (alpha-char-p key))
           (setf input (concatenate 'string input (string key)))))))))

(defun show-inventory (scr gs)
  "Display full inventory screen with item categories. Press any key to dismiss."
  (let* ((inventory (get-inventory gs "player"))
         (food (remove-if-not #'food-p inventory))
         (drinks (remove-if-not #'drink-p inventory))
         (other (remove-if (lambda (e) (or (food-p e) (drink-p e))) inventory)))
    (croatoan:clear scr)
    (croatoan:add-string scr "Inventory" :y 0 :x 0
                         :fgcolor :white :bgcolor :black
                         :attributes '(:bold))
    (croatoan:add-string scr (make-string 40 :initial-element #\-)
                         :y 1 :x 0 :fgcolor :white :bgcolor :black)
    (if (null inventory)
        (croatoan:add-string scr "Empty." :y 3 :x 2
                             :fgcolor :white :bgcolor :black)
        (let ((row 3)
              (letter-idx 0))
          ;; Food section
          (when food
            (croatoan:add-string scr "Food" :y row :x 1
                                 :fgcolor :white :bgcolor :black :attributes '(:bold))
            (incf row)
            (dolist (item food)
              (multiple-value-bind (ch fgcolor attrs) (entity-tile item)
                (croatoan:add-string scr
                  (format nil "~a - ~c ~a" (item-letter letter-idx) ch (entity-tag item))
                  :y row :x 2
                  :fgcolor fgcolor :bgcolor :black :attributes attrs))
              (incf row)
              (incf letter-idx)))
          ;; Drink section
          (when drinks
            (incf row)
            (croatoan:add-string scr "Drinks" :y row :x 1
                                 :fgcolor :white :bgcolor :black :attributes '(:bold))
            (incf row)
            (dolist (item drinks)
              (multiple-value-bind (ch fgcolor attrs) (entity-tile item)
                (croatoan:add-string scr
                  (format nil "~a - ~c ~a" (item-letter letter-idx) ch (entity-tag item))
                  :y row :x 2
                  :fgcolor fgcolor :bgcolor :black :attributes attrs))
              (incf row)
              (incf letter-idx)))
          ;; Other section
          (when other
            (incf row)
            (croatoan:add-string scr "Other" :y row :x 1
                                 :fgcolor :white :bgcolor :black :attributes '(:bold))
            (incf row)
            (dolist (item other)
              (multiple-value-bind (ch fgcolor attrs) (entity-tile item)
                (croatoan:add-string scr
                  (format nil "~a - ~c ~a" (item-letter letter-idx) ch (entity-tag item))
                  :y row :x 2
                  :fgcolor fgcolor :bgcolor :black :attributes attrs))
              (incf row)
              (incf letter-idx)))))
    (croatoan:add-string scr "(press any key)"
                         :y (+ 5 (length inventory)) :x 2
                         :fgcolor :white :bgcolor :black)
    (croatoan:refresh scr)
    (croatoan:get-event scr)))

;;; Help screen

(defun show-help (scr)
  "Display keybindings help screen."
  (croatoan:clear scr)
  (let ((bindings '("Keybindings"
                     "----------"
                     ""
                     "Movement:"
                     "  h/j/k/l     Move W/S/N/E"
                     "  y/u/b/n     Move NW/NE/SW/SE"
                     "  Arrow keys  Move in cardinal directions"
                     "  H/J/K/L     Run in direction (shift+move)"
                     "  Y/U/B/N     Run diagonally"
                     ""
                     "Actions:"
                     "  ,  g        Pick up item"
                     "  d           Drop item"
                     "  D           Drop multiple items"
                     "  e           Eat food"
                     "  q           Quaff (drink)"
                     "  >           Descend stairs"
                     "  <           Ascend stairs"
                     "  p           Piss (choose direction)"
                     "  .  Space    Wait a turn"
                     ""
                     "Interface:"
                     "  i           Inventory"
                     "  ;           Look mode"
                     "  #           Extended command (type name, Tab completes)"
                     "  ?           This help screen"
                     "  C-p         Message log"
                     "  C-q         Quit")))
    (loop for line in bindings
          for y from 0
          do (croatoan:add-string scr line :y y :x 1
                                  :fgcolor :white :bgcolor :black)))
  (croatoan:add-string scr "(press any key)" :y 27 :x 1
                       :fgcolor :white :bgcolor :black)
  (croatoan:refresh scr)
  (croatoan:get-event scr))

;;; Message log scrollback

(defun show-message-log (scr gs)
  "Display full message log with scrolling. j/k or arrows to scroll, ESC to exit."
  (let* ((msgs (reverse (game-state-log gs)))
         (total (length msgs))
         (visible-lines 20)
         (offset 0))
    (loop
      (croatoan:clear scr)
      (croatoan:add-string scr "Message Log" :y 0 :x 0
                           :fgcolor :white :bgcolor :black :attributes '(:bold))
      (croatoan:add-string scr (make-string 40 :initial-element #\-)
                           :y 1 :x 0 :fgcolor :white :bgcolor :black)
      ;; Show messages from offset
      (loop for i from 0 below visible-lines
            for idx = (+ offset i)
            when (< idx total)
            do (let ((msg (nth idx msgs)))
                 (croatoan:add-string scr
                   (subseq msg 0 (min (length msg) (- +screen-width+ 2)))
                   :y (+ 2 i) :x 1
                   :fgcolor :cyan :bgcolor :black)))
      ;; Scroll indicators
      (when (> offset 0)
        (croatoan:add-string scr "-- more above --" :y (+ 2 visible-lines) :x 1
                             :fgcolor :white :bgcolor :black))
      (when (< (+ offset visible-lines) total)
        (croatoan:add-string scr "-- more below --" :y (+ 3 visible-lines) :x 1
                             :fgcolor :white :bgcolor :black))
      (croatoan:add-string scr "(j/k to scroll, ESC to close)"
                           :y (+ 4 visible-lines) :x 1
                           :fgcolor :white :bgcolor :black)
      (croatoan:refresh scr)
      (let* ((event (croatoan:get-event scr))
             (key (when event (croatoan:event-key event))))
        (cond
          ((or (eql key #\Esc) (eql key #\Space) (eql key #\q))
           (return))
          ((or (eql key #\j) (eql key :down))
           (when (< (+ offset visible-lines) total)
             (incf offset)))
          ((or (eql key #\k) (eql key :up))
           (when (> offset 0)
             (decf offset))))))))
