(in-package :flaghack)

(defun key-to-action (key gs)
  "Map a keypress to a game action."
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
      ;; Arrow keys
      ((eql key :up)    (list :type :move :dir :n))
      ((eql key :down)  (list :type :move :dir :s))
      ((eql key :left)  (list :type :move :dir :w))
      ((eql key :right) (list :type :move :dir :e))
      ;; Pickup
      ((or (eql key #\g) (eql key #\,))
       (let ((items (when player (get-items-at gs (entity-at player)))))
         (if items
             (list :type :pickup :key (entity-key (first items)))
             (progn (gs-log gs "Nothing here to pick up.") nil))))
      ;; Drop
      ((eql key #\d)
       (let ((inventory (when player (get-inventory gs "player"))))
         (if inventory
             (list :type :drop :key (entity-key (first inventory)))
             (progn (gs-log gs "Nothing to drop.") nil))))
      ;; Wait
      ((or (eql key #\.) (eql key #\Space))
       (list :type :noop))
      ;; Quit
      ((eql key #\q) :quit)
      ;; Unknown key
      (t nil))))

(defun game-loop (scr gs)
  "Main game loop."
  (loop
    ;; Render
    (croatoan:clear scr)
    (render-world scr gs)
    (render-status scr gs)
    (croatoan:refresh scr)
    ;; Input
    (let* ((event (croatoan:get-event scr))
           (key (when event (croatoan:event-key event))))
      (let ((action (key-to-action key gs)))
        (when (eq action :quit)
          (return))
        (when action
          ;; Execute AI
          (let ((ai-plans (plan-all-ai gs)))
            (dolist (plan ai-plans)
              (execute-action gs (car plan) (cdr plan))))
          ;; Execute player action
          (let ((player (get-player gs)))
            (when player
              (execute-action gs player action))))))))

(defun run ()
  "Entry point — start the game."
  (croatoan:with-screen (scr :input-echoing nil
                             :input-blocking t
                             :enable-function-keys t
                             :cursor-visible nil)
    (let ((gs (campground-level)))
      ;; Place player on a floor tile
      (let ((floor-pos (find-random-floor gs)))
        (when floor-pos
          (let ((player (make-player (pos-x floor-pos)
                                     (pos-y floor-pos)
                                     (pos-z floor-pos))))
            (world-set gs player))))
      (game-loop scr gs))))
