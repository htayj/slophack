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
      (:tunnel   (values #\# :white nil))
      (:floor    (values #\. :white nil))
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

;;; Screen rendering with croatoan

(defun render-world (scr gs)
  "Render all visible entities to the ncurses screen."
  ;; Build a 2D grid of the top-priority entity at each position
  (let ((grid (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (string= (entity-container v) "world")
                 (let* ((p (entity-at v))
                        (gk (cons (pos-x p) (pos-y p)))
                        (existing (gethash gk grid)))
                   (when (or (null existing)
                             (> (entity-render-priority v)
                                (entity-render-priority existing)))
                     (setf (gethash gk grid) v)))))
             (game-state-world gs))
    ;; Draw each cell
    (maphash (lambda (gk entity)
               (let ((x (car gk))
                     (y (cdr gk)))
                 (when (and (>= x 0) (< x +screen-width+)
                            (>= y 0) (< y +screen-height+))
                   (multiple-value-bind (ch fgcolor attrs) (entity-tile entity)
                     (croatoan:add-char scr ch
                                        :y y :x x
                                        :fgcolor fgcolor
                                        :bgcolor :black
                                        :attributes attrs)))))
             grid)))

(defun render-status (scr gs)
  "Render status line below the map."
  (let* ((player (get-player gs))
         (inventory (when player (get-inventory gs "player")))
         (items-here (when player (get-items-at gs (entity-at player))))
         (status-y +screen-height+))
    ;; Inventory line
    (croatoan:add-string scr
      (format nil "Inventory: ~{~a~^, ~}"
              (or (mapcar (lambda (e) (entity-tag e)) inventory)
                  '("empty")))
      :y status-y :x 0
      :fgcolor :white :bgcolor :black)
    ;; Items on ground
    (when items-here
      (croatoan:add-string scr
        (format nil "Here: ~{~a~^, ~}"
                (mapcar (lambda (e) (entity-tag e)) items-here))
        :y (1+ status-y) :x 0
        :fgcolor :yellow :bgcolor :black))
    ;; Log message
    (when (game-state-log gs)
      (croatoan:add-string scr (or (first (game-state-log gs)) "")
        :y (+ status-y 2) :x 0
        :fgcolor :cyan :bgcolor :black))))
