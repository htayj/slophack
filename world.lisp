(in-package :flaghack)

(defparameter +screen-width+ 78)
(defparameter +screen-height+ 20)

;;; Wall variant determination

(defun wall-or-tentwall-at-p (gs x y z)
  "Check if there is a wall or tentwall at position (x,y,z)."
  (let ((entities (get-entities-at gs (make-pos x y z))))
    (some (lambda (e) (or (eq (entity-tag e) :wall)
                          (eq (entity-tag e) :tentwall)))
          entities)))

(defun determine-wall-variant (entity gs)
  (let* ((p (entity-at entity))
         (x (pos-x p)) (y (pos-y p)) (z (pos-z p))
         (n  (wall-or-tentwall-at-p gs x (1- y) z))
         (s  (wall-or-tentwall-at-p gs x (1+ y) z))
         (e  (wall-or-tentwall-at-p gs (1+ x) y z))
         (w  (wall-or-tentwall-at-p gs (1- x) y z)))
    (cond
      ;; Four-way
      ((and n s e w) :cross)
      ;; T-junctions
      ((and n s e (not w)) :t-right)
      ((and n s w (not e)) :t-left)
      ((and n e w (not s)) :t-up)
      ((and s e w (not n)) :t-down)
      ;; Corners
      ((and s e (not n) (not w)) :top-left)
      ((and s w (not n) (not e)) :top-right)
      ((and n e (not s) (not w)) :bottom-left)
      ((and n w (not s) (not e)) :bottom-right)
      ;; Straight
      ((and n s) :vertical)
      ((and e w) :horizontal)
      ;; Dead ends (treat as straight)
      ((or n s) :vertical)
      ((or e w) :horizontal)
      ;; Isolated
      (t :none))))

(defun apply-wall-variants (gs)
  (let (walls)
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (or (eq (entity-tag v) :wall)
                         (eq (entity-tag v) :tentwall))
                 (push v walls)))
             (game-state-world gs))
    (dolist (w walls)
      (when (has-variant-p w)
        (let ((variant (determine-wall-variant w gs)))
          (setf (terrain-variant w) variant))))))

;;; Terrain generation

(defun make-perimeter-walls (width height z)
  (let (walls)
    ;; Top and bottom
    (loop for x from 0 below width do
      (push (make-wall x 0 z) walls)
      (push (make-wall x (1- height) z) walls))
    ;; Left and right (excluding corners)
    (loop for y from 1 below (1- height) do
      (push (make-wall 0 y z) walls)
      (push (make-wall (1- width) y z) walls))
    walls))

(defun make-floors (width height z)
  (let (floors)
    (loop for x from 1 below (1- width) do
      (loop for y from 1 below (1- height) do
        (push (make-floor-tile x y z) floors)))
    floors))

(defun make-rect-walls (x1 y1 x2 y2 z &optional wall-fn)
  (let ((wfn (or wall-fn #'make-wall))
        walls)
    ;; Top and bottom
    (loop for x from x1 to x2 do
      (push (funcall wfn x y1 z) walls)
      (push (funcall wfn x y2 z) walls))
    ;; Left and right (excluding corners)
    (loop for y from (1+ y1) below y2 do
      (push (funcall wfn x1 y z) walls)
      (push (funcall wfn x2 y z) walls))
    walls))

(defun make-tent (x y w h entrance-offset z)
  (let* ((x2 (+ x w -1))
         (y2 (+ y h -1))
         (walls (make-rect-walls x y x2 y2 z #'make-tent-wall))
         (entrance-x (+ x entrance-offset))
         (entrance-pos (make-pos entrance-x y2 z)))
    ;; Remove the wall at the entrance
    (remove-if (lambda (e) (pos-equal (entity-at e) entrance-pos)) walls)))

(defun make-fire-ring (cx cy z)
  (let (walls)
    (loop for dx from -1 to 1 do
      (loop for dy from -1 to 1 do
        (unless (and (= dx 0) (= dy 0))
          (push (make-wall (+ cx dx) (+ cy dy) z) walls))))
    walls))

;;; Campground level

(defun campground-level ()
  (let ((gs (make-game-state))
        (z 0))
    ;; Perimeter and floors
    (add-entities gs (make-perimeter-walls +screen-width+ +screen-height+ z))
    (add-entities gs (make-floors +screen-width+ +screen-height+ z))
    ;; Tents
    (add-entities gs (make-tent 6 4 8 5 3 z))
    (add-entities gs (make-tent 20 3 10 6 4 z))
    (add-entities gs (make-tent 36 5 9 4 4 z))
    (add-entities gs (make-tent 54 4 10 5 5 z))
    ;; Fire ring
    (add-entities gs (make-fire-ring 30 12 z))
    ;; Items
    (add-entities gs (list (make-flag 10 10 z)
                           (make-flag 40 15 z)
                           (make-flag 60 10 z)
                           (make-water 15 12 z)
                           (make-water 50 8 z)))
    ;; Creatures
    (add-entities gs (list (make-hippie 12 12 z)))
    ;; Determine wall variants
    (apply-wall-variants gs)
    gs))

;;; BSP dungeon generation

(defun make-all-walls (width height z)
  (let (walls)
    (loop for x from 0 below width do
      (loop for y from 0 below height do
        (push (make-wall x y z) walls)))
    walls))

(defun carve-room (gs rng min-x max-x min-y max-y z)
  "Carve a room in the given bounds. Returns updated rng."
  (let* ((room-w (+ 3 (mod rng (max 1 (- max-x min-x 2)))))
         (room-h (+ 3 (mod (ash rng -8) (max 1 (- max-y min-y 2)))))
         (room-x (+ min-x (mod (ash rng -16) (max 1 (- max-x min-x room-w)))))
         (room-y (+ min-y (mod (ash rng -24) (max 1 (- max-y min-y room-h))))))
    ;; Replace walls with floors in the room interior
    (loop for x from (1+ room-x) below (+ room-x room-w -1) do
      (loop for y from (1+ room-y) below (+ room-y room-h -1) do
        (let ((entities (get-entities-at gs (make-pos x y z))))
          (dolist (e entities)
            (when (eq (entity-tag e) :wall)
              (world-remove gs (entity-key e))
              (world-set gs (make-floor-tile x y z)))))))
    ;; Return next rng state
    (logxor (ash rng -1) (* rng 1103515245))))

(defun find-floor-in-region (gs min-x max-x min-y max-y z)
  "Find a floor tile in the given region."
  (loop for x from min-x below max-x do
    (loop for y from min-y below max-y do
      (let ((entities (get-entities-at gs (make-pos x y z))))
        (when (some (lambda (e) (eq (entity-tag e) :floor)) entities)
          (return-from find-floor-in-region (make-pos x y z))))))
  nil)

(defun carve-tunnel (gs from-pos to-pos z)
  "Carve an L-shaped tunnel between two positions."
  (let ((x1 (pos-x from-pos)) (y1 (pos-y from-pos))
        (x2 (pos-x to-pos)) (y2 (pos-y to-pos)))
    ;; Horizontal segment
    (loop for x from (min x1 x2) to (max x1 x2) do
      (let ((p (make-pos x y1 z)))
        (let ((entities (get-entities-at gs p)))
          (dolist (e entities)
            (when (eq (entity-tag e) :wall)
              (world-remove gs (entity-key e))
              (world-set gs (make-tunnel x y1 z)))))))
    ;; Vertical segment
    (loop for y from (min y1 y2) to (max y1 y2) do
      (let ((p (make-pos x2 y z)))
        (let ((entities (get-entities-at gs p)))
          (dolist (e entities)
            (when (eq (entity-tag e) :wall)
              (world-remove gs (entity-key e))
              (world-set gs (make-tunnel x2 y z)))))))))

(defun bsp-split (gs rng min-x max-x min-y max-y z depth)
  "Recursively split and carve rooms via BSP. Returns updated rng."
  (let ((w (- max-x min-x))
        (h (- max-y min-y)))
    (if (or (<= w 10) (<= h 10) (> depth 5))
        ;; Base case: carve a room
        (carve-room gs rng min-x max-x min-y max-y z)
        ;; Split
        (let* ((split-vertical (> w h))
               (split-pos (if split-vertical
                              (+ min-x 4 (mod rng (max 1 (- w 8))))
                              (+ min-y 4 (mod rng (max 1 (- h 8))))))
               (rng2 (logxor (ash rng -3) (* rng 6364136223846793005))))
          (if split-vertical
              (let* ((rng3 (bsp-split gs rng2 min-x split-pos min-y max-y z (1+ depth)))
                     (rng4 (bsp-split gs rng3 split-pos max-x min-y max-y z (1+ depth))))
                ;; Link the two halves
                (let ((a (find-floor-in-region gs min-x split-pos min-y max-y z))
                      (b (find-floor-in-region gs split-pos max-x min-y max-y z)))
                  (when (and a b)
                    (carve-tunnel gs a b z)))
                rng4)
              (let* ((rng3 (bsp-split gs rng2 min-x max-x min-y split-pos z (1+ depth)))
                     (rng4 (bsp-split gs rng3 min-x max-x split-pos max-y z (1+ depth))))
                ;; Link the two halves
                (let ((a (find-floor-in-region gs min-x max-x min-y split-pos z))
                      (b (find-floor-in-region gs min-x max-x split-pos max-y z)))
                  (when (and a b)
                    (carve-tunnel gs a b z)))
                rng4))))))

(defun bsp-gen-level (seed)
  "Generate a BSP dungeon level."
  (let ((gs (make-game-state))
        (z 0))
    (add-entities gs (make-all-walls +screen-width+ +screen-height+ z))
    (bsp-split gs seed 0 +screen-width+ 0 +screen-height+ z 0)
    (apply-wall-variants gs)
    gs))

;;; Find a random floor position for player placement

(defun find-random-floor (gs)
  (let (floors)
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (eq (entity-tag v) :floor)
                 (push (entity-at v) floors)))
             (game-state-world gs))
    (when floors
      (nth (random (length floors)) floors))))
