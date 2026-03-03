(in-package :flaghack)

;;; Burn level — fractal road network generator
;;;
;;; The burn is a large festival campground with looping roads at multiple
;;; scales. Roads vary in width and the layout has self-similar (fractal)
;;; structure: large main loops contain medium sub-loops which contain
;;; small sub-sub-loops.

(defparameter +burn-width+ 300)
(defparameter +burn-height+ 150)

;;; 2D grid utilities

(defun make-grid (w h)
  "Create a WxH grid initialized to 0."
  (make-array (list h w) :element-type 'fixnum :initial-element 0))

(defun grid-ref (grid x y)
  "Safe grid read; returns 0 for out-of-bounds."
  (let ((h (array-dimension grid 0))
        (w (array-dimension grid 1)))
    (if (and (>= x 0) (< x w) (>= y 0) (< y h))
        (aref grid y x)
        0)))

(defun grid-set (grid x y val)
  "Safe grid write; no-op for out-of-bounds."
  (let ((h (array-dimension grid 0))
        (w (array-dimension grid 1)))
    (when (and (>= x 0) (< x w) (>= y 0) (< y h))
      (setf (aref grid y x) val))))

;;; Road rasterization

(defun paint-disc (grid cx cy radius)
  "Paint a filled disc of road (value 1) at (cx,cy)."
  (let ((r2 (* radius radius)))
    (loop for dy from (- radius) to radius do
      (loop for dx from (- radius) to radius do
        (when (<= (+ (* dx dx) (* dy dy)) r2)
          (grid-set grid (+ cx dx) (+ cy dy) 1))))))

(defun paint-road-segment (grid x1 y1 x2 y2 half-width)
  "Paint a thick line from (x1,y1) to (x2,y2)."
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (steps (max (abs dx) (abs dy) 1)))
    (loop for i from 0 to steps
          for x = (round (+ x1 (* dx (/ i steps))))
          for y = (round (+ y1 (* dy (/ i steps))))
          do (paint-disc grid x y half-width))))

(defun paint-loop (grid points base-hw)
  "Paint a closed loop of points as road with drifting width."
  (let ((n (length points))
        (hw-drift 0))
    (loop for i from 0 below n
          for p1 = (nth i points)
          for p2 = (nth (mod (1+ i) n) points)
          do (progn
               ;; Slowly drift width for organic variation
               (when (zerop (random 3))
                 (incf hw-drift (- (random 3) 1))
                 (setf hw-drift (max -1 (min 1 hw-drift))))
               (paint-road-segment grid (car p1) (cdr p1)
                                   (car p2) (cdr p2)
                                   (max 0 (+ base-hw hw-drift)))))))

;;; Loop geometry

(defun generate-loop-points (cx cy rx ry n-points wobble)
  "Generate N points forming a deformed elliptical loop."
  (loop for i from 0 below n-points
        for angle = (* 2.0d0 pi (/ (float i 1.0d0) (float n-points 1.0d0)))
        for r-noise = (- (random (1+ (* 2 wobble))) wobble)
        collect (cons (round (+ cx (* (+ rx r-noise) (cos angle))))
                      (round (+ cy (* (+ ry r-noise) (sin angle)))))))

(defun pick-evenly-spaced (points n)
  "Pick N evenly-spaced points from a list."
  (let* ((len (length points))
         (step (max 1 (floor len (max 1 n)))))
    (loop for i from 0 below (min n len)
          collect (nth (* i step) points))))

;;; Fractal road generation

(defun generate-fractal-loop (grid depth cx cy rx ry base-hw)
  "Recursively generate a loop with sub-loops branching off it.
   Depth 0 = main loop (wide), depth 1 = secondary (medium), depth 2 = tertiary (narrow)."
  (when (and (> rx 4) (> ry 3) (>= base-hw 0) (< depth 3))
    (let* ((n-points (max 10 (* 16 (- 3 depth))))
           (wobble (max 1 (floor (min rx ry) 5)))
           (points (generate-loop-points cx cy rx ry n-points wobble)))
      ;; Paint this loop
      (paint-loop grid points base-hw)
      ;; Generate child loops at junction points
      (let* ((n-children (+ 2 (random (max 1 (- 4 depth)))))
             (junctions (pick-evenly-spaced points n-children)))
        (dolist (jp junctions)
          (let* ((jx (car jp)) (jy (cdr jp))
                 ;; Offset child center from junction
                 (angle (* 2.0d0 pi (random 1.0d0)))
                 (dist (+ (floor (min rx ry) 2)
                          (random (max 1 (floor (min rx ry) 3)))))
                 (sub-cx (round (+ jx (* dist (cos angle)))))
                 (sub-cy (round (+ jy (* dist (sin angle)))))
                 ;; Child loop is roughly 1/3 the parent size
                 (sub-rx (max 5 (+ (floor rx 3) (random (max 1 (floor rx 5))))))
                 (sub-ry (max 4 (+ (floor ry 3) (random (max 1 (floor ry 5))))))
                 (child-hw (max 0 (1- base-hw)))
                 (child-points (generate-loop-points
                                sub-cx sub-cy sub-rx sub-ry
                                (max 8 (floor n-points 2))
                                (max 1 (floor wobble 2)))))
            (when child-points
              ;; Connect parent junction to child loop
              (let ((cp (first child-points)))
                (paint-road-segment grid jx jy (car cp) (cdr cp) child-hw))
              ;; Paint child loop
              (paint-loop grid child-points child-hw)
              ;; Recurse into child
              (generate-fractal-loop grid (1+ depth) sub-cx sub-cy
                                     sub-rx sub-ry child-hw))))))))

(defun generate-road-network (width height)
  "Generate a fractal road network. Returns a 2D grid (0=open, 1=road)."
  (let ((grid (make-grid width height))
        (cx (floor width 2))
        (cy (floor height 2))
        (rx (floor width 3))
        (ry (floor height 3)))
    (generate-fractal-loop grid 0 cx cy rx ry 2)
    grid))

;;; Theme camp names

(defparameter +theme-camp-names+
  '("Camp Questionable Decisions"
    "The Dusty Unicorn"
    "Camp No Pants"
    "The Flaming Lotus"
    "Whiskey & Wisdom"
    "Camp Lost & Found"
    "Sunset Soundcamp"
    "The Cuddle Puddle"
    "Camp Costco"
    "Electric Lemonade"
    "Space Cowboy Outpost"
    "Camp Free Hugs"
    "Midnight Poutine"
    "The Thunderdome"
    "Camp Maybe Tomorrow"
    "Cosmic Coyote"
    "Camp Last Resort"
    "Playa del Fuego"
    "Camp Existential Crisis"
    "The Giving Tree"
    "Sparkle Pony Ranch"
    "Camp FOMO"
    "The Infinite Jest"
    "Dr Bronner's Brain Wash"
    "Root Society"
    "Camp Frozen Burrito"
    "The Hug Deli"
    "Cats vs Dogs"
    "The Wobble Dome"
    "Camp Arctica"))

;;; Camp placement utilities

(defun check-clear (zone-grid x y w h)
  "Check if a WxH rectangle at (x,y) is all clear playa (0)."
  (loop for ty from y below (+ y h) do
    (loop for tx from x below (+ x w) do
      (unless (= (grid-ref zone-grid tx ty) 0)
        (return-from check-clear nil))))
  t)

(defun mark-zone (zone-grid x y w h val)
  "Mark a WxH rectangle at (x,y) with val in the zone grid."
  (loop for ty from y below (+ y h) do
    (loop for tx from x below (+ x w) do
      (grid-set zone-grid tx ty val))))

(defun road-direction-from (road-grid x y)
  "Determine which cardinal direction has road relative to (x,y)."
  (cond
    ((= (grid-ref road-grid x (1+ y)) 1) :s)
    ((= (grid-ref road-grid x (1- y)) 1) :n)
    ((= (grid-ref road-grid (1+ x) y) 1) :e)
    ((= (grid-ref road-grid (1- x) y) 1) :w)
    (t :s)))

(defun opposite-dir (dir)
  (ecase dir (:n :s) (:s :n) (:e :w) (:w :e)))

(defun make-oriented-tent (x y w h entrance-side z)
  "Create a tent with entrance on the specified side (:n :s :e :w)."
  (let* ((x2 (+ x w -1))
         (y2 (+ y h -1))
         (walls (make-rect-walls x y x2 y2 z #'make-tent-wall))
         (mid-x (+ x (floor w 2)))
         (mid-y (+ y (floor h 2)))
         (entrance-pos (ecase entrance-side
                         (:s (make-pos mid-x y2 z))
                         (:n (make-pos mid-x y z))
                         (:e (make-pos x2 mid-y z))
                         (:w (make-pos x mid-y z)))))
    (remove-if (lambda (e) (pos-equal (entity-at e) entrance-pos)) walls)))

(defun find-camp-sites (road-grid width height n-sites min-dist)
  "Find N well-spaced playa positions adjacent to roads."
  (let (candidates)
    (loop for y from 5 below (- height 5) do
      (loop for x from 5 below (- width 5) do
        (when (and (= (grid-ref road-grid x y) 0)
                   (or (= (grid-ref road-grid (1+ x) y) 1)
                       (= (grid-ref road-grid (1- x) y) 1)
                       (= (grid-ref road-grid x (1+ y)) 1)
                       (= (grid-ref road-grid x (1- y)) 1)))
          (push (cons x y) candidates))))
    ;; Sample well-spaced sites via random selection with distance check
    (let (sites
          (shuffled (sort (copy-list candidates)
                          (lambda (a b) (declare (ignore a b)) (zerop (random 2))))))
      (dolist (pos shuffled)
        (when (>= (length sites) n-sites) (return))
        (unless (some (lambda (s)
                        (let ((dx (- (car pos) (car s)))
                              (dy (- (cdr pos) (cdr s))))
                          (< (+ (* dx dx) (* dy dy)) (* min-dist min-dist))))
                      sites)
          (push pos sites)))
      sites)))

(defun place-camp-cluster (gs zone-grid site-x site-y road-dir z n-tents)
  "Place a cluster of tents near (site-x, site-y) stepping away from road.
   Returns number of tents successfully placed."
  (let* ((entrance-side road-dir)
         ;; Step away from road
         (away-dx (ecase road-dir (:s 0) (:n 0) (:e -1) (:w 1)))
         (away-dy (ecase road-dir (:s -1) (:n 1) (:e 0) (:w 0)))
         (base-x (+ site-x (* away-dx 2)))
         (base-y (+ site-y (* away-dy 2)))
         ;; Perpendicular direction for tent row
         (row-dx (if (member road-dir '(:n :s)) 1 0))
         (row-dy (if (member road-dir '(:e :w)) 1 0))
         (placed 0))
    (loop for i from 0 below n-tents
          for tent-w = (+ 4 (random 4))
          for tent-h = (+ 3 (random 3))
          for offset = (* i (+ (if (= row-dx 1) tent-w tent-h) 1))
          for tx = (+ base-x (* row-dx offset))
          for ty = (+ base-y (* row-dy offset))
          do (when (check-clear zone-grid tx ty tent-w tent-h)
               (add-entities gs (make-oriented-tent tx ty tent-w tent-h entrance-side z))
               (mark-zone zone-grid tx ty tent-w tent-h 2)
               (incf placed)))
    placed))

(defun place-camps (gs road-grid z)
  "Place theme camps and open camping along roads."
  (let ((zone-grid (make-grid +burn-width+ +burn-height+))
        (sites nil)
        (name-pool (copy-list +theme-camp-names+))
        (name-idx 0))
    ;; Initialize zone grid from road grid
    (loop for y from 0 below +burn-height+ do
      (loop for x from 0 below +burn-width+ do
        (grid-set zone-grid x y (grid-ref road-grid x y))))
    ;; Find camp sites
    (setf sites (find-camp-sites road-grid +burn-width+ +burn-height+ 30 15))
    ;; Place camps
    (dolist (site sites)
      (let* ((sx (car site)) (sy (cdr site))
             (road-dir (road-direction-from road-grid sx sy))
             (is-theme-camp (< (random 10) 8))
             (n-tents (if is-theme-camp (+ 2 (random 2)) 1))
             (placed (place-camp-cluster gs zone-grid sx sy road-dir z n-tents)))
        ;; Place camp sign for theme camps
        (when (and is-theme-camp (> placed 0) (< name-idx (length name-pool)))
          (let ((name (nth name-idx name-pool)))
            (world-set gs (make-camp-sign sx sy z name))
            (incf name-idx)))))))

;;; Burn level construction

(defun find-random-road (gs)
  "Find a random road tile position."
  (let (roads)
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (eq (entity-tag v) :road)
                 (push (entity-at v) roads)))
             (game-state-world gs))
    (when roads
      (nth (random (length roads)) roads))))

(defun burn-level ()
  "Generate the burn starting level (z=0)."
  (let* ((gs (make-game-state :heat (level-heat 0)))
         (z 0)
         (road-grid (generate-road-network +burn-width+ +burn-height+)))
    ;; Perimeter walls
    (add-entities gs (make-perimeter-walls +burn-width+ +burn-height+ z))
    ;; Interior terrain from road grid
    (loop for y from 1 below (1- +burn-height+) do
      (loop for x from 1 below (1- +burn-width+) do
        (world-set gs (if (= (grid-ref road-grid x y) 1)
                          (make-road x y z)
                          (make-playa x y z)))))
    ;; Place theme camps and open camping along roads
    (place-camps gs road-grid z)
    ;; Wall variants for walls and tent walls
    (apply-wall-variants gs)
    gs))
