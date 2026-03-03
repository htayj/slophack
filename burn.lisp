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
    ;; Wall variants for the perimeter
    (apply-wall-variants gs)
    gs))
