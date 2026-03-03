(in-package :flaghack)

(defvar *key-counter* 0)

(defun gen-key ()
  (format nil "e~a" (incf *key-counter*)))

;;; Base entity structure — all entities have these slots
(defstruct entity
  (key "" :type string)
  (at (make-pos 0 0 0) :type pos)
  (container "world" :type string)
  (tag :floor :type keyword))

;;; Creatures — with stats
(defstruct (creature (:include entity) (:predicate nil))
  (name "" :type string)
  (hp 10 :type fixnum)
  (max-hp 10 :type fixnum)
  (vril 0 :type fixnum)
  (max-vril 0 :type fixnum)
  (hunger 900 :type fixnum)      ; counts down per turn; starving at 0
  (max-hunger 900 :type fixnum)
  ;; Six attributes (0-20)
  (cha 10 :type fixnum)
  (str 10 :type fixnum)
  (int 10 :type fixnum)
  (dex 10 :type fixnum)
  (con 10 :type fixnum)
  (wis 10 :type fixnum)
  ;; Status effects: alist of (effect . turns-remaining)
  (status-effects nil :type list)
  ;; Hidden stats
  (hydration 200 :type fixnum)
  (max-hydration 200 :type fixnum)
  (bac 0 :type fixnum)            ; blood alcohol content 0-100+
  (bladder 0 :type fixnum)        ; bladder fullness 0-100
  (max-bladder 100 :type fixnum)
  (electrolytes 100 :type fixnum) ; depleted by pissing, never displayed
  (max-electrolytes 100 :type fixnum))

(defun make-player (x y z)
  (make-creature :key "player" :at (make-pos x y z)
                 :container "world" :tag :player :name "you"
                 :hp 20 :max-hp 20 :vril 5 :max-vril 10
                 :hunger 900 :max-hunger 900
                 :hydration 200 :max-hydration 200
                 :bac 0 :bladder 0 :max-bladder 100
                 :electrolytes 100 :max-electrolytes 100
                 :str 12 :dex 12 :con 12 :int 10 :wis 10 :cha 10))

(defun make-hippie (x y z &optional (name "Ian"))
  (make-creature :key (gen-key) :at (make-pos x y z)
                 :container "world" :tag :hippie :name name))

(defun make-acid-kop (x y z &optional (name "Kop"))
  (make-creature :key (gen-key) :at (make-pos x y z)
                 :container "world" :tag :acidcop :name name))

(defun make-ranger (x y z &optional (name "Ranger"))
  (make-creature :key (gen-key) :at (make-pos x y z)
                 :container "world" :tag :ranger :name name))

(defun make-wook (x y z &optional (name "Wook"))
  (make-creature :key (gen-key) :at (make-pos x y z)
                 :container "world" :tag :wook :name name))

(defun make-lesser-egregore (x y z &optional (name "egregore"))
  (make-creature :key (gen-key) :at (make-pos x y z)
                 :container "world" :tag :lesser-egregore :name name))

(defun make-greater-egregore (x y z &optional (name "Egregore"))
  (make-creature :key (gen-key) :at (make-pos x y z)
                 :container "world" :tag :greater-egregore :name name))

(defun make-collective-egregore (x y z &optional (name "EGREGORE"))
  (make-creature :key (gen-key) :at (make-pos x y z)
                 :container "world" :tag :collective-egregore :name name))

;;; Items — with BUC and properties
(defstruct (item-entity (:include entity) (:predicate nil))
  (buc :uncursed :type keyword)       ; :blessed :uncursed :cursed
  (fixed nil :type boolean)
  (wet nil :type boolean)
  (props nil :type list))             ; generic plist for hidden properties

(defun make-flag (x y z)
  (make-item-entity :key (gen-key) :at (make-pos x y z)
                    :container "world" :tag :flag))

(defun make-water (x y z &optional (container "world"))
  (make-item-entity :key (gen-key) :at (make-pos x y z)
                    :container container :tag :water))

(defun make-acid-item (x y z)
  (make-item-entity :key (gen-key) :at (make-pos x y z)
                    :container "world" :tag :acid))

(defun make-booze (x y z)
  (make-item-entity :key (gen-key) :at (make-pos x y z)
                    :container "world" :tag :booze))

(defun make-milk (x y z)
  (make-item-entity :key (gen-key) :at (make-pos x y z)
                    :container "world" :tag :milk
                    :props (list :source (if (zerop (random 2)) :cow :human)
                                 :freshness (nth (random 3)
                                                 '(:fresh :chunky :sour)))))

(defun make-poptart (x y z)
  (make-item-entity :key (gen-key) :at (make-pos x y z)
                    :container "world" :tag :poptart))

(defun make-trailmix (x y z)
  (make-item-entity :key (gen-key) :at (make-pos x y z)
                    :container "world" :tag :trailmix))

(defun make-pancake (x y z)
  (make-item-entity :key (gen-key) :at (make-pos x y z)
                    :container "world" :tag :pancake))

(defun make-bacon (x y z)
  (make-item-entity :key (gen-key) :at (make-pos x y z)
                    :container "world" :tag :bacon))

(defun make-soup (x y z)
  (make-item-entity :key (gen-key) :at (make-pos x y z)
                    :container "world" :tag :soup))

;;; Terrain
(defstruct (terrain (:include entity) (:predicate nil))
  (variant :none :type keyword))

(defun make-wall (x y z &optional (variant :none))
  (make-terrain :key (gen-key) :at (make-pos x y z)
                :container "world" :tag :wall :variant variant))

(defun make-tent-wall (x y z &optional (variant :none))
  (make-terrain :key (gen-key) :at (make-pos x y z)
                :container "world" :tag :tentwall :variant variant))

(defun make-floor-tile (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :floor))

(defun make-tunnel (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :tunnel))

(defun make-road (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :road))

(defun make-playa (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :playa))

;;; Stairs for level transitions
(defun make-stairs-down (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :stairs-down))

(defun make-stairs-up (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :stairs-up))

;;; Type predicates
(defparameter +creature-tags+
  '(:player :hippie :acidcop :ranger :wook
    :lesser-egregore :greater-egregore :collective-egregore))

(defparameter +item-tags+
  '(:flag :water :acid :booze :milk
    :poptart :trailmix :pancake :bacon :soup))

(defparameter +food-tags+
  '(:poptart :trailmix :pancake :bacon :soup))

(defparameter +drink-tags+
  '(:water :booze :milk :acid))

(defparameter +terrain-tags+
  '(:wall :tentwall :floor :tunnel :stairs-down :stairs-up :road :playa))

(defparameter +impassable-tags+
  '(:wall :tentwall))

(defun creature-p (e)
  (member (entity-tag e) +creature-tags+))

(defun item-p (e)
  (member (entity-tag e) +item-tags+))

(defun food-p (e)
  (member (entity-tag e) +food-tags+))

(defun drink-p (e)
  (member (entity-tag e) +drink-tags+))

(defun terrain-tag-p (e)
  (member (entity-tag e) +terrain-tags+))

(defun has-variant-p (e)
  "True if entity is a terrain struct with a variant slot."
  (typep e 'terrain))

(defun impassable-p (e)
  (member (entity-tag e) +impassable-tags+))

;;; Status description helpers

(defun hunger-status (creature)
  (let ((h (creature-hunger creature)))
    (cond
      ((<= h 0)   "Starving")
      ((<= h 150) "Hungry")
      ((<= h 300) "Peckish")
      (t           nil))))

(defun heat-status (heat-value)
  "Return display string for heat level."
  (cond
    ((<= heat-value 30)  "Cool")
    ((<= heat-value 55)  "Warm")
    ((<= heat-value 75)  "Hot")
    (t                    "Sweltering")))

(defun bac-status (bac-value)
  "Return keyword for BAC threshold level, or nil if sober."
  (cond
    ((<= bac-value 10) nil)
    ((<= bac-value 25) :tipsy)
    ((<= bac-value 45) :plastered)
    ((<= bac-value 65) :drunk)
    ((<= bac-value 85) :wasted)
    (t                  :beyond-wasted)))

;;; Status effect helpers
(defun add-status-effect (creature effect duration)
  (let ((existing (assoc effect (creature-status-effects creature))))
    (if existing
        (setf (cdr existing) (max (cdr existing) duration))
        (push (cons effect duration) (creature-status-effects creature)))))

(defun has-status-p (creature effect)
  (assoc effect (creature-status-effects creature)))

(defun tick-status-effects (creature)
  "Decrement all status effect durations, removing expired ones."
  (setf (creature-status-effects creature)
        (loop for (effect . dur) in (creature-status-effects creature)
              when (> dur 1) collect (cons effect (1- dur)))))
