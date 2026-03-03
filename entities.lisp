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

;;; Creatures — add name slot
(defstruct (creature (:include entity) (:predicate nil))
  (name "" :type string))

(defun make-player (x y z)
  (make-creature :key "player" :at (make-pos x y z)
                 :container "world" :tag :player :name "you"))

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

;;; Items
(defun make-flag (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :flag))

(defun make-water (x y z &optional (container "world"))
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container container :tag :water))

(defun make-acid-item (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :acid))

(defun make-booze (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :booze))

(defun make-milk (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :milk))

(defun make-poptart (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :poptart))

(defun make-trailmix (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :trailmix))

(defun make-pancake (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :pancake))

(defun make-bacon (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
               :container "world" :tag :bacon))

(defun make-soup (x y z)
  (make-entity :key (gen-key) :at (make-pos x y z)
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

;;; Type predicates
(defparameter +creature-tags+
  '(:player :hippie :acidcop :ranger :wook
    :lesser-egregore :greater-egregore :collective-egregore))

(defparameter +item-tags+
  '(:flag :water :acid :booze :milk
    :poptart :trailmix :pancake :bacon :soup))

(defparameter +terrain-tags+
  '(:wall :tentwall :floor :tunnel))

(defparameter +impassable-tags+
  '(:wall :tentwall))

(defun creature-p (e)
  (member (entity-tag e) +creature-tags+))

(defun item-p (e)
  (member (entity-tag e) +item-tags+))

(defun terrain-tag-p (e)
  (member (entity-tag e) +terrain-tags+))

(defun has-variant-p (e)
  "True if entity is a terrain struct with a variant slot."
  (typep e 'terrain))

(defun impassable-p (e)
  (member (entity-tag e) +impassable-tags+))
