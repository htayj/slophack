(defsystem "flaghack"
  :description "A roguelike game about flags"
  :license "AGPL-3.0-or-later"
  :depends-on ("croatoan")
  :serial t
  :components ((:file "package")
               (:file "position")
               (:file "entities")
               (:file "gamestate")
               (:file "world")
               (:file "burn")
               (:file "actions")
               (:file "ai")
               (:file "display")
               (:file "main")))
