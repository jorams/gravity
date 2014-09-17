(asdf:defsystem #:gravity
  :serial t
  :description "Planet Simulation"
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:lispbuilder-sdl #:bordeaux-threads)
  :components ((:file "package")
               (:file "util")
               (:file "world")
               (:file "render")
               (:file "gravity")))
