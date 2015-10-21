;;;; growlife.asd

(asdf:defsystem #:growlife
  :serial t
  :description "Describe growlife here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl
               #:cl-opengl
               #:cl-glu
               #:alexandria)
  :components ((:file "package")
               (:file "growlife")))

