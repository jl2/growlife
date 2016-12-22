;;;; growlife.asd

(asdf:defsystem #:growlife
  :serial t
  :description "Describe growlife here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (
               #:alexandria
               #:qt
               #:qtools
               #:qtgui
               #:qtcore
               #:qtopengl
               #:cl-opengl
               #:cl-glu
               #:trivial-garbage
               #:trivial-main-thread
               )
  :components ((:file "package")
               (:file "growlife")))

