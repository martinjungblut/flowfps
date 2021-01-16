;;;; flowfps.asd

(asdf:defsystem #:flowfps
  :description "FlowFPS is a tool to increase the FPS of video files."
  :author "Martin Jungblut Schreiner <martinjungblut@gmail.com>"
  :license  "BSD-2-Clause"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "flowfps")))
