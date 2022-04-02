(asdf:defsystem :flowfps
  :description "FlowFPS is a tool to increase the FPS of video files."
  :author "Martin Jungblut Schreiner <martinjungblut@gmail.com>"
  :license  "BSD-2-Clause"
  :version "0.0.2"
  :serial t
  :depends-on (:cl-fad :opticl :iterate)
  :components ((:file "package")
                (:file "lib")
                (:file "cli")))
