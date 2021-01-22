(load "flowfps.asd")
(ql:quickload :flowfps)

(defun main ()
  (flowfps:blend-directory "images" 3)
  (quit))

(main)
