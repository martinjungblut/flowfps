(in-package :flowfps)

(defun cli ()
  (open-and-apply "input.png" "greyscale.png" (filter-greyscale-average))
  (open-and-apply "greyscale.png" "darker-30.png" (filter-darker 30))
  (format t "Done!~&")
  (finish-output nil))
