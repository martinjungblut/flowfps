(in-package :flowfps)

(defun cli ()
  (open-and-apply "input.png" "greyscale.png" (filter-greyscale-average))
  (open-and-apply "greyscale.png" "darker-30.png" (filter-darker 30))
  (open-and-apply "input.png" "experimental.png" (filter-experimental))
  (open-and-apply "experimental.png" "experimental-darker.png" (filter-darker 30))
  (format t "Done!~&")
  (finish-output nil))
