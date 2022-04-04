(in-package :flowfps)

(defun cli ()
  (open-and-apply "input.png" "experimental.png" (filter-noise 5 40 5))
  (open-and-apply "experimental.png" "experimental.png" (filter-rgb-lshift))
  (open-and-apply "experimental.png" "experimental.png" (filter-rgb-lshift))
  (open-and-apply "experimental.png" "experimental.png" (filter-decrease-brightness 10))
  (format t "Done!~&")
  (finish-output nil))
