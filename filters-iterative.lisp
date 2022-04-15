(in-package :flowfps)

(defmacro def-iterative-filter (name params &rest body)
  `(defun ,name ,params
     (lambda (image-target image-static)
       (image-iterate image-static y x
         (pixel-bind image-static y x r g b a
           (setf (pixel image-target y x) ,@body))))))

(def-iterative-filter filter-greyscale-average ()
  (let ((n (nth-value 0 (round (/ (+ r g b) 3)))))
    (values n n n a)))

(def-iterative-filter filter-decrease-brightness (factor)
  (let ((new-r (rgb- r factor))
         (new-g (rgb- g factor))
         (new-b (rgb- b factor)))
    (values new-r new-g new-b a)))

(def-iterative-filter filter-increase-brightness (factor)
  (let ((new-r (rgb+ r factor))
         (new-g (rgb+ g factor))
         (new-b (rgb+ b factor)))
    (values new-r new-g new-b a)))

(def-iterative-filter filter-noise (r-factor g-factor b-factor)
  (let ((new-r (rgb-noise r r-factor))
         (new-g (rgb-noise g g-factor))
         (new-b (rgb-noise b b-factor)))
    (values new-r new-g new-b a)))

(def-iterative-filter filter-rgb-rshift ()
  (values b r g a))

(def-iterative-filter filter-rgb-lshift ()
  (values g b r a))

(def-iterative-filter filter-sepia (factor)
  (let ((n (nth-value 0 (round (/ (+ r g b) 3)))))
    (values (rgb+ n factor) (rgb+ n factor) n a)))

(def-iterative-filter filter-saturation (factor)
  (let ((nr (+ (* (- r 127) factor) 127))
         (ng (+ (* (- g 127) factor) 127))
         (nb (+ (* (- b 127) factor) 127)))
    (values (rgb-fit nr) (rgb-fit ng) (rgb-fit nb) a)))
