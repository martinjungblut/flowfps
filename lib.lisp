(in-package :flowfps)
(declaim (optimize (speed 3) (debug 1) (safety 1)))

(defmacro image-iterate (image y x &rest body)
  `(with-image-bounds (height width) ,image
     (loop for ,y below height do
       (loop for ,x below width do
         ,@body))))

(defmacro pixel-bind (image y x r g b a &rest body)
  `(multiple-value-bind (,r ,g ,b ,a) (pixel ,image ,y ,x)
     (declare (type (unsigned-byte 8) ,r ,g ,b ,a))
     ,@body))

(defun open-apply-write (input-image-path output-image-path filter)
  (let* ((image-target (read-png-file input-image-path))
         (image-static (copy-image image-target)))
    (declare (type 8-bit-rgba-image image-target))
    (declare (type 8-bit-rgba-image image-static))
    (funcall filter image-target image-static)
    (write-png-file output-image-path image-target)))

(defun rgb+ (value factor)
  (min (+ value factor) 255))

(defun rgb- (value factor)
  (max (- value factor) 0))

(defun rgb/ (value factor)
  (nth-value 0 (round (/ value factor))))

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

(defun random-noise (value factor)
  (rgb- (rgb+ value (random factor)) (random factor)))

(def-iterative-filter filter-noise (r-factor g-factor b-factor)
  (let ((new-r (random-noise r r-factor))
         (new-g (random-noise g g-factor))
         (new-b (random-noise b b-factor)))
    (values new-r new-g new-b a)))

(def-iterative-filter filter-rgb-rshift ()
  (values b r g a))

(def-iterative-filter filter-rgb-lshift ()
  (values g b r a))

(def-iterative-filter filter-sepia (factor)
  (let ((n (nth-value 0 (round (/ (+ r g b) 3)))))
    (values (rgb+ n factor) (rgb+ n factor) n a)))
