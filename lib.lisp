(in-package :flowfps)
(declaim (optimize (speed 3) (debug 1) (safety 1)))

(defun open-apply-write (input-image-path output-image-path filter)
  (let* ((image-target (read-png-file input-image-path))
         (image-static (copy-image image-target)))
    (declare (type 8-bit-rgba-image image-target))
    (declare (type 8-bit-rgba-image image-static))
    (funcall filter image-target image-static)
    (write-png-file output-image-path image-target)))

(defmacro image-iterate (image y x &rest body)
  `(with-image-bounds (height width) ,image
     (loop for ,y below height do
       (loop for ,x below width do
         ,@body))))

(defmacro pixel-bind (image y x r g b a &rest body)
  `(multiple-value-bind (,r ,g ,b ,a) (pixel ,image ,y ,x)
     (declare (type (unsigned-byte 8) ,r ,g ,b ,a))
     ,@body))

(defun rgb+ (value factor)
  (min (+ value factor) 255))

(defun rgb- (value factor)
  (max (- value factor) 0))

(defun rgb/ (value factor)
  (nth-value 0 (round (/ value factor))))

(defun rgb-fit (value)
  (cond
    ((< value 0) 0)
    ((> value 255) 255)
    (t (nth-value 0 (round value)))))

(defun rgb-noise (value factor)
  (rgb- (rgb+ value (random factor)) (random factor)))
