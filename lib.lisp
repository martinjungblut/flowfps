(in-package :flowfps)
(declaim (optimize (speed 3) (debug 1) (safety 1)))

(defun rgb+ (value factor)
  (min (+ value factor) 255))

(defun rgb- (value factor)
  (max (- value factor) 0))

(defmacro image-iterate (image y x &rest body)
  `(with-image-bounds (height width) ,image
     (loop for ,y below height do
       (loop for ,x below width do
         ,@body))))

(defmacro pixel-bind (image y x r g b a &rest body)
  `(multiple-value-bind (,r ,g ,b ,a) (pixel ,image ,y ,x)
     (declare (type (unsigned-byte 8) ,r ,g ,b ,a))
     ,@body))

(defun apply-single-image-rgba-filter (image callback)
  (declare (type 8-bit-rgba-image image))
  (image-iterate image y x
    (pixel-bind image y x r g b a
      (setf (pixel image y x) (funcall callback r g b a)))))

(defun open-and-apply (input-image-path output-image-path callback)
  (let ((image (read-png-file input-image-path)))
    (apply-single-image-rgba-filter image callback)
    (write-png-file output-image-path image)))

(defun filter-greyscale-average ()
  (lambda (r g b a)
    (multiple-value-bind (n _) (round (/ (+ r g b) 3))
      (values n n n a))))

(defun filter-darker (factor)
  (lambda (r g b a)
    (let* ((new-r (rgb- r factor))
            (new-g (rgb- g factor))
            (new-b (rgb- b factor)))
      (values new-r new-g new-b a))))

(defun filter-experimental ()
  (lambda (r g b a)
    (values g b r a)))
