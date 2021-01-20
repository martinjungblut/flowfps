(in-package #:flowfps)
(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defun get-png-filepath (i)
  (format nil "~d.png" i))

(defmacro image-iterate (image y x &rest body)
  `(with-image-bounds (height width) ,image
     (loop for ,y below height do
       (loop for ,x below width do
             ,@body))))

(declaim (ftype (function ((unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 8)) channel-apply-step))
(declaim (inline channel-apply-step))
(defun channel-apply-step (a b frame framecount)
  (let ((channel-step (channel-calculate-step a b frame framecount)))
	(let ((result (funcall (if (> a b) '- '+) a channel-step)))
	  result)))

(declaim (ftype (function ((unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 8)) channel-calculate-step))
(declaim (inline channel-calculate-step))
(defun channel-calculate-step (a b frame framecount)
  (let ((result (truncate (* frame (/ (abs (- a b)) (+ 1 framecount))))))
	result))

(defmacro pixel-bind (image y x r g b &rest body)
  `(multiple-value-bind (,r ,g ,b) (pixel ,image ,y ,x)
     (declare (type (unsigned-byte 8) ,r ,g ,b))
     ,@body))

(defun pixel-set-intermediate (img-a img-b img-target y x frame framecount)
  (declare (type 8-bit-rgb-image img-a img-b img-target))
  (declare (type (unsigned-byte 32) y x frame framecount))
  (pixel-bind img-a y x red-a green-a blue-a
			  (pixel-bind img-b y x red-b green-b blue-b
						  (let ((red-c (channel-apply-step red-a red-b frame framecount))
								(green-c (channel-apply-step green-a green-b frame framecount))
								(blue-c (channel-apply-step blue-a blue-b frame framecount))
								(result 1))
							(declare (type (unsigned-byte 1) result))
							(setf (pixel img-target y x)
								  (values red-c green-c blue-c))
							result))))

(defun write-intermediate-png-files (img-a img-b framecount)
  (let ((img-target (copy-image img-a)))
    (iterate:iter (iterate:for frame from 1 to framecount)
      (image-iterate img-a y x
					 (pixel-set-intermediate img-a img-b img-target y x frame framecount))
      (write-png-file (get-png-filepath (+ 1 frame)) img-target))))

(defun blend (img-a-path img-b-path framecount)
  (let ((img-a (read-png-file img-a-path))
        (img-b (read-png-file img-b-path)))
    (write-png-file (get-png-filepath 1) img-a)
    (write-png-file (get-png-filepath (+ 2 framecount)) img-b)
    (write-intermediate-png-files img-a img-b framecount)))
