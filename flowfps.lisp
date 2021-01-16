;;;; flowfps.lisp

(in-package #:flowfps)

(defun main ()
  (let ((img (read-jpeg-file "harry.jpeg")))
	(typecase img
	  (8-bit-rgb-image
	   (locally
		   (declare (type 8-bit-rgb-image img))
		 (with-image-bounds (height width)
							img
		   (time
			(loop for i below height
				  do (loop for j below width
						   do
							  (multiple-value-bind (r g b)
								  (pixel img i j)
								(declare (type (unsigned-byte 8) r g b))
								(setf (pixel img i j)
									  (values (- 255 r) g b))))))))))
	(write-jpeg-file "harry-output.jpeg" img)))
