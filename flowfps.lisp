;;;; flowfps.lisp

(in-package #:flowfps)

(defun average (&rest args)
  (when args
	(truncate
	 (/ (apply #'+ args) (length args)))))

(defun blend (img-a-path img-b-path output-path)
  (let ((img-a (read-jpeg-file img-a-path))
		(img-b (read-jpeg-file img-b-path)))
	(typecase img-a
	  (8-bit-rgb-image
	   (locally (declare (type 8-bit-rgb-image img-a))
		 (typecase img-b
		   (8-bit-rgb-image
			(locally (declare (type 8-bit-rgb-image img-b))
			  (with-image-bounds (height width) img-a
				(loop for y below height do
				  (loop for x below width do
					(multiple-value-bind (r-a g-a b-a) (pixel img-a y x)
					  (declare (type (unsigned-byte 8) r-a g-a b-a))
					  (multiple-value-bind (r-b g-b b-b) (pixel img-b y x)
						(declare (type (unsigned-byte 8) r-b g-b b-b))
						(setf (pixel img-a y x)
							  (values (average r-a r-b)
									  (average g-a g-b)
									  (average b-a b-b))))))))))))))
	(write-jpeg-file output-path img-a)))
