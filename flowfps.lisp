;;;; flowfps.lisp

(in-package #:flowfps)

(defun average (&rest args)
  (when args
    (truncate
      (/ (apply #'+ args) (length args)))))

(defmacro cast-image (symbol &rest body)
  `(typecase ,symbol
   (8-bit-rgb-image
     (locally (declare (type 8-bit-rgb-image ,symbol))
       ,@body))))

(defmacro bind-pixel (image y x r g b &rest body)
  `(multiple-value-bind (,r ,g ,b) (pixel ,image ,y ,x)
     (declare (type (unsigned-byte 8) ,r ,g ,b))
       ,@body))

(defmacro iterate-on-image (image y x &rest body)
  `(with-image-bounds (height width) ,image
     (loop for ,y below height do
       (loop for ,x below width do
         ,@body))))

(defun blend (img-a-path img-b-path output-path)
  (let ((img-a (read-jpeg-file img-a-path))
        (img-b (read-jpeg-file img-b-path)))
    (cast-image img-a
      (cast-image img-b
        (let ((img-target (copy-image img-a)))
          (cast-image img-target
            (iterate-on-image img-a y x
              (bind-pixel img-a y x r-a g-a b-a
                (bind-pixel img-b y x r-b g-b b-b
                  (setf (pixel img-target y x)
                    (values (average r-a r-b)
                            (average g-a g-b)
                            (average b-a b-b))))))
            (write-jpeg-file output-path img-target)))))))
