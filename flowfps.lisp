;;;; flowfps.lisp

(in-package #:flowfps)

(defun average (&rest args)
  (when args
    (truncate
      (/ (apply #'+ args) (length args)))))

(defun get-jpeg-filepath (i)
  (format nil "~d.jpeg" i))

(defmacro image-cast (symbol &rest body)
  `(typecase ,symbol
     (8-bit-rgb-image
      (locally (declare (type 8-bit-rgb-image ,symbol))
        ,@body))))

(defmacro image-iterate (image y x &rest body)
  `(with-image-bounds (height width) ,image
     (loop for ,y below height do
       (loop for ,x below width do
         ,@body))))

(defun channel-apply-step (a b frame framecount)
  (let ((channel-step (channel-calculate-step a b frame framecount)))
    (min 255 (max 0 (funcall (if (> a b) '- '+) a channel-step)))))

(defun channel-calculate-step (a b frame framecount)
  (truncate
   (* frame
      (/ (abs (- a b)) (+ 1 framecount)))))

(defmacro pixel-bind (image y x r g b &rest body)
  `(multiple-value-bind (,r ,g ,b) (pixel ,image ,y ,x)
     (declare (type (unsigned-byte 8) ,r ,g ,b))
     ,@body))

(defun pixel-set-intermediate (img-a img-b img-target y x frame framecount)
  (pixel-bind img-a y x red-a green-a blue-a
    (pixel-bind img-b y x red-b green-b blue-b
      (let ((red-c (channel-apply-step red-a red-b frame framecount))
            (green-c (channel-apply-step green-a green-b frame framecount))
            (blue-c (channel-apply-step blue-a blue-b frame framecount)))
        (setf (pixel img-target y x)
              (values red-c green-c blue-c))))))

(defun write-intermediate-jpeg-files (img-a img-b framecount)
  (iterate:iter (iterate:for frame from 1 to framecount)
    (let ((img-target (copy-image img-a)))
      (image-cast img-target
        (image-iterate img-a y x
            (pixel-set-intermediate img-a img-b img-target y x frame framecount))
        (write-jpeg-file (get-jpeg-filepath (+ 1 frame)) img-target)))))

(defun blend (img-a-path img-b-path framecount)
  (let ((img-a (read-jpeg-file img-a-path))
        (img-b (read-jpeg-file img-b-path)))
    (write-jpeg-file (get-jpeg-filepath 1) img-a)
    (write-jpeg-file (get-jpeg-filepath (+ 2 framecount)) img-b)
    (image-cast img-a
        (image-cast img-b
            (write-intermediate-jpeg-files img-a img-b framecount)))))
