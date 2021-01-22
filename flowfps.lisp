(in-package #:flowfps)
(declaim (optimize (speed 3) (debug 1) (safety 0)))

(defun get-png-filepath (i)
  (format nil "images_output/~d.png" i))

(declaim (ftype (function ((unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 8)) channel-calculate-step))
(declaim (inline channel-calculate-step))
(defun channel-calculate-step (a b frame framecount)
  (nth-value 0 (truncate (* frame (/ (abs (- a b)) (+ 1 framecount))))))

(declaim (ftype (function ((unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 8)) channel-apply-step))
(declaim (inline channel-apply-step))
(defun channel-apply-step (a b frame framecount)
  (let ((channel-step (channel-calculate-step a b frame framecount)))
    (if (> a b)
      (- a channel-step)
      (+ a channel-step))))

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
             (blue-c (channel-apply-step blue-a blue-b frame framecount)))
        (setf (pixel img-target y x)
          (values red-c green-c blue-c))))))

(defmacro image-iterate (image y x &rest body)
  `(with-image-bounds (height width) ,image
     (loop for ,y below height do
       (loop for ,x below width do
         ,@body))))

(defun write-intermediate-png-files (img-a img-b img-counter framecount)
  (let ((img-target (copy-image img-a)))
    (iterate:iter (iterate:for frame from 1 to framecount)
      (image-iterate img-a y x
        (pixel-set-intermediate img-a img-b img-target y x frame framecount))
      (write-png-file (get-png-filepath (+ frame img-counter)) img-target))))

(defun blend-images (img-a-path img-b-path img-counter framecount)
  (let ((img-a (read-png-file img-a-path))
         (img-b (read-png-file img-b-path)))
    (write-png-file (get-png-filepath img-counter) img-a)
    (write-intermediate-png-files img-a img-b img-counter framecount)))

(defun blend-directory (dir-path framecount)
  (let* ((img-counter 1))
    (loop for pair in (loop for (a b) on (cl-fad:list-directory dir-path) collect (list a b)) do
      (let* ((img-a-path (nth 0 pair))
              (img-b-path (nth 1 pair)))
        (if img-b-path
          (progn
            (print (format nil "Blending images ~A + ~A" img-a-path img-b-path))
            (print (format nil "img-counter=~d" img-counter))
            (blend-images img-a-path img-b-path img-counter framecount)
            (setf img-counter (+ img-counter framecount 1))))))))
