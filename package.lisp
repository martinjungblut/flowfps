;;;; package.lisp

(defpackage #:flowfps
  (:use #:cl #:cl-fad #:opticl #:iterate)
  (:export
    :blend-directory
    :blend-images
    :channel-apply-step
    :channel-calculate-step
    :pixel-bind
    :pixel-set-intermediate))
