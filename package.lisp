;;;; package.lisp

(defpackage #:flowfps
  (:use #:cl #:opticl #:iterate)
  (:export :blend :channel-apply-step :channel-calculate-step :pixel-set-intermediate :pixel-bind))
