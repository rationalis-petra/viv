(in-package :viv)


(defclass viv-type (viv-value)
  ((viv-type)
   (viv-value)))

(defclass any-type (viv-type) ())
(defvar *any-type* (make-instance 'any-type))

(defclass unique-type (viv-type) ())

(defclass procedure-type (viv-type)
  ())

(defclass variant-type (viv-type)
  ())

(defclass structure-type (viv-type)
  ((fields
    :accessor fields
    :initarg :fields)))

(defclass primitive-type (viv-type) ()
  ((:name
    :accessor name
    :initarg :name)))

(defun make-primitive-type (symbol)
  (make-instance 'primitive-type :name symbol))
