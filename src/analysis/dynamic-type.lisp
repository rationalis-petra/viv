(in-package :viv)

;; -----------------------------------------------------------------------------
;; Given an object, get its' dispatch type
;; -----------------------------------------------------------------------------

(declaim (ftype (function (viv-value) viv-type) runtime-type))
(defgeneric dispatch-type (value)
  (:documentation "Given a viv-value, the type to perform implicit dispatch on"))

(defmethod dispatch-type :around ((value viv-value))
  (if (slot-boundp value 'attached-type)
      (attached-type value)
      (call-next-method)))

(defmethod dispatch-type ((viv-value t)) *any-type*)

(defmethod dispatch-type (viv-num)
  (make-primitive-type :number))

(defmethod dispatch-type (viv-string)
  (make-primitive-type :string))

(defmethod dispatch-type (viv-symbol)
  (make-primitive-type :symbol))

(defmethod dispatch-type (viv-struct)
  (make-instance
   'struct-type
   :fields (maptable (lambda (val) *any-type*))))

