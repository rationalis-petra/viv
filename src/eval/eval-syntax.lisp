

;;(declaim (ftype eval (function (abstract env:environment))))
(defgeneric eval-term (term env)
  (:documentation "Evaluate term in env"))

(defmethod eval-term ((term viv-syntax) env))


