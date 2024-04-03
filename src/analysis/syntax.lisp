(in-package :viv)

(declaim (ftype (function (sy-pattern) list) pattern-vars)) 
(defun pattern-vars (pattern)
  (typecase pattern
    (pattern-any (list (var pattern)))
    (pattern-ival
     (apply #'append (mapcar #'pattern-vars (subpatterns pattern))))))
