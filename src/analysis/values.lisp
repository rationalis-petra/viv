(in-package :viv)

;; Predicates on viv-values

(declaim (ftype (function (viv-value &optional keyword) boolean) enum-p))
(defun enum-p (value &optional symbol)
  (and (typep value 'viv-ival)
       (= 0 (length (vals value)))
       (if symbol 
           (eq symbol (name value))
           t)))

