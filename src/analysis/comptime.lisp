(in-package :viv)

;; TODO t → viv-value
(declaim (ftype (function (t) boolean) comptime-p)) 
(defun comptime-p (value) (or (typep value 'viv-former)))


