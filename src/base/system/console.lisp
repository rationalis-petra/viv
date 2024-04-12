(in-package :viv-base)

(defun print-ln (object)
  (format t "~A~%" object)
  (pure +null-values+))

(defun make-console-module ()
  (let* ((console-entries (make-hash-table))
         (console-module (make-instance 'viv:viv-module
                                        :name "console"
                                        :fields console-entries)))
    ;; (setf (gethash :|stdout| console-entries) ())
    ;; (setf (gethash :|stdin| console-entries) ())

    (setf (gethash :|print-ln| console-entries)
          (builtin #'print-ln 1))

    console-module))
