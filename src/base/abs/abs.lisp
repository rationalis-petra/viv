(in-package :viv-base)


(defun make-abs-module ()
  (let* ((abs-entries (make-hash-table))
        (abs-module (make-instance 'viv:viv-module
                                    :name "abs"
                                    :fields abs-entries)))

    (setf (gethash :|functor| abs-entries) (make-lang-module))
    (setf (gethash :|num| abs-entries) (make-num-module))
    (setf (gethash :|reflect| abs-entries) (make-reflect-module))

    abs-module))

