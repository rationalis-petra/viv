(in-package :viv-base)


(defun make-core-module ()
  (let* ((core-entries (make-hash-table))
        (core-module (make-instance 'viv:viv-module
                                    :name "core"
                                    :fields core-entries)))

    (setf (gethash :|lang| core-entries) (make-lang-module))
    (setf (gethash :|num| core-entries) (make-num-module))
    (setf (gethash :|reflect| core-entries) (make-reflect-module))

    core-module))
