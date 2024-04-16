(in-package :viv-base)

(defun make-data-module ()
  (let* ((data-entries (make-hash-table))
        (data-module (make-instance 'viv:viv-module
                                    :name "data"
                                    :fields data-entries)))

    (setf (gethash :|fixed| data-entries)  (make-fixed-module))

    data-module))
