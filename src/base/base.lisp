(in-package :viv-base)


(defun make-base ()
  (let* ((base-modules (make-hash-table))
         (base-package (make-instance
                        'viv-package
                        :name "base"
                        :exports '(:|core| :|abs| :|system|)
                        :prelude '((:|base| :|core| :|lang|))
                        :modules base-modules)))

    (setf (gethash :|core| base-modules)   (make-core-module))
    (setf (gethash :|abs| base-modules)   (make-abs-module))
    ;(setf (gethash :|data| base-modules)   (make-data-module))
    ;(setf (gethash :|object| base-modules) (make-object-module))
    (setf (gethash :|system| base-modules) (make-system-module))

    base-package))


;(defreadtable )
