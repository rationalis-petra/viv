(in-package :viv-base)

(define-condition builtin-exit (error) ())

(defun builtin-exit ()
  (error 'builtin-exit))

(defun make-system-module ()
  (let* ((system-entries (make-hash-table))
         (system-module (make-instance 'viv:viv-module
                                       :name "system"
                                       :fields system-entries)))

    (setf (gethash :|console| system-entries)  (make-console-module))
    ;; (setf (gethash :|net| system-entries)      (make-net-module))
    ;; (setf (gethash :|file| system-entries)     (make-file-module))
    ;; (setf (gethash :|time| system-entries)     (make-time-module))
    ;; (setf (gethash :|media| system-entries)    (make-media-module))
    ;; (setf (gethash :|interface| system-entries)   (make-interface-module))


    (setf (gethash :|exit| system-entries)  (builtin #'builtin-exit 0))

    system-module))
