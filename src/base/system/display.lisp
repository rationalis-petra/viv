(in-package :viv-base)



(defun make-window ()
  (pure (sdl2:create-window :title "viv" :w 1280 :h 720)))

(defun make-display-module ()
  (let* ((display-entries (make-hash-table))
         (display-module (make-instance 'viv:viv-module
                                        :name "display"
                                        :fields display-entries)))
    ;; (setf (gethash :|stdout| display-entries) ())
    ;; (setf (gethash :|stdin| display-entries) ())

    (setf (gethash :|make-window| display-entries)
          (builtin #'make-window 0))

    display-module))
