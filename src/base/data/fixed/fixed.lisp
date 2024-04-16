(in-package :viv-base)

;; Stream module interface
;; Streams are a covalue
;; fn (v f s) object stream (v f s)
;;   (.head (stream val _ _) → val)
;;   (.tail (stream _ f s) → ![stream (f s)])




(defun make-stream-module ()
  (let* ((stream-entries (make-hash-table))
        (stream-module (make-instance 'viv:viv-module
                                    :name "stream"
                                    :fields stream-entries)))

    (setf (gethash :|stream| stream-entries)  (make-stream-module))
    (setf (gethash :|| stream-entries)  (make-stream-module))
    (setf (gethash :|stream| stream-entries)  (make-stream-module))

    stream-module))

