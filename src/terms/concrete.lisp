

(defclass concrete ()
  ()
  (:documentation "Concrete Syntax Root"))

;; Types:
;; • expr, stack, query

(defclass concrete-node ()
  ((type
    :accessor node-type
    :initarg :type)
   (contents
    :accessor contents
    :initarg :contents))
  (:documentation "Concrete Syntax"))

(defclass concrete-atom ()
  ((contents
    :accessor contents
    :initarg :contents))
  (:documentation "Concrete Syntax"))


(defmethod print-object ((atom concrete-atom) stream)
  (print-object (contents atom) stream))

(defmethod print-object ((node concrete-node) stream)
  (destructuring-bind (begin end)
      (case (node-type node)
        (:expr (list #\( #\)))
        (:stack (list #\[ #\]))
        (:query (list #\{ #\}))
        (t (list #\! #\!)))
    (write-char begin stream)
    (loop for item in (contents node)
          do (progn
               (print-object item stream)
               (write-char #\Space stream)))
    (write-char end stream)))
