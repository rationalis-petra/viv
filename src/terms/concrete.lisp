(in-package :viv)


(defclass concrete ()
  ()
  (:documentation "Concrete Syntax Root"))

;; Types:
;; • expr, stack, query

(defclass concrete-node (concrete)
  ((type
    :accessor node-type
    :initarg :type)
   (contents
    :accessor contents
    :initarg :contents))
  (:documentation "Concrete Syntax"))

(defclass concrete-atom (concrete)
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
    (labels ((fgo (terms)
               (cond
                 ((= 1 (length terms))
                  (print-object (car terms) stream)
                  (write-char end stream))
                 ((null terms) (write-char end stream))
                 (t (print-object (car terms) stream)
                    (write-char #\Space stream)
                    (fgo (cdr terms))))))
      (fgo (contents node)))))


;; Util
(defun get-symbol (term)
  (assert (typep (contents term) 'keyword))
  (contents term))

(defun get-symval-pair (terms)
  (assert (typep terms 'concrete-node))
  (assert (= (length (contents terms)) 2))
  (unless (typep (contents (car (contents terms))) 'keyword)
    (error "Term list element must contain symbol as first pair"))
  (list (contents (elt (contents terms) 0)) (elt (contents terms) 1)))

(defun get-symlist (expr)
  (unless (typep expr 'concrete-node)
    (error "Symbol list require to be list"))
  (loop for elt in (contents expr)
        do (unless (typep (contents elt) 'keyword)
          (error "Symbollist required to contain only symbols"))
        collect (contents elt)))
