(in-package :viv)

;; Predicates on viv-values

(declaim (ftype (function (viv-value &optional keyword) boolean) enum-p))
(defun enum-p (value &optional symbol)
  (and (typep value 'viv-ival)
       (= 0 (length (vals value)))
       (if symbol 
           (eq symbol (name value))
           t)))


(defgeneric logic-eql (l r))

(defmethod logic-eql ((l t) (r t)) (equal l r))

(defmethod logic-eql ((l viv-lvar) (r viv-lvar))
  (= (uid l) (uid r)))

(defmethod logic-eql ((l viv-symbol) (r viv-symbol))
  (eq (m-symbol l) (m-symbol r)))

(defmethod logic-eql ((l viv-num) (r viv-num))
  (= (num l) (num r)))

(defmethod logic-eql ((l viv-ival) (r viv-ival))
  (and (eq (name l)
             (name r))
       (= (length (vals l)) (length (vals r)))
       (every (lambda (l r) (logic-eql l r))
              (vals l)
              (vals r))))

;; (defmethod logic-eql ((l viv-struct) (r viv-struct)))
