(in-package :viv)

;; Here, we define the functions that allow us to convert between
;;  an internal representation of concrete syntax trees, and
;;  a langauge-level representation (as a viv-value)

(declaim (ftype (function (t) viv-value) lisp->viv))  
(defun lisp->viv (val)
  (typecase val
    (number (make-instance 'viv-num :num val))
    (string (make-instance 'viv-string :str val))
    (keyword (make-instance 'viv-symbol :symbol val))
    (viv-value val)
    (t (error (format nil "Don't know how to convert value to viv: ~A~%" val)))))


(declaim (ftype (function (viv-value) t) viv->lisp))
(defun viv->lisp (val)
  (typecase val
    (viv-symbol (m-symbol val))
    (viv-string (str val))
    (viv-num (num val))
    (t (error "Cannot convert viv to lisp: "))))


(declaim (ftype (function (concrete) viv-value) concrete->val)) 
(defun concrete->val (cst)
  (typecase cst
    (concrete-node
     (make-instance 'viv-ival
                    :name :node
                    :vals
                    (list
                     (make-instance 'viv-ival
                                    :name (node-type cst)
                                    :vals nil)
                     (list->val (contents cst) #'concrete->val))))
    (concrete-atom
     (lisp->viv (contents cst)))
    (t (error (format nil "Unrecognized concrete type: ~A~%" cst)))))

(declaim (ftype (function (viv-value) concrete) val->concrete))
(defun val->concrete (val)
  (typecase val
    (viv-ival
     (if (and (eq (name val) :node) (= 2 (length (vals val))))
         (let ((type (elt (vals val) 0))
               (nodes (elt (vals val) 1)))
           (make-instance 'concrete-node
                          :type (from-enum type)
                          :contents (val->list #'val->concrete nodes)))
         (make-instance 'concrete-atom :contents val)))
    (viv-symbol
     (make-instance 'concrete-atom :contents (m-symbol val)))
    (t (make-instance 'concrete-atom :contents val))))

(declaim (ftype (function (list function) viv-ival) list->val)) 
(defun list->val (list f)
  (cond
    ((consp list)
     (make-instance 'viv-ival
                    :name :cons
                    :vals (list (funcall f (car list))
                                (list->val (cdr list) f))))
     (t (make-instance 'viv-ival
                       :name :nil
                       :vals nil))))

(declaim (ftype (function (function viv-ival) list) val->list)) 
(defun val->list (f val)
  (cond
    ((eq (name val) :cons)
      (assert (= 2 (length (vals val))))
      (cons (funcall f (elt (vals val) 0)) (val->list f (elt (vals val) 1))))
    ((eq (name val) :nil) nil)
    (t (error (format nil "Expecting viv list (cons/nil), got, ~A" val)))))


(declaim (ftype (function (viv-ival) keyword))) 
(defun from-enum (val)
  (assert (= (length (vals val)) 0))
  (name val))
