(defpackage environment
  (:nicknames :env)
  (:use :cl)
  (:export :env
   :copy
   :empty :lookup
   :insert :insert-inplace))

(in-package :environment)

(defclass env ()
  ((locals
    :accessor locals
    :initarg :locals
    :initform nil)))

(defun empty () (make-instance 'env))

(defun lookup (var env)
  (cdr (assoc var (locals env))))

(defun insert (var val env)
  "Insert a new variable into an environment"
  (make-instance 'env
                 :locals (acons var val (locals env))))

(defun insert-inplace (var val env)
  "Insert a new variable into an environment"
  (setf (locals env) (acons var val (locals env))))

(defun copy (env)
  (make-instance 'env :locals (locals env)))
