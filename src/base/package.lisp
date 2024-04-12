(defpackage :viv-base
  (:use :viv :foundation :cl)
  (:export
   :make-base
   :builtin-exit))

(in-package :viv-base)

(defconstant +null-values+
  (if (boundp '+null-values+)
      +null-values+
      (make-instance 'viv:viv-values :values nil)))

(defun builtin (fn n) (make-instance 'viv:primop :fun fn :arity n))

(defun mk-former (former) (make-instance 'viv:viv-former :former former))
