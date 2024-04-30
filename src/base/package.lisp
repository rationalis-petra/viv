(defpackage :viv-base
  (:use :viv :foundation :cl :named-readtables)
  (:export
   :make-base
   :builtin-exit))

(in-package :viv-base)

(defconstant +null-values+
  (if (boundp '+null-values+)
      +null-values+
      (make-instance 'viv:viv-values :values nil)))

(defun builtin (fn n) (make-instance 'viv:primop :fun fn :arity n))

(defun mk-macro (v-function) (make-instance 'viv:viv-macro :body v-function))
(defun mk-former (former) (make-instance 'viv:viv-former :former former))

;(defreadtable  )

(sdl2:init :video)
