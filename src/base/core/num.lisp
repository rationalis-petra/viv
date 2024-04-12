(in-package :viv-base)

;; Arithmetic

(defun make-num (n) (make-instance 'viv:viv-num :num n))

(defvar *builtin-plus*
  (make-instance 'primop
                 :arity 2
                 :fun (lambda (x y) (pure (make-num (+ (viv:num x) (viv:num y)))))))

(defvar *builtin-minus*
  (make-instance 'primop
                 :arity 2
                 :fun (lambda (x y) (pure (make-num (- (viv:num x) (viv:num y)))))))

(defvar *builtin-multiply*
  (make-instance 'primop
                 :arity 2
                 :fun (lambda (x y) (pure (make-num (* (viv:num x) (viv:num y)))))))

(defvar *builtin-divide*
  (make-instance 'primop
                 :arity 2
                 :fun (lambda (x y) (pure (make-num (/ (viv:num x) (viv:num y)))))))


(defun make-num-module ()
  (let* ((num-entries (make-hash-table))
         (num-module (make-instance 'viv:viv-module
                                    :name "num"
                                    :fields num-entries)))

    (setf (gethash :|+| num-entries) *builtin-plus*)
    (setf (gethash :|-| num-entries) *builtin-minus*)
    (setf (gethash :|÷| num-entries) *builtin-divide*)
    (setf (gethash :|*| num-entries) *builtin-multiply*)
    num-module))
