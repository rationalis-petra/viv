(in-package :viv-base)

;; Arithmetic

(defun make-num (n) (make-instance 'viv:viv-num :num n))

(defun builtin-plus (x y)
  (pure (make-num (+ (viv:num x) (viv:num y)))))

(defun builtin-minus (x y)
  (pure (make-num (- (viv:num x) (viv:num y)))))

(defun builtin-multiply (x y)
  (pure (make-num (* (viv:num x) (viv:num y)))))

(defun builtin-divide (x y)
  (pure (make-num (/ (viv:num x) (viv:num y)))))


(defun make-num-module ()
  (let* ((num-entries (make-hash-table))
         (num-module (make-instance 'viv:viv-module
                                    :name "num"
                                    :fields num-entries)))

    (setf (gethash :|+| num-entries) (builtin #'builtin-plus 2))
    (setf (gethash :|-| num-entries) (builtin #'builtin-minus 2))
    (setf (gethash :|/| num-entries) (builtin #'builtin-divide 2))
    (setf (gethash :|*| num-entries) (builtin #'builtin-multiply 2))
    num-module))
