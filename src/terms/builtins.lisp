(in-package :viv)

(defvar *builtin-plus*
  (make-instance 'primop
                 :arity 2
                 :fun (lambda (x y) (pure (+ x y)))))

(defvar *builtin-minus*
  (make-instance 'primop
                 :arity 2
                 :fun (lambda (x y) (pure (- x y)))))

(defvar *builtin-multiply*
  (make-instance 'primop
                 :arity 2
                 :fun (lambda (x y) (pure (* x y)))))

(defvar *builtin-divide*
  (make-instance 'primop
                 :arity 2
                 :fun (lambda (x y) (pure (/ x y)))))
