(in-package :viv-base)

;; introspection
(defun viv-make-package ()
  (pure (make-instance 'viv-package)))

(defun set-package (name package)
  (let* ((symbol (viv->lisp name)))
    (assert (typep symbol 'keyword))
    (pure (make-instance 'viv-package))))

(defvar *builtin-get-prelude*
  (make-instance
   'primop
   :arity 1
   :fun (lambda (package)
          (pure (lisp->viv (prelude package))))))

(defvar *builtin-set-prelude*
  (make-instance
   'primop
   :arity 2
   :fun (lambda (package prelude)
          (let ((new-prelude (viv->lisp prelude)))
            (setf (prelude package) new-prelude)
            (pure +null-values+)))))

(defvar *builtin-make-module*
  (make-instance
   'primop
   :arity 0
   :fun (lambda () (pure (make-instance 'viv:viv-module)))))

(defvar *builtin-get-exports*
  (make-instance
   'primop
   :arity 1
   :fun (lambda (module)
          (pure (lisp->viv (exports module))))))

(defvar *builtin-set-exports*
  (make-instance
   'primop
   :arity 2
   :fun (lambda (exports module)
          (setf (exports module) (viv->lisp exports))
          (pure +null-value+))))

;; insert module @ path
(defvar *builtin-insert-module*
  (make-instance
   'primop
   :arity 3
   :fun (lambda (path child parent)
          (declare (ignore path child parent))
          (error "set-module not implemented!"))))

;; val into module
(defvar *bulitin-module-insert*
  (make-instance 
   'primop
   :arity 3
   :fun (lambda (name val module)
          (assert (typep module 'viv:viv-module))
          (let ((symbol (viv->lisp name)))
            (assert (typep symbol 'keyword))
            (setf (gethash symbol module) val)))))

;; remove val from module
(defun bulitin-module-remove (name module)
  (assert (typep module 'viv:viv-module))
  (let ((symbol (viv->lisp name)))
    (assert (typep symbol 'keyword))
    (remhash symbol module)))

(defun builtin-repr (val)
  (pure
   (let ((viv:*print-repr* t))
     (make-instance
      'viv:viv-string :str
      (with-output-to-string (out)
        (print-object val out))))))


(defun make-reflect-module ()
  (let* ((reflect-entries (make-hash-table))
         (reflect-module (make-instance 'viv:viv-module
                                        :name "reflect"
                                        :fields reflect-entries)))


    ;; Introspection module
    ;; Allows
    ;; • Constructing abstract syntax (with de-bruijn indices)
    ;; • Constructing concrete syntax
    ;; • Manipulating modules and packages
    ;; • Manipulating environments??

    ;; Packages

    (setf (gethash :|make-package| reflect-entries)  (builtin #'viv-make-package 0))
    (setf (gethash :|set-package| reflect-entries)   (builtin #'set-package 2))
    (setf (gethash :|get-prelude| reflect-entries)   *builtin-get-prelude*)
    (setf (gethash :|set-prelude| reflect-entries)   *builtin-set-prelude*)

    ;; Modules
    (setf (gethash :|make-module| reflect-entries) *builtin-make-module*)
    (setf (gethash :|get-exports| reflect-entries)   *builtin-get-exports*)
    ;(setf (gethash :|set-exports| reflect-entries)   *builtin-set-package*)
    (setf (gethash :|module-insert| reflect-entries) *bulitin-module-insert*)
    (setf (gethash :|module-remove| reflect-entries) #'bulitin-module-remove)


    (setf (gethash :|repr| reflect-entries)  (builtin #'builtin-repr 1))

    reflect-module))
  
