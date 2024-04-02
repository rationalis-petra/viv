(in-package :viv)


;; The state of an instance
(defclass world ()
  ((packages
    :accessor packages
    :initarg :packages))

  ;; Loaded Resources
  (:documentation "The world state of a viv instance"))

(defmethod initialize-instance ((world world) &key packages &allow-other-keys)
  (let* ((packages (or packages (make-hash-table)))
         (modules (make-hash-table))
         (core-package (make-instance 'viv-package
                                      :name "core"
                                      :exports '(:|lang|)
                                      :prelude '((:|core| :|lang|))
                                      :modules modules))

         (lang-entries (make-hash-table))
         (lang-module (make-instance 'viv-module
                                     :name "lang"
                                     :fields lang-entries))


         (user-modules (make-hash-table))
         (user-package (make-instance 'viv-package
                                      :name "viv-user"
                                      :prelude '((:|core| :|lang|))
                                      :dependencies '(:|core|)
                                      :modules user-modules ))
         (user-module (make-instance 'viv-module
                                     :name "viv-user"
                                     :fields (make-hash-table))))

    (setf (packages world) packages)

    (setf (gethash :|core| packages) core-package)
    (setf (gethash :|viv-user| packages) user-package)

    (setf (gethash :|viv-user| user-modules) user-module)

    (setf (gethash :|lang| modules) lang-module)

    (setf (gethash :|if| lang-entries) (mk-former :if))

    (setf (gethash :|def| lang-entries) (mk-former :define))

    (setf (gethash :|fn| lang-entries)(mk-former :function))
    (setf (gethash :|app| lang-entries) (mk-former :app))

    (setf (gethash :|,| lang-entries) (mk-former :destructor))
    (setf (gethash :|object| lang-entries) (mk-former :corecursor))

    (setf (gethash :|:| lang-entries) (mk-former :constructor))
    (setf (gethash :|match| lang-entries) (mk-former :recursor))

    (setf (gethash :|/| lang-entries) (mk-former :projector))
    (setf (gethash :|struct| lang-entries) (mk-former :structure))

    (setf (gethash :|shift| lang-entries) (mk-former :shift))
    (setf (gethash :|reset| lang-entries) (mk-former :reset))

    (setf (gethash :|+| lang-entries) *builtin-plus*)
    (setf (gethash :|-| lang-entries) *builtin-minus*)
    (setf (gethash :|÷| lang-entries) *builtin-divide*)
    (setf (gethash :|*| lang-entries) *builtin-multiply*)))


;; Messages
;; • Reduce term: A semantically pure operation. Convert to normal form 
;; • Eval term:   Evaluate - run a program, including execution of side effects
;; • Run term:    Evaluate a program and run the resultant monad.


(declaim (ftype (function (t world) null) world-process-message))
(defun world-process-message (message world)
  "Given a specific WORLD, perform the action(s) requested by MESSAGE."
  (case (first message)
    (:eval
     (eval-concrete (elt message 1)
                    (default-dynamic (packages world))))

    (:reduce (print "reduce not implemented"))

    (:run (print "run not implemented"))

    (t (format t "Unrecognised Message Header: ~A~%" message)))
  nil)


(declaim (ftype (function (world) env:dynamic-env) default-dynamic-env))
(defun default-dynamic-env (world)
  (make-instance 'env:dynamic-env :world world :package :|viv-user| :module '(:|viv-user|)))

(declaim (ftype (function (world) env:macro-env) default-macro-env))
(defun default-macro-env (world)
  (make-instance 'env:macro-env :world world :package :|viv-user| :module '(:|viv-user|)))

(declaim (type world *prime-world*))
(defvar *prime-world* (make-instance 'world))

(defun setup-world ()
  (setf *prime-world* (make-instance 'world)))

(defun add-def (var val world package-name module-name)
  (let ((module (env:lookup-path (append (list package-name) module-name) world)))
    (setf (gethash var (fields module)) val) ))

