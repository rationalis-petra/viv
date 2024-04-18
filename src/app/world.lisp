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
         (base-package (viv-base:make-base))

         (user-modules (make-hash-table))
         (user-package (make-instance 'viv-package
                                      :name "viv-user"
                                      :prelude '((:|base| :|core| :|lang|)
                                                 (:|base| :|core| :|reflect|)
                                                 (:|base| :|core| :|num|)

                                                 (:|base| :|system|))
                                      :dependencies '(:|base|)
                                      :modules user-modules))
         (user-module (make-instance 'viv-module
                                     :name "viv-user"
                                     :fields (make-hash-table))))

    (setf (packages world) packages)

    (setf (gethash :|base| packages) base-package)
    (setf (gethash :|viv-user| packages) user-package)

    (setf (gethash :|viv-user| user-modules) user-module)))





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

