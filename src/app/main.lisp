(in-package :viv)


(defclass interactive-session ()
  ((package
    :initarg :package)
   (module
    :initarg :module)
   (world
    :initarg :world)))

(defvar *current-session* (make-instance 'interactive-session
                                         :world *prime-world*
                                         :package :|viv-user|
                                         :module '(:|viv-user|)))

(defun run-conc-line()
  "Synchronously read a line from *stadnard-input* and evaluate it"
  (let* ((*current-world* *prime-world*)
         (concrete (parse *standard-input*)))
    (format t "Concrete: ~A~%" concrete)
    (eval-concrete concrete (default-dynamic-env *prime-world*))))

(defun repl ()
  (handler-case
      (let* ((*current-world* *prime-world*))
        (loop
          (format t "~%> ")
          (fd:->> (parse *standard-input*)
                  (to-abstract (default-macro-env *prime-world*))
                  (eval-term (default-dynamic-env *prime-world*))
                  (run)
                  (print))))
    (viv-base:builtin-exit (exit)
      (declare (ignore exit))
      "exited")))

(defun run-line ()
  (let* ((*current-world* *prime-world*)
         (concrete (parse *standard-input*))
         (abstract (to-abstract (default-macro-env *prime-world*) concrete)))
    (run (eval-term (default-dynamic-env *prime-world*) abstract))))

(defun abs-line ()
  (let* ((*current-world* *prime-world*)
         (concrete (parse *standard-input*)))
    (to-abstract (default-macro-env *prime-world*) concrete)))

(defun conc-line ()
  (parse *standard-input*))

