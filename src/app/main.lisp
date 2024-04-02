(in-package :viv)


(defun run-conc-line-sync ()
  "Synchronously read a line from *stadnard-input* and evaluate it"
  (let* ((concrete (parse *standard-input*)))
    (format t "Concrete: ~A~%" concrete)
    (eval-concrete concrete (default-dynamic-env *prime-world*))))

(defun run-line-sync ()
  (let* ((concrete (parse *standard-input*))
         (abstract (to-abstract (default-macro-env *prime-world*) concrete)))
    (run (eval-term (default-dynamic-env *prime-world*) abstract))))

(defun abs-line-sync ()
  (let* ((concrete (parse *standard-input*)))
    (to-abstract (default-macro-env *prime-world*) concrete)))

(defun conc-line-sync () (parse *standard-input*))
