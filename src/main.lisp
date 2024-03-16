

(defun run-line ()
  (let* ((concrete (parse *standard-input*)))
    (format t "Concrete: ~A~%" concrete)
    (eval-concrete concrete (default-environment))))


(defun default-environment ()
  (let ((env (env:empty)))
    (env:insert-inplace :|if| (mk-former :if) env)
    (env:insert-inplace :|fn| (mk-former :function) env)

    (env:insert-inplace :|,| (mk-former :destructor) env)
    (env:insert-inplace :|object| (mk-former :corecursor) env)

    (env:insert-inplace :|:| (mk-former :constructor) env)
    (env:insert-inplace :|match| (mk-former :recursor) env)

    (env:insert-inplace :|/| (mk-former :projector) env)
    (env:insert-inplace :|struct| (mk-former :structure) env)

    (env:insert-inplace :|shift| (mk-former :shift) env)
    (env:insert-inplace :|reset| (mk-former :reset) env)

    (env:insert-inplace :|+| *builtin-plus* env)
    (env:insert-inplace :|-| *builtin-minus* env)
    (env:insert-inplace :|÷| *builtin-divide* env)
    (env:insert-inplace :|*| *builtin-multiply* env)
    
    env))
