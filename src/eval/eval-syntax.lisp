(in-package :viv)

(define-condition bad-sort (error)
  ((expected-sort
    :initarg :expected-sort
    :accessor expected-sort)
   (actual-sort
    :initarg actual-sort
    :accessor actual-sort)
   (value
    :initarg :value
    :accessor value)))

(define-condition bad-arity (error)
  ((expected-arity
    :initarg :expected-arity
    :accessor expected-arity)
   (actual-arity
    :initarg actual-arity
    :accessor actual-arity)
   (value
    :initarg :value
    :accessor value)))


;;(declaim (ftype eval (function (abstract env:environment))))
(defgeneric eval-term (env term)
  (:documentation "Evaluate TERM in ENV. Here, evaluate means to"))

(defmethod eval-term (env (term sy-variable))
  (pure (env:lookup (viv:m-symbol term) env)))

(defmethod eval-term (env (term sy-literal))
  (pure (value term)))

(defmethod eval-term (env (term sy-def))
  (mdo
   (bind val (eval-term env (body term)))
   (progn 
     (add-def (var term) val (env:env-world env) (env:env-package env) (env:env-module env))
     (pure val))))

(defmethod eval-term (env (term sy-function))
  (pure (make-instance 'viv-fun
                       :arity (length (args term))
                       :fun (lambda (&rest args)
                              (eval-term
                               (env:insert-many (mapcar #'cons (args term) args) env)
                               (body term))))))

(defmethod eval-term (env (term sy-apply))
  (mdo
   (bind callee (eval-term env (fun term)))
   (cond
     ((or (typep callee 'primop) (typep callee 'viv-fun))
      (unless (= (length (args term)) (arity callee))
        (error 'bad-arity
               :expected-arity (arity callee)
               :actual-arity (length (args term))
               :value callee))
      (mdo (bind args (mapM (lambda (e) (eval-term env e)) (args term)))
           (apply (fun callee) args)))
     (t (error  'bad-sort :expected-sort '(:fun :inductive))))))

(defmethod eval-term (env (term sy-reset))
  (mreset (eval-term env (body term))))

(defmethod eval-term (env (term sy-shift))
  (mshift (lambda (k) (eval-term (env:insert (var term) (make-instance 'primop :arity 1 :fun k) env) (body term)))))


(defmethod eval-term (env (term sy-structure))
  (let ((map (make-hash-table)))
    (mdo 
     (mapM (lambda (pair) (mdo (bind res (eval-term env (cdr pair)))
                               (pure (setf (gethash (car pair) map) res))))
           (fields term))
     (pure (make-instance 'viv-struct :fields map)))))

(defmethod eval-term (env (term sy-projector))
  (mdo
   (bind struct (eval-term env (value term)))
   (pure (or (gethash (field term) (fields struct))
             (error (format nil "field not found: ~A in ~A~%"
                            (field term)
                            (fields struct)))))))

(defmethod eval-term (env (term sy-constructor))
  (mdo
   (bind vals (mapM (lambda (val) (eval-term env val)) (args term)))
   (pure (make-instance 'viv-ival :name (name term) :vals vals))))

(defun match-pattern (val pattern)
  (typecase pattern
    (pattern-any (list (cons (var pattern) val)))
    (pattern-ival
     (if (and (eq (name pattern) (name val))
              (= (length (subpatterns pattern))
                 (length (vals val))))
         (let ((submatches (mapcar #'match-pattern (vals val) (subpatterns pattern))))
           (if (every (lambda (x) (not (eq x :false))) submatches)
               (apply #'append submatches)
               :false)
         :false)))

    (t (error "bad pattern"))))

(defun try-match (clause args)
  (assert (= (length args) (length (car clause))))
  (let* ((pattern (car clause))
         (binds mapcar #'match-pattern pattern args))
    (when (every (lambda (x) (not (eq x :false))) binds)
      (cons binds (cdr clause)))))

(defun get-match-term (args clauses)
  (or 
   (loop for clause in clauses
         do (let ((res (try-match clause args)))
              (when res (return res))))
   (error "match failed!")))
                          

(defmethod eval-term (env (term sy-recursor))
  (mdo
   (bind vals (mapM (lambda (val) (eval-term env val)) (vals term)))
   (labels ((recur (&rest args)
              (let* ((match-res (get-match args (clauses term)))
                     (new-env (env:insert-many
                               (append (car match-res)
                                       (list (cons (name term)
                                                   (make-instance 'primop
                                                                  :arity (length args)
                                                                  :fun #'recur))))
                               env)))
                (eval-term new-env (cdr match-res)))))
     (apply #'recur vals))))

;; eval-term
;; env:insert-many (append binds (cons name (make-instance 'primop)))
;;
