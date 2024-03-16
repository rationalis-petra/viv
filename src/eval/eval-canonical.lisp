(define-condition variable-not-found (error)
  ((missing-var
    :initarg :missing-var
    :accessor missing-var)
   (env
    :initarg :env
    :accessor env)))

;; The 'Canonical' evaluation of terms: 
;; • Can be thought of as an operational specification of how concrete syntax
;;   should evaluate
;; • All other evaluators/compilers should match the runtime behaviour of this
;;   algorithm (plus or minus debugging behaviour, symbol/module locking etc.) 

(defun eval-concrete (term env)
  (cond
    ((typep term 'concrete-node)
     (case (node-type term)
       (:stack (eval-stack (reverse (contents term)) (make-instance 'viv-stack) env))
       (:expr  (eval-expr-node (contents term) env))
       (:query (error "query not implemented"))))
    ((typep term 'concrete-atom)
     (eval-expr-atom (contents term) env))))

;; Like eval-concrete, but also runs the monadic result
(defun eval-run-concrete (term env)
  (run (eval-concrete term env)))

;;------------------------------------------------------------------------------
;; Stack Evaluation
;;------------------------------------------------------------------------------


(defun eval-stack (list stack env)
  "Evaluate a list of terms as a stack program"
  ;; iterate through stack, evaluate each element in turn, 
  (loop for term in list do
    (cond
      ((typep term 'concrete-node)
       (case (node-type term)
         (:stack (stack-push term stack)) ;; TODO: make stack-fn??
         (:expr (stack-process-val (eval-expr term env) stack))
         (:query (error "query not implemented"))))
      ((typep term 'concrete-atom)
       (eval-stack-atom term stack env))
      (t (error (format nil "Unrecogniszed stack program: ~A~%" term)))))
  stack)

(defun eval-stack-any (term stack env)
  "Evaluate an arbitrary langauge term which is embedded in a stack program"
  (cond
    ((typep term 'concrete-node)
     (ecase (node-type term)
       (:stack (stack-push term stack)) ;; TODO make stack-fn!
       (:expr  (stack-push (eval-expr-node (contents term) env) stack))
       (:query (error "query not implemented"))))
    ((typep term 'concrete-atom)
     (eval-stack-atom (contents term) stack env))))

(defun run-stack-function (primop stack)
  (when (> (arity primop) (length (stack stack)))
    (error "not enough args to stack function!"))
  (let ((result
          (apply (fun primop)
                 (loop for i from 1 to (arity primop)
                       collect (stack-pop stack)))))
    (stack-push result stack)))

(defun eval-stack-atom (atom stack env)
  "Evaluate an atom on the stack. Updates the stack accordingly & returns nil"
  (cond
    ((typep (contents atom) 'keyword)
      (let ((val (env:lookup (contents atom) env)))
        (unless val (error 'variable-not-found
                           :missing-var (contents atom)
                           :env env))
        (stack-process-val val stack)))
    (t (stack-process-val (contents atom) stack))))

(defun stack-process-val (val stack)
  (cond
    ((or (typep val 'primop) (typep val 'viv-fun))
     (run-stack-function val stack))
    
    (t (stack-push val stack))))



;;------------------------------------------------------------------------------
;; Expression Evaluation
;;------------------------------------------------------------------------------

(defun eval-expr (term env)
  (cond
   ((typep term 'concrete-node)
    (eval-expr-node (contents term) env))
   ((typep term 'concrete-atom)
    (eval-expr-atom (contents term) env))))

(defun eval-any-expr (term env)
  (cond
    ((typep term 'concrete-node)
     (case (node-type term)
       (:stack (stack-pop (eval-stack (reverse (contents term)) (make-instance 'viv-stack) env)))
       (:expr  (eval-expr-node (contents term) env))
       (:query (error "query not implemented"))))
    ((typep term 'concrete-atom)
     (eval-expr-atom (contents term) env))))

(defun eval-expr-atom (value env)
  (cond
    ((typep value 'keyword)
     (let ((val (env:lookup value env)))
       (unless val (error 'variable-not-found
                          :missing-var value
                          :env env))
         (pure val)))
    (t (pure value))))

(defun eval-expr-node (terms env)
  (when (null terms) (error "expression-node cannot be emoty"))
  (mdo 
   (bind result (eval-any-expr (car terms) env))
   (cond
     ((or (typep result 'primop) (typep result 'viv-fun))
      (if (= (length (cdr terms)) (arity result))
          (mdo
           (bind arglist (mapM (lambda (e) (eval-any-expr e env)) (cdr terms)))
           (apply (fun result) arglist))
          (error (format nil "bad arg count to primop: got ~A, expected ~A"
                         (length (cdr terms))
                         (arity result)))))
     ((typep result 'viv-former)
      (eval-expr-former (former result) (cdr terms) env))
     
     (t (error (format nil "Cannot call object as function: ~A~%" result))))))


(defun eval-expr-former (former args env)
  (ecase former
    (:shift
     (assert (= (length args) 2))
     (let ((sym (get-symbol (elt args 0)))
           (body (elt args 1)))
       (mshift
        (lambda (k)
          (eval-any-expr
           body
           (env:insert sym (make-instance 'primop :arity 1 :fun k) env))))))

    (:reset
     (assert (= (length args) 1))
     (mreset (eval-any-expr (first args) env)))

    (:function
     (assert (= (length args) 2))
     (let ((arglist (get-symlist (elt args 0)))
           (body (elt args 1)))
       (pure (make-instance 'viv-fun
                            :arity (length arglist)
                            :fun (lambda (&rest args)
                                   (loop for val in args
                                         for sym in arglist
                                         with my-env = (env:copy env)
                                         do (env:insert-inplace sym val my-env)
                                         finally (return (eval-any-expr body
                                                                        my-env))))))))
    (:structure
     (mdo
      (bind fields (mapM #'(lambda (x) (get-symval-pair x env)) args))
      (pure (make-instance 'viv-struct :fields fields))))
    (:projector
     (assert (= (length args) 2))
     (let ((sym (get-symbol (elt args 1)))
           (struct (eval-any-expr (elt args 0) env)))
       (cdr (assoc sym (fields struct)))))

    (:constructor
     (let ((sym (get-symbol (elt args 0))))
       (make-instance
        'viv-ival
        :name sym
        :vals (mapcar (lambda (term) (eval-any-expr term env)) (cdr args)))))))

(defun get-symbol (term)
  (assert (typep (contents term) 'keyword))
  (contents term))

(defun get-symval-pair (terms env)
  (assert (= (length terms) 2))
  (unless (typep (contents (car terms)) 'keyword)
    (error "Term list element must contain symbol as first pair"))
  (mdo
    (bind val (eval-any-expr (elt terms 1) env))
    (pure (cons (contents (elt terms 0)) val))))

(defun get-symlist (expr)
  (unless (typep expr 'concrete-node)
    (error "Symbol list require to be list"))
  (loop for elt in (contents expr)
        do (unless (typep (contents elt) 'keyword)
          (error "Symbollist required to contain only symbols"))
        collect (contents elt)))
