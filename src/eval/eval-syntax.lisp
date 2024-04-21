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
  (pure
   (let ((res (env:lookup (viv:m-symbol term) env)))
     (if (typep res 'viv-slot)
       (gethash (name res) (slots (object res)))
       res))))

(defmethod eval-term (env (term sy-literal))
  (pure (value term)))

(defmethod eval-term (env (term sy-def))
  (mdo
   (bind val (eval-term env (body term)))
   (progn 
     (add-def (var term) val (env:env-world env) (env:env-package env) (env:env-module env))
     (pure val))))

(defmethod eval-term (env (term sy-seq))
  (mdo
   (bind terms (mapM (lambda (term) (eval-term env term)) (terms term)))
   (pure (car (last terms)))))

(defmethod eval-term (env (term sy-function))
  (pure (make-instance 'viv-fun
                       :arity (length (args term))
                       :fun (lambda (&rest args)
                              (eval-term
                               (env:insert-many (mapcar #'cons (args term) args) env)
                               (body term))))))

(defmethod eval-term (env (term sy-macro))
  (mdo
   (bind val (eval-term env (body term)))
   (pure (make-instance 'viv-macro :body val))))

(defmethod eval-term (env (term sy-apply))
  (mdo
   (bind callee (eval-term env (fun term)))
   (typecase callee
     ((or primop viv-fun)
      (unless (= (length (args term)) (arity callee))
        (error 'bad-arity
               :expected-arity (arity callee)
               :actual-arity (length (args term))
               :value callee))
      (mdo (bind args (mapM (lambda (e) (eval-term env e)) (args term)))
           (apply (fun callee) args)))
     (viv-stackfun
      (mdo (bind args (mapM (lambda (e) (eval-term env e)) (args term)))
           (bind final-stack (stack-run (env callee) (program callee) args))
           (pure (to-values final-stack))))
     (viv-ival
      (mdo (bind args (mapM (lambda (e) (eval-term env e)) (args term)))
           (pure
            (make-instance 'viv-ival :name (name callee) :vals (append args (vals callee))))))
     (viv-projector
      (assert (= (length (args term)) 1))
      (mdo (bind val (eval-term env (car (args term))))
           (pure
            (progn
              (assert (typep val 'viv-struct))
              (gethash (field callee) (fields val))))))
     (viv-destructor
      (mdo (bind val (eval-term env (car (args term))))
           (bind args (mapM (lambda (v) (eval-term env v)) (cdr (args term))))
           (typecase val
             (viv-coval
              (assert (null args))
              (funcall (gethash (field callee) (fields val)) (vals val)))
             (viv-object
              (funcall (gethash (field callee) (messages val)) args))
             (viv-actor
              (funcall (gethash (field callee) (messages val)) args))
             (t (error "Destructor operated on bad sort!")))))

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
  (pure (make-instance 'viv-projector :field (field term))))

(defmethod eval-term (env (term sy-corecursor))
  ;; Corecursor (object) evaluation
  ;; 1. The primary representation of the corecursive object is map of functions,
  ;;    each of which takes 'internal state' parameters as arguments, 
  ;; 
  (labels ((make-fun (clauses rec)
             (lambda (vals)
               (loop
                 for clause in clauses
                 for match-res = (try-comatch clause vals)
                 do (when match-res
                      (return (exec-match match-res rec)))
                 ;; if no match has been successful, error!
                 finally (error "comatch failed!"))))

           (exec-match (match-res rec)
             (eval-term 
              ;; TODO: what if recursive name?
              (env:insert-many (append (car match-res)
                                       (when (name term)
                                         (list (cons (name term) rec))))
                               env)
              (cdr match-res))))
    (mdo 
     (bind vals (mapM (lambda (v) (eval-term env v)) (vals term)))
     (loop
       with out-map = (make-hash-table)
       for clause-set in (group-by (clauses term)
                                   :key (lambda (clause) (name (car clause)))
                                   :value #'identity)
       do (setf (gethash (car clause-set) out-map)
                (make-fun (cdr clause-set)
                          (make-instance 
                           'primop
                           :arity (length vals)
                           :fun 
                           (lambda (&rest vals)
                             (pure (make-instance 'viv-coval
                                                  :fields out-map
                                                  :vals vals))))))
       finally (return (pure (make-instance 'viv-coval :fields out-map :vals vals)))))))

(defmethod eval-term (env (term sy-object))
  ;; Corecursor (object) evaluation
  ;; 1. The primary representation of the corecursive object is map of functions,
  ;;    each of which takes 'internal state' parameters as arguments, 
  ;; 
  (labels ((make-fun (clauses self env)
             (lambda (vals)
               (loop
                 for clause in clauses
                 for match-res = (try-comatch clause vals)
                 do (when match-res
                      (return (exec-match match-res self env)))
                 ;; if no match has been successful, error!
                 finally (error "comatch failed!"))))

           (exec-match (match-res self env)
             (eval-term 
              (env:insert-many (append (car match-res)
                                       (when (name term)
                                         (list (cons (name term) self))))
                               env)
              (cdr match-res))))
    (mdo 
     (bind sbinds (mapM (lambda (v) (eval-term env (cdr v))) (slots term)))
     (loop
       with self = (make-instance
                    (if (eq :synchronous (synchrony term))
                        'viv-object
                        'viv-actor)
                    :slots (alist->table
                            (mapcar (lambda (s v) (cons (car s) v))
                                    (slots term) sbinds))
                    :name (name term))
       with slots = (mapcar (lambda (pr) (make-instance 'viv-slot
                                                        :object self
                                                        :name (car pr)))
                            (slots term))
       with new-env = (env:insert-many (mapcar (lambda (pr val) (cons (car pr) val))
                                                (slots term)
                                                slots)
                                     env)
       with out-map = (make-hash-table)
       for clause-set in (group-by (clauses term)
                                   :key (lambda (clause) (name (car clause)))
                                   :value #'identity)
       do (setf (gethash (car clause-set) out-map)
                (make-fun (cdr clause-set) self new-env))
       finally
          (progn
            (setf (messages self) out-map)
            (return (pure self)))))))

(defmethod eval-term (env (term sy-slot-set))
  (let ((res (env:lookup (slot term) env)))
    (if (typep res 'viv-slot)
        (mdo 
         (bind value (eval-term env (val term)))
         (progn
           (setf (gethash (name res) (slots (object res))) value)
           (pure viv-base::+null-values+)))
        (error "must set a slot; setting: ..."))))

(defmethod eval-term (env (term sy-constructor))
  (mdo
   (bind vals (mapM (lambda (val) (eval-term env val)) (args term)))
   (pure (make-instance 'viv-ival :name (name term) :vals vals))))
                          

(defmethod eval-term (env (term sy-recursor))
  (mdo
   (bind vals (mapM (lambda (val) (eval-term env val)) (vals term)))
   (labels ((recur (&rest args)
              (let* ((match-res (get-match-term args (clauses term)))
                     (new-env (env:insert-many
                               (append (car match-res)
                                       ;; TODO: if recur not bound, don't add to environment 
                                       (list (cons (name term)
                                                   (make-instance 'primop
                                                                  :arity (length args)
                                                                  :fun #'recur))))
                               env)))
                (eval-term new-env (cdr match-res)))))
     (apply #'recur vals))))




;; Stack Evaluation
(defmethod eval-term (env (term sy-stackprog))
  (pure (make-instance 'viv-stackfun
                 :program (terms term)
                 :env env)))

(defun stack-run (env program stack)
  (if (null program) (pure stack)
      (typecase (car program)
        (sy-stackprog
         (stack-run
          env
          (cdr program)
          (make-instance 'viv-stackfun :env env
                                       :program (terms (car program)))))
        (t (mdo (bind res (eval-term env (car program)))
                (bind new-stack (stack-word env res stack))
                (stack-run env (cdr program) new-stack))))))

(defun stack-word (env word stack)
  (declare (ignore env))
  (typecase word
    ((or primop viv-fun)
     (if (< (arity word) (length stack))
         (error "not enough args on stack")
         (mdo (bind val (apply (fun word) (subseq stack 0 (arity word))))
              (pure (cons val (subseq stack (arity word)))))))
    (viv-stackfun
     (stack-run (env word) (program word) stack))
    (t (pure (cons word stack)))))

(defun to-values (stack)
  (if (= (length stack) 1)
      (car stack)
      (make-instance 'viv-values :values stack)))



;;------------------------------------------------------------------------------
;; Logic Syntax
;;------------------------------------------------------------------------------

;; The search/query resolution algorithm. Uses delimited continuations to do
;; depth-first proof search.

(defmethod eval-term (env (term sy-predicate))
  (mdo
   (bind rules (mapM (lambda (term) (eval-term env term)) (rules term)))
   (pure
    (progn
      (assert (every (lambda (v) (typep v 'viv-rule)) rules))
      (make-instance 'viv-predicate
                     :arity (length (args term))
                     :rules rules)))))

(defmethod eval-term (env (term sy-rule))
  (pure (make-instance 'viv-rule
                       :vars (vars term)
                       :goal (goal term)
                       :subgoals (subgoals term))))

(defmethod eval-term (env (term sy-query))
  (let* ((next (run-query env (vars term) (goal term))))
    (pure next)))


;;------------------------------------------------------------------------------
;;   Pattern Matching Utilities. 
;;------------------------------------------------------------------------------

(declaim (ftype (function (sy-pattern t) t) match-pattern))
(defun match-pattern (pattern val)
  (typecase pattern
    (pattern-any
     (list (cons (var pattern) val)))
    (pattern-ival
     (if (and (eq (name pattern) (name val))
              (= (length (subpatterns pattern))
                 (length (vals val))))
         (let ((submatches (mapcar #'match-pattern (subpatterns pattern) (vals val))))
           (if (every (lambda (x) (not (eq x :false))) submatches)
               (apply #'append submatches)
               :false))
         :false))
    (t (error (format nil "bad-pattern: ~A~%" pattern)))))

(defun try-match (coclause args)
  (let* ((pattern (car clause))
         (binds (mapcar #'match-pattern pattern args)))
    (when (every (lambda (x) (not (eq x :false))) binds)
      (cons (reduce #'append  binds) (cdr clause)))))

(declaim (ftype (function (list list) cons) try-comatch))
(defun try-comatch (clause args)
  (assert (= (length args) (length (subpatterns (car clause)))))
  (let* ((pattern (car clause))
         (binds (mapcar #'match-pattern (subpatterns pattern) args)))
    (when (every (lambda (x) (not (eq x :false))) binds)
      (cons (reduce #'append  binds) (cdr clause)))))

(defun get-match-term (args clauses)
  (or 
   (loop for clause in clauses
         do (let ((res (try-match clause args)))
              (when res (return res))))
   (error "match failed!")))


;;------------------------------------------------------------------------------
;;   Unification. 
;;------------------------------------------------------------------------------

;; Queries run as follows:
;(defun run-query (vars head))


;; The unification algorithm
(declaim (ftype (function (env:dynamic-env viv-value viv-value) list) unify))
(defun unify (env v1 v2)
  (typecase v1
    (viv-ivar
     (occurs-check v1 v2)
     (list (cons (name v1) v2)) ;; assoc-list

     
     )))

(defun occurs-check (l r)
  (declare (ignore l r)))


