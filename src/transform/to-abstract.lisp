(in-package :viv)
;; Take a concrete term and convert it into a syntax tree

(declaim (ftype (function (env:macro-env concrete) viv-syntax) to-abstract))
(defun to-abstract (env term)
  (cond
    ((and (typep term 'concrete-atom)
          (typep (contents term) 'keyword))
     (match (env:lookup (contents term) env)
       ((cons :val val) (make-lit (contents term)))
       ((cons :var var) (make-var var))
       (_ (error "Internal error: bad value returned from macro env"))))

    ((typep term 'concrete-atom)
     (make-lit (lisp->viv (contents term))))

    ((typep term 'concrete-node)
     (case (node-type term)
       (:stack (to-abstract-stackprog env (contents term)))
       (:expr  (to-abstract-expr env (contents term)))
       (:logic (to-abstract-query env (contents term)))
       (t (error "Unrecognized node type"))))
    (t (error (format nil "Unrecognized concrete term")))))

;;------------------------------------------------------------------------------
;; Stack Syntax
;;------------------------------------------------------------------------------

(declaim (ftype (function (env:macro-env list) viv-syntax) to-abstract-stackprog))
(defun to-abstract-stackprog (env program)
  "Convert a list of terms to a stack program"
  (make-instance 'sy-stackprog
                 :terms (reverse (mapcar (lambda (term) (to-abstract env term)) program))))

;;------------------------------------------------------------------------------
;; Expression Syntax
;;------------------------------------------------------------------------------

(declaim (ftype (function (env:macro-env list) viv-syntax) to-abstract-expr))
(defun to-abstract-expr (env elements)
  (when (= 0 (length elements))
    (error "Concrete expression node must have at least one element"))
  (let ((comptime-val (get-comptime env (car elements))))
    (typecase comptime-val
      (viv-former (form-expr env comptime-val (cdr elements)))
      (viv-macro (macro-expand env comptime-val :expr (cdr elements)))
      (t 
       (make-instance 'sy-apply :fun (to-abstract env (car elements))
                                :args (mapcar (lambda (e) (to-abstract env e))
                                              (cdr elements)))))))

(declaim (ftype (function (env:macro-env viv-former list) viv-syntax) form-expr)) 
(defun form-expr (env former args)
  (case (former former)
    (:define
     (assert (> (length args) 1))
     (let ((symbol (get-symbol (elt args 0)))
           (body (to-abstract env (sub-expr (subseq args 1)))))
       (make-instance 'sy-def :var symbol :body body)))

    ;; meta
    (:macro
     (make-instance 'sy-macro :body (to-abstract env (sub-expr args))))
    (:quote
     (make-instance 'sy-literal :value (concrete->val (sub-expr args))))

    ;; imperative
    (:seq
     (make-instance 'sy-seq
                    :terms (mapcar (lambda (e) (to-abstract env e)) args)))
    (:with-jumps
        (assert (< 0 (length args)))
      (let* ((raw-locs (mapcar #'get-sym-body (cdr args)))
             (new-env (env:shadow-vars (mapcar #'car raw-locs) env))
             (locs (mapcar (lambda (elt)
                             (cons (car elt) (to-abstract new-env (sub-expr (cdr elt)))))
                           raw-locs)))
        (make-instance 'sy-with-jumps
                       :body (to-abstract new-env (elt args 0))
                       :jump-locations locs)))
    (:jump-to
     (assert (= 1 (length args)))
     (make-instance 'sy-jump-to :target (to-abstract env (elt args 0))))

    (:object
     (let* ((recsym (when (typep (contents (elt args 0)) 'keyword)
                      (contents (elt args 0))))
            (slots (mapcar (lambda (elt)
                                    (destructuring-bind (sym body) (get-symval-pair elt)
                                      (cons sym (to-abstract env body))))
                                  (contents (elt args (if recsym 1 0)))))
            (raw-clauses (subseq args (if recsym 2 1)))
            (new-env (env:shadow-vars (append (when recsym (list recsym))
                                              (mapcar #'car slots))
                                      env)))
       (make-instance
        'sy-object
        :synchrony :synchronous
        :name recsym
        :slots slots
        :clauses (mapcar (lambda (c) (to-coclause new-env c)) raw-clauses))))

    (:actor
     (let* ((recsym (when (typep (contents (elt args 0)) 'keyword)
                      (contents (elt args 0))))
            (slots (mapcar (lambda (elt)
                                    (destructuring-bind (sym body) (get-symval-pair elt)
                                      (cons sym (to-abstract env body))))
                                  (contents (elt args (if recsym 1 0)))))
            (raw-clauses (subseq args (if recsym 2 1)))
            (new-env (env:shadow-vars (append (when recsym (list recsym))
                                              (mapcar #'car slots))
                     env)))
       (make-instance
        'sy-object
        :synchrony :asynchronous
        :name recsym
        :slots slots
        :clauses (mapcar (lambda (c) (to-coclause new-env c)) raw-clauses))))

    (:slot-set
     (let* ((name (get-symbol (elt args 0)))
            (value (to-abstract env (elt args 1))))
       (make-instance 'sy-slot-set
                      :slot name
                      :val value)))

    ;; logic
    (:predicate
     (assert (< 1 (length args)))
     (make-instance 'sy-predicate
                    :args (get-symlist (first args))
                    :rules (mapcar (lambda (e) (to-abstract env e)) (subseq args 1))))

    (:rule 
     ;; TODO: auto-capture vars if arglist is omitted
     (assert (< 1 (length args)))
     ;(assert (typep (elt args 0) 'concrete-node))
     (let* ((vars (get-symlist (elt args 0)))
            (new-env (env:shadow-vars vars env))
            (goal (get-rule-head new-env (elt args 1)))
            (subgoals (mapcar (lambda (c) (to-abstract env c))
                              (subseq args 2))))
       (make-instance 'sy-rule
                      :vars vars
                      :goal goal
                      :subgoals subgoals)))
    (:query
     (assert (= 2 (length args)))
     ;; assert (typep (elt args 0))
     (let* ((vars (get-symlist (elt args 0)))
            (goal (to-abstract env (elt args 1))))
       (make-instance 'sy-query :vars vars :goal goal)))


    ;; functional
    (:function
     (assert (> (length args) 1))
     (let ((symlist (get-symlist (elt args 0))))
       (make-instance 'sy-function :args symlist
                                   :body (to-abstract (env:shadow-vars symlist env) (sub-expr (subseq args 1))))))


    ;; 
    (:shift
     (assert (> (length args) 1))
     (let ((symbol (get-symbol (elt args 0))))
       (make-instance 'sy-shift :var symbol
                                :body (to-abstract (env:shadow-var symbol env) (sub-expr (subseq args 1))))))

    (:reset
     (assert (= (length args) 1))
     (make-instance 'sy-reset :body (to-abstract env (first args))))

    ;; Data values
    (:structure
     (make-instance
      'sy-structure
      :fields
      (mapcar (lambda (elt)
                (destructuring-bind (sym body) (get-symval-pair elt)
                  (cons sym (to-abstract env body))))
              args)))

    (:projector
     (assert (= (length args) 1))
     (let ((sym (get-symbol (elt args 1))))
       (make-instance 'sy-projector :field sym)))

    (:destructor
     (assert (= (length args) 1))
     (let ((sym (get-symbol (elt args 0))))
       (make-instance 'sy-destructor :field sym)))

    (:corecursor
     (let* ((recsym (when (typep (contents (elt args 0)) 'keyword)
                      (contents (elt args 0))))
            (initial-vals (mapcar (lambda (v) (to-abstract env v))
                                (get-list (elt args (if recsym 1 0)))))
            (raw-clauses (subseq args (if recsym 2 1)))
            (new-env (if recsym (env:shadow-var recsym env) env)))
       (make-instance
        'sy-corecursor
        :name recsym
        :vals initial-vals
        :clauses (mapcar (lambda (c) (to-coclause new-env c)) raw-clauses))))

    (:constructor
     (let ((sym (get-symbol (elt args 0))))
       (make-instance
        'sy-constructor
        :name sym
        :args (mapcar (lambda (term) (to-abstract env term)) (cdr args)))))
    (:recursor
     (let* ((recsym (when (typep (contents (elt args 0)) 'keyword)
                      (contents (elt args 0))))
            (match-vals (mapcar (lambda (v) (to-abstract env v))
                                (get-list (elt args (if recsym 1 0)))))
            (raw-clauses (subseq args (if recsym 2 1)))
            (new-env (if recsym (env:shadow-var recsym env) env)))
       (make-instance
        'sy-recursor
        :name recsym
        :vals match-vals
        :clauses (mapcar (lambda (c) (to-clause new-env c)) raw-clauses))))

    (t (error "unrecognized former!"))))

(declaim (ftype (function (env:macro-env viv-macro keyword list) viv-syntax) macro-expand)) 
(defun macro-expand (env macro tipe arglist)
  (to-abstract
   env
   (val->concrete
    (cadr 
    (run (eval-term (env:macro->dynamic env)
                    (make-instance 'sy-apply
                                   :fun (make-instance 'sy-literal :value (body macro))
                                   :args (list
                                          (make-instance 'sy-literal :value (make-instance 'viv-ival :name tipe :vals nil))
                                          (make-instance 'sy-literal :value (list->val arglist #'concrete->val))))))))))

(defun to-patterns (env pat)
  (labels ((rec (term)
             (typecase term
               (concrete-atom
                (pat-atom (contents term)))
               (concrete-node
                (pat-node (contents term)))
               (t (error "expected concrete"))))

           (pat-atom (atom)
             (typecase atom
               (keyword (make-instance 'pattern-any :var atom))
               (viv-ival (make-instance 'pattern-ival
                                        :name (name atom)
                                        :subpatterns nil))
               ;; TODO: what if ;; matching ival has values?
               (t (error "expecting symbol or ival in pat-atom!"))))

           (pat-node (nodes)
             (let ((head (car nodes))
                   (rest (mapcar (lambda (r) (rec r))
                                 (cdr nodes))))
               (make-instance 'pattern-ival
                              :name (get-pattern-head head)
                              :subpatterns rest)))
           (get-pattern-head (head)
             (if (typep head 'concrete-atom) 
                 (progn
                   (assert (typep (contents head) 'viv-ival))
                   (assert (= 0 (length (vals (contents head)))))
                   (name (contents head)))
                 (progn
                   (assert (= 2 (length (contents head))))
                   (assert (eq (former (get-comptime env (car (contents head)))) :constructor))
                   (cadr (contents head))))))
    (mapcar #'rec pat)))

(defun to-copattern (env nodes)
  (flet ((get-copattern-head (head)
           (if (typep head 'concrete-atom) 
               (field (contents head))
               (progn
                 (assert (= 2 (length (contents head))))
                 (assert (eq (former (get-comptime env (car (contents head)))) :destructor))
                 (cadr (contents head))))))
    (let* ((head (car nodes))
           (rest (to-patterns env (cdr nodes))))
      (make-instance 'sy-copattern
                     :name (get-copattern-head head)
                     :subpatterns rest))))

;;------------------------------------------------------------------------------
;; Logic Syntax
;;------------------------------------------------------------------------------

(declaim (ftype (function (env:macro-env list) viv-syntax) to-abstract-goal))
(defun to-abstract-goal (env elements)
  ;; head needs to be
  (assert (< 1 (length elements)))
  (let ((head (to-abstract (elt elements 0)))
        (args (to-abstract (subeq elements 1))))
    (make-instace 'sy-goal
                  :head head
                  :args args)))



;;------------------------------------------------------------------------------
;; Utility
;;------------------------------------------------------------------------------

(defun get-comptime (env term)
  (or
   (and (typep (contents term) 'keyword)
        (cdr (env:lookup (contents term) env)))
   (and (comptime-p (contents term))
        (contents term))))

(defun get-rule-head (env term)
  (assert (typep term 'concrete-node))
  (assert (eq (node-type term) :logic))
  (mapcar (lambda (term) (to-abstract env term)) (contents term)))


(defun to-clause (env raw)
  (assert (typep raw 'concrete-node))
  (let* ((idx (position nil (contents raw)
                        :test (lambda (_ val)
                                (declare (ignore _))
                                (or (eq (contents val) :→)
                                    (eq (contents val) :->)))))
         (patterns (to-patterns env (subseq (contents raw) 0 idx)))
         (new-env (env:shadow-vars (reduce #'append (mapcar #'pattern-vars patterns)) env)))
    (cons patterns (to-abstract new-env (sub-expr (subseq (contents raw) (+ idx 1)))))))

(defun to-coclause (env raw)
  (assert (typep raw 'concrete-node))
  (let* ((idx (position nil (contents raw)
                        :test (lambda (_ val)
                                (declare (ignore _))
                                (or (eq (contents val) :→)
                                    (eq (contents val) :->)))))
         (patterns (to-copattern env (subseq (contents raw) 0 idx)))
         (new-env (env:shadow-vars (copattern-vars patterns) env)))
    (cons patterns (to-abstract new-env (sub-expr (subseq (contents raw) (+ idx 1)))))))


(declaim (ftype (function (list) concrete)))
(defun sub-expr (args)
  (if (= (length args) 1)
      (elt args 0)
      (make-instance 'concrete-node :type :expr :contents args)))
