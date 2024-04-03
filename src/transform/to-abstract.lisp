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
     (make-lit (contents term)))

    ((typep term 'concrete-node)
     (case (node-type term)
       (:stack (to-abstract-stackprog env (contents term)))
       (:expr  (to-abstract-expr env (contents term)))
       (:query (to-abstract-query env (contents term)))
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
  (let ((comptime-val (and (typep (contents (car elements)) 'keyword)
                           (env:lookup (contents (car elements)) env))))

    (if (typep (cdr comptime-val) 'viv-former)
        (form-expr env (cdr comptime-val) (cdr elements))
        (make-instance 'sy-apply :fun (to-abstract env (car elements))
                                 :args (mapcar (lambda (e) (to-abstract env e))
                                               (cdr elements))))))

(declaim (ftype (function (env:macro-env viv-former list) viv-syntax) form-expr)) 
(defun form-expr (env former args)
  (case (former former)
    (:define
     (assert (> (length args) 1))
     (let ((symbol (get-symbol (elt args 0)))
           (body (to-abstract env (sub-expr (subseq args 1)))))
       (make-instance 'sy-def :var symbol :body body)))

    (:function
     (assert (> (length args) 1))
     (let ((symlist (get-symlist (elt args 0))))
       (make-instance 'sy-function :args symlist
                                   :body (to-abstract (env:shadow-vars symlist env) (sub-expr (subseq args 1))))))

    (:shift
     (assert (> (length args) 1))
     (let ((symbol (get-symbol (elt args 0))))
       (make-instance 'sy-shift :var symbol
                                :body (to-abstract (env:shadow-var symbol env) (sub-expr (subseq args 1))))))

    (:reset
     (assert (= (length args) 1))
     (make-instance 'sy-reset :body (to-abstract env (first args))))

    (:structure
     (make-instance
      'sy-structure
      :fields
      (mapcar (lambda (elt)
                (destructuring-bind (sym body) (get-symval-pair elt)
                  (cons sym (to-abstract env body))))
              args)))

    (:projector
     (assert (= (length args) 2))
     (let ((sym (get-symbol (elt args 1)))
           (struct (to-abstract env (elt args 0))))
       (make-instance 'sy-projector :field sym :value struct)))

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
               (t (error "expecting symbol!"))))

           (pat-node (nodes)
             (let ((head (get-symbol (car nodes)))
                   (name (get-symbol (cadr nodes)))
                   (rest (mapcar (lambda (r) (rec r))
                                 (cddr nodes))))
               (assert (eq (former (cdr (env:lookup head env))) :constructor))
               (make-instance 'pattern-ival
                              :name name
                              :subpatterns rest))))
    (mapcar #'rec pat)))

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
  


(declaim (ftype (function (list) concrete)))
(defun sub-expr (args)
  (if (= (length args) 1)
      (elt args 0)
      (make-instance 'concrete-node :type :expr :contents args)))

(declaim (ftype (function (env:macro-env list) viv-syntax) to-abstract-query))
(defun to-abstract-query (env elements)
  (declare (ignore env elements))
  (error "abstrcat query not implemented"))
