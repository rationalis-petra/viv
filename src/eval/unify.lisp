(in-package :viv)

;;------------------------------------------------------------------------------
;;   Unification. 
;;------------------------------------------------------------------------------

;; Queries run as follows:
;; predicates split the query in a depth-first manner
;; results are returned as a stream (coval)
;; (defun run-query (vars head))

;; Proof search: given a set of free vars & a predicate,
;; 
;; (defun proof-search ())

;; streams
;; (defclass util-stream ()
;;   ((head))
;;   ((next)))

;; (defun stream-head (stream))
;; (defun stream-tail (stream))

;; hook each stream up in a line
;; (defun stream-append (initial &rest constructors))

;; nondeterminism via delimited continuations
;; (defun scatter ()) 
;; (defun gather ())

;; Given a predicate and a set of vals, return either
;;  :fail on failure
;;  the substitutions on a success
;;  
;;  
;; (declaim (ftype (function (list viv-predicate list) t) search-predicate))
;; (defun search-predicate (vars predicate vals)
;;   (loop for rule in rules
;;         do ()))

;; Unification: produce a list of subsitutions to apply to a term
(declaim (ftype (function (viv-value viv-value) (or list keyword)) unify)) 
(defun unify (l r)
  (loop
    with unsolved = (list (cons l r)) 
    with result = nil
    while unsolved
    do (let* ((unify-pair (pop unsolved))
              (step (unify-step (car unify-pair) (cdr unify-pair))))
         (if (eq step :fail) 
             (return :fail)
             (let ((new-binds (car result))
                   (extra-problems (cdr result)))
               (when extra-problems
                 (setf unsolved (append extra-problems unsolved)))
               (when new-binds
                 ;; Step 1: update result
                 (setf result (subst-concat result new-binds))
                 ;; Step 2: apply new substitution to all remaining unification steps
                 (setf unsolved
                       (mapcar (lambda (pair)
                                 (cons
                                  (lsubst new-binds (car pair))
                                  (lsubst new-binds (cdr pair))))
                               unsolved))))))

    finally (return result)))


;; The unification algorithm
;; return a pair
;; (new-binds .  extra-unification)
;; or nil if neither are 
(declaim (ftype (function (viv-value viv-value) (or cons keyword)) unify-step))
(defun unify-step (v1 v2)
  (cond
    ((logic-eql v1 v2) nil)
    ((typep v1 'viv-lvar)
     ;; TODO: occurs-check
     (cons (list (cons (uid v1) v2)) nil))
    ((typep v2 'viv-lvar)
     (cons (list (cons (uid v2) v1)) nil))
    ((not (eq (type-of v1) (type-of v2)))
     :fail)
    (t
     (cons nil (break-down v1 v2)))))

(defun break-down (v1 v2)
  ;; NOTE: we assume v1 & v2 have equal type!
  ;; also assume that they are not equal values (logically
  (typecase v1
    ;; literals have no substructure; ignore
    (viv-num :fail)
    (viv-symbol :fail)
    (viv-string :fail)
    (viv-fun :fail)
    (viv-object :fail)
    (viv-coval :fail)

    (viv-ival
     (if (and
          (eq (name v1) (name v2))
          (= (length (vals v1)) (length (vals v2))))
         (loop for fst in (vals v1)
               for snd in (vals v2)
               collect (cons fst snd))
         :fail))
    (viv-struct
     (if (alexandria:set-equal (alexandria.2:hash-table-keys (fields v1)) (alexandria.2:hash-table-keys (fields v2)))
         (loop
           for key being each hash-key in (fields v1)
           collect (cons (gethash key (fields v1)) (gethash key (fields v2))))
         :fail))))


(defun occurs-check (l r)
  (declare (ignore l r))
  (error "occurs-check not implemented"))





;;------------------------------------------------------------------------------
;;   Substitution. 
;;------------------------------------------------------------------------------

(declaim (ftype (function (list viv-value) viv-value) lsubst)) 
(defun lsubst (new-binds value)
  (etypecase value
    ;; if logical variable, look it up
    (viv-lvar
     (or (assoc (uid value) new-binds) value))
    ;; if structured value, recurse
    (viv-struct
     (make-instance 'viv-struct
                    :fields (maptable (lambda (val) (lsubst new-binds val))
                                     (fields value))))
    (viv-ival
     (make-instance 'viv-ival
                    :name (name value)
                    :vals (mapcar (lambda (val) (lsubst new-binds val))
                                  (vals value))))

    ;; if literal/const, return self
    (viv-fun value)
    (viv-num value)
    (viv-symbol value)
    (viv-string value)))


(defun subst-concat (old new)
  (append 
   (loop for pair in old
         collect (cons (car pair) (lsubst new (cdr pair))))
   new))
