(in-package :viv)


;; Abstract Syntax Tree
;; There are three types of syntax:
;; + Expressions
;; + Stacks
;; + Queries

(defclass viv-syntax () ())

(defclass sy-def (viv-syntax)
  ((var
    :accessor var
    :initarg :var)
   (body
    :accessor body
    :initarg :body)))

(defclass sy-variable (viv-syntax)
  ((symbol
    :accessor m-symbol
    :initarg :symbol)))
(defun make-var (var) (make-instance 'sy-variable :symbol var))

(defclass sy-literal (viv-syntax)
  ((value
    :initarg :value
    :accessor value)))
(defun make-lit (val) (make-instance 'sy-literal :value val))

(defclass sy-seq (viv-syntax)
  ((terms
    :initarg :terms
    :accessor terms)))

;; Expresssions
(defclass sy-function (viv-syntax)
  ((args
    :initarg :args
    :accessor args)
   (body
    :initarg :body
    :accessor body)))
(defclass sy-apply (viv-syntax)
  ((function
    :initarg :fun
    :accessor fun)
   (args
    :initarg :args
    :accessor args)))

(defclass sy-shift (viv-syntax)
  ((var
    :initarg :var
    :accessor var)
   (body
    :initarg :body
    :accessor body)))
(defclass sy-reset (viv-syntax)
  ((body
    :initarg :body
    :accessor body)))

(defclass sy-corecursor (viv-syntax)
  ((recname)
   (vals)
   (coclauses)))
(defclass sy-destructor (viv-syntax)
  ((name)
   (val)))

(defclass sy-constructor (viv-syntax)
  ((name
    :initarg :name
    :accessor name)
   (args
    :initarg :args
    :accessor args)))
(defclass sy-recursor (viv-syntax)
  ((name
    :initarg :name
    :accessor name)
   (vals
    :initarg :vals
    :accessor vals)
   (clauses
    :initarg :clauses
    :accessor clauses)))

(defclass sy-pattern () ())
(defclass pattern-any (sy-pattern)
  ((var
    :initarg :var
    :accessor var)))
(defclass pattern-ival (sy-pattern)
  ((name
    :initarg :name
    :accessor name)
   (subpatterns
    :initarg :subpatterns
    :accessor subpatterns)))


(defclass sy-structure (viv-syntax)
  ((fields
    :initarg :fields
    :accessor fields)))
(defclass sy-projector (viv-syntax)
  ((value
    :initarg :value
    :accessor value)
   (field
    :initarg :field
    :accessor field)))

(defclass sy-macro (viv-syntax)
  ((body
    :accessor body
    :initarg :body)))

;; "Special Forms" (modify evaluation)
(defclass sy-let (viv-syntax)
  ())

;; Stack evaluation
;; • Stackprogram = like function, but for stacks
(defclass sy-stackprog (viv-syntax)
  ((terms
    :accessor terms
    :initarg :terms)))
(defclass sy-former (viv-syntax)
  ((symbol)))

;; Query evaluation
(defclass sy-predicate (viv-syntax)
  ((args
    :accessor args
    :initarg :args)
   (rules
    :accessor rules
    :initarg :rules
    :initform nil)))

(defclass sy-rule (viv-syntax)
  ((vars
    :accessor vars
    :initarg :vars)
   (goal
    :accessor goal
    :initarg :goal)
   (subgoals
    :accessor subgoals
    :initarg subgoals)))


(defclass sy-lterm (viv-syntax)
  ())

(defclass sy-query (viv-syntax)
  ((terms)))

(defclass conj (viv-syntax)
  ((terms)))
(defclass disj (viv-syntax)
  ((terms)))
(defclass metavar (viv-syntax)
  ((symbol)))


