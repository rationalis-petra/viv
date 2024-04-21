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


;; Imperative Statements
(defclass sy-seq (viv-syntax)
  ((terms
    :initarg :terms
    :accessor terms)))

(defclass sy-with-jumps (viv-syntax)
  ((body
    :initarg :body
    :accessor body)
   (jump-locations
    :initarg :jump-locations
    :accessor jump-locations)))

(defclass sy-object (viv-syntax)
  ((synchrony
    :initarg :synchrony
    :accessor synchrony)
   (name
    :initarg :name
    :accessor name)
   (slots
    :initarg :slots
    :accessor slots)
   (clauses
    :initarg :clauses
    :accessor clauses)))

(defclass sy-slot-set (viv-syntax)
  ((slot
    :initarg :slot
    :accessor slot)
   (val
    :initarg :val
    :accessor val)))

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
  ((name
    :accessor name
    :initarg :name)
   (vals
    :accessor vals
    :initarg :vals)
   (clauses
    :accessor clauses
    :initarg :clauses)))
(defclass sy-destructor (viv-syntax)
  ((field
    :initarg :field
    :accessor field))
  (:documentation "Project out a value from a destructor."))

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

(defclass sy-copattern ()
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
  ((field
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
    :initarg :vars
    :documentation "The (logical) variables introduced by the rule.")
   (goal
    :accessor goal
    :initarg :goal)
   (subgoals
    :accessor subgoals
    :initarg :subgoals))
  (:documentation "Form which generates a new rule. Rules can be inserted into
  predicates as long as their arities match."))

(defclass sy-goal (viv-syntax)
  ((head
    :accessor head
    :initarg :head)
   (args
    :accessor args
    :initarg :args))
  (:documentation "A unification goal. Like a function call."))

(defclass sy-query (viv-syntax)
  ((vars
    :accessor vars
    :initarg :vars)
   (goal
    :accessor goal
    :initarg :goal))
  (:documentation "Generate all substitutions as a stream of [values] objects"))

(defclass conj (viv-syntax)
  ((terms)))
(defclass disj (viv-syntax)
  ((terms)))
(defclass metavar (viv-syntax)
  ((symbol)))


;;------------------------------------------------------------------------------
;; Printing
;;------------------------------------------------------------------------------

(defmethod print-object ((value sy-copattern) stream)
  (write-string "co(" stream)
  (print-object (name value) stream)
  (format stream "~{~A ~}" (subpatterns value))
  (print-object (name value) stream)
  (write-string ")" stream))

(defmethod print-object ((value pattern-any) stream)
  (format stream "#s~A" (var value)))


