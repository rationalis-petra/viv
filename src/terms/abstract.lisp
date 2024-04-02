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
(defclass pattern-any ()
  ((var
    :initarg :var
    :accessor var)))
(defclass pattern-ival ()
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

;; "Special Forms" (modify evaluation)
(defclass sy-let (viv-syntax)
  ())

;; Stack evaluation
;; • Stackprogram = like function, but for stacks
(defclass sy-stackkprog (viv-syntax)
  ((terms
    :initarg :terms)))
(defclass sy-former (viv-former)
  ((symbol)))

;; Query evaluation
(defclass pred (viv-syntax)
  ((arity)
   (rules)))
(defclass rule (viv-syntax)
  ((parent-pred)
   (name)
   (terms)))
(defclass conj (viv-syntax)
  ((terms)))
(defclass disj (viv-syntax)
  ((terms)))
(defclass seek-goal (viv-syntax)
  ((terms)))
(defclass metavar (viv-syntax)
  ((symbol)))


