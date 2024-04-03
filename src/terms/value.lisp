(in-package :viv)

(defclass viv-value () ()
  (:documentation ""))

;; Metaprogramming 
(defclass viv-former (viv-value)
  ((former
    :accessor former
    :initarg :former
    :documentation "The head of an abstract syntax tree node")))
(defun mk-former (symbol) (make-instance 'viv-former :former symbol))

;; Primitives and metaprogramming
;; formers: if, let, destructor, corecursor, constructor, recursor, stack
(defclass viv-symbol (viv-value)
  ((m-symbol
    :accessor m-symbol)))

(defclass primop (viv-value)
  ((arity
    :accessor arity
    :initarg :arity
    :documentation "The number of arguments it consumes")
   (fun
    :accessor fun
    :initarg :fun
    :documentation "The function to invoke")))


;; Data Values 
(defclass viv-coval (viv-value) ())

(defclass viv-ival (viv-value)
  ((name
    :accessor name
    :initarg :name)
   (vals
    :accessor vals
    :initarg :vals)))

(defclass viv-struct (viv-value)
  ((fields
    :accessor fields
    :initarg :fields
    :initform (make-hash-table)
    :documentation "The set of name,value pairs in the struct")))

;; Stack values
(defclass viv-stackfun (viv-value)
  ((program
    :accessor program
    :initarg :program)
   (env
    :accessor env
    :initarg :env)))

(defclass viv-values (viv-value)
  ((m-values
    :accessor m-values
    :initarg :values)))

;; Expression Values
(defclass viv-fun (viv-value)
  ((arity
    :accessor arity
    :initarg :arity)
   (fun
    :accessor fun
    :initarg :fun)))

(defclass viv-package (viv-value)
  ((modules
    :accessor modules
    :initform (make-hash-table)
    :initarg :modules)
   (exports
    :accessor exports
    :initform nil 
    :initarg :exports)
   (prelude
    :accessor prelude
    :initform nil
    :initarg :prelude)
   (dependencies
    :accessor dependencies
    :initform nil
    :initarg :dependencies)
   (name
    :accessor name
    :initarg :name)))

(defclass viv-module (viv-struct)
  ((name
    :accessor name
    :initarg :name))
  (:documentation "A Module is much like a struct, but is able to influence how
  its' members are run (typechecked, not typechecked etc.)"))
