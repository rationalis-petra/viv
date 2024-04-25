(in-package :viv)

(defclass viv-value () ()
  (:documentation ""))

;; Metaprogramming 
(defclass viv-former (viv-value)
  ((former
    :accessor former
    :initarg :former
    :documentation "The head of an abstract syntax tree node")))
(defclass viv-symbol (viv-value)
  ((name
    :accessor name
    :initarg :name
    :type keyword)))
(defclass viv-unique (viv-value)
  ((name
    :accessor name
    :initarg :name
    :type symbol)))

(declaim (ftype (function (string) viv-unique) mk-unique)) 
(defun mk-unique (string) (make-instance 'viv-unique :name (gensym string)))

(defun mk-former (symbol) (make-instance 'viv-former :former symbol))

;; Primitives and metaprogramming
;; formers: if, let, destructor, corecursor, constructor, recursor, stack
(defclass viv-symbol (viv-value)
  ((m-symbol
    :accessor m-symbol
    :initarg :symbol)))

(defclass viv-num (viv-value)
  ((num
    :accessor num
    :initarg :num)))

(defclass viv-string (viv-value)
  ((str
    :accessor str
    :initarg :str)))

(defclass viv-foreign (viv-value)
  ((value
    :accessor value
    :initarg :value)))

(defclass primop (viv-value)
  ((arity
    :accessor arity
    :initarg :arity
    :documentation "The number of arguments it consumes")
   (fun
    :accessor fun
    :initarg :fun
    :documentation "The function to invoke")))

;; Imperative and Object oriented programming
(defclass viv-jump-target (viv-value)
  ((monad
    :initarg :monad
    :accessor monad)))

(defclass viv-slot (viv-value)
  ((object
    :initarg :object
    :accessor object)
   (name
    :initarg :name
    :accessor name)))

(defclass viv-object (viv-value)
  ((name
    :accessor name
    :initarg :name)
   (slots
    :accessor slots
    :initarg :slots)
   (messages
    :accessor messages
    :initarg :messages))
  (:documentation "An object hides all its internal state (slots), and can only
  be interacted with via message sends, each of which synchronously 'locks' the
  object until completion."))

(defclass viv-actor (viv-value)
  ((slots
    :accessor slots
    :initarg :slots)
   (messages
    :accessor messages
    :initarg :messages))
   (:documentation "An actor hides all its internal state (slots), and can only
  be interacted with via messages, which are send and processed asynchronously (concurrently)."))


;; Logic programming
(defclass viv-lvar (viv-value)
  ((name
   :accessor name
   :initarg :name)
   (uid
    :accessor uid
    :initarg :uid)))

(defclass viv-rule (viv-value)
  ((vars
    :accessor vars
    :initarg :vars)
   (goal
    :accessor goal
    :initarg :goal)
   (subgoals
    :accessor subgoals
    :initarg :subgoals
    :initform nil)))

(defclass viv-predicate (viv-value)
  ((arity
    :accessor arity
    :initarg :arity)
   (rules
    :accessor rules
    :initarg :rules)))

;; Data Values 
(defclass viv-coval (viv-value)
  ((vals
    :accessor vals
    :initarg :vals)
   (fields
    :accessor fields
    :initarg :fields)))

(defclass viv-destructor (viv-value)
  ((field
    :accessor field
    :initarg :field)))

(defclass viv-ival (viv-value)
  ((name
    :accessor name
    :initarg :name)
   (vals
    :accessor vals
    :initarg :vals
    :initform nil)))

(defclass viv-struct (viv-value)
  ((fields
    :accessor fields
    :initarg :fields
    :initform (make-hash-table)
    :documentation "The set of name,value pairs in the struct")))

(defclass viv-projector (viv-value)
  ((field
    :accessor field
    :initarg :field)))


(defclass viv-macro (viv-value)
  ((body
    :accessor body
    :initarg :body)))

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




;; Printing

(defmethod print-object ((value viv-former) stream)
  (if *print-repr*
      (print-object (former value) stream)
      (format stream "#f~A" (former value))))

(defmethod print-object ((value viv-num) stream)
  (if *print-repr*
      (print-object (num value) stream)
      (format stream "#n~A" (num value))))

(defmethod print-object ((value viv-symbol) stream)
  (if *print-repr*
      (print-object (m-symbol value) stream)
      (format stream "#s~A" (m-symbol value))))

(defmethod print-object ((value viv-string) stream)
  (if *print-repr*
      (format stream "\"~a\"" (str value))
      (format stream "#s\"~a\"" (str value))))

(defmethod print-object ((value viv-ival) stream)
  (if *print-repr*
      (format stream "(:~a ~{~a~^ ~})" (name value) (vals value))
      (format stream "(#i :~a ~{~a~^ ~})" (name value) (vals value))))

(defmethod print-object ((value viv-values) stream)
  (if *print-repr*
      (format stream "[~{~a~^ ~}]" (m-values value))
      (format stream "(#v ~{~a~^ ~})" (m-values value))))
