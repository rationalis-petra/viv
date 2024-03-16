

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
    :documentation "The set of name,value pairs in the struct")))

;; Stack values
(defclass viv-stackfun (viv-value)
  ((program
    :accessor program
    :initarg :program)))

;; Expression Values
(defclass viv-fun (viv-value)
  ((arity
    :accessor arity
    :initarg :arity)
   (fun
    :accessor fun
    :initarg :fun)))




;; Stack
(defclass viv-stack (viv-value)
  ((stack
    :accessor stack
    :initarg :stack
    :initform (make-array 100 :fill-pointer 0))))

(defun stack-push (val stack) (vector-push val (stack stack)))
(defun stack-pop (stack) (vector-pop (stack stack)))

(defmethod print-object ((stack viv-stack) stream)
  (write-string "Stack:" stream)
  (loop for elem across (stack stack)
        do (progn
             (write-char #\newline stream)
             (write-string "- " stream)
             (print-object elem stream))))
