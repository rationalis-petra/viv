

;; Abstract Syntax Tree
;; There are three types of syntax:
;; + Expressions
;; + Stacks
;; + Queries

(defclass viv-syntax () ())

(defclass sy-variable (viv-syntax)
  ((symbol)))
(defclass sy-value (viv-syntax)
  ((value)))

;; Expresssions
(defclass sy-corecursor (viv-syntax)
  ((recname)
   (vals)
   (coclauses)))
(defclass sy-destructor (viv-syntax)
  ((name)
   (val)))

(defclass sy-constructor (viv-syntax)
  ((name)
   (vals)))
(defclass sy-recursor (viv-syntax)
  ((name)
   (vals)
   (clauses)))

(defclass sy-structure (viv-syntax)
  ((fields)))
(defclass sy-projector (viv-syntax)
  ((value)
   (field)))


;; Stack evaluation
(defclass sy-stackkprog (viv-syntax)
  ((terms)))
(defclass sy-stackfreeze (viv-syntax)
  ((program)))

;; "Special Forms" (modify evaluation)
(defclass sy-let (viv-syntax)
  ())
