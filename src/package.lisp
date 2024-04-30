(defpackage viv
  (:use :cl :foundation :trivia)
  (:export
   :world

   ;; values
   :primop
   :viv-num
   :viv-string
   :viv-module
   :viv-former
   :viv-values
   :viv-macro

   :*print-repr*

   ;; accessors
   :num

   :viv-package
   :packages
   :prelude
   :fields
   :modules
   :name
   :m-symbol
   :comptime-p))

(in-package :viv)

;; Globals

(defvar *current-world*) 
(defvar *print-repr* nil)

