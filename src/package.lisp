(defpackage viv
  (:use :cl :foundation :trivia)
  (:export
   :world

   ;; values
   :primop
   :viv-num
   :viv-module
   :viv-former
   :viv-values

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
