(in-package :viv-base)

;; Arithmetic

(defun make-combinator-module ()
  ;; TODO: special character indicating start viv!
  (viv-module combinator
   (def id fn (x) x)
   (def dup fn (x) ![x x])
   (def swap fn (x y) ![y x])
   (def rot fn (x y z) ![y z x])
   )
