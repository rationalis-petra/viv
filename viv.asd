
(require 'asdf)
(defsystem :viv
  :name "Viv"
  :version "0.0.1"
  :maintainer "Connor Redfern"
  :author "Connor Redfern"
  :license "GPL3"

  ;; :defsystem-depends-on (:deploy)
  ;; :build-operation "deploy-op"
  ;; :build-pathname "wbc.exe"
  ;; :entry-point #'cl-user::main ;main
  ;;:depends-on (:iup)
  :pathname "src"
  :components
  ((:file "main" :depends-on (transform terms "eval"))
   (:file "parse" :depends-on (terms))
   (:module
    eval
    :pathname "eval"
    :depends-on (terms binding)
    :components ((:file "eval-canonical" :depends-on ("monad"))
                 (:file "eval-syntax")
                 (:file "monad")))
   (:module
    binding
    :pathname "binding"
    :components ((:file "environment")))
   (:module
    transform
    :pathname "transform"
    :components ((:file "to-abstract")))
   (:module
    terms
    :pathname "terms"
    :components
    ((:file "builtins" :depends-on ("value"))
     (:file "value")
     (:file "abstract")
     (:file "concrete")))))
