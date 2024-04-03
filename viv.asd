
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
  :depends-on (:bordeaux-threads :trivia)
  :pathname "src"
  :components
  ((:file "package")

   (:module
    app
    :pathname "app"
    :depends-on (foundation eval)
    :components
    ((:file "app" :depends-on ("world"))
     (:file "main" :depends-on ("world"))
     (:file "world")))

   (:module
    eval
    :pathname "eval"
    :depends-on (foundation terms binding)
    :components ((:file "eval-canonical" :depends-on ("monad"))
                 (:file "eval-syntax")
                 (:file "monad")))

   (:module
    analysis
    :pathname "analysis"
    :depends-on (foundation terms)
    :components
    ((:file "comptime")
     (:file "syntax")))

   (:module
    transform
    :pathname "transform"
    :depends-on (foundation binding)
    :components
    ((:file "to-abstract")
     (:file "parse")))

   (:module
    binding
    :pathname "binding"
    :depends-on (foundation terms)
    :components ((:file "environment")))

   (:module
    terms
    :pathname "terms"
    :depends-on (foundation)
    :components
    ((:file "builtins" :depends-on ("value"))
     (:file "value")
     (:file "abstract")
     (:file "concrete")))

   (:module
    foundation
    :pathname "foundation"
    :depends-on ("package")
    :components
    ((:file "agent")
     (:file "function")))))
