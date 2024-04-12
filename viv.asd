
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
  :depends-on (:bordeaux-threads :trivia :sdl2)
  :pathname "src"
  :components
  ((:file "package" :depends-on (foundation))

   (:module
    app
    :pathname "app"
    :depends-on (foundation base eval)
    :components
    ((:file "app" :depends-on ("world"))
     (:file "main" :depends-on ("world"))
     (:file "world")))

   (:module
    base
    :pathname "base"
    :depends-on (foundation terms)
    :components
    ((:file "base" :depends-on (core system))
     (:module
      core
      :pathname "core"
      :depends-on ("package")
      :components
      ((:file "core" :depends-on ("lang" "reflect" "num"))
       (:file "lang")
       (:file "reflect")
       (:file "num")))
     (:module
      system
      :pathname "system"
      :depends-on ("package")
      :components
      ((:file "system" :depends-on ("console"))
       (:file "console")))

     (:file "package")))

   (:module
    eval
    :pathname "eval"
    :depends-on (foundation terms binding)
    :components ((:file "eval-canonical")
                 (:file "eval-syntax")))

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
    ((:file "concrete-value")
     (:file "to-abstract")
     (:file "to-bytecode")
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
    ((:file "value")
     (:file "abstract")
     (:file "concrete")))

   (:module
    foundation
    :pathname "foundation"
    :components
    ((:file "monad" :depends-on ("package"))
     (:file "agent" :depends-on ("package"))
     (:file "function" :depends-on ("package"))
     (:file "package")))))
