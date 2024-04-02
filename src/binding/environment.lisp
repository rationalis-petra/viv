(defpackage environment
  (:nicknames :env)
  (:use :cl :viv)
  (:export
   :dynamic-env :macro-env :canon-env

   :env-world :env-package :env-module

   :copy
   :lookup
   :insert :insert-many :insert-inplace
   
   :shadow-var :shadow-vars :shadow-var-inplace

   :lookup-path

   :variable-not-found))

(in-package :environment)

;; Here in the environment package, we handle various types of binding. 
;; There are two types of environment (so far)
;; • Macro environments (macro-env) map symbols to either other symbols, 
;;   or expanded paths.  
;; • Lexical environments map symbols to local variables, and lookup paths in an environment
;;   

(defclass env ()
  ((env-world
    :accessor env-world
    :initarg :world
    :documentation "Current world")
   (env-package
    :accessor env-package
    :initarg :package
    :documentation "Current package")
   (env-module
    :accessor env-module
    :initarg :module
    :initform nil
    :documentation "Current package")))


(defclass canon-env (env)
  ((locals
    :accessor locals
    :initarg :locals
    :initform nil)))

(defclass dynamic-env (env)
  ((locals
    :accessor locals
    :initarg :locals
    :initform nil)))

(defclass macro-env (env)
  ((path-map
    :accessor path-map
    :documentation "Map symbols to paths. Constructed when the macro environment is.")
   (shadows
    :accessor shadows
    :initarg :shadows
    :initform nil)))


(define-condition variable-not-found (error)
  ((missing-var
    :initarg :missing-var
    :accessor missing-var)
   (env
    :initarg :env
    :accessor env)))


(declaim (ftype (function ((or list symbol) env) t) lookup))
(defgeneric lookup (variable environment)
  (:documentation "Lookup VARIABLE in ENVIRONMENT. If not value is found,
  return nil."))

(defgeneric insert (var val env)
  (:documentation "Insert a new variable into an environment"))

(defgeneric insert-many (arglist env)
  (:documentation "Insert a new variable into an environment"))

(defgeneric copy (object)
  (:documentation "Produce a shallow copy of OBJECT"))


;; Dynamic environment methods

(defmethod lookup (var (env dynamic-env))
  (if (typep var 'keyword)
      (or (cdr (assoc var (locals env)))
          (error 'variable-not-found :missing-var var :env env))
      (lookup-path var (env-world env))))

(defmethod insert (var val (env dynamic-env))
  (make-instance 'dynamic-env
                 :locals (acons var val (locals env))
                 :world (env-world env)
                 :package (env-package env)
                 :module (env-module env)))

(defmethod insert-many (arglist (env dynamic-env))
  (make-instance 'dynamic-env
                 :locals (append arglist (locals env))
                 :package (env-package env)
                 :world (env-world env)
                 :module (env-module env)))

(defmethod copy ((env dynamic-env))
  (make-instance 'dynamic-env
                 :locals (locals env)
                 :package (env-package env)
                 :world (env-world env)))

(defun insert-inplace (var val env)
  "Insert a new variable into an environment"
  (setf (locals env) (acons var val (locals env))))



;; Macro environment

;; (defun map-var (variable environment)
;;   (:documentation "Lookup VARIABLE in ENVIRONMENT. If not value is found,
;;   return nil."))

(defmethod initialize-instance ((env macro-env) &key &allow-other-keys)
  (call-next-method)
  (if (env-module env)
      (setf (path-map env) (calc-path-map (env-world env) (env-package env) (env-module env)))
      (setf (path-map env) (calc-prelude-map (env-world env) (env-package env)))))

(defmethod lookup (var (env macro-env))
  (if (member var (shadows env))
      (cons :var var) ;; if shadowed, return variable
      ;; 
      (let* ((path (gethash var (path-map env)))
             (val (and path (lookup-path path (env-world env)))))
        ;; check path not null
        (unless path (error 'variable-not-found :missing-var var :env env))

        (if (viv:comptime-p val)
            (cons :val val)
            (cons :var path)))))

(defmethod insert (var val (env macro-env))
  (make-instance 'macro-env
                 :locals (acons var val (locals env))
                 :env-package (env-package env)))

(defmethod copy ((env macro-env))
  (make-instance 'macro-env
                 :shadows (shadows env)
                 :world (env-world env)
                 :package (env-package env)
                 :module (env-module env)))

(defun shadow-var (var env)
  "Hide definition of VAR in ENV"
  (make-instance 'macro-env
                 :shadows (cons var (shadows env))
                 :world (env-world env)
                 :package (env-package env)
                 :module (env-module env)))

(defun shadow-vars (vars env)
  "Hide definition of all variables in VARS in ENV"
  (make-instance 'macro-env
                 :shadows (append vars (shadows env))
                 :package (env-package env)
                 :module (env-module env)
                 :world (env-world env)))

(defun shadow-var-inplace (var env)
  (setf (shadows env) (cons var (shadows env))))






;; Helper functions
;; Convert a symbol to a path based on current module/package 
(declaim (ftype (function (viv:world keyword list) hash-table) calc-prelude-map))
(defun calc-path-map (world package-name module-name)
  (let* ((path-map (calc-prelude-map world package-name))
         (package (gethash package-name (packages world)))
         (module (lookup-package-path module-name package)))
    (loop
      with prefix = (cons package-name module-name)
      for key being each hash-key of (fields module)
          do (setf (gethash key path-map) (append prefix (list key))))
    path-map))

(declaim (ftype (function (viv:world keyword) hash-table) calc-prelude-map))
(defun calc-prelude-map (world package-name)
  (loop with package = (gethash package-name (packages world))
        with map = (make-hash-table)
        for path in (viv:prelude package)
        for module = (viv:fields (lookup-path path world))
        do (loop for key being each hash-key of module
                 do (setf (gethash key map) (append path (list key))))
        finally (return map)))


(defun get-path (var env))

(declaim (ftype (function (list viv:world) t) lookup-path))
(defun lookup-path (path world)
  (lookup-package-path
   (cdr path)
   (gethash (car path) (viv:packages world))))

(defun lookup-package-path (path package)
  (loop for name in path
        for current = (gethash name (viv:modules package))
          then (gethash name (viv:fields current))
        finally (return current)))
