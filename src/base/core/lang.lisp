(in-package :viv-base)



;; References
(defvar *builtin-ref*
  (make-instance 'primop
                 :arity 1
                 :fun (lambda (x) (pure (make-instance 'viv-ref :element x)))))

(defvar *builtin-get*
  (make-instance 'primop
                 :arity 1
                 :fun (lambda (ref) (pure (element ref)))))

(defvar *builtin-set*
  (make-instance 'primop
                 :arity 2
                 :fun (lambda (ref val) (pure (progn (setf (element ref) val) nil)))))

;; Unique atoms & nominal types
(defun unique
    (x) (pure (mk-unique (str x))))
  

(defun make-lang-module ()
  (let* ((lang-entries (make-hash-table))
         (lang-module (make-instance 'viv:viv-module
                                     :name "lang"
                                     :fields lang-entries)))

    (setf (gethash :|macro| lang-entries) (mk-former :macro))
    (setf (gethash :|quote| lang-entries) (mk-former :quote))

    (setf (gethash :|if| lang-entries) (mk-former :if))

    (setf (gethash :|def| lang-entries) (mk-former :define))

    ;; functional
    (setf (gethash :|fn| lang-entries)(mk-former :function))
    (setf (gethash :|app| lang-entries) (mk-former :app))

    ;; imperative
    (setf (gethash :|with-jumps| lang-entries) (mk-former :with-jumps))
    (setf (gethash :|jump-to| lang-entries) (mk-former :jump-to))
    (setf (gethash :|seq| lang-entries) (mk-former :seq)) ;; TODO: make into macro
    ;; TODO: Loop Macro
    ;; TODO: (block lbl e1) → (with-jumps e1 (lbl))

    (setf (gethash :|object| lang-entries) (mk-former :object))
    (setf (gethash :|actor| lang-entries) (mk-former :actor))
    (setf (gethash :|sets| lang-entries) (mk-former :slot-set))

    ;; TOOD: rewrite ref, get, set as objects!
    (setf (gethash :|ref!| lang-entries) *builtin-ref*)
    (setf (gethash :|get!| lang-entries) *builtin-get*)
    (setf (gethash :|set!| lang-entries) *builtin-set*)

    ;; logic
    (setf (gethash :|query| lang-entries) (mk-former :query))
    (setf (gethash :|rule| lang-entries) (mk-former :rule))
    (setf (gethash :|pred| lang-entries) (mk-former :predicate))

    ;; other
    (setf (gethash :|;| lang-entries) (mk-former :destructor))
    (setf (gethash :|producer| lang-entries) (mk-former :corecursor))

    (setf (gethash :|:| lang-entries) (mk-former :constructor))
    (setf (gethash :|match| lang-entries) (mk-former :recursor))


    (setf (gethash :|.| lang-entries) (mk-former :projector))
    (setf (gethash :|struct| lang-entries) (mk-former :structure))

    (setf (gethash :|shift| lang-entries) (mk-former :shift))
    (setf (gethash :|reset| lang-entries) (mk-former :reset))


    (setf (gethash :|unique| lang-entries) (builtin #'unique 1))

    lang-module))

