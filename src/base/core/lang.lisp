(in-package :viv-base)

(defun builtin-for (tipe args)
  (declare (ignore type))
  (let ((iterators (car args))
        (clauses (remove-if-not #'clausep (cdr args)))
        (exprs (remove-if #'clausep (cdr args))))
    (error "for not implemented!")))

;; References

;; Unique atoms & nominal types
(defun unique (x)
  (pure (mk-unique (str x))))
  

(defun make-lang-module ()
  (let* ((lang-entries (make-hash-table))
         (lang-module (make-instance 'viv:viv-module
                                     :name "lang"
                                     :fields lang-entries)))

    (setf (gethash :|macro| lang-entries) (mk-former :macro))
    (setf (gethash :|quote| lang-entries) (mk-former :quote))

    (setf (gethash :|def| lang-entries) (mk-former :define))
    (setf (gethash :|if| lang-entries) (mk-former :if))

    ;; implicits
    (setf (gethash :|implicit| lang-entries) (mk-former :implicit))

    ;; functional
    (setf (gethash :|fn| lang-entries)(mk-former :function))
    (setf (gethash :|app| lang-entries) (mk-former :app))

    ;; imperative
    (setf (gethash :|with-jumps| lang-entries) (mk-former :with-jumps))
    (setf (gethash :|jump-to| lang-entries) (mk-former :jump-to))
    ;; TODO: add local which creates 'ref-like' value that is locally allocated 
    ;; (setf (gethash :|local| lang-entries) (mk-former :local)) 
    (setf (gethash :|seq| lang-entries) (mk-former :seq)) ;; TODO: make into macro
    (setf (gethash :|for| lang-entries)
          (make-instance 'viv-macro :body (builtin #'builtin-for 2)))
    ;; TODO: Loop Macro
    ;; 
    ;; TODO: (block lbl e1) → (with-jumps e1 (lbl))

    (setf (gethash :|object| lang-entries) (mk-former :object))
    (setf (gethash :|actor| lang-entries) (mk-former :actor))
    (setf (gethash :|sets| lang-entries) (mk-former :slot-set))

    ;; TOOD: rewrite ref, get, set as objects!
    ;; (setf (gethash :|ref!| lang-entries) *builtin-ref*)
    ;; (setf (gethash :|get!| lang-entries) *builtin-get*)
    ;; (setf (gethash :|set!| lang-entries) *builtin-set*)

    ;; logic
    (setf (gethash :|query| lang-entries) (mk-former :query))
    (setf (gethash :|rule|  lang-entries) (mk-former :rule))
    (setf (gethash :|pred|  lang-entries) (mk-former :predicate))
    (setf (gethash :|fresh| lang-entries) (mk-former :fresh))
    ;;(setf (gethash :|amb|   lang-entries) (mk-former :or))

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

    ;; Types
    ;; 

    lang-module))

