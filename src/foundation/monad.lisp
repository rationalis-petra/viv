(in-package :foundation)

;; Eval monad
;; For now, we get delimited continuations

(defclass monad () ())

(defclass monad-pure (monad)
  ((val
    :accessor val
    :initarg :val)))

(defclass monad-bind (monad)
  ((monad
    :accessor monad
    :initarg :monad)
   (fun
    :accessor fun
    :initarg :fun)))

(defclass monad-shift (monad)
  ((fun
    :accessor fun
    :initarg :fun)))

(defclass monad-reset (monad)
  ((inner
    :accessor inner
    :initarg :inner)))

(defclass monad-with-jumps (monad)
  ((body
    :accessor body
    :initarg :body)
   (targets
    :accessor targets
    :initarg :targets)))

(defclass monad-jump-to (monad)
  ((target
    :accessor target
    :initarg :target)))

(defgeneric run (monad)
  (:documentation "Generic function which runs the delimited continuation
  computation represented by MONAD"))

(defmethod run ((monad monad-pure))
  (list :value (val monad)))

(defmethod run ((bind monad-bind))
  (let ((result (run (monad bind))))
    (case (first result)
      ;; No delimited continuation; continue 
      (:value (run (funcall (fun bind) (elt result 1))))
      ;; delimited continuation
      ;; built up a new continuation
      (:delcont
       (list
        :delcont
        (elt result 1)
        (lambda (v)
          (make-instance 'monad-bind 
                         :monad (funcall (elt result 2) v)
                         :fun (fun bind)))))
      ;; goto tag - keep unwinding stack
      (:go result))))

(defmethod run ((shift monad-shift))
  (list :delcont (fun shift) (lambda (v) (make-instance 'monad-pure :val v))))

(defmethod run ((reset monad-reset))
  (match (run (inner reset))
    ((list :value val) (list :value val))
    ((list :delcont fun cont) (funcall fun cont))
    ((list :go set tag) (list :go set tag))))


;; With-jumps : initial-body
;;              targets → take jump functions, return monad
;;              
(defmethod run ((with-jumps monad-with-jumps))
  (let* ((jump-id (gensym "jump-stack-marker"))
         (jumps (mapcar
                 (lambda (idx)
                   (make-instance 'monad-jump-to :target (list :go jump-id idx)))
                 (iota (length (targets with-jumps)))))
         (targets (mapcar (lambda (target) (funcall target jumps)) (targets with-jumps)))
         (current-monad (funcall (body with-jumps) jumps)))

    ;; Loop while there is a body to run
    (loop while current-monad 
          do (match (run current-monad)
               ((list :delcont fun cont)
                (return (list :delcont fun
                              (lambda (v)
                                (make-instance 'monad-with-jumps
                                               :body (funcall cont v)
                                               :targets (targets with-jumps))))))
               ((list :value val)
                (return (list :value val)))
               ((list :go set tag)
                (if (eq set jump-id)
                    (setf current-monad (elt targets tag))
                    (return (list :go set tag))))))))

(defmethod run ((jump-to monad-jump-to))
  (target jump-to))

(defun mreset (inner) (make-instance 'monad-reset :inner inner))
(defun mshift (fun) (make-instance 'monad-shift :fun fun))
(defun bind (monad fun) (make-instance 'monad-bind :monad monad :fun fun))
(defun mwith-jumps (body jumps)
  (make-instance 'monad-with-jumps :body body :targets jumps))

(declaim (ftype (function (monad monad) monad) seq))
(defun seq  (m1 m2)
  "Create a delimited continuation monad representing running first M1 and then M2"
  (make-instance 'monad-bind :monad m1 :fun (lambda (x) m2)))
(defun pure (val)
  "Create a (delimited continiation) monad that does nothing and returns VAL"
  (make-instance 'monad-pure :val val))

(defmacro mdo (&rest computations)
  (labels ((gofun (terms)
           (if (consp terms)
               (let ((term  (car terms))
                     (rest (gofun (cdr terms))))
                 (cond 
                   ((and (listp term)
                         (eq 'bind (car term)))
                    `(bind ,(elt term 2)
                           (lambda (,(elt term 1))
                             ,rest)))
                   (rest `(seq ,term ,rest))
                   (t term)))
               nil)))
    (gofun computations)))

;; Monadic Map
(defgeneric mapM (func sequence))
(defmethod mapM (func (sequence list))
  (labels ((rec (l)
               (if l
                   (mdo
                    (bind hd (funcall func (car l)))
                    (bind tl (rec (cdr l)))
                    (pure (cons hd tl)))
                   (pure nil))))
    (rec sequence)))

