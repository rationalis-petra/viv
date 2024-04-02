(in-package :viv)

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




(defgeneric run (monad))

(defmethod run ((monad monad-pure))
  (list :value (val monad)))

(defmethod run ((bind monad-bind))
  (let ((result (run (monad bind))))
    (case (first result)
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
      ;; No delimited continuation; continue 
      (:value (run (funcall (fun bind) (elt result 1)))))))

(defmethod run ((shift monad-shift))
  (list :delcont (fun shift) (lambda (v) (make-instance 'monad-pure :val v))))

(defmethod run ((reset monad-reset))
  (let ((result (run (inner reset))))
    (case (first result)
      (:delcont (run (funcall (elt result 1) (elt result 2))))
      (:value result))))



(defun mreset (inner) (make-instance 'monad-reset :inner inner))
(defun mshift (fun) (make-instance 'monad-shift :fun fun))
(defun bind (monad fun) (make-instance 'monad-bind :monad monad :fun fun))

(declaim (ftype (function (monad monad) monad) seq))
(defun seq  (m1 m2) (make-instance 'monad-bind :monad m1 :fun (lambda (x) m2)))
(defun pure (val) (make-instance 'monad-pure :val val))

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

(defparameter *test-comp* 
  (mapM
   (lambda (x) (pure (+ x 2)))
   (list 1 2 3)))

