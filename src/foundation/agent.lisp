(in-package :foundation)

(defclass system ()
  ((agents
    :initform nil)))

(defclass agent ()
  ((mailbox
    :initarg :mailbox)
   (thread
    :initarg :thread)
   (name
    :initarg :name
    :accessor name)))

(defclass mailbox ()
  ((lock
    :accessor lock
    :initform (bt2:make-lock))
   (condvar
    :accessor condvar
    :initform (bt2:make-condition-variable))
   (condvarlock
    :accessor condvarlock
    :initform (bt2:make-lock))
   (messages
    :accessor messages
    :initform nil)))



(declaim (ftype (function (system function &key (name string)) agent) make-agent)) 
(defun make-agent (system function &key name)
  (let* ((mailbox (make-instance 'mailbox))
         (thread (bt2:make-thread (lambda () (agent-mainloop mailbox function)) :name name))
         (agent (make-instance 'agent :mailbox mailbox :name name :thread thread)))
    (with-slots (agents) system (push agent agents))
    agent))


(declaim (ftype (function (t agent) null) send-message)) 
(defun send-message (message agent)
  (with-slots (mailbox) agent
    (bt2:with-lock-held ((lock mailbox))
      (push message (messages mailbox)))
    (bt2:condition-notify (condvar mailbox))))


(declaim (ftype (function (mailbox function) t) agent-mainloop)) 
(defun agent-mainloop (mailbox function)
  "Run in a separate thread. Monitor incoming mailbox for messages, "
  (loop do
    (let ((message (bt2:with-lock-held ((lock mailbox))
                     (if (messages mailbox) 
                         (cons (pop (messages mailbox)) nil)))))
      (cond
        ((null message)
         (bt2:with-lock-held ((condvarlock mailbox))
           (bt2:condition-wait (condvar mailbox) (condvarlock mailbox))))
        ((eq (car message) :kill) (return (make-instance 'world)))
        (t (funcall function (car message)))))))
