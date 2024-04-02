(in-package :viv)




(defvar *world-state* nil)
(defvar *world-agent* nil)
(defvar *prime-system* (make-instance 'system))

(defun start-world-agent  ()
  "Make a new thread hosting the world agent. The *world-agent-thread* and
*world-agent* variables should be set appropriately"
  (setf *world-state* (make-instance 'world))
  (setf *world-agent* (make-agent
                       *prime-system*
                       (lambda (message) (world-process-message message world))
                       :name "Viv World")))


(defun run-line ()
  (let* ((concrete (parse *standard-input*)))
    (send-message (list :eval concrete) *world-agent*)))

(defun run-abs-line ()
  (let* ((concrete (parse *standard-input*))
         (abstract (to-abstract (default-environment) concrete)))
    (format t "Concrete: ~A~%" concrete)
    (format t "abs: ~A~%" abstract)
    (eval-term abstract)))

(defun abs-line ()
  (let* ((concrete (parse *standard-input*)))
    (format t "Concrete: ~A~%" concrete)
    (to-abstract (default-environment) concrete)))
