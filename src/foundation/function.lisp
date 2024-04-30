(in-package :foundation)

(defun id (x) x)

(defmacro -> (val &rest terms)
  (reduce (lambda (val lst)
            (cons (car lst) (cons val (cdr lst))))
          terms
          :initial-value val))

(defmacro ->> (val &rest terms)
  (reduce (lambda (val lst) (append lst (list val))) terms
          :initial-value val))

(defun alist->table (alist)
  (loop with out = (make-hash-table)
        for cell in alist
        do (setf (gethash (car cell) out) (cdr cell))
        finally (return out)))

