(in-package :viv)


;; Take an input stream; convert to a concrete syntax tree
;; The current parse-state represents row/col number
;; 


(defun start-state () (cons 0 0))

(defun parse (stream)
  (parse-any stream (start-state)))

(defun special-p (char)
  (some (lambda (c) (char= char c)) (list #\( #\) #\[ #\] #\{ #\} #\:)))

(defun parse-any (stream state)
  (let ((head (peek-char t stream)))
    (cond
      ((char= head #\()    (parse-list stream state))
      ((char= head #\[)    (parse-stack stream state))
      ((char= head #\{)    (parse-logic stream state))
      ((char= head #\")    (parse-string stream state))
      ((char= head #\:)    (parse-ival stream state))
      ((char= head #\!)
       (read-char t stream)
       (make-instance 'concrete-node
                      :type :expr
                      :contents (list (parse-any stream state))))
      ((digit-char-p head) (parse-number stream state))
      ((not (special-p head)) (parse-symbol stream state))
      (t (error "Bad Parse")))))

(defun parse-list (stream state)
  (read-char nil stream)
  (loop 
    for head = (peek-char t stream) then (peek-char t stream)
    do (when (char= #\) head)
         (read-char t stream)
         (return
           (make-instance 'concrete-node :type :expr :contents exprs)))
    collect (parse-any stream state) into exprs))

(defun parse-ival (stream state)
  (read-char nil stream)
  (let ((next (parse-any stream state)))
    (assert (typep (contents next) 'keyword))
    (make-instance 
     'concrete-atom
     :contents (make-instance 'viv-ival :name (contents next)))))

(defun parse-stack (stream state)
  (read-char nil stream) ;; remove leading '['
  (loop 
    for head = (peek-char t stream) then (peek-char t stream)
    do
       (when (char= #\] head)
         (read-char t stream)
         (return
           (make-instance 'concrete-node :type :stack :contents exprs)))
    collect (parse-any stream state) into exprs))

(defun parse-logic (stream state)
  (read-char nil stream)
  (loop 
    for head = (peek-char t stream) then (peek-char t stream)
    do
       (when (char= #\} head)
         (return
           (make-instance 'concrete-node :type :stack :contents exprs)))
    collect (parse-any stream state) into exprs))

(defun list-to-num (lst)
  (make-instance
   'concrete-atom
   :contents
   (loop
     for num in (reverse lst)
     and digit = 1 then (* 10 digit)
     sum (* num digit))))

(defun parse-number (stream state)
  (list-to-num 
    (loop 
      do
         (unless (digit-char-p (peek-char nil stream)) (return nums))
      collect (digit-char-p (read-char nil stream)) into nums)))

(declaim (ftype (function (stream t) concrete) parse-string))  
(defun parse-string (stream state)
  (declare (ignore state))
  (read-char nil stream)
  (make-instance
   'concrete-atom
   :contents
   (coerce
    (loop 
      do
         (unless (char/= #\" (peek-char nil stream))
           (read-char nil stream)
           (return chars))
      collect (read-char nil stream) into chars)
    'string)))

(defun parse-symbol (stream state)
  (make-instance 
   'concrete-atom
   :contents
   (loop 
     for head = (peek-char nil stream) then (peek-char nil stream)
     do 
        (when (or (sb-unicode:whitespace-p head) (special-p head))
          (return (intern (coerce chars 'string) :keyword)))
     collect (read-char stream) into chars)))
