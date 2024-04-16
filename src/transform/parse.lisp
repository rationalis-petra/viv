(in-package :viv)


;; Take an input stream; convert to a concrete syntax tree
;; 


(defun parse (stream)
  (parse-any stream))

(defun special-p (char)
  (some (lambda (c) (char= char c)) (list #\( #\) #\[ #\] #\{ #\} #\:)))

(defun parse-any (stream)
  (let ((head (peek-char t stream)))
    (cond
      ((char= head #\()    (parse-node stream :expr #\)))
      ((char= head #\[)    (parse-node stream :stack #\]))
      ((char= head #\{)    (parse-node stream :logic #\}))
      ((char= head #\")    (parse-string stream))
      ((char= head #\:)
       (parse-symbolic
        stream
        (lambda (next)
          (make-instance 'viv-ival :name (contents next)))))
      ((char= head #\.)
       (parse-symbolic
        stream
        (lambda (next) (make-instance 'viv-destructor :field (contents next))))) 
      ((char= head #\!)
       (read-char t stream)
       (make-instance 'concrete-node
                      :type :expr
                      :contents (list (parse-any stream))))
      ((digit-char-p head) (parse-number stream))
      ((not (special-p head)) (parse-symbol stream))
      (t (error "Bad Parse")))))

(declaim (ftype (function (stream keyword character) concrete-node) parse-node))
(defun parse-node (stream type terminator)
  (read-char nil stream)
  (loop 
    for head = (peek-char t stream) then (peek-char t stream)
    do (when (char= terminator head)
         (read-char t stream)
         (return
           (make-instance 'concrete-node :type type :contents exprs)))
    collect (parse-any stream) into exprs))

(defun parse-symbolic (stream fun)
  (read-char nil stream)
  (let ((next (parse-any stream)))
    (assert (typep (contents next) 'keyword))
    (make-instance 
     'concrete-atom
     :contents (funcall fun next))))

(defun list-to-num (lst)
  (make-instance
   'concrete-atom
   :contents
   (loop
     for num in (reverse lst)
     and digit = 1 then (* 10 digit)
     sum (* num digit))))

(defun parse-number (stream)
  (list-to-num 
    (loop 
      do
         (unless (digit-char-p (peek-char nil stream)) (return nums))
      collect (digit-char-p (read-char nil stream)) into nums)))

(declaim (ftype (function (stream) concrete) parse-string))  
(defun parse-string (stream)
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

(defun parse-symbol (stream)
  (make-instance 
   'concrete-atom
   :contents
   (loop 
     for head = (peek-char nil stream) then (peek-char nil stream)
     do 
        (when (or (sb-unicode:whitespace-p head) (special-p head))
          (return (intern (coerce chars 'string) :keyword)))
     collect (read-char stream) into chars)))
