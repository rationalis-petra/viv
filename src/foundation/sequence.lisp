(in-package :foundation)


(defun group-by (list &key (key #'car) (value #'cdr) (key-fn #'identity) (test #'equal))
  "Groups the list into an alist using the key function and value function to group by key,
with a list of all values for that key.

key is used to determine the key in the a-list
value is used to determin the value in the a-list
key-fn is passed as the :key to assoc
test is passed as the :test to assoc

eg: (group-by '((a 1 2) (a 3 4) (b 5 6)))
=> ((A (1 2) (3 4)) (B (5 6)))"
  ;; we keep 2 alist -  ( key . value-list-head ) & ( key . value-list-tail )
  ;; so we can collect at the end (which saves us infinitesimal time and space)
  (loop
    with tails = nil
    with results = nil

    for i in list
    for k = (funcall key i)
    for v = (cons (funcall value i) nil)
    for cell = (assoc k tails :test test :key key-fn)
    do (cond
         (cell (setf (cddr cell) v
                     (cdr cell) v))
         (t               ;; dont reuse this cons cell, we want two distinct ones
          (push (cons k v) results)
          (push (cons k v) tails)))
    finally (return results)))


(defun iota (end)
  (loop for i from 0 to (- end 1)
        collect i))
