(defpackage foundation
  (:nicknames :fd)
  (:use :cl)
  (:export
   ;; function
   :->
   :->>
   :id
   :alist->table

   ;; sequecne
   :group-by

   ;; agent 
   :system
   :agents

   :agent
   :name
   :thread
   :mailbox

   ;; monad
   :monad
   :run
   :mreset
   :mshift
   :bind
   :seq
   :pure
   :mdo
   :mapM
   ))
