(defpackage foundation
  (:nicknames :fd)
  (:use :cl :trivia)
  (:export
   ;; function
   :->
   :->>
   :id
   :alist->table

   ;; sequecne
   :group-by
   :iota

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
   :mwith-jumps
   :bind
   :seq
   :pure
   :mdo
   :mapM
   ))
