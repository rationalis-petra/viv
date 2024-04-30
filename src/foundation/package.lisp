(defpackage foundation
  (:nicknames :fd)
  (:use :cl :trivia)
  (:export
   ;; function
   :->
   :->>
   :id
   :alist->table
   ;lazy
   :freeze
   :thaw

   ;; sequecne
   :group-by
   :iota
   :maptable
   :make-ht
   :alist->ht

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
