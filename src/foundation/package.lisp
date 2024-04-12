(defpackage foundation
  (:nicknames :fd)
  (:use :cl)
  (:export
   ;; function
   :->
   :->>
   :id

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
