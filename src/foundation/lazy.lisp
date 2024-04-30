(in-package :foundation)


(defmacro freeze (val)
  `(lambda () ,val))

(defmacro thaw (val) `(val))
