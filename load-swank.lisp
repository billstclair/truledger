(load "slime/swank-loader.lisp")
(push `(*package* . ,*package*) swank::*default-worker-thread-bindings*)

;;(swank-loader::load-swank)

(swank:create-server :port 4006 :dont-close t)
