(load (sb-ext:posix-getenv "ASDF"))
(asdf:load-system :server)
(sb-ext:save-lisp-and-die #P"server-bin" :toplevel #'server:start :executable t)
