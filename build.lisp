(load "system.asd")
(ql:quickload :flowfps)

(sb-ext:save-lisp-and-die "flowfps" :executable t :toplevel 'flowfps:cli)
