
;; call the asdf to load the general-library
;; this is in a separate lisp file so that other operations can 
;; more easily be added (if need be)

;; not sure if I need to compile before load
(asdf:oos 'asdf:compile-op :general-library)
(asdf:oos 'asdf:load-op :general-library)
