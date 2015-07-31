;;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

;; setup a logical pathname to reference files in the project
(setf (logical-pathname-translations "vdw")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild
				  :defaults *load-pathname*))))

;; register the  development-library system and load the asd file
;; the asdf system calls for compiling and loading are found in development-library/load-development-library.lisp
;; this lisp file is reference in the development-library module below
(when (not (member (truename "vdw:development-library") asdf:*central-registry* :test #'equalp))
  (push (truename "vdw:development-library") asdf:*central-registry*))
(load (truename "vdw:development-library;development-library.asd"))

(defsystem :vdw
  :serial t
  :components
  ((:module development-library
	   :pathname "development-library"
	   :components
	   ((:file "load-development-library"))) ;; end general library

   (:module macros
	    :pathname "macros"
	    :components
	    (
	     
	     )
	    ) ;; end macros

   (:module classes
	    :pathname "classes"
	    :components
	    (
	     
	     )
	    ) ;; end classes
   
   (:module utils
	    :pathname "utils"
	    :components
	    ((:file "label-sources")
	     (:file "iterator-utils")
	     (:file "uri-utils")
	     (:file "data-utils")
	     (:file "cdt-code-utils")
	     (:file "fma-utils")
	     (:file "restoration-material-utils")
	     )
	    );; end utils
   
   
   (:module translate
	    :pathname "translate"
	    :components
	    (
	     
	     )
	    ) ;; end translate
   ) ;; end modules
  
  :depends-on (owl2 read-ms-docs)) ;; end defsystem


;;;; eof
