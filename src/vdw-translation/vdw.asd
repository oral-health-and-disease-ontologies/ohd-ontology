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

;; register the  general-library system and load the asd file
;; the asdf system calls for compiling and loading are found in general-library/load-general-library.lisp
;; this lisp file is reference in the general-library module below
(when (not (member (truename "vdw:general-library") asdf:*central-registry* :test #'equalp))
  (push (truename "vdw:general-library") asdf:*central-registry*))
(load (truename "vdw:general-library;general-library.asd"))

(defsystem :vdw
  :serial t
  :components
  ((:module general-library
	   :pathname "general-library"
	   :components
	   ((:file "load-general-library"))) ;; end general library
   (:module project-library
	    :pathname "project-library"
	    :components
	    ((:module macros
		      :pathname "macros"
		      :components
		      ((:file "project-macros")))
	     (:module utils
		      :pathname "utils"
		      :components
		      ((:file "cdt-code-utils")
		       (:file "fma-utils")
		       (:file "restoration-material-utils")
		       (:file "project-label-sources")
		       (:file "project-iterator-utils")
		       (:file "project-uri-utils")
		       (:file "project-data-utils"))))) ; end project library
   (:module translate
	    :pathname "translate"
	    :components
	    (
	     ))) ;; end modules

    :depends-on (owl2 read-ms-docs)) ;; end defsystem


;;;; eof
