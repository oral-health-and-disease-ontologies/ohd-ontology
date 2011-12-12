;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

(defvar cl-user::*cdt-dist* "needs to be defined")

(setf (logical-pathname-translations "cdtcode")
      `(
	("cdtdist;**;*.*" ,(format nil "~a/**/*.*" cl-user::*cdt-source*))
	("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild
				  :defaults *load-pathname*))))

(in-package :asdf)

(defsystem :cdtcode
  :name "CDT code ontology generator"
  :author "Bill Duncan"
  :license "BSD"
  :components
  ((:module "source"
	    :pathname ""
 	    :components
	    ((:file "write-cdt-xml")
	     (:file "write-cdtcode-ont")
	     )
 	    :depends-on ())
   )
  :depends-on (owl2 inspect xmls))

;;;; eof
