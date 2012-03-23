;;;; -*- Mode: LISP -*-

(in-package :asdf)

(setf (logical-pathname-translations "ohd")
      `(("imports;**;*.*" 
	 ,(make-pathname :directory 
			 (append 
			  (butlast (pathname-directory *load-pathname*))
			  '("ontology" "penn-ub-ohsu-prototype" "imports" :wild-inferiors))
			 :name :wild
			 :type :wild))
	("ontology;**;*.*" 
	 ,(make-pathname :directory 
			 (append 
			  (butlast (pathname-directory *load-pathname*))
			  '("ontology" "penn-ub-ohsu-prototype" :wild-inferiors))
			 :name :wild
			 :type :wild))
	("**;*.*" 
	 ,(make-pathname :directory 
			 (append 
			  (pathname-directory *load-pathname*)
			  '(:wild-inferiors))
			 :name :wild
			 :type :wild))))

(defsystem :r21
  :author "Alan Ruttenberg et al"
  :components
  ((:module early
	    :pathname ""
	    :components ((:file "label-source")
			 (:file "r21-utilities") ))
   (:module tools :pathname ""
	    :components
	    ((:file "queries")
	     (:file "eaglesoft-dental-patients")
	     (:file "eaglesoft-fillings")
	     (:file "eaglesoft-crowns")
	     ) 
	    :depends-on
	    (early)))
  :depends-on (owl2) )

;;;; eof
