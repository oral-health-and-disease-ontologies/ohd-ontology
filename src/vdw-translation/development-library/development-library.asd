;;;; -*- Mode: LISP -*-
;;;;

(in-package :asdf)

;; setup a logical pathname to reference files in the development library
(setf (logical-pathname-translations "development-library")
      `(("**;*.*" ,(make-pathname :directory (append (pathname-directory *load-pathname*)
						     '(:wild-inferiors))
				  :name :wild
				  :type :wild
				  :defaults *load-pathname*))))

(defsystem :development-library
  :serial t
  :components
  ((:module classes
	    :pathname "classes"
	    :components
	    ((:file "data-resource-classes")
	     (:file "data-source-classes")
	     (:file "data-resultset-classes")
	     (:file "data-record-classes")
	     (:file "iterator-classes")
	     (:file "data-source-methods")
	     (:file "data-resultset-methods")
	     (:file "data-record-methods")
	     (:file "iterator-closure-functions")
	     (:file "iterator-methods")))
   (:module macros
	    :depends-on (classes)
	    :pathname "macros"
	    :components
	    ((:file "with-iterator-macro")
	     (:file "push-macros")))
   (:module utils
	    :depends-on (classes macros)
	    :pathname "utils"
	    :components
	    ((:file "hash-table-utils")
	     (:file "string-utils")
	     (:file "development-uri-utils")
	     (:file "jdbc-utils")))
   (:module test-scripts
	    :depends-on (classes macros utils)
	    :pathname "test-scripts"
	    :components
	    ((:file "test-file-iterator")
	     (:file "test-jdbc-iterator"))))

  ) ;; end defsytem
  

;;;; eof
