;; needs "/Applications/SQLAnywhere12/System/lib32"
;;  added to DYLD_LIBRARY_PATH so it can find libdbjdbc12.jnilib and dependencies.
;; for now add (setenv "DYLD_LIBRARY_PATH" (concatenate 'string (getenv "DYLD_LIBRARY_PATH") 
;; ":/Applications/SQLAnywhere12/System/lib32")) to your .emacs
;; (#"load" 'System "/Applications/SQLAnywhere12/System/lib32/libdbjdbc12.jnilib")  
;; fails there is no easy way to update DYLD_LIBRARY_PATH within a running java instance. POS.
(defparameter *results-ht* nil)

(add-to-classpath "/Applications/SQLAnywhere12/System/java/sajdbc4.jar")

(defun get-filling-ont (&key iri ont-iri patient-id file-name print-results return-rows)
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(column-names nil)
	(count 0)
	(rows nil)
	(url nil))
    
    ;; set default base and ontology iri's 
    (when (null iri) (setf iri "http://purl.obolibrary.org/obo/"))
    (when (null ont-iri) (setf ont-iri "http://purl.obolibrary.org/obo/fillings.owl"))

    ;; set up connection string and query
    (setf url"jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password=4561676C65536F6674")
    (setf query (get-amalgam-query))

    (with-ontology ont (:collecting t :base iri :ontology-iri ont-iri)
	((unwind-protect
	      (progn
		;; connect to db and get data
		(setf connection (#"getConnection" 'java.sql.DriverManager url))
		(setf statement (#"createStatement" connection))
		(setf results (#"executeQuery" statement query))
	   	
		;; import the ohd ontology
		(as `(imports (make-uri "http://purl.obolibrary.org/obo/ohd/dev/ohd.owl")))
		(loop while (#"next" results) do
		     (as (get-amalgam-axioms 
			  iri
			  (#"getString" results "patient_id")
			  (#"getString" results "tran_date")
			  (#"getString" results "description")
			  (#"getString" results "tooth")
			  (#"getString" results "surface")
			  (#"getString" results "ada_code")
			  (#"getString" results "ada_code_description")))))
	   
	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement))))

      ;; return the ontology
      ont)))

(defun get-amalgam-axioms (iri patient-id tran-date description 
			       tooth surface ada-code ada-code-description)
      (let ((axioms nil)
	    (uri nil))
	
	;; patient-id is an instance of !ohd:'dental patient'
	(setf uri (make-uri (str+ iri patient-id)))
	(push `(declaration (named-individual ,uri)) axioms)
	(push `(class-assertion !obo:OHD_0000012 ,uri) axioms)

	;; tran_date todo..
	(setf uri (make-uri (str+ iri tran-date)))

	;; description
	(setf uri (make-uri (str+ iri description)))

	;; tooth
	(setf uri (make-uri (str+ iri tooth)))

	;; surface
	(setf uri (make-uri (str+ iri surface)))


	;; ada_code 
	(setf uri (make-uri (str+ iri ada-code)))

	;; ada_code_description
	(setf uri (make-uri (str+ iri ada-code-description)))

	;; return axioms
	axioms))
  

(defun get-amalgam-query ()
  (str+ 
   "select top 10 * from patient_history "
   "where ada_code in ('D2140', 'D2150', 'D2160', 'D2161') "
   "and table_name = 'transactions' ")
  )

(defun str+ (&rest values)
  "A shortcut for concatenting a list of strings. Code found at:
http://stackoverflow.com/questions/5457346/lisp-function-to-concatenate-a-list-of-strings
Parameters:
  values:  The string to be concatenated.
Usage:
  (str+ \"string1\"   \"string1\" \"string 3\" ...)
  (str+ s1 s2 s3 ...)"

  ;; the following make use of the format functions
  ;;(format nil "~{~a~}" values)
  ;;; use (format nil "~{~a^ ~}" values) to concatenate with spaces
  ;; this may be the simplist to understant...
  (apply #'concatenate 'string values))
