;; needs "/Applications/SQLAnywhere12/System/lib32"
;;  added to DYLD_LIBRARY_PATH so it can find libdbjdbc12.jnilib and dependencies.
;; for now add (setenv "DYLD_LIBRARY_PATH" (concatenate 'string (getenv "DYLD_LIBRARY_PATH") 
;; ":/Applications/SQLAnywhere12/System/lib32")) to your .emacs
;; (#"load" 'System "/Applications/SQLAnywhere12/System/lib32/libdbjdbc12.jnilib")  
;; fails there is no easy way to update DYLD_LIBRARY_PATH within a running java instance. POS.
(defparameter *results-ht* nil)

;; the global variables are used to generate unique iri's
(defparameter *iri* nil)
(defparameter *iri-count* nil)

(add-to-classpath "/Applications/SQLAnywhere12/System/java/sajdbc4.jar")

(defun get-filling-ont (&key iri ont-iri patient-id file-name print-results)
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(count 0))
    
    ;; set default base and ontology iri's 
    (when (null iri) (setf iri "http://purl.obolibrary.org/obo/individuals/"))
    (when (null ont-iri) (setf ont-iri "http://purl.obolibrary.org/obo/fillings.owl"))
    
    ;; set global variables 
    (setf *iri* iri)
    (setf *iri-count* 1)
    
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
			  (#"getString" results "patient_id")
			  (#"getString" results "tran_date")
			  (#"getString" results "description")
			  (#"getString" results "tooth_data")
			  (#"getString" results "surface")
			  (#"getString" results "ada_code")
			  (#"getString" results "ada_code_description")))
		     (incf count)))
	   
	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement))))

      ;; return the ontology
      ont)))

(defun get-amalgam-axioms (patient-id tran-date description 
			   tooth-data surface ada-code ada-code-description)
  (let ((axioms nil)
	(patient-uri nil)
	(patient-role-uri nil)
	(tooth-uri nil)
	(tooth-role-uri nil)
	(amalgam-uri nil)
	(amalgam-retoration-uri nil)
	(teeth-list nil))
	
    ;; get axioms abou the patient
    ;;(push (get-patient-axioms iri patient-id) axioms) ;; this doesn' work.. why not?
    (setf patient-uri (get-iri))
    (setf patient-role-uri (get-iri))
    (setf axioms (get-patient-axioms patient-uri patient-role-uri patient-id))

    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-teeth tooth-data))
    (loop for tooth in teeth-list do
         
         ;;;;  declare instances of participating entitie ;;;;

         ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-uri (get-iri))
	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 (push `(class-assertion !obo:FMA_12516 ,tooth-uri) axioms)	     
         
         ;; declare instance of !ohd:'tooth to be filled role'
	 (setf tooth-role-uri (get-iri))
	 (push `(declaration (named-individual ,tooth-role-uri)) axioms)
	 (push `(class-assertion !obo:OHD_0000008 ,tooth-role-uri) axioms)

         ;; declare instance of amalgam (!ohd:amalgam) for tooth
	 (setf amalgam-uri (get-iri))
	 (push `(declaration (named-individual ,amalgam-uri)) axioms)
	 (push `(class-assertion !obo:OHD_0000001 ,amalgam-uri) axioms)	 

         ;; declare instance of amalgam restoration (!ohd:'amalgam filling restoration')
	 (setf amalgam-retoration-uri (get-iri))
	 (push `(declaration (named-individual ,amalgam-retoration-uri)) axioms)
	 (push `(class-assertion !obo:OHD_0000041 ,amalgam-retoration-uri) axioms)

	 ;; add date property !ohd:'occurence date' to 'amalgam filling restoration'
	 (push `(data-property-assertion !obo:OHD_0000015
					 ,amalgam-retoration-uri ,tran-date) axioms)

	 
 
         ;;;; relate instances ;;;;
       
         ;; 'tooth to be filled role' inheres in tooth
	 (push `(object-property-assertion !obo:BFO_0000052 
					   ,tooth-role-uri ,tooth-uri) axioms)

         ;; 'amalgam filling restoration' realizes 'tooth to be filled role'
	 (push `(object-property-assertion !obo:BFO_0000055
					   ,amalgam-retoration-uri ,tooth-role-uri) axioms)

         ;; 'amalgam filling restoration' has particpant tooth
	 (push `(object-property-assertion !obo:BFO_0000057 
					   ,amalgam-retoration-uri ,tooth-uri) axioms)
	 
      

         ;; 'amalgam filling restoration' has particpant amalgam
	 (push `(object-property-assertion !obo:BFO_0000057 
					   ,amalgam-retoration-uri ,amalgam-uri) axioms)

         ;; 'amalgam filling restoration' has particpant patient
	 (push `(object-property-assertion !obo:BFO_0000057 
					   ,amalgam-retoration-uri ,patient-uri) axioms)

       
	 
	 )
    
    ;; surface - for now I'll skip this
    ;;(setf uri (make-uri (str+ iri surface)))
	
    ;; description
    ;; for now put the descripton in a label
    ;;(setf uri (get-iri))
	
    ;; ada_code 
    ;;(setf uri (get-iri))

    ;; ada_code_description
    ;;(setf uri (get-iri))
	
    ;; tran_date todo..
    ;;(setf uri (get-iri))
    ;;(data-property-assertion !obo:OHD_0000015 !obo:OHD_0000020 tran-date)

    ;; return axioms
    axioms))
  

(defun get-patient-axioms (patient-uri patient-role-uri patient-id)
  "Returns a list of axioms about a patient that is identified by patient-id."
  (let ((axioms nil))
    ;; create instance/indiviual  patient
    (push `(declaration (named-individual ,patient-uri)) axioms) 

     ;; patient role is an instance of !ohd:'dental patient'
    (push `(class-assertion !obo:OHD_0000012 ,patient-role-uri) axioms)

    ;; patient fulfills !obi:'patient role'
    (push `(class-assertion !obo:OBI_0000093 ,patient-uri) axioms) 
    
    ;; 'patient role' inheres in patient
    (push `(object-property-assertion !obo:BFO_0000052 
				      ,patient-role-uri ,patient-uri) axioms)

    ;; add data property 'patient id' to patient
    (push `(data-property-assertion !obo:OHD_0000014
				    ,patient-uri ,patient-id) axioms)

    ;; add annotation about patient
    (push `(annotation-assertion !rdfs:label ,patient-uri ,(str+ "patient " patient-id)) axioms)
    
    ;; return axioms
    axioms))

(defun get-teeth (tooth-data)
  "Reads the tooth_data array and returns a list of tooth numbers referenced in the array."
  (let ((teeth-list nil))

    ;; verify that there are at least 32 tooth items in tooth-data
    (when (>= (length tooth-data) 32)
      (loop 
	 for tooth from 0 to 31 do
	   ;; when a postion in tooth-data is marked 'Y' add to teeth list
	   (when (or (equal (char tooth-data tooth) #\Y) 
		     (equal (char tooth-data tooth) #\y))
	     (push (1+ tooth) teeth-list))))

    ;; return list of teeth
    teeth-list))
	   

(defun get-iri ()
  "Returns a unique iri using 'make-uri'."
  (make-uri (get-iri-string)))
  

(defun get-iri-string ()
  "Returns a unique string that is used make a unique iri."
 (let ((iri-string nil))
   ;; build iri; note cdt uri is zero padded length 7: "~7,'0d"
   (setf iri-string (str+ "OHD_" (format nil "~7,'0d" *iri-count*)))
   
   ;; increment iri count
   (incf *iri-count*)

   ;; return new iri string
   (str+ *iri* iri-string)))

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
