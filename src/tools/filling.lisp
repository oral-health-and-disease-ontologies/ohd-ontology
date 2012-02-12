;;****************************************************************
;; Instructions for being able to connect to database
;;
;; needs "/Applications/SQLAnywhere12/System/lib32"
;;  added to DYLD_LIBRARY_PATH so it can find libdbjdbc12.jnilib and dependencies.
;; for now add (setenv "DYLD_LIBRARY_PATH" (concatenate 'string (getenv "DYLD_LIBRARY_PATH") 
;; ":/Applications/SQLAnywhere12/System/lib32")) to your .emacs
;; (#"load" 'System "/Applications/SQLAnywhere12/System/lib32/libdbjdbc12.jnilib")  
;; fails there is no easy way to update DYLD_LIBRARY_PATH within a running java instance. POS.

;;****************************************************************
;; Database preparation for running this file. See dropbox:
;; "R21 Work/Data/Queries For Extracting Data/Temp table of union of existing services, patient condtions, transactions.txt"
;; Create and populate the two tables as instructed. 

(defparameter *results-ht* nil)

;; the global variables are used to generate unique iri's
(defparameter *iri* nil)
(defparameter *iri-count* nil)

;; for ease of use, set up some aliases to reference ohd classes
(def-uri-alias "fma_tooth" !obo:FMA_12516)
(def-uri-alias "dental_patient" !obo:OHD_0000012)
(def-uri-alias "patient_role" !obo:OBI_0000093)
(def-uri-alias "dental_finding" !obo:OHD_0000010)
(def-uri-alias "caries_finding" !obo:OHD_0000024)
(def-uri-alias "fractured_tooth_finding" !obo:OHD_0000030)
(def-uri-alias "missing_tooth_finding" !obo:OHD_0000026)
(def-uri-alias "dentition" !obo:OHD_0000027)
(def-uri-alias "caries" !obo:OHD_0000021)
(def-uri-alias "fractured_tooth" !obo:OHD_0000029)
(def-uri-alias "restoration_material" !obo:OHD_0000000)
(def-uri-alias "amalgam" !obo:OHD_0000001)
(def-uri-alias "gold" !obo:OHD_0000034)
(def-uri-alias "porcelain" !obo:OHD_0000035)
(def-uri-alias "resin" !obo:OHD_0000036)
(def-uri-alias "dental_exam" !obo:OHD_0000019)
(def-uri-alias "hard_tissue_exam" !obo:OHD_0000025)
(def-uri-alias "soft_tissue_exam" !obo:OHD_0000032)
(def-uri-alias "dental_visit" !obo:OHD_0000009)
(def-uri-alias "performing_a_dental_clinical_assement" !obo:OHD_0000011)
(def-uri-alias "procedure" !obo:OHD_0000002)
(def-uri-alias "endodontic_procedure" !obo:OHD_0000003)
(def-uri-alias "restorative_procedure" !obo:OHD_0000004)
(def-uri-alias "crown_restoration" !obo:OHD_0000033)
(def-uri-alias "filling_restoration" !obo:OHD_0000006)
(def-uri-alias "direct_restoration" !obo:OHD_0000037)
(def-uri-alias "amalgam_filling_restoration" !obo:OHD_0000041)
(def-uri-alias "resin_filling_restoration" !obo:OHD_0000042)
(def-uri-alias "indirect_restoration" !obo:OHD_0000038)
(def-uri-alias "gold_filling_restoration" !obo:OHD_0000039)
(def-uri-alias "procelain_filling_restoration" !obo:OHD_0000040)
(def-uri-alias "surgical_dental_procedure" !obo:OHD_0000044)
(def-uri-alias "surgical_procedure" !obo:OHD_0000005)
(def-uri-alias "tooth_to_be_restored_role" !obo:OHD_0000007)
(def-uri-alias "tooth_to_be_filled_role" !obo:OHD_0000008)
(def-uri-alias "has_participant" !obo:BFO_0000057)
(def-uri-alias "has_role" !obo:BFO_0000087)
(def-uri-alias "has_part" !<http://www.obofoundry.org/ro/ro.owl#has_part>)
(def-uri-alias "has_specified_output" !obi:OBI_0000299)
(def-uri-alias "inheres_in" !obo:BFO_0000052)
(def-uri-alias "is_about" !obo:IAO_0000136)
(def-uri-alias "is_part_of" !obo:BFO_0000050)
(def-uri-alias "realizes" !obo:BFO_0000055)
(def-uri-alias "occurrence_date" !obo:OHD_0000015)
(def-uri-alias "patient_ID" !obo:OHD_0000014)

(add-to-classpath "/Applications/SQLAnywhere12/System/java/sajdbc4.jar")

;; Assuming that FMA uses the same numbering system, here's how the vector is computed
;; (loop with alist 
;; 	    for (rel tooth) in (get FMA::|Secondary dentition| :slots)
;; 	      when (eq rel fma::|member|)
;; 	      do 
;; 	      (let ((syns (remove-if-not (lambda(e) (eq (car e) fma::|Synonym|)) (get tooth :slots))))
;; 		(loop for (rel syn) in syns
;; 		     for name = (second (find fma::|name| (get syn :slots) :key 'car))
;; 		   for number = (caar (all-matches name ".*?(\\d+).*" 1))
;; 		     when number do (push (cons (parse-integer number) tooth) alist)))
;; 	      finally (return (coerce (mapcar (lambda(e) (cons (fma-uri e) (string e))) (mapcar 'cdr (sort alist '< :key 'car))) 'vector)))

(defun number-to-fma-tooth (number)
  (car (aref #((!obo:FMA_55696 . "Right upper third secondary molar tooth") (!obo:FMA_55697 . "Right upper second secondary molar tooth") (!obo:FMA_55698 . "Right upper first secondary molar tooth") (!obo:FMA_55688 . "Right upper second secondary premolar tooth") (!obo:FMA_55689 . "Right upper first secondary premolar tooth") (!obo:FMA_55798 . "Right upper secondary canine tooth") (!obo:FMA_55680 . "Right upper lateral secondary incisor tooth") (!obo:FMA_55681 . "Right upper central secondary incisor tooth") (!obo:FMA_55682 . "Left upper central secondary incisor tooth") (!obo:FMA_55683 . "Left upper lateral secondary incisor tooth") (!obo:FMA_55799 . "Left upper secondary canine tooth") (!obo:FMA_55690 . "Left upper first secondary premolar tooth") (!obo:FMA_55691 . "Left upper second secondary premolar tooth") (!obo:FMA_55699 . "Left upper first secondary molar tooth") (!obo:FMA_55700 . "Left upper second secondary molar tooth") (!obo:FMA_55701 . "Left upper third secondary molar tooth") (!obo:FMA_55702 . "Left lower third secondary molar tooth") (!obo:FMA_55703 . "Left lower second secondary molar tooth") (!obo:FMA_55704 . "Left lower first secondary molar tooth") (!obo:FMA_55692 . "Left lower second secondary premolar tooth") (!obo:FMA_55693 . "Left lower first secondary premolar tooth") (!obo:FMA_55687 . "Left lower secondary canine tooth") (!obo:FMA_57141 . "Left lower lateral secondary incisor tooth") (!obo:FMA_57143 . "Left lower central secondary incisor tooth") (!obo:FMA_57142 . "Right lower central secondary incisor tooth") (!obo:FMA_57140 . "Right lower lateral secondary incisor tooth") (!obo:FMA_55686 . "Right lower secondary canine tooth") (!obo:FMA_55694 . "Right lower first secondary premolar tooth") (!obo:FMA_55695 . "Right lower second secondary premolar tooth") (!obo:FMA_55705 . "Right lower first secondary molar tooth") (!obo:FMA_55706 . "Right lower second secondary molar tooth") (!obo:FMA_55707 . "Right lower third secondary molar tooth"))
	(1- number))))

(defun get-filling-ont (&key iri ont-iri patient-id file-name print-results)
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(url nil)
	(count 0))
    
    ;; set default base and ontology iri's 
    (when (null iri) (setf iri "http://purl.obolibrary.org/obo/individuals/"))
    (when (null ont-iri) (setf ont-iri "http://purl.obolibrary.org/obo/ohd/dev/patterson-partial.owl"))
    
    ;; set global variables 
    (setf *iri* iri)
    (setf *iri-count* 1)
    
    ;; set up connection string and query. Put password in ~/.pattersondbpw
    (setf url (concatenate 'string "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
			   (with-open-file (f "~/.pattersondbpw") (read-line f))))
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

		;; declare data properties
		(as `(declaration (data-property !occurrence_date)))
		(as `(declaration (data-property !patient_ID)))
		    
		(loop while (#"next" results) do
		     (as (get-amalgam-axioms 
			  (#"getString" results "patient id")
			  (#"getString" results "date entered / trans date")
			  (#"getString" results "description")
			  (#"getString" results "tooth")
			  (#"getString" results "surface")
			  (#"getString" results "ada code")
			  (#"getString" results "ada code description")))
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
	(amalgam-restoration-uri nil)
	(tooth-string nil)
	(teeth-list nil))
	
    ;; get axioms abou the patient
    ;;(push (get-patient-axioms iri patient-id) axioms) ;; this doesn' work.. why not?
    (setf patient-uri (get-iri))
    (setf patient-role-uri (get-iri))
    (setf axioms (get-patient-axioms patient-uri patient-role-uri patient-id))

    ;; tooth_data
    ;; get list of teeth in tooth_data array
;    (setf teeth-list (get-teeth tooth-data))
    (setf teeth-list (parse-teeth-list tooth-data)) ; alanr - parse the list since that's what's in our table
    (loop for tooth in teeth-list do
         
         ;;;;  declare instances of participating entitie ;;;;

         ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-uri (get-iri))
	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 (push `(class-assertion ,(number-to-fma-tooth tooth) ,tooth-uri) axioms)	     

	 ;; add annotation about tooth
	 (setf tooth-string (format nil "~a" tooth))
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ "tooth " tooth-string
					     " of patient " patient-id)) axioms)

         ;; declare instance of !ohd:'tooth to be filled role'
	 (setf tooth-role-uri (get-iri))
	 (push `(declaration (named-individual ,tooth-role-uri)) axioms)
	 (push `(class-assertion !tooth_to_be_filled_role ,tooth-role-uri) axioms)

	 ;; add annotation about 'tooth to be filled role'
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-role-uri
				      ,(str+ "tooth to be filled role for tooth " 
					     tooth-string " of patient " patient-id)) axioms)

         ;; declare instance of amalgam (!ohd:amalgam) for tooth
	 (setf amalgam-uri (get-iri))
	 (push `(declaration (named-individual ,amalgam-uri)) axioms)
	 (push `(class-assertion !amalgam ,amalgam-uri) axioms)	 
	 
	 ;; add annotation about this instance of amalgam
	 (push `(annotation-assertion !rdfs:label 
				      ,amalgam-uri
				      ,(str+ "amalgam placed in tooth " tooth-string
					     " of patient " patient-id)) axioms)

         ;; declare instance of amalgam restoration (!ohd:'amalgam filling restoration')
	 (setf amalgam-restoration-uri (get-iri))
	 (push `(declaration (named-individual ,amalgam-restoration-uri)) axioms)
	 (push `(class-assertion !amalgam_filling_restoration ,amalgam-restoration-uri) axioms)

	 ;; add annotation about this amalgam restoration procedure
	 (push `(annotation-assertion !rdfs:label 
				      ,amalgam-restoration-uri
				      ,(str+ "amalgam restoration procedure on tooth " 
					     tooth-string " in patient " patient-id)) axioms)

	 ;; add date property !ohd:'occurence date' to 'amalgam filling restoration'
	 ;;(push `(data-property-assertion !occurrence_date
	 ;;				 ,amalgam-restoration-uri ,tran-date) axioms)

	 (push `(data-property-assertion !occurrence_date
					 ,amalgam-restoration-uri 
					 (:literal ,tran-date !xsd:date)) axioms)

	  ;;;; relate instances ;;;;
       
         ;; 'tooth to be filled role' inheres in tooth
	 (push `(object-property-assertion !inheres_in
					   ,tooth-role-uri ,tooth-uri) axioms)

         ;; 'amalgam filling restoration' realizes 'tooth to be filled role'
	 (push `(object-property-assertion !realizes
					   ,amalgam-restoration-uri ,tooth-role-uri) axioms)

         ;; 'amalgam filling restoration' has particpant tooth
	 (push `(object-property-assertion !has_participant
					   ,amalgam-restoration-uri ,tooth-uri) axioms)
	 
      

         ;; 'amalgam filling restoration' has particpant amalgam
	 (push `(object-property-assertion !has_participant 
					   ,amalgam-restoration-uri ,amalgam-uri) axioms)

         ;; 'amalgam filling restoration' has particpant patient
	 (push `(object-property-assertion !has_participant 
					   ,amalgam-restoration-uri ,patient-uri) axioms)
       
	 ) ;; end loop
    
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
    (push `(class-assertion !dental_patient ,patient-uri) axioms)

    ;; patient role is an instance of !obi:'patient role'
    (push `(class-assertion !patient_role ,patient-role-uri) axioms) 

    ;; 'patient role' inheres in patient
    (push `(object-property-assertion !inheres_in
				      ,patient-role-uri ,patient-uri) axioms)

    ;; add data property 'patient id' to patient
    (push `(data-property-assertion !patient_ID
				    ,patient-uri ,patient-id) axioms)

    ;; add annotation about patient
    (push `(annotation-assertion !rdfs:label 
				 ,patient-uri ,(str+ "dental patient " patient-id)) axioms)

    ;; add annotation about patient role
    (push `(annotation-assertion !rdfs:label 
				 ,patient-role-uri 
				 ,(str+ "'patient role' for patient " patient-id)) axioms)
    
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

;; alanr
(defun parse-teeth-list (teeth-description)
  "Parses the tooth list format '-' is range, ',' separates, into a list of teeth numbers"
  (let ((teeth-list nil))

    (let ((pieces (split-at-char teeth-description #\,)))
      (loop for piece in pieces
	   do
	   (cond ((find #\- piece) (destructuring-bind (start end) (mapcar 'parse-integer (car (all-matches piece "^(\\d+)-(\\d+)" 1 2)))
				     (loop for i from start to end do (push i teeth-list))))
		 ;; otherwise it is a single number
		 (t (push (parse-integer piece) teeth-list)))))
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
   "where \"ada code\" in ('D2140', 'D2150', 'D2160', 'D2161') "
   "and \"table/view name\" = 'transactions' ")
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
  ;; this may be the simplist to understand...
  (apply #'concatenate 'string values))
