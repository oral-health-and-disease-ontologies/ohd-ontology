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
;; Database preparation: 
;; In order to set up the Patterson datase, you must first run the queries 
;; continaned in these two files in the files dropbox:
;; 1. "R21 Work/Data/Queries For Extracting Data/create action codes table.txt"
;; 2. "R21 Work/Data/Queries For Extracting Data/create patient  history table.txt"
;; It is necessarry for you to run file #1 then file #2, since the patient_history table
;; references the action_codes table.
;; After the database has been set up, you will only need to run these quereies if the table
;; definitions change.

;; the global variables are used to generate unique iri's
(defparameter *iri* nil)
(defparameter *iri-count* nil)


;; global hash table used to keep track of which patients have been processed.
;; hash key is patient_id; value is the uri for the patient
;; this is need to keep from generating a new uri when a patient id occurs in the 
;; the database more than once
(defparameter *patient-uri-ht* nil)

;; list of ada codes for amalgam, resin, and gold fillings/restorations
(defparameter *amalgam-code-list* 
  '("D2140" "D2150" "D2160" "D2161"))
(defparameter *resin-code-list* 
  '("D2330" "D2332" "D2335" "D2390" "D2391""D2392" "D2392" "D2394"))
(defparameter *gold-code-list* 
  '("D2410" "D2420" "D2430"))

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
;;    for (rel tooth) in (get FMA::|Secondary dentition| :slots)
;;    nwhen (eq rel fma::|member|)
;;    do 
;;      (let ((syns (remove-if-not (lambda(e) (eq (car e) fma::|Synonym|)) (get tooth :slots))))
;;        (loop for (rel syn) in syns
;; 	  for name = (second (find fma::|name| (get syn :slots) :key 'car))
;; 	  for number = (caar (all-matches name ".*?(\\d+).*" 1))
;; 	  when number do (push (cons (parse-integer number) tooth) alist)))
;;    finally (return (coerce 
;; 		    (mapcar (lambda(e) (cons (fma-uri e) (string e)))
;; 			    (mapcar 'cdr (sort alist '< :key 'car))) 'vector)))

(defun number-to-fma-tooth (number)
  (car (aref #((!obo:FMA_55696 . "Right upper third secondary molar tooth") 
	       (!obo:FMA_55697 . "Right upper second secondary molar tooth")
	       (!obo:FMA_55698 . "Right upper first secondary molar tooth")
	       (!obo:FMA_55688 . "Right upper second secondary premolar tooth")
	       (!obo:FMA_55689 . "Right upper first secondary premolar tooth")
	       (!obo:FMA_55798 . "Right upper secondary canine tooth")
	       (!obo:FMA_55680 . "Right upper lateral secondary incisor tooth")
	       (!obo:FMA_55681 . "Right upper central secondary incisor tooth")
	       (!obo:FMA_55682 . "Left upper central secondary incisor tooth")
	       (!obo:FMA_55683 . "Left upper lateral secondary incisor tooth")
	       (!obo:FMA_55799 . "Left upper secondary canine tooth")
	       (!obo:FMA_55690 . "Left upper first secondary premolar tooth")
	       (!obo:FMA_55691 . "Left upper second secondary premolar tooth")
	       (!obo:FMA_55699 . "Left upper first secondary molar tooth")
	       (!obo:FMA_55700 . "Left upper second secondary molar tooth")
	       (!obo:FMA_55701 . "Left upper third secondary molar tooth")
	       (!obo:FMA_55702 . "Left lower third secondary molar tooth")
	       (!obo:FMA_55703 . "Left lower second secondary molar tooth")
	       (!obo:FMA_55704 . "Left lower first secondary molar tooth")
	       (!obo:FMA_55692 . "Left lower second secondary premolar tooth")
	       (!obo:FMA_55693 . "Left lower first secondary premolar tooth")
	       (!obo:FMA_55687 . "Left lower secondary canine tooth")
	       (!obo:FMA_57141 . "Left lower lateral secondary incisor tooth")
	       (!obo:FMA_57143 . "Left lower central secondary incisor tooth")
	       (!obo:FMA_57142 . "Right lower central secondary incisor tooth")
	       (!obo:FMA_57140 . "Right lower lateral secondary incisor tooth")
	       (!obo:FMA_55686 . "Right lower secondary canine tooth")
	       (!obo:FMA_55694 . "Right lower first secondary premolar tooth")
	       (!obo:FMA_55695 . "Right lower second secondary premolar tooth")
	       (!obo:FMA_55705 . "Right lower first secondary molar tooth")
	       (!obo:FMA_55706 . "Right lower second secondary molar tooth")
	       (!obo:FMA_55707 . "Right lower third secondary molar tooth"))
	(1- number))))

(defun get-filling-ont (&key iri ont-iri patient-id file-name print-results)
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(url nil)
	(count 0))
    
    ;; create hash table for keeping track of patient uri's
    (setf *patient-uri-ht* (make-hash-table :test 'equal))

    ;; set default base and ontology iri's 
    (when (null iri) (setf iri "http://purl.obolibrary.org/obo/ohd/individuals/"))
    (when (null ont-iri) 
      (setf ont-iri "http://purl.obolibrary.org/obo/ohd/dev/patterson-partial.owl"))
    
    ;; set global variables 
    (setf *iri* iri)
    (setf *iri-count* 1)
    
    ;; set up connection string and query. Put password in ~/.pattersondbpw
    (setf url (concatenate 'string 
			   "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
			   (with-open-file (f "~/.pattersondbpw") (read-line f))))

    ;; get query string for amalgam restorations
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
		     (as (get-filling-axioms 
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
      (values ont count))))


;; (defun get-amalgam-axioms (patient-id occurrence-date description 
;; 			   tooth-data surface ada-code ada-code-description)
;;   (let ((axioms nil)
;; 	(patient-uri nil)
;; 	(patient-role-uri nil)
;; 	(tooth-uri nil)
;; 	(tooth-role-uri nil)
;; 	(amalgam-uri nil)
;; 	(amalgam-restoration-uri nil)
;; 	(tooth-string nil)
;; 	(teeth-list nil))


;;     ;; get axioms about the patient
;;     ;; test to see if patient has already been created:
;;     ;; if not, then generate axioms about patient
;;     ;; otherwise, simply get the patient uri
;;     (if (not (gethash patient-id *patient-uri-ht*))
;; 	(progn 
;; 	  ;; get uri based on patient id
;; 	  (setf patient-uri (get-patient-uri patient-id))
;; 	  (print-db patient-uri)
;; 	  ;; note append puts lists together and doesn't put items in list (like push)
;; 	  (setf axioms (append (get-patient-axioms patient-uri patient-id) axioms)))
;; 	(setf patient-uri (get-patient-uri patient-id)))


;;     ;; generate the patient role axioms
;;     (setf patient-role-uri (get-iri))
;;     (setf axioms 
;; 	  (append (get-patient-role-axioms patient-uri patient-role-uri patient-id) axioms))

;;     ;; tooth_data
;;     ;; get list of teeth in tooth_data array
;;     (setf teeth-list (get-teeth tooth-data))
    
;;     ;(setf teeth-list (parse-teeth-list tooth-data)) ; alanr - parse the list since that's what's in our table  
;;     (loop for tooth in teeth-list do
         
;;          ;;;;  declare instances of participating entitie ;;;;

;;          ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
;; 	 (setf tooth-uri (get-iri))
;; 	 (push `(declaration (named-individual ,tooth-uri)) axioms)
;; 	 (push `(class-assertion ,(number-to-fma-tooth tooth) ,tooth-uri) axioms)	     

;; 	 ;; add annotation about tooth
;; 	 (setf tooth-string (format nil "~a" tooth))
;; 	 (push `(annotation-assertion !rdfs:label 
;; 				      ,tooth-uri
;; 				      ,(str+ "tooth " tooth-string
;; 					     " of patient " patient-id)) axioms)

;;          ;; declare instance of !ohd:'tooth to be filled role'
;; 	 (setf tooth-role-uri (get-iri))
;; 	 (push `(declaration (named-individual ,tooth-role-uri)) axioms)
;; 	 (push `(class-assertion !tooth_to_be_filled_role ,tooth-role-uri) axioms)

;; 	 ;; add annotation about 'tooth to be filled role'
;; 	 (push `(annotation-assertion !rdfs:label 
;; 				      ,tooth-role-uri
;; 				      ,(str+ "tooth to be filled role for tooth " 
;; 					     tooth-string " of patient " patient-id)) axioms)

;;          ;; declare instance of amalgam (!ohd:amalgam) for tooth
;; 	 (setf amalgam-uri (get-iri))
;; 	 (push `(declaration (named-individual ,amalgam-uri)) axioms)
;; 	 (push `(class-assertion !amalgam ,amalgam-uri) axioms)	 
	 
;; 	 ;; add annotation about this instance of amalgam
;; 	 (push `(annotation-assertion !rdfs:label 
;; 				      ,amalgam-uri
;; 				      ,(str+ "amalgam placed in tooth " tooth-string
;; 					     " of patient " patient-id)) axioms)

;;          ;; declare instance of amalgam restoration (!ohd:'amalgam filling restoration')
;; 	 (setf amalgam-restoration-uri (get-iri))
;; 	 (push `(declaration (named-individual ,amalgam-restoration-uri)) axioms)
;; 	 (push `(class-assertion !amalgam_filling_restoration ,amalgam-restoration-uri) axioms)

;; 	 ;; add annotation about this amalgam restoration procedure
;; 	 (push `(annotation-assertion !rdfs:label 
;; 				      ,amalgam-restoration-uri
;; 				      ,(str+ "amalgam restoration procedure on tooth " 
;; 					     tooth-string " in patient " patient-id)) axioms)

;; 	 ;; add date property !ohd:'occurence date' to 'amalgam filling restoration'
;; 	 (push `(data-property-assertion !occurrence_date
;; 					 ,amalgam-restoration-uri 
;; 					 (:literal ,occurrence-date !xsd:date)) axioms)

;; 	  ;;;; relate instances ;;;;
       
;;          ;; 'tooth to be filled role' inheres in tooth
;; 	 (push `(object-property-assertion !inheres_in
;; 					   ,tooth-role-uri ,tooth-uri) axioms)

;;          ;; 'amalgam filling restoration' realizes 'tooth to be filled role'
;; 	 (push `(object-property-assertion !realizes
;; 					   ,amalgam-restoration-uri ,tooth-role-uri) axioms)

;;          ;; 'amalgam filling restoration' has particpant tooth
;; 	 (push `(object-property-assertion !has_participant
;; 					   ,amalgam-restoration-uri ,tooth-uri) axioms)
	 
      

;;          ;; 'amalgam filling restoration' has particpant amalgam
;; 	 (push `(object-property-assertion !has_participant 
;; 					   ,amalgam-restoration-uri ,amalgam-uri) axioms)

;;          ;; 'amalgam filling restoration' has particpant patient
;; 	 (push `(object-property-assertion !has_participant 
;; 					   ,amalgam-restoration-uri ,patient-uri) axioms)
       
;; 	 ) ;; end loop
    

;;     ;; return axioms
;;     axioms))

(defun get-filling-axioms (patient-id occurrence-date description 
				  tooth-data surface ada-code ada-code-description)
  (let ((axioms nil)
	(patient-uri nil)
	(patient-role-uri nil)
	(material-name nil)
	(material-uri nil)
	(obo-material-uri nil)
	(obo-restoration-uri nil)
	(restoration-name nil)
	(restoration-uri nil)
	(tooth-uri nil)
	(tooth-role-uri nil)
	(tooth-string nil)
	(teeth-list nil))


    ;; get axioms about the patient
    ;; test to see if patient has already been created:
    ;; if not, then generate axioms about patient
    ;; otherwise, simply get the patient uri
    (if (not (gethash patient-id *patient-uri-ht*))
	(progn 
	  ;; get uri based on patient id
	  (setf patient-uri (get-patient-uri patient-id))
	  ;; note append puts lists together and doesn't put items in list (like push)
	  (setf axioms (append (get-patient-axioms patient-uri patient-id) axioms)))
	(setf patient-uri (get-patient-uri patient-id)))


    ;; generate the patient role axioms
    (setf patient-role-uri (get-iri))
    (setf axioms 
	  (append (get-patient-role-axioms patient-uri patient-role-uri patient-id) axioms))

    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-teeth tooth-data))

    ;; alanr - parse the list since that's what's in our table 
    ;;(setf teeth-list (parse-teeth-list tooth-data)) ; commented out by billd

    (loop for tooth in teeth-list do
         ;;(print-db tooth)

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

         ;; declare instance of material (i.e.,  amalgam/resin/gold) used in tooth
	 (setf material-uri (get-iri))
	 (setf obo-material-uri (get-obo-material-uri ada-code))
	 (push `(declaration (named-individual ,material-uri)) axioms)
	 (push `(class-assertion ,obo-material-uri ,material-uri) axioms)

	 ;; add annotation about this instance of material
	 (setf material-name (get-material-name ada-code))
	 (push `(annotation-assertion !rdfs:label 
				      ,material-uri
				      ,(str+ material-name " placed in tooth " tooth-string
					     " of patient " patient-id)) axioms)

         ;; declare instance of restoration 
	 (setf restoration-uri (get-iri))
	 (setf obo-restoration-uri (get-obo-restoration-uri ada-code))
	 (push `(declaration (named-individual ,restoration-uri)) axioms)
	 (push `(class-assertion ,obo-restoration-uri ,restoration-uri) axioms)

	 ;; add annotation about this restoration procedure
	 (setf restoration-name (get-restoraton-name ada-code))
	 (push `(annotation-assertion !rdfs:label 
				      ,restoration-uri
				      ,(str+ restoration-name " restoration procedure on tooth " 
					     tooth-string " in patient " patient-id)) axioms)

	 ;; add date property !ohd:'occurence date' to 'amalgam filling restoration'
	 (push `(data-property-assertion !occurrence_date
					 ,restoration-uri 
					 (:literal ,occurrence-date !xsd:date)) axioms)

	  ;;;; relate instances ;;;;
       
         ;; 'tooth to be filled role' inheres in tooth
	 (push `(object-property-assertion !inheres_in
					   ,tooth-role-uri ,tooth-uri) axioms)

         ;; 'filling restoration' realizes 'tooth to be filled role'
	 (push `(object-property-assertion !realizes
					   ,restoration-uri ,tooth-role-uri) axioms)

         ;; 'filling restoration' has particpant tooth
	 (push `(object-property-assertion !has_participant
					   ,restoration-uri ,tooth-uri) axioms)
	 
         ;; 'filling restoration has particpant material
	 (push `(object-property-assertion !has_participant 
					   ,restoration-uri ,material-uri) axioms)

         ;; 'filling restoration' has particpant patient
	 (push `(object-property-assertion !has_participant 
					   ,restoration-uri ,patient-uri) axioms)
       
	 ) ;; end loop
    
    ;; return axioms
    axioms))
  

(defun get-patient-axioms (patient-uri patient-id)
  "Returns a list of axioms about a patient that is identified by patient-id."
  (let ((axioms nil))
    ;; create instance/indiviual  patient
    (push `(declaration (named-individual ,patient-uri)) axioms) 
    (push `(class-assertion !dental_patient ,patient-uri) axioms)
    
    ;; add data property 'patient id' to patient
    (push `(data-property-assertion !patient_ID
				    ,patient-uri ,patient-id) axioms)

    ;; add label annotation about patient
    (push `(annotation-assertion !rdfs:label 
				 ,patient-uri ,(str+ "dental patient " patient-id)) axioms)

    ;; return axioms
    axioms))

(defun get-patient-role-axioms (patient-uri patient-role-uri patient-id)
  "Returns a list of axioms about patient role."
  (let ((axioms nil))

    ;; create instance of patient role; patient role is an instance of !obi:'patient role'
    (push `(declaration (named-individual ,patient-role-uri)) axioms)
    (push `(class-assertion !patient_role ,patient-role-uri) axioms) 

    ;; 'patient role' inheres in patient
    (push `(object-property-assertion !inheres_in
				      ,patient-role-uri ,patient-uri) axioms)
    
    ;; add label annotation about patient role
    (push `(annotation-assertion !rdfs:label 
				 ,patient-role-uri 
				 ,(str+ "'patient role' for patient " patient-id)) axioms)
    
    ;; return axioms
    axioms))

(defun get-material-name (ada-code)
  "Returns the name the material used in a filling/restoration based on ada code."
  (let ((material-name nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equal) (setf material-name "amalgam"))
      ((member ada-code *resin-code-list* :test 'equal) (setf material-name "resin"))
      ((member ada-code *gold-code-list* :test 'equal) (setf material-name "gold"))
      (t (setf material-name "other material")))

    ;; return material name
    material-name))

(defun get-restoraton-name (ada-code)
  "Returns the name the type of restoration based on ada code."
  (let ((restoration-name nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equal) (setf restoration-name "amalgam"))
      ((member ada-code *resin-code-list* :test 'equal) (setf restoration-name "resin"))
      ((member ada-code *gold-code-list* :test 'equal) (setf restoration-name "gold"))
      (t (setf restoration-name "other")))

    ;; return material name
    restoration-name))

(defun get-obo-restoration-uri (ada-code)
  "Returns the uri of the restoration type based on ada code."
  (let ((obo-restoration-uri nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equal) 
       (setf obo-restoration-uri !amalgam_filling_restoration))
      ((member ada-code *resin-code-list* :test 'equal)  
       (setf obo-restoration-uri !resin_filling_restoration))
      ((member ada-code *gold-code-list* :test 'equal)  
       (setf restoration-uri !gold_filling_restoration))
      (t (setf obo-restoration-uri !other_restoration)))

    ;; return restoration
    obo-restoration-uri))

(defun get-obo-material-uri (ada-code)
  "Returns the uri of the material used in a filling/restoration based on ada code."
  (let ((material-uri nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equal)  (setf material-uri !amalgam))
      ((member ada-code *resin-code-list* :test 'equal)  (setf material-uri !resin))
      ((member ada-code *gold-code-list* :test 'equal)  (setf material-uri !gold))
      (t (setf material-uri !other_material)))

    ;; return material uri
    material-uri))
		     
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

(defun get-patient-uri (patient-id)
  (let ((patient-uri nil))
    ;; look in patient uri hash table for a ur
    (setf patient-uri (gethash patient-id *patient-uri-ht*))

    ;; if the patient-uri is nil, then create a new iri
    ;; and add to patient uri hash table
    (when (not patient-uri)
      ;;(setf patient-uri (get-iri-string))
      (setf patient-uri (get-iri))
      (setf (gethash patient-id *patient-uri-ht*) patient-uri))

    ;; return the patient uri
    ;; note: this must be done using make-uri
    ;;(make-uri patient-uri)))
    patient-uri))

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
  ;;(str+ 
  ;; "select top 10 * from patient_history "
  ;; "where \"ada code\" in ('D2140', 'D2150', 'D2160', 'D2161') "
  ;; "and \"table/view name\" = 'transactions' ")
  ;; (str+ 
  ;;  "select top 10 * from patient_history "
  ;;  "where ada_code in ('D2140', 'D2150', 'D2160', 'D2161') "
  ;;  "and table_name = 'transactions' ")
;;   "select *
;; from patient_history
;; where ada_code in ('D2140', 'D2150', 'D2160', 'D2161') 
;; and table_name = 'transactions'
;; and (patient_id = 5708 or patient_id = 5482)"
"SELECT
  *
FROM
  patient_history
WHERE
  patient_id IN ('930',
                 '444',
                 '790',
                 '529',
                 '112',
                 '330',
                 '1690',
                 '327',
                 '1680',
                 '304') 

AND 
  ada_code IN ('D2140', -- Restorative: Amalgam
              'D2150',
              'D2160',
              'D2161',
              'D2330', -- Restorative: Resin
              'D2332',
              'D2335',
              'D2390',
              'D2391',
              'D2392',
              'D2393',
              'D2394',
              'D2410', -- Restorative: Gold Foil
              'D2420',
              'D2430' )
AND 
  table_name = 'transactions'"
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

(defun create-action-codes-table (force-create)

)

(defun create-patient-history-table (force-create)

)

(defun test-existence-of-table (table-name)
  (let ((url nil)
	(connection nil)
	(statement nil)
	(results nil)
	(query nil))
  
    (setf url (concatenate 'string 
			   "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
			   (with-open-file (f "~/.pattersondbpw") (read-line f))))

    ;; get query string for amalgam restorations
    (setf query (get-amalgam-query))


    (unwind-protect
	 (progn
	   ;; connect to db and get data
	   (setf connection (#"getConnection" 'java.sql.DriverManager url))
	   (setf statement (#"createStatement" connection))
	   (setf results (#"executeQuery" statement query))

	 
	   )
      ;; database cleanup
      (and connection (#"close" connection))
      (and results (#"close" results))
      (and statement (#"close" statement)))

    ))