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

;; list of ada codes for amalgam, resin, and gold fillings/restorations
(defparameter *amalgam-code-list* 
  '("D2140" "D2150" "D2160" "D2161"))
(defparameter *resin-code-list* 
  '("D2330" "D2332" "D2335" "D2390" "D2391" "D2392" "D2393" "D2394"))
(defparameter *gold-code-list* 
  '("D2410" "D2420" "D2430"))

;; global variable used for to salt the md5 checksum
(defparameter *salt* nil)

;; ensure that sql libary is loaded
(add-to-classpath "/Applications/SQLAnywhere12/System/java/sajdbc4.jar")

(defun get-filling-ont (&key iri ont-iri)
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(occurrence-date nil)
	(url nil)
	(count 0))

    ;; set md5 salt
    (setf *salt* (get-eaglesoft-salt))
    
    ;; set default base and ontology iri's 
    (when (null iri) (setf iri "http://purl.obolibrary.org/obo/ohd/individuals/"))
    (when (null ont-iri) 
      (setf ont-iri "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-fillngs.owl"))
    
    ;; set global variables 
    (setf *iri* iri)

    ;; set up connection string and query.
    (setf url (get-eaglesoft-database-url))

    ;; get query string for restorations
    (setf query (get-fillings-query))

    (with-ontology ont (:collecting t :base iri :ontology-iri ont-iri)
	((unwind-protect
	      (progn
		;; connect to db and get data
		(setf connection (#"getConnection" 'java.sql.DriverManager url))
		(setf statement (#"createStatement" connection))
		(setf results (#"executeQuery" statement query))
	   	
		;; import the ohd ontology
		(as `(imports ,(make-uri "http://purl.obolibrary.org/obo/ohd/dev/ohd.owl")))
		(as `(imports ,(make-uri "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-dental-patients.owl")))

		;; declare data properties
		(as `(declaration (data-property !'occurence date'@ohd)))
		(as `(declaration (data-property !'patient ID'@ohd)))
		    
		(loop while (#"next" results) do
		     ;; determine this occurrence date
		     (setf occurrence-date
			   (get-eaglesoft-occurrenc-date 
			    (#"getString" results "table_name")
			    (#"getString" results "date_entered")
			    (#"getString" results "date_completed")
			    (#"getString" results "tran_date")))
		     
		     ;; get axioms
		     (as (get-filling-axioms 
			  (#"getString" results "patient_id")
			  occurrence-date
			  (#"getString" results "tooth_data")
			  (#"getString" results "surface")
			  (#"getString" results "ada_code")
			  count))
		     (incf count)))

	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement))))

      ;; return the ontology
      (values ont count))))

(defun get-filling-axioms (patient-id occurrence-date tooth-data surface ada-code record-count)
  (let ((axioms nil)
	(material-name nil)
	(material-uri nil)
	(obo-material-uri nil)
	(obo-restoration-uri nil)
	(restoration-name nil)
	(restoration-uri nil)
	(patient-uri nil)
	(tooth-uri nil)
	(tooth-type-uri nil)
	(tooth-role-uri nil)
	(tooth-name nil)
	(teeth-list nil))

    ;; alanr - parse the list since that's what's in our table 
    ;; billd - since we are using the tooth_data array, this procedure is skipped
    ;;(setf teeth-list (parse-teeth-list tooth-data)) ; commented out by billd


    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-teeth-list tooth-data))

    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 
	 ;; get uri of patient
	 (setf patient-uri 
	       (get-unique-iri patient-id 
			       :salt *salt*
			       :iri-base *iri*
			       :class-type !'dental patient'@ohd 			
			       :args "eaglesoft"))
	 
         ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-uri 
	       (get-unique-iri patient-id
			       :salt *salt*
			       :iri-base *iri*
			       :class-type tooth-type-uri
			       :args `(,tooth "eaglesoft")))					 
	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 (push `(class-assertion ,tooth-type-uri ,tooth-uri) axioms)	     
	 
	 ;; add annotation about tooth
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-name t))
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ tooth-name
					     " of patient " patient-id)) axioms)
	 
         ;; declare instance of !ohd:'tooth to be filled role'
	 (setf tooth-role-uri
	       (get-unique-iri patient-id
			       :salt *salt*
			       :iri-base *iri*
			       :class-type !'tooth to be filled role'@ohd
			       :args `(,tooth ,record-count "eaglesoft")))
		
	 (push `(declaration (named-individual ,tooth-role-uri)) axioms)
	 (push `(class-assertion !'tooth to be filled role'@ohd ,tooth-role-uri) axioms)

	 ;; add annotation about 'tooth to be filled role'
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-role-uri
				      ,(str+ "tooth to be filled role for " 
					     tooth-name " of patient " 
					     patient-id)) axioms)

         ;; declare instance of material (i.e.,  amalgam/resin/gold) used in tooth
	 (setf obo-material-uri (get-obo-material-uri ada-code))
	 (setf material-uri 
	       (get-unique-iri patient-id
			       :salt *salt*
			       :iri-base *iri*
			       :class-type obo-material-uri
			       :args `(,tooth ,record-count "eaglesoft")))
	 (push `(declaration (named-individual ,material-uri)) axioms)
	 (push `(class-assertion ,obo-material-uri ,material-uri) axioms)

	 ;; add annotation about this instance of material
	 (setf material-name (get-material-name ada-code))
	 (push `(annotation-assertion !rdfs:label 
				      ,material-uri
				      ,(str+ material-name " placed in " tooth-name
					     " of patient " patient-id)) axioms)

         ;; declare instance of restoration 
	 (setf obo-restoration-uri (get-obo-restoration-uri ada-code))
	 (setf restoration-uri
	       (get-unique-iri patient-id
			       :salt *salt*
			       :iri-base *iri*
			       :class-type obo-restoration-uri
			       :args `(,tooth ,record-count "eaglesoft")))
		
	 (push `(declaration (named-individual ,restoration-uri)) axioms)
	 (push `(class-assertion ,obo-restoration-uri ,restoration-uri) axioms)

	 ;; add annotation about this restoration procedure
	 (setf restoration-name (get-restoraton-name ada-code))
	 (push `(annotation-assertion !rdfs:label 
				      ,restoration-uri
				      ,(str+ restoration-name 
					     " restoration procedure on tooth " 
					     tooth-name " in patient " 
					     patient-id)) axioms)

	 ;; add data property !ohd:'occurence date' to restoration
	 (push `(data-property-assertion !'occurence date'@ohd
					 ,restoration-uri 
					 (:literal ,occurrence-date !xsd:date)) axioms)
	  ;;;; relate instances ;;;;

	 ;; toot is located in the patient
	 (push `(object-property-assertion !'is part of'@ohd
					   ,tooth-uri ,patient-uri) axioms)

         ;; 'tooth to be filled role' inheres in tooth
	 (push `(object-property-assertion !'inheres in'@ohd
					   ,tooth-role-uri ,tooth-uri) axioms)

         ;; 'filling restoration' realizes 'tooth to be filled role'
	 (push `(object-property-assertion !'realizes'@ohd
					   ,restoration-uri ,tooth-role-uri) axioms)

         ;; 'filling restoration' has particpant tooth
	 (push `(object-property-assertion !'has participant'@ohd
					   ,restoration-uri ,tooth-uri) axioms)
	 
         ;; 'filling restoration' has particpant restoration material
	 (push `(object-property-assertion !'has participant'@ohd
					   ,restoration-uri ,material-uri) axioms)

	 ;; restoration material is located in the tooth
	 (push `(object-property-assertion !'is located in'@ohd
					   ,material-uri ,tooth-uri) axioms)
	 ) ;; end loop
    
    ;;(pprint axioms)

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
      ((member ada-code *amalgam-code-list* :test 'equalp) (setf restoration-name "amalgam"))
      ((member ada-code *resin-code-list* :test 'equalp) (setf restoration-name "resin"))
      ((member ada-code *gold-code-list* :test 'equalp) (setf restoration-name "gold"))
      (t (setf restoration-name "other")))

    ;; return material name
    restoration-name))

(defun get-obo-restoration-uri (ada-code)
  "Returns the uri of the restoration type based on ada code."
  (let ((obo-restoration-uri nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equalp) 
       (setf obo-restoration-uri !'amalgam filling restoration'@ohd))
      ((member ada-code *resin-code-list* :test 'equalp)  
       (setf obo-restoration-uri !'resin filling restoration'@ohd))
      ((member ada-code *gold-code-list* :test 'equalp)
       (setf obo-restoration-uri !'gold filling restoration'@ohd))
      (t (setf obo-restoration-uri !'filling restoration'@ohd)))

    ;; return restoration
    obo-restoration-uri))

(defun get-obo-material-uri (ada-code)
  "Returns the uri of the material used in a filling/restoration based on ada code."
  (let ((obo-material-uri nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equalp)
       (setf obo-material-uri !'amalgam'@ohd))
      ((member ada-code *resin-code-list* :test 'equalp)
       (setf obo-material-uri !'resin'@ohd))
      ((member ada-code *gold-code-list* :test 'equalp)
       (setf obo-material-uri !'gold'@ohd))
      (t 
       (setf obo-material-uri !'restoration material'@ohd)))
    
    ;; return obo material uri
    obo-material-uri))
		     
(defun get-teeth-list (tooth-data)
  "Reads the tooth_data array and returns a list of tooth numbers referenced in the tooth_data (i.e., field) array."
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


(defun get-fillings-query ()
"
SET rowcount 0

SELECT 
  *
FROM
  patient_history
WHERE
  ada_code IN (
              'D2140', -- Restorative: Amalgam
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
/**** This was used for testing
AND 
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
******/
AND tooth_data IS NOT NULL
AND LENGTH(tooth_data) > 31
AND surface_detail IS NOT NULL
--AND table_name = 'transactions' -- This was used for testing
ORDER BY
  patient_id
"
)
