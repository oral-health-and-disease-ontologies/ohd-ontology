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
;; When get-eaglesoft-fillings-ont is ran, the program verifies that the action_codes and
;; patient_history tables exist.  This is done by calling prepare-eaglesoft-db.  However, this
;; only tests that these tables exist in the user's database. If these table need to be 
;; recreated, the call get-eaglesoft-fillings-ont with :force-create-table key set to t.

;; the global variables are used to generate unique iri's
(defparameter *eaglesoft-iri* nil)


;; global variable used for to salt the md5 checksum
(defparameter *eaglesoft-salt* nil)

;; ensure that sql libary is loaded
(add-to-classpath "/Applications/SQLAnywhere12/System/java/sajdbc4.jar")

(defun get-eaglesoft-crown-ont (&key iri ont-iri force-create-table)
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(occurrence-date nil)
	(url nil)
	(count 0))

    ;; set md5 salt
    (setf *eaglesoft-salt* (get-eaglesoft-salt))
    
    ;; set default base and ontology iri's 
    (when (null iri) (setf iri "http://purl.obolibrary.org/obo/ohd/individuals/"))
    (when (null ont-iri) 
      (setf ont-iri "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-crowns.owl"))
    
    ;; set global variables 
    (setf *eaglesoft-iri* iri)

    ;; set up connection string and query.
    (setf url (get-eaglesoft-database-url))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db url :force-create-table force-create-table)

    ;; get query string for restorations
    (setf query (get-eaglesoft-crowns-query))

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
			   (get-eaglesoft-occurrence-date 
			    (#"getString" results "table_name")
			    (#"getString" results "date_entered")
			    (#"getString" results "date_completed")
			    (#"getString" results "tran_date")))
		     
		     ;; get axioms
		     (as (get-eaglesoft-crown-axioms 
		     	  (#"getString" results "patient_id")
		     	  occurrence-date
		     	  (#"getString" results "tooth_data")
		     	  (#"getString" results "ada_code")
		     	  count))
		     (incf count)))

	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-crown-axioms 
    (patient-id occurrence-date tooth-data ada-code record-count)
  (let ((axioms nil)
	(patient-uri nil)
	(crown-role-uri nil)
	(crown-material-uri nil)
	(crown-restoration-uri nil)
	(tooth-name nil)
	(tooth-uri nil)
	(tooth-type-uri nil)
	(teeth-list nil))

    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-eaglesoft-teeth-list tooth-data))

    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 
	 ;; get uri of patient
	 (setf patient-uri 
	       (get-unique-individual-iri patient-id 
					  :salt *eaglesoft-salt*
					  :iri-base *eaglesoft-iri*
					  :class-type !'dental patient'@ohd 			
					  :args "eaglesoft"))
	 
         ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-name t))
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-uri 
	       (get-unique-individual-iri patient-id
					  :salt *eaglesoft-salt*
					  :iri-base *eaglesoft-iri*
					  :class-type tooth-type-uri
					  :args `(,tooth-name "eaglesoft")))	

	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 (push `(class-assertion ,tooth-type-uri ,tooth-uri) axioms)	     
	 
       ;; add annotation about tooth
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ tooth-name
					     " of patient " patient-id)) axioms)
	 
       ;; declare instance of !ohd:'tooth to be filled role'
	 (setf crown-role-uri
	       (get-unique-individual-iri patient-id
					  :salt *eaglesoft-salt*
					  :iri-base *eaglesoft-iri*
					  :class-type !'tooth to be crowned role'@ohd
					  :args `(,tooth-name ,record-count "eaglesoft")))
		
	 (push `(declaration (named-individual ,crown-role-uri)) axioms)
	 (push `(class-assertion !'tooth to be crowned role'@ohd ,crown-role-uri) axioms)

	 ;; add annotation about 'tooth to be crowned role'
	 (push `(annotation-assertion !rdfs:label 
				      ,crown-role-uri
				      ,(str+ "tooth to be crowned role for " 
					     tooth-name " of patient " 
					     patient-id)) axioms)

         ;; declare instance of material (i.e.,  amalgam/resin/gold) used in tooth
	 (setf crown-material-uri 
	       (get-unique-individual-iri patient-id
					  :salt *eaglesoft-salt*
					  :iri-base *eaglesoft-iri*
					  :class-type !'restoration material'@ohd
					  :args `(,tooth-name ,record-count "eaglesoft")))

	 (push `(declaration (named-individual ,crown-material-uri)) axioms)
	 (push `(class-assertion !'restoration material'@ohd ,crown-material-uri) axioms)

	 ;; add annotation about this instance of material
	 (push `(annotation-assertion !rdfs:label 
				      ,crown-material-uri
				      ,(str+ "restorative material used for crown on  " 
					     tooth-name " of patient " patient-id)) axioms)

         ;; declare instance of restoration procedure
	 (setf crown-restoration-uri
	       (get-unique-individual-iri patient-id
					  :salt *eaglesoft-salt*
					  :iri-base *eaglesoft-iri*
					  :class-type !'crown restoration'@ohd
					  :args `(,tooth-name ,record-count "eaglesoft")))
		
	 (push `(declaration (named-individual ,crown-restoration-uri)) axioms)
	 (push `(class-assertion !'crown restoration'@ohd ,crown-restoration-uri) axioms)

	 ;; add annotation about this restoration procedure
	 (push `(annotation-assertion !rdfs:label 
				      ,crown-restoration-uri
				      ,(str+ "crown procedure on tooth " 
					     tooth-name " in patient " 
					     patient-id)) axioms)

	 ;; add data property !ohd:'occurence date' to restoration
	 (push `(data-property-assertion !'occurence date'@ohd
					 ,crown-restoration-uri 
					 (:literal ,occurrence-date !xsd:date)) axioms)
	  ;;;; relate instances ;;;;

	 ;; tooth is located in the patient
	 (push `(object-property-assertion !'is part of'@ohd
					   ,tooth-uri ,patient-uri) axioms)

         ;; 'tooth to be filled role' inheres in tooth
	 (push `(object-property-assertion !'inheres in'@ohd
					   ,crown-role-uri ,tooth-uri) axioms)

         ;; 'crown restoration' realizes 'tooth to be crowned role'
	 (push `(object-property-assertion !'realizes'@ohd
					   ,crown-restoration-uri ,crown-role-uri) axioms)

         ;; 'crown restoration' has particpant tooth
	 (push `(object-property-assertion !'has participant'@ohd
					   ,crown-restoration-uri ,tooth-uri) axioms)
	 
         ;; 'filling restoration' has particpant restoration material
	 (push `(object-property-assertion !'has participant'@ohd
					   ,crown-restoration-uri ,crown-material-uri) axioms)

	 ;; restoration material is located in the tooth
	 (push `(object-property-assertion !'is located in'@ohd
					   ,crown-material-uri ,tooth-uri) axioms)
	 ) ;; end loop
    
    ;;(pprint axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-crowns-query ()
"
SET rowcount 0 

-- Note: D2390 and 02390 (resin-based composite crown, anterior)  have been added to query
-- 12,374 records returned from query
SELECT
  *
FROM
  patient_history
WHERE
  ada_code IN ('D2390', -- This was originally in the fillings query
               'D2710',
               'D2712',
               'D2721',
               'D2722',
               'D2740',
               'D2750',
               'D2751',
               'D2752',
               'D2780',
               'D2781',
               'D2782',
               'D2783',
               'D2790',
               'D2791',
               'D2792',
               'D2794',
               'D2799',
               'D2931',
               'D2932',
               'D2933',
               'D2940',
               'D2950',
               'D2952',
               'D2954',
               'D2960',
               'D2961',
               'D2962',
               'D2970')

 -- Older ada codes beging with a '0'
OR ada_code IN('02390', -- This was origanally in the fillings query
               '02710',
               '02712',
               '02721',
               '02722',
               '02740',
               '02750',
               '02751',
               '02752',
               '02780',
               '02781',
               '02782',
               '02783',
               '02790',
               '02791',
               '02792',
               '02794',
               '02799',
               '02931',
               '02932',
               '02933',
               '02940',
               '02950',
               '02952',
               '02954',
               '02960',
               '02961',
               '02962',
               '02970')
"
)
