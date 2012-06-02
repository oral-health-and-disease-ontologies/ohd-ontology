;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-crowns-ont is ran, the program verifies that the action_codes and
;; patient_history tables exist.  This is done by calling prepare-eaglesoft-db.  However, this
;; only tests that these tables exist in the user's database. If these table need to be 
;; recreated, the call get-eaglesoft-fillings-ont with :force-create-table key set to t.

(defun get-eaglesoft-crowns-ont (&key force-create-table)
  "Returns an ontology of the crowns contained in the Eaglesoft database.  They force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(occurrence-date nil)
	(url nil)
	(count 0))

    ;; set up connection string and query.
    (setf url (get-eaglesoft-database-url))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db url :force-create-table force-create-table)

    ;; get query string for restorations
    (setf query (get-eaglesoft-crowns-query))

    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-crowns-iri-base* 
			:ontology-iri *eaglesoft-crowns-ontology-iri*)
	((unwind-protect
	      (progn
		;; connect to db and get data
		(setf connection (#"getConnection" 'java.sql.DriverManager url))
		(setf statement (#"createStatement" connection))
		(setf results (#"executeQuery" statement query))
	   	
		;; import the ohd ontology
		(as `(imports ,(make-uri *ohd-ontology-iri*)))
		(as `(imports ,(make-uri *eaglesoft-dental-patients-ontology-iri*)))

		;; declare data properties
		(as `(declaration (data-property !'occurrence date'@ohd)))
		(as `(declaration (data-property !'patient ID'@ohd)))
		
		;; declare object property relations
		(as `(declaration  (object-property !'is part of'@ohd)))
		(as `(declaration  (object-property !'inheres in'@ohd)))
		(as `(declaration  (object-property !'has participant'@ohd)))
		(as `(declaration  (object-property !'is located in'@ohd)))
		(as `(declaration  (object-property !'is about'@ohd)))

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
	(cdt-class-uri nil)
	(cdt-uri nil)
	(patient-uri nil)
	(crown-role-uri nil)
	(crown-material-uri nil)
	(crown-restoration-uri nil)
	(restoration-type-uri nil)
	(material-type-uri nil)
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
	       (get-eaglesoft-dental-patient-iri patient-id))
	 
         ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-with-number t))
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-uri (get-eaglesoft-tooth-iri patient-id tooth-type-uri))

	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 (push `(class-assertion ,tooth-type-uri ,tooth-uri) axioms)	     
	 
       ;; add annotation about tooth
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ tooth-name
					     " of patient " patient-id)) axioms)
	 
       ;; declare instance of !ohd:'tooth to be filled role'
	 (setf crown-role-uri
	       (get-eaglesoft-tooth-to-be-crowned-role-iri patient-id tooth record-count))
		
	 (push `(declaration (named-individual ,crown-role-uri)) axioms)
	 (push `(class-assertion !'tooth to be crowned role'@ohd ,crown-role-uri) axioms)

	 ;; add annotation about 'tooth to be crowned role'
	 (push `(annotation-assertion !rdfs:label 
				      ,crown-role-uri
				      ,(str+ "tooth to be crowned role for " 
					     tooth-name " of patient "patient-id)) axioms)

         ;; declare instance of material (i.e.,  amalgam/resin/gold) used in tooth
	 (setf material-type-uri !'restoration material'@ohd)
	 (setf crown-material-uri 
	       (get-eaglesoft-crown-material-iri 
		patient-id tooth-name material-type-uri record-count))

	 (push `(declaration (named-individual ,crown-material-uri)) axioms)
	 (push `(class-assertion !'restoration material'@ohd ,crown-material-uri) axioms)

	 ;; add annotation about this instance of material
	 (push `(annotation-assertion !rdfs:label 
				      ,crown-material-uri
				      ,(str+ "restorative material used for crown on  " 
					     tooth-name " of patient " patient-id)) axioms)

         ;; declare instance of restoration procedure
	 (setf restoration-type-uri !'crown restoration'@ohd)
	 (setf crown-restoration-uri
	       (get-eaglesoft-crown-restoration-iri
		patient-id tooth-name restoration-type-uri record-count))
	 
	 (push `(declaration (named-individual ,crown-restoration-uri)) axioms)
	 (push `(class-assertion !'crown restoration'@ohd ,crown-restoration-uri) axioms)

	 ;; add annotation about this restoration procedure
	 (push `(annotation-assertion !rdfs:label 
				      ,crown-restoration-uri
				      ,(str+ "crown procedure on " tooth-name 
					     " of patient " patient-id)) axioms)

	 ;; add data property !ohd:'occurrence date' to restoration
	 (push `(data-property-assertion !'occurrence date'@ohd
					 ,crown-restoration-uri 
					 (:literal ,occurrence-date !xsd:date)) axioms)

         ;; declare instance of cdt code as identified by the ada code that is about the procedure
	 (setf cdt-class-uri (get-cdt-class-iri ada-code))
	 (setf cdt-uri (get-eaglesoft-cdt-instance-iri patient-id ada-code cdt-class-uri record-count))
	 (push `(declaration (named-individual ,cdt-uri)) axioms)
	 (push `(class-assertion ,cdt-class-uri ,cdt-uri) axioms)

	 ;; add annotion about cdt code
	 (push `(annotation-assertion !rdfs:label
				      ,cdt-uri
				      ,(str+ "billing code " ada-code " for crown restoration "
					     "on " tooth-name " of patient " patient-id)) axioms)

	  ;;;; relate instances ;;;;
	 
	 ;; tooth is part of patient
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
	 
         ;; 'crown restoration' has particpant restoration material
	 (push `(object-property-assertion !'has participant'@ohd
					   ,crown-restoration-uri ,crown-material-uri) axioms)

	 ;;  'crown restoration' has particpant patient
	 (push `(object-property-assertion !'has participant'@ohd
					   ,crown-restoration-uri ,patient-uri) axioms)

	 ;; restoration material is located in the tooth
	 (push `(object-property-assertion !'is located in'@ohd
					   ,crown-material-uri ,tooth-uri) axioms)

         ;; cdt code instance is about the 'crown restoration' process
	 (push `(object-property-assertion !'is about'@ohd
					   ,cdt-uri ,crown-restoration-uri) axioms)
	 
	 ) ;; end loop
    
    ;;(pprint axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-tooth-to-be-crowned-role-iri (patient-id tooth-name instance-count)
  "Returns an iri for a 'tooth to be crowned role' that is generated by the patient id, the name of the type of the tooth, and a count variable that used differientiate tooth role intances that have the same patient-id/tooth-name but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'tooth to be crowned role'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-crown-material-iri (patient-id tooth-name
					 material-type-iri instance-count)
  "Returns an iri for the material used in crown restoration that is generated by the patient id, the name of the type of tooth, the type of restoration's material, and a count variable that used differientiate crown procedure intances that have the same patient-id/material-type-iri but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-crowns-iri-base*
				     :class-type material-type-iri
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-crown-restoration-iri (patient-id tooth-name
					    restoration-type-iri instance-count)
  "Returns an iri for a crown restoration procedure that is generated by the patient id, the name of the type of tooth, the type of crown restoration, and a count variable that used differientiate crown procedure intances that have the same patient-id/restoration-type-iri but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-crowns-iri-base*
				     :class-type restoration-type-iri
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))


(defun get-eaglesoft-cdt-instance-iri (patient-id ada-code cdt-class instance-count)
  "Returns an iri for a CDT code instance that is generated by the patient id, the the ADA code associated with the CDT code, the type/class of CDT code, and a count variable that used differientiate CDT code intances that have the same ADA code but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id
				     :salt *eaglesoft-salt*
				     :iri-base *cdt-iri-base*
				     :class-type cdt-class
				     :args `(,ada-code ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-crowns-query ()
"
SET rowcount 0 

-- Note: D2390 and 02390 (resin-based composite crown, anterior)  have been added to query
SELECT
  -- TOP 10 -- used for testing
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
               'D2970',

                -- Older ada codes beging with a '0'
               '02390', -- This was origanally in the fillings query
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
