;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-missing-teeth-findings-ont is ran, the program verifies that the 
;; action_codes and patient_history tables exist.  This is done by calling 
;; prepare-eaglesoft-db.  However, this only tests that these tables exist in the 
;; user's database. If these table need to be recreated, the call 
;; get-eaglesoft-unerupted-teeth-findings-ont with :force-create-table key set to t.

(defun get-eaglesoft-unerupted-teeth-findings-ont (&key patient-id tooth limit-rows force-create-table)
  "Returns an ontology of the unerupted findings contained in the Eaglesoft database.  This includes both impacted and embedded teeth. The patient-id key creates an ontology based on that specific patient. The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."
  (let ((results nil)
	(query nil)
	(occurrence-date nil)
	(count 0))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)

    ;; get query string for findings
    (setf query (get-eaglesoft-unerupted-teeth-findings-query 
		 :patient-id patient-id :tooth tooth :limit-rows limit-rows))

    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-unerupted-teeth-findings-iri-base*
			:ontology-iri *eaglesoft-unerupted-teeth-findings-ontology-iri*)
	(;; import needed ontologies
	 (as (get-ohd-import-axioms))

	 ;; get axioms for declaring annotation, object, and data properties used for ohd
	 (as (get-ohd-declaration-axioms))
	 
	 ;; get records from eaglesoft db and create axioms
	 (with-eaglesoft (results query)
	   (loop while (#"next" results) do
	        ;; determine the occurrence date
		(setf occurrence-date
		      (get-eaglesoft-occurrence-date 
		       (#"getString" results "table_name")
		       (#"getString" results "date_entered")
		       (#"getString" results "date_completed")
		       (#"getString" results "tran_date")))

	        ;; generate axioms
		(as (get-eaglesoft-unerupted-tooth-finding-axioms 
		     (#"getString" results "patient_id")
		     occurrence-date
		     (#"getString" results "tooth_data")
		     (#"getString" results "r21_provider_id")
		     (#"getString" results "r21_provider_type")
		     (#"getString" results "practice_id")
		     (#"getString" results "row_id")))
		(incf count))))

      ;; return the ontology
      (values ont count))))


(defun get-eaglesoft-unerupted-tooth-finding-axioms 
    (patient-id occurrence-date tooth-data provider-id provider-type practice-id record-count)
  (let ((axioms nil)
	(temp-axioms nil) ; used for appending new axioms into the axioms list
	(patient-uri nil)
	(finding-uri nil)
	(finding-type-uri nil)
	(tooth-uri nil)
	(tooth-type-uri nil)
	(tooth-name nil)
	(tooth-num nil)
	(teeth-list nil))


    ;; make sure *ohd-label-source* hash table has been created
    ;; this value will be discarded below when the patient uri is set
    ;; again. 
    (setf patient-uri !'dental patient'@ohd)
    
     ;; get uri of patient
    (setf patient-uri (get-eaglesoft-dental-patient-iri patient-id))

    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-eaglesoft-teeth-list tooth-data))

    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 
	 ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-with-number t))
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-uri (get-eaglesoft-tooth-iri patient-id tooth-type-uri))
	 
	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 (setf temp-axioms (get-ohd-instance-axioms tooth-uri tooth-type-uri))
	 (setf axioms (append temp-axioms axioms))

	 ;; add annotation about tooth
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ tooth-name
					     " of patient " patient-id)) axioms)

	 ;; get iri associated with the type of unerupted tooth finding
	 (setf finding-type-uri !'unerupted tooth finding'@ohd)
	 
         ;; declare instance of !ohd:'unerupted tooth finding'
	 (setf finding-uri
	       (get-eaglesoft-unerupted-tooth-finding-iri patient-id tooth-name record-count))
	 (setf temp-axioms (get-ohd-instance-axioms finding-uri finding-type-uri))
	 (setf axioms (append temp-axioms axioms))

         ;; add annotation about unerupted tooth finding
	 (push `(annotation-assertion !rdfs:label 
				      ,finding-uri
				      ,(str+ "unerupted tooth " tooth-num " finding "
					     "for patient " patient-id )) axioms)
         
         ;; instance of unerupted tooth finding 'is about' the unerupted tooth
	 (push `(object-property-assertion !'is about'@ohd ,finding-uri ,tooth-uri) axioms)
	 
         ;; add data property !ohd:'occurrence date' of the unerupted tooth finding
	 (push `(data-property-assertion !'occurrence date'@ohd
					 ,finding-uri
					 (:literal ,occurrence-date !xsd:date)) axioms)

	 ;; the unerupted tooth 'is part of' the patient
	 (push `(object-property-assertion !'is part of'@ohd ,tooth-uri ,patient-uri) axioms)

         ;; insert axioms about the dental exam in which the provider (if known)
         ;; found the unerupted  tooth
	 (when provider-id
	   (setf temp-axioms (get-eaglesoft-dental-exam-axioms 
			      finding-uri provider-id provider-type practice-id record-count))

	   ;; ensure that axioms were returned
	   (when temp-axioms (setf axioms (append temp-axioms axioms))))
	 
	 ) ;; end loop
        ;;(pprint axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-unerupted-tooth-finding-iri (patient-id tooth-name instance-count)
  "Returns an iri for a 'unerepted tooth finding' that is generated by the patient id, the name of the type of the tooth, and a count variable that differentiates multipe unerupted teeth findings that are about the same tooth."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'unerupted tooth finding'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-unerupted-teeth-findings-query (&key patient-id tooth limit-rows)
  "Returns query string for retrieving data. The patient-id key restricts records only that patient or patients.  Multiple are patients are specified using commas; e.g: \"123, 456, 789\".  The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. However, the tooth key only takes a single value. The limit-rows key restricts the number of records to the number specified."

#|
Returns records that indicate a tooth is unerupted
I.e., Records that have an action code '18'.
289 records returned.
|#

  (let ((sql nil))
    ;; build query string
    (setf sql "SET rowcount 0 ")
    
    ;; SELECT clause
    (cond 
      (limit-rows
       (setf limit-rows (format nil "~a" limit-rows)) ;ensure that limit rows is a string
       (setf sql (str+ sql " SELECT  TOP " limit-rows " * "))) 
      (t (setf sql (str+ sql " SELECT * "))))

    ;; FROM clause
    (setf sql (str+ sql " FROM patient_history "))

    ;; WHERE clause
    (setf sql
	  (str+ sql
		"WHERE
                   action_code = '18'
                 AND tooth_data IS NOT NULL
                 AND LENGTH(tooth_data) > 31
                 AND tooth_data LIKE '%Y%' "))
    
    ;; check for patient id
    (when patient-id
      (setf sql
	    (str+ sql " AND patient_id IN (" (get-single-quoted-list patient-id) ") ")))

    ;; check for tooth
    (when tooth
      ;; ensure tooth is a string
      (setf tooth (format nil "~a" tooth))
      (setf sql 
	    (str+ sql " AND substring(tooth_data, " tooth ", 1) = 'Y' ")))

     ;; ORDER BY clause
    (setf sql
	  (str+ sql " ORDER BY patient_id "))

    sql))