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
;; get-eaglesoft-missing-teeth-findings-ont with :force-create-table key set to t.

(defun get-eaglesoft-missing-teeth-findings-ont (&key patient-id tooth limit-rows force-create-table)
  "Returns an ontology of the missing teeth findings contained in the Eaglesoft database. The patient-id key creates an ontology based on that specific patient. The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."
  (let ((results nil)
	(query nil)
	(occurrence-date nil)
	(count 0))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)

    ;; get query string for findings
    (setf query (get-eaglesoft-missing-teeth-findings-query 
		 :patient-id patient-id :tooth tooth :limit-rows limit-rows))

    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-missing-teeth-findings-iri-base*
			:ontology-iri *eaglesoft-missing-teeth-findings-ontology-iri*)
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
		(as (get-eaglesoft-missing-tooth-finding-axioms 
		     (#"getString" results "patient_id")
		     occurrence-date
		     (#"getString" results "tooth_data")
		     (#"getString" results "r21_provider_id")
		     (#"getString" results "r21_provider_type")
		     (#"getString" results "action_code")
		     (#"getString" results "row_id")))
		(incf count))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-missing-tooth-finding-axioms 
    (patient-id occurrence-date tooth-data provider-id provider-type action-code record-count)
  (let ((axioms nil)
	(temp-axioms nil) ; used for appending new axioms into the axioms list
	(patient-uri nil)
	(finding-uri nil)
	(finding-type-uri nil)
	(exam-uri nil)
	(tooth-num nil)
	(dentition-uri nil)
	(dentition-type-uri nil)
	(universal-tooth-num-uri nil)
	(teeth-list nil))

    
    ;; get uri of patient
    (setf patient-uri  (get-eaglesoft-dental-patient-iri patient-id))
    
    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-eaglesoft-teeth-list tooth-data))
    
    ;; generate instances of the dental exam in which the missing tooth/teeth was/were discovered
    (setf exam-uri (get-eaglesoft-dental-exam-iri patient-id occurrence-date))
    (setf axioms (get-eaglesoft-dental-exam-axioms exam-uri patient-id occurrence-date
						   provider-id provider-type record-count))
    
    (loop for tooth in teeth-list do
       	 (setf tooth-num (format nil "~a" tooth)) ; converts tooth number to string
	 
         ;; declare instance of !ohd:'missing tooth finding'
	 (setf finding-type-uri
	       (get-eaglesoft-finding-type-iri description :tooth-num tooth-num))
	 (setf finding-uri 
	       (get-eaglesoft-finding-iri 
		patient-id description :tooth-num tooth-num :instance-count record-count))
	 (push-instance axioms finding-uri finding-type-uri)
	
         ;; add annotation about missing tooth finding
	 (push `(annotation-assertion 
		 !rdfs:label 
		 ,finding-uri
		 ,(get-eaglesoft-finding-rdfs-label patient-id description :tooth tooth-num)) axioms)
         
         ;; get iri for secondary dentition
	 (setf dentition-uri
	       (get-eaglesoft-secondary-dentition-iri patient-id))
	 
         ;; make dentition instance of 'Secondary dentition'
	 (setf dentition-type-uri !'Secondary dentition'@ohd)
	 (setf temp-axioms (get-ohd-instance-axioms dentition-uri dentition-type-uri))
	 (setf axioms (append temp-axioms axioms))

	 (push `(annotation-assertion 
		 !rdfs:label 
		 ,dentition-uri
		 ,(str+ "secondary dentition of patient " patient-id)) axioms)
	 
         ;; instance of secondary dentition is part of patient
	 (push `(object-property-assertion 
		 !'is part of'@ohd 
		 ,dentition-uri 
		 ,patient-uri) axioms)
	 
         ;; instance of missing tooth finding 'is about' the dentition instance
	 (push `(object-property-assertion 
		 !'is about'@ohd 
		 ,finding-uri
		 ,dentition-uri) axioms)
	 
         ;; get iri for universal tooth number of missing tooth
	 (setf universal-tooth-num-uri
	       (get-eaglesoft-universal-tooth-number-iri tooth-num))

	 ;; relate missing tooth finding to universal tooth number
	 (push `(object-property-assertion 
		 !'has part'@ohd 
		 ,finding-uri 
		 ,universal-tooth-num-uri) axioms)
	 
         ;; add data property !ohd:'occurrence date' of the missing tooth finding
	 (push `(data-property-assertion 
		 !'occurrence date'@ohd
		 ,finding-uri
		 (:literal ,occurrence-date !xsd:date)) axioms)

         ;; instance of missing tooth finding is the specified output of the dental exam
	 (push `(object-property-assertion !'has_specified_output'@ohd ,exam-uri ,finding-uri) axioms)
	 
       	 
	 ) ;; end loop
        ;;(pprint axioms)

    ;; return axioms
    axioms))


(defun get-eaglesoft-universal-tooth-number-iri (adatoothnum)
  "Returns the iri for a universal tooth number that is associated with an ADA universal tooth number.  For example if adatoothnum is '17', this will return the iri for universal tooth number 17."
  (let ((uri nil)
	(uri-string nil))

    ;; left zero pad the tooth number; e.g. 7 -> 07
    (setf adatoothnum (str-right (str+ "0" adatoothnum) 2))
    
    ;; creath uri string using tooth number
    (setf uri-string (format nil "OHD_00001~a" adatoothnum))

    ;; create uri
    ;; note: I could also use (make-uri nil (str+ "obo:OHD_00001" adatoothnum))
    (setf uri (make-uri-base-relative uri-string "obo:"))
    
    ;; return uri
    uri))

(defun get-eaglesoft-missing-teeth-findings-query (&key patient-id tooth limit-rows)
  "Returns query string for retrieving data. The patient-id key restricts records only that patient or patients.  Multiple are patients are specified using commas; e.g: \"123, 456, 789\".  The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. However, the tooth key only takes a single value. The limit-rows key restricts the number of records to the number specified."

#|
Returns records that indicate a tooth has been found to be missing.
16,750 records returned.
Note:
- The query does not filter out primary (baby) teeth.
- The some multiple enteries for the same missing tooth.
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
		" WHERE action_code <> 'n/a'
                  AND description like '%missing%'  
                  AND LENGTH(tooth_data) > 31
                  AND tooth_data IS NOT NULL
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

    ;; return query string
    ;;(pprint sql)
    sql))
