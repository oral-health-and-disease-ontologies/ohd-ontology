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
		     (#"getString" results "practice_id")
		     (#"getString" results "row_id")))
		(incf count))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-missing-tooth-finding-axioms 
    (patient-id occurrence-date tooth-data provider-id provider-type practice-id record-count)
  (let ((axioms nil)
	(temp-axioms nil) ; used for appending new axioms into the axioms list
	(patient-uri nil)
	(finding-uri nil)
	(finding-type-uri nil)
	(tooth-name nil)
	(tooth-num nil)
	;;(tooth-type-uri nil)
	(dentition-uri nil)
	(dentition-type-uri nil)
	(universal-tooth-num-uri nil)
	(teeth-list nil))

    
    ;; make sure *ohd-label-source* hash table has been created
    ;; this value will be discarded below when the patient uri is set
    ;; again. 
    (setf patient-uri !'dental patient'@ohd)

    ;; get uri of patient
    (setf patient-uri  (get-eaglesoft-dental-patient-iri patient-id))
	 

    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-eaglesoft-teeth-list tooth-data))
    
    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 
         ;; get info about the type of missing tooth
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-name t))
         ;;(setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-num (format nil "~a" tooth)) ; converts tooth number to string
	 
	 ;; get iri associated with the type of missing tooth finding
	 ;; note: tooth-num is the string representation of the tooth number
	 ;; this can also be done using:
	 ;;(make-uri-from-label-source :OHD "missing tooth finding" nil)
	 ;; (make-uri-from-label-source :OHD 
	 ;;       (str+ "missing tooth " tooth-num " finding") nil)
	 (setf finding-type-uri 
	       (gethash (str+ "missing tooth " tooth-num " finding") *ohd-label-source*))
	 
         ;; declare instance of !ohd:'missing tooth finding'
	 (setf finding-uri
	       (get-eaglesoft-missing-tooth-finding-iri patient-id tooth-name record-count))
	 (setf temp-axioms (get-ohd-instance-axioms finding-uri finding-type-uri))
	 (setf axioms (append temp-axioms axioms))

         ;; add annotation about missing tooth finding
	 (push `(annotation-assertion !rdfs:label 
				      ,finding-uri
				      ,(str+ "missing tooth " tooth-num " finding "
					     "for patient " patient-id )) axioms)
         
         ;; get iri for secondary dentition
	 (setf dentition-uri
	       (get-eaglesoft-secondary-dentition-iri patient-id))
	 
         ;; make dentition instance of 'Secondary dentition'
	 (setf dentition-type-uri !'Secondary dentition'@ohd)
	 (setf temp-axioms (get-ohd-instance-axioms dentition-uri dentition-type-uri))
	 (setf axioms (append temp-axioms axioms))

	 (push `(annotation-assertion 
		 !rdfs:label ,dentition-uri
		 ,(str+ "secondary dentition of patient " patient-id)) axioms)
	 
         ;; instance of secondary dentition is part of patient
	 (push `(object-property-assertion !'is part of'@ohd ,dentition-uri ,patient-uri) axioms)

         ;; instance of missing tooth finding 'is about' the dentition instance
	 (push `(object-property-assertion !'is about'@ohd ,finding-uri ,dentition-uri) axioms)
	 
         ;; get iri for universal tooth number of missing tooth
	 (setf universal-tooth-num-uri
	       (get-eaglesoft-universal-tooth-number-iri tooth-num))

	 ;; relate missing tooth finding to universal tooth numberl
	 (push `(object-property-assertion 
		 !'has part'@ohd ,finding-uri ,universal-tooth-num-uri) axioms)
	 
         ;; add data property !ohd:'occurrence date' of the missing tooth finding
	 (push `(data-property-assertion !'occurrence date'@ohd
					 ,finding-uri
					 (:literal ,occurrence-date !xsd:date)) axioms)
	 
         ;; insert axioms about the dental exam in which the provider (if known)
         ;; found the missing tooth
	 (when provider-id
	   (setf temp-axioms (get-eaglesoft-dental-exam-axioms 
			      finding-uri provider-id provider-type practice-id record-count))

	   ;; ensure that axioms were returned
	   (when temp-axioms (setf axioms (append temp-axioms axioms))))
	 
	 ) ;; end loop
        ;;(pprint axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-missing-tooth-finding-iri (patient-id tooth-name instance-count)
  "Returns an iri for a 'missing tooth finding' that is generated by the patient id, the name of the type of the tooth, and a count variable that differentiates multipe missing teeth finds that are about the same tooth."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'missing tooth finding'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-secondary-dentition-iri (patient-id)
  "Returns an iri for a 'secondary dentition' that is generated by the patient id."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'Secondary dentition'@ohd
				     :args `("eaglesoft")))
    ;; return uri
    uri))

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
I.e., Records that have an action code '1'.
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
		"WHERE
                   action_code = '1'
                AND LENGTH(tooth_data) > 31
                AND description IN ('Missing/Extracted tooth',
                                    'Missing Tooth',
                                    'Missing tooth, more than a year') 
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

    ;; return query string
    ;;(pprint sql)
    sql))
