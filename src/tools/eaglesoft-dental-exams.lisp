;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation:
;; When get-eaglesoft-dental-exams-ont is ran, the program verifies that the
;; action_codes and patient_history tables exist.  This is done by calling
;; prepare-eaglesoft-db.  However, this only tests that these tables exist in the
;; user's database. If these table need to be recreated, the call
;; get-eaglesoft-fillings-ont with :force-create-table key set to t.

(defun get-eaglesoft-dental-exams-ont
    (&key patient-id r21-provider-id limit-rows force-create-table)
  "Returns an ontology of the dental exams contained in the Eaglesoft database. A dental exam is broadly defined as any oral evaluation found in the patient_history table. Findings, such caries or missing teeth, are associated with dental exams. The patient-id key creates an ontology based on that specific patient. The r21-provider-key creates an ontology for a specific provider.  The patient-id and r21-provider-id keys can both be used in tandem. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."

  (let ((results nil)
	(query nil)
	(occurrence-date nil)
	(count 0))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)


    (with-ontology ont (:collecting t
			:base *eaglesoft-individual-dental-exam-iri-base*
			:ontology-iri *eaglesoft-dental-exams-ontology-iri*)
	(;; import the ohd, patient, and provider ontologies
	 (as `(imports ,(make-uri *ohd-ontology-iri*)))
	 (as `(imports ,(make-uri *eaglesoft-dental-patients-ontology-iri*)))
	 (as `(imports ,(make-uri *eaglesoft-dental-providers-ontology-iri*)))
	 (as `(imports ,(make-uri *eaglesoft-dental-visits-ontology-iri*)))

	 ;; get axioms for declaring annotation, object, and data properties used for ohd
	 (as (get-ohd-declaration-axioms))

	 ;; first get all the findings out of the db; these are records that have an action_code
	 (setf query (get-eaglesoft-dental-exam-tooth-findings-query
		      :patient-id patient-id :r21-provider-id r21-provider-id :limit-rows limit-rows))

	 ;; get records from eaglesoft for which there was an exam, but no findinings where reported
	 (setf query (get-eaglesoft-dental-exams-with-no-findings-query 
		      :patient-id patient-id :r21-provider-id r21-provider-id :limit-rows limit-rows))

	 (with-eaglesoft (results query)
	   (loop while (#"next" results) do
	        ;; determine this occurrence date
		(setf occurrence-date
		      (get-eaglesoft-occurrence-date
		       (#"getString" results "table_name")
		       (#"getString" results "date_entered")
		       (#"getString" results "date_completed")
		       (#"getString" results "tran_date")))

	        ;; get axioms
		(as (get-eaglesoft-dental-exam-axioms
		     (#"getString" results "patient_id")
		     occurrence-date
		     (#"getString" results "r21_provider_id")
		     (#"getString" results "r21_provider_type")
		     (#"getString" results "row_id")))
		(incf count))))

      ;; return the ontology
      (values ont count))))

;; the function has been repalaced by the function of the same name in r21-utiliities
;; (defun get-eaglesoft-dental-exam-axioms (patient-id occurrence-date provider-id provider-type record-id)
;;   "Returns a list of axioms about patient's dental exams."
;;   (let ((axioms nil)
;; 	(temp-axioms nil)	  ; used for appending instance axioms
;; 	(exam-uri nil)
;; 	(exam-name nil)
;; 	(visit-uri nil)
;; 	(provider-role-uri nil)
;; 	(patient-role-uri nil))

;;     ;; get axioms for instance of exam
;;     (setf exam-uri (get-eaglesoft-dental-exam-iri patient-id occurrence-date))
;;     (setf temp-axioms (get-ohd-instance-axioms exam-uri !'dental exam'@ohd))
;;     (setf axioms (append temp-axioms axioms))

;;     ;; format exam's label and add annotation
;;     (setf exam-name (get-eaglesoft-dental-exam-name patient-id occurrence-date))
;;     (push `(annotation-assertion !rdfs:label ,exam-uri ,exam-name) axioms)

;;     ;; add data property !ohd:'occurrence date' of the exam
;;     (push `(data-property-assertion
;; 	    !'occurrence date'@ohd
;; 	    ,exam-uri
;; 	    (:literal ,occurrence-date !xsd:date)) axioms)

;;     ;; get the visit that the dental exam is part of
;;     (setf visit-uri (get-eaglesoft-dental-visit-iri patient-id occurrence-date))
;;     (push `(object-property-assertion !'is part of'@ohd ,exam-uri ,visit-uri) axioms)
    

;;     ;; get patient-role and provider-role iri
;;     (setf patient-role-uri (get-eaglesoft-dental-patient-role-iri patient-id))

;;     ;; if provider has been identified as a specific person use r21-provider-id;
;;     ;; otherwise use record-id
;;     (cond
;;       ((equalp provider-type "person")
;;        (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri provider-id)))
;;       (t
;;        (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri "x" :anonymous-id record-id))))

;;     ;; exam realizes patient-role and provider-role
;;     (push `(object-property-assertion !'realizes'@ohd ,exam-uri ,patient-role-uri) axioms)
;;     (push `(object-property-assertion !'realizes'@ohd ,exam-uri ,provider-role-uri) axioms)

;;     ;; return axioms
;;     axioms))


(defun get-eaglesoft-dental-exams-query (&key patient-id r21-provider-id limit-rows)
  "Returns query string for retrieving exam data. The r21-provider-id key restricts records only that provider or providers as identified in the r21_provider table.  Multiple are providers are specified using commas; e.g: \"123, 456, 789\".  The limit-rows key restricts the number of records to the number specified."
  (let ((sql nil))

    ;; build query string
    (setf sql "SET rowcount 0 ")

    ;; determine number of rows for SELECT clause
    (cond
      (limit-rows
       (setf limit-rows (format nil "~a" limit-rows)) ;ensure that limit rows is a string
       (setf sql (str+ sql " SELECT  TOP " limit-rows " ")))
      (t (setf sql (str+ sql " SELECT "))))

    ;; SELECT clause
    (setf sql
	  (str+ sql
		"table_name,
                 date_entered,
                 date_completed,
                 tran_date,
                 patient_id,
                 r21_provider_id,
                 r21_provider_type,
                 row_id,
                 action_code,
                 description, 
                 tooth_data "))

    ;; FROM clause
    (setf sql (str+ sql " FROM patient_history "))

    ;; WHERE clause
    ;; pull out records that have action codes and tooth_data
    ;; note: I am looking at conditions that have tooth data associated with them
    ;; there are three records for missing teeth action code (i.e., 1) but no tooth data
    ;; I am ingnoring these records (they don't make sense)
    (setf sql
	  (str+ sql
		"WHERE action_code <> 'n/a' " 
		"AND action_code IS NOT NULL "
		"AND tooth_data IS NOT NULL "))
    
    
    ;; criteria for WHERE clause
    (cond
      (;; check for patient id
       patient-id
       (setf sql
	     (str+ sql " AND patient_id IN (" (get-single-quoted-list patient-id) ") ")))

      (;; check for r21-provider-id
       r21-provider-id
       (setf sql
	     (str+ sql " AND r21_provider_id IN (" (get-single-quoted-list r21-provider-id) ") "))))

    ;; ORDER BY clause
    (setf sql (str+ sql " ORDER BY patient_id  "))

    ;; return query string
    ;;(pprint sql)
    sql))

(defun get-eaglesoft-dental-exams-with-no-findings-query (&key patient-id r21-provider-id limit-rows)
  "Returns query string for retrieving exam data. The r21-provider-id key restricts records only that provider or providers as identified in the r21_provider table.  Multiple are providers are specified using commas; e.g: \"123, 456, 789\".  The limit-rows key restricts the number of records to the number specified."
  (let ((sql nil))

    ;; build query string
    (setf sql "SET rowcount 0 ")
    
    ;; determine number of rows for SELECT clause
    (cond 
      (limit-rows
       (setf limit-rows (format nil "~a" limit-rows)) ;ensure that limit rows is a string
       (setf sql (str+ sql " SELECT  TOP " limit-rows " "))) 
      (t (setf sql (str+ sql " SELECT "))))

    ;; SELECT clause
    (setf sql  
	  (str+ sql 
		"h.table_name, 
                 h.date_entered, 
                 h.date_completed, 
                 h.tran_date, 
                 h.patient_id, 
                 h.r21_provider_id,
                 h.r21_provider_type, 
                 h.row_id, 
                 h.description, 
                 h.tooth_data, 
                 c.action_code "))

    ;; FROM clause
    (setf sql (str+ sql 
		    " FROM patient_history h "
		    " LEFT JOIN patient_conditions c "
		    " ON h.patient_id = c.patient_id "
		    " AND h.tran_date = c.date_entered "))

    ;; WHERE clause
    ;; pull out oral evaluations cdt codes
    (setf sql
	  (str+ sql
		"WHERE RIGHT(h.ada_code, 4) IN (
                             /* Periodic Oral Evaluation */
                             '0120',
                             /* Limited Oral Evaluation */
                             '0140',
                             /* Comprehensive Oral Evaluation */
                             '0150',
                             /* Comprehensive Periodontal Evaluation */
                             '0180')
               AND LEFT(ada_code, 1) IN ('D', '0')
               OR action_code <> 'n/a' "))

    ;; further criteria on WHERE clause
    (cond
      (;; check for patient id
       patient-id
       (setf sql
	     (str+ sql
		   " WHERE patient_id IN (" (get-single-quoted-list patient-id) ") "
		   " AND r21_provider_id IN (" (get-single-quoted-list r21-provider-id) ") ")))
      (t ;; check for patient-id and r21-provider-id separately
       ;; check for patient id
       (when patient-id
	 (setf sql
	       (str+ sql " WHERE patient_id IN (" (get-single-quoted-list patient-id) ") ")))

      (;; check for r21-provider-id
       r21-provider-id
       (setf sql
	     (str+ sql " AND h.r21_provider_id IN (" (get-single-quoted-list r21-provider-id) ") "))))

    ;; ORDER BY clause
    (setf sql (str+ sql " ORDER BY h.patient_id  "))

    ;; return query string
    ;;(pprint sql)
    sql))




