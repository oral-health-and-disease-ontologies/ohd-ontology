;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-dental-visits-ont is ran, the program verifies that the 
;; action_codes and patient_history tables exist.  This is done by calling 
;; prepare-eaglesoft-db.  However, this only tests that these tables exist in the 
;; user's database. If these table need to be recreated, the call 
;; get-eaglesoft-fillings-ont with :force-create-table key set to t.

(defun get-eaglesoft-dental-visits-ont 
    (&key patient-id r21-provider-id limit-rows force-create-table)
  "Returns an ontology of the dental visits contained in the Eaglesoft database. A dental visit is broadly defined as any encouter found in the patient_history table regardless of code/reason associated the encounter. The patient-id key creates an ontology based on that specific patient. The r21-provider-key creates an ontology for a specific provider.  The patient-id and r21-provider-id keys can both be used in tandem. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."

  (let ((results nil)
	(query nil)
	(occurrence-date nil)
	(count 0))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)

    
    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-dental-visits-iri-base*
			:ontology-iri *eaglesoft-dental-visits-ontology-iri*)
	(;; import the ohd, patient, and provider ontologies
	 (as `(imports ,(make-uri *ohd-ontology-iri*)))
	 (as `(imports ,(make-uri *eaglesoft-dental-patients-ontology-iri*)))
	 (as `(imports ,(make-uri *eaglesoft-dental-providers-ontology-iri*)))

	 ;; get axioms for declaring annotation, object, and data properties used for ohd
	 (as (get-ohd-declaration-axioms))
	 
	 ;; extract records from eaglesoft db and create axioms
	 ;; get query string for dental visits
	 (setf query (get-eaglesoft-dental-visits-query
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
		(as (get-eaglesoft-dental-visits-axioms 
		     (#"getString" results "patient_id")
		     occurrence-date
		     (#"getString" results "r21_provider_id")
		     (#"getString" results "r21_provider_type")
		     (#"getString" results "row_id")))
		(incf count))))
      
      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-dental-visits-axioms (patient-id occurrence-date provider-id provider-type record-id)
  "Returns a list of axioms about patient's dental visits."
  (let ((axioms nil)
	(temp-axioms nil)	  ; used for appending instance axioms
	(visit-uri nil)
	(visit-name nil)
	(provider-role-uri nil)
	(patient-role-uri nil))

    ;; get axioms for instance of visit
    (setf visit-uri (get-eaglesoft-dental-visit-iri patient-id occurrence-date))
    (setf temp-axioms (get-ohd-instance-axioms visit-uri !'dental visit'@ohd))
    (setf axioms (append temp-axioms axioms))
        
    ;; format visit's label and add annotation
    (setf visit-name (get-eaglesoft-dental-visit-name patient-id occurrence-date)) 
    (push `(annotation-assertion !rdfs:label ,visit-uri ,visit-name) axioms)
    
    
    ;; get patient-role and provider-role iri
    (setf patient-role-uri (get-eaglesoft-dental-patient-role-iri patient-id))
    
    ;; if provider has been identified as a specific person use r21-provider-id; 
    ;; otherwise use record-id
    (cond
      ((equalp provider-type "person")
       (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri provider-id)))
      (t
       (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri "x" :anonymous-id record-id))))

    ;; visit realizes patient-role and provider-role
    (push `(object-property-assertion !'realizes'@ohd ,visit-uri ,patient-role-uri) axioms)
    (push `(object-property-assertion !'realizes'@ohd ,visit-uri ,provider-role-uri) axioms)

    ;; return axioms
    axioms))


(defun get-eaglesoft-dental-visits-query (&key patient-id r21-provider-id limit-rows)
  "Returns query string for retrieving provider data. The r21-provider-id key restricts records only that provider or providers as identified in the r21_provider table.  Multiple are providers are specified using commas; e.g: \"123, 456, 789\".  The limit-rows key restricts the number of records to the number specified."
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
                 row_id "))

    ;; FROM clause
    (setf sql (str+ sql " FROM patient_history "))

    ;; WHERE clause
    ;; check to see if both patient-id and r21-provider-id are specified
    (cond
      ((and patient-id r21-provider-id)
       (setf sql
	     (str+ sql 
		   " WHERE patient_id IN (" (get-single-quoted-list patient-id) ") "
		   " AND r21_provider_id IN (" (get-single-quoted-list r21-provider-id) ") ")))
      (t ;; check for patient-id and r21-provider-id separately
       ;; check for patient id
       (when patient-id
	 (setf sql
	       (str+ sql " WHERE patient_id IN (" (get-single-quoted-list patient-id) ") ")))

       ;; check for r21-provider-id
       (when r21-provider-id
	 (setf sql
	       (str+ sql " WHERE r21_provider_id IN (" (get-single-quoted-list r21-provider-id) ") ")))))

    ;; ORDER BY clause
    (setf sql (str+ sql " ORDER BY patient_id  "))

    ;; return query string
    ;;(pprint sql)
    sql))



    