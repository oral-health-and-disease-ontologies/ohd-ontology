;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-oral-evaluations-ont is ran, the program verifies that the action_codes and
;; patient_history tables exist.  This is done by calling prepare-eaglesoft-db.  However, this
;; only tests that these tables exist in the user's database. If these table need to be 
;; recreated, the call get-eaglesoft-dental-exams-ont with :force-create-table key set to t.

(defun get-eaglesoft-oral-evaluations-ont (&key patient-id limit-rows force-create-table)
  "Returns an ontology of the dental exams contained in the Eaglesoft database. The patient-id key creates an ontology based on that specific patient. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."

  (let ((results nil)
	(query nil)
	(occurrence-date nil)
	(count 0))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)

    ;; get query string for dental exams
    (setf query (get-eaglesoft-oral-evaluations-query 
		 :patient-id patient-id :limit-rows limit-rows))

    (with-ontology ont (:collecting t
			:base *eaglesoft-individual-dental-exams-iri-base*
			:ontology-iri  *eaglesoft-dental-exams-ontology-iri*)
	(;; import needed ontologies
	 (as (get-ohd-import-axioms))

	 ;; get axioms for declaring annotation, object, and data properties used for ohd
	 (as (get-ohd-declaration-axioms))
	 
	 ;; get records from eaglesoft db and create axioms
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
			  (#"getString" results "ada_code")
			  (#"getString" results "r21_provider_id")
			  (#"getString" results "r21_provider_type")
			  (#"getString" results "practice_id")
		     	  (#"getString" results "row_id")))
		     (incf count))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-dental-exam-axioms (patient-id occurrence-date ada-code 
					 provider-id provider-type practice-id record-id)
  (let ((axioms nil)
	(temp-axioms nil) ; used for appending new axioms into the axioms list
	(cdt-class-uri nil)
	(cdt-uri nil)
	(oral-eval-name nil)
	(oral-eval-uri nil)
	(visit-name nil)
	(visit-uri nil)
	(patient-uri nil)
	(patient-role-uri nil)
	(provider-role-name nil)
	(provider-uri nil))

    ;; get uri of patient and that patient's role
    (setf patient-uri (get-eaglesoft-dental-patient-iri patient-id))
    (setf patient-role-uri (get-eaglesoft-dental-patient-role-iri patient-id))


    ;; declare instance of dental visit with annotations and date of visit
    (setf visit-uri (get-eaglesoft-dental-visit-iri patient-id occurrence-date))
    (setf visit-name (get-eaglesoft-dental-visit-name patient-id occurrence-date))
    (push `(annotation-assertion !rdfs:label visit-uri visit-name) axioms)
    (push `(data-property-assertion !'occurrence date'@ohd
				    ,visit-uri 
				    (:literal ,occurrence-date !xsd:date)) axioms)
    (push `(declaration (named-individual ,visit-uri)) axioms)
    (setf temp-axioms (get-ohd-instance-axioms visit-uri !'dental visit'@ohd))
    (setf axioms (append temp-axioms axioms))

    ;; declare instance of oral evaluation with annotations and date of evaluation
    ;; note: the oral evaluation is part of the visit
    (setf oral-eval-uri (get-eaglesoft-oral-evaluation-iri patient-id occurrence-date))
    (setf oral-eval-name (get-eaglesoft-oral-evaluation-name ada-code patient-id))
    (push `(annotation-assertion !rdfs:label oral-eval-uri oral-eval-name) axioms)
    (push `(data-property-assertion !'occurrence date'@ohd
				    ,oral-eval-uri 
				    (:literal ,occurrence-date !xsd:date)) axioms)
    (push `(declaration (named-individual ,oral-eval-uri)) axioms)
    (setf temp-axioms (get-ohd-instance-axioms oral-eval-uri !'dental exam'@ohd))
    (setf axioms (append temp-axioms axioms))

    ;; declare instance of cdt code as identified by the ada code that is about the procedure
    (setf cdt-class-uri (get-cdt-class-iri ada-code))
    (setf cdt-uri (get-eaglesoft-cdt-instance-iri patient-id ada-code cdt-class-uri record-id))
    (push `(declaration (named-individual ,cdt-uri)) axioms)
    (setf temp-axioms (get-ohd-instance-axioms cdt-uri cdt-class-uri))
    (setf axioms (append temp-axioms axioms))
	 
    ;; add annotion about cdt code
    (push `(annotation-assertion !rdfs:label
				 ,cdt-uri
				 ,(str+ "billing code " ada-code " for " oral-eval-name)) axioms)

;;;; relate instances ;;;;
    
    ;; oral evaluation processual part of visit
    (push `(object-property-assertion !'is part of'@ohd ,oral-eval-uri ,visit-uri) axioms)

    ;; visit realizes patient role
    (push `(object-property-assertion !'realizes'@ohd ,visit-uri ,patient-role-uri) axioms)

    ;; visit realizes provider role
    

    ;; cdt code instance is about the oral evaluation
    (push `(object-property-assertion !'is about'@ohd ,cdt-uri ,oral-eval-uri) axioms)
	 
         
    ;; if a provider is given,  get axioms that a dental visit has particpant provider
    (when provider-id
      (setf temp-axioms (get-eaglesoft-dental-provider-participant-axioms
			 restoration-uri provider-id provider-type practice-id record-id))
      ;; ensure that axioms were returned
      (when temp-axioms (setf axioms (append temp-axioms axioms))))


    
    ;;(pprint axioms)

    ;; return axioms
    axioms))



(defun get-eaglesoft-oral-evaluation-iri (patient-id occurrence-date)
  "Returns a uri for a dental visit that is generated by the patient id."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-oral-evaluations-iri-base*
				     :class-type !'dental exam'@ohd 			
				     :args `(,occurrence-date "oral evaluation" "eaglesoft")))
    ;; return uri
    uri))
  
(defun get-eaglesoft-oral-evaluation-name (ada-code patient-id)
  "Returns the name the type of restoration based on ada code."
  (let ((eval-name nil))
    ;; get the numeric part of code
    (setf ada-code (str-right ada-code 4))

    ;; check ada code to determine name of evaluation
    (cond
      ((equalp ada-code "0120") 
       (setf eval-name "periodic oral evaluation "))
      ((equalp ada-code "0140") 
       (setf eval-name "limited oral evaluation "))
      ((equalp ada-code "0150") 
       (setf eval-name "comprehensive oral evaluation "))
      ((equalp ada-code "0180") 
       (setf eval-name "comprehensive periodontal evaluation "))
      (t (setf eval-name "other oral evaluation ")))

    ;; append patient id to name of exam
    (setf eval-name (str+ eval-name "for patient " patient-id))

    ;; return name of evaluation
    eval-name))



(defun get-eaglesoft-oral-evaluations-query (&key patient-id tooth limit-rows)
  "Returns query string for retrieving data. The patient-id key restricts records only that patient or patients.  Multiple are patients are specified using commas; e.g: \"123, 456, 789\". The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. However, the tooth key only takes a single value. The limit-rows key restricts the number of records to the number specified."

#| 
This queries the eaglesoft database for all ADA codes that indicate an oral 
evaluation has been performed for ADA codes D0120, D0140, D0150, D0180.

|#

  (let ((sql nil))
    ;; build query string
    (setf sql "SET rowcount 0 ")
    
    ;; SELECT clause
    (cond 
      (limit-rows
       (setf limit-rows (format nil "~a" limit-rows)) ;ensure that limit rows is a string
       (setf sql (str+ sql " SELECT  TOP " limit-rows "  "))) 
      (t (setf sql (str+ sql " SELECT "))))
    
    (setf sql  
	  (str+ sql 
		"table_name, 
                 date_entered, 
                 date_completed, 
                 tran_date, 
                 patient_id, 
                 tooth_data, 
                 ada_code, 
                 r21_provider_id,
                 r21_provider_type,
                 practice_id,
                 row_id,
                 surface as billed_surface, 
                 get_surface_summary_from_detail(surface_detail, tooth) as charted_surface "))

    ;; FROM clause
    (setf sql (str+ sql " FROM patient_history "))

    ;; WHERE clause
    (setf sql
	  (str+ sql 
		"WHERE
                  RIGHT(ada_code, 4) IN ('0120', /* Periodic Oral Evaluation */
                                         '0140', /* Limited Oral Evaluation */
                                         '0150', /* Comprehensive Oral Evaluation */
                                         '0180') /* Comprehensive Periodontal Evaluation – New or Established Patient */
                   AND LEFT(ada_code, 1) IN ('D','0')"))


    ;; check for patient id
    (when patient-id
      (setf sql
	    (str+ sql " AND patient_id IN (" (get-single-quoted-list patient-id) ") ")))


    ;; ORDER BY clause
    (setf sql
	  (str+ sql " ORDER BY patient_id "))

    ;; return query string
    ;;(pprint sql)
    sql))
