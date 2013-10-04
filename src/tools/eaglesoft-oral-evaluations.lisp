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
			:base *eaglesoft-individual-oral-evaluations-iri-base*
			:ontology-iri  *eaglesoft-oral-evaluations-ontology-iri*)
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
		     (as (get-eaglesoft-oral-evaluation-axioms 
		     	  (#"getString" results "patient_id")
		     	  occurrence-date
			  (#"getString" results "ada_code")
			  (#"getString" results "r21_provider_id")
			  (#"getString" results "r21_provider_type")
			  (#"getString" results "row_id")))
		     (incf count))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-oral-evaluation-axioms (patient-id occurrence-date ada-code provider-id provider-type record-id)
  (let ((axioms nil)
	(patient-uri nil)
	(patient-role-uri nil)
	(temp-axioms nil) ; used for appending new axioms into the axioms list
	(cdt-class-uri nil)
	(cdt-uri nil)
	(eval-type nil)
	(oral-eval-name nil)
	(oral-eval-uri nil)
	(mouth-uri nil)
	(finding-uri nil)
	(visit-uri nil))
    
    ;; get uri of patient and patient's role
    (setf patient-uri (get-eaglesoft-dental-patient-iri patient-id))
    (setf patient-role-uri (get-eaglesoft-dental-patient-role-iri patient-id))

    ;; determine the type of oral evaluation
    (setf eval-type (get-eaglesoft-oral-evaluation-type ada-code))

    ;; declare instance of oral evaluation with annotations and date of evaluation
    ;; note: the oral evaluation is part of the visit
    (setf oral-eval-uri (get-eaglesoft-oral-evaluation-iri patient-id eval-type occurrence-date))
    (setf oral-eval-name (get-eaglesoft-oral-evaluation-name ada-code patient-id))
    (push `(annotation-assertion !rdfs:label ,oral-eval-uri ,oral-eval-name) axioms)
    (push `(data-property-assertion !'occurrence date'@ohd
				    ,oral-eval-uri 
				    (:literal ,occurrence-date !xsd:date)) axioms)
    (push `(declaration (named-individual ,oral-eval-uri)) axioms)
    (setf temp-axioms (get-ohd-instance-axioms oral-eval-uri eval-type))
    (setf axioms (append temp-axioms axioms))

    ;; get axioms that describe how the evaluation realizes the patient and provider roles
    (setf temp-axioms (get-eaglesoft-patient-provider-realization-axioms oral-eval-uri patient-id provider-id provider-type record-id))
    (setf axioms (append temp-axioms axioms))
    
    ;; declare instance of cdt code as identified by the ada code that is about the procedure
    (setf cdt-class-uri (get-cdt-class-iri ada-code))
    (setf cdt-uri (get-eaglesoft-cdt-instance-iri patient-id ada-code cdt-class-uri record-id))
    (push `(declaration (named-individual ,cdt-uri)) axioms)
    (setf temp-axioms (get-ohd-instance-axioms cdt-uri cdt-class-uri))
    (setf axioms (append temp-axioms axioms))
    
    ;; an oral evaluation has a dental finding about the patient's mouth as output
    ;; so, create instances of dental finding and mouth
    (setf mouth-uri (get-eaglesoft-mouth-iri patient-id))
    (push `(declaration (named-individual ,mouth-uri)) axioms)
    (setf temp-axioms (get-ohd-instance-axioms mouth-uri eval-type))
    (setf axioms (append temp-axioms axioms))

    ;; declare instance of 'dental finding'
    (setf finding-uri (get-eaglesoft-dental-finding-iri patient-id occurrence-date record-id))
    (setf temp-axioms (get-ohd-instance-axioms finding-uri !'dental finding'@ohd))
    (setf axioms (append temp-axioms axioms))

    ;; add annotion about cdt code
    (push `(annotation-assertion !rdfs:label
				 ,cdt-uri 
				 ,(str+ "billing code " ada-code " for " oral-eval-name)) axioms)

    ;;;; relate instances ;;;;
    
    ;; oral evaluation realizes patient role
    (push `(object-property-assertion !'realizes'@ohd ,oral-eval-uri ,patient-role-uri) axioms)
        
    ;; oral evaluation processual part of visit
    (setf visit-uri (get-eaglesoft-dental-visit-iri patient-id occurrence-date))
    (push `(object-property-assertion !'is part of'@ohd ,oral-eval-uri ,visit-uri) axioms)
    
    ;; mouth is part of patient
    (push `(object-property-assertion !'is part of'@ohd ,mouth-uri ,patient-uri) axioms)

    ;; finding is about patient's mouth
    (push `(object-property-assertion !'is about'@ohd ,finding-uri ,mouth-uri) axioms)

    ;; cdt code instance is about the oral evaluation
    (push `(object-property-assertion !'is about'@ohd ,cdt-uri ,oral-eval-uri) axioms)
	 
    ;;(pprint axioms)

    ;; return axioms
    axioms))


(defun get-eaglesoft-dental-finding-iri (patient-id occurrence-date instance-count)
  "Returns an iri for a 'dental finding' that is generated by the patient id, the occurrence date of the finding, and a count variable that differentiates multipe caries findings that are about the same tooth."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'dental finding'@ohd
				     :args `(,occurrence-date ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-oral-evaluation-type (ada-code)
  "Returns the uri for they type/class of evaluation as determined by the ada code"
  (let ((eval-type nil))
    ;; check ada code to determine class type
    (cond
      ((equalp ada-code "0120") 
       (setf eval-type !'periodic oral evaluation'@ohd))
      ((equalp ada-code "0140") 
       (setf eval-type !'problem focused oral evaluation'@ohd))
      ((equalp ada-code "0150") 
       (setf eval-type !'comprehensive oral evaluation'@ohd))
      ((equalp ada-code "0160") 
       (setf eval-type !'extensive problem focused oral evaluation'@ohd))
      ((equalp ada-code "0180") 
       (setf eval-type !'comprehensive periodontal evaluation'@ohd)))
    
    ;; return the type of evaluation
    eval-type))

(defun get-eaglesoft-oral-evaluation-iri (patient-id eval-type occurrence-date)
  "Returns a uri for an oral evaluation that is generated by the patient id, the type of evaluation, and occurrence date."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-oral-evaluations-iri-base*
				     :class-type eval-type
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
       (setf eval-name "problem focused oral evaluation "))
      ((equalp ada-code "0150") 
       (setf eval-name "comprehensive oral evaluation "))
      ((equalp ada-code "0160") 
       (setf eval-name "extensive problem focused oral evaluation"))
      ((equalp ada-code "0180") 
       (setf eval-name "comprehensive periodontal evaluation "))
      (t (setf eval-name "other oral evaluation ")))

    ;; append patient id to name of exam
    (setf eval-name (str+ eval-name "for patient " patient-id))

    ;; return name of evaluation
    eval-name))



(defun get-eaglesoft-oral-evaluations-query (&key patient-id limit-rows)
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
                 ada_code, 
                 r21_provider_id,
                 r21_provider_type,
                 practice_id,
                 row_id,
                 surface as billed_surface "))

    ;; FROM clause
    (setf sql (str+ sql " FROM patient_history "))

    ;; WHERE clause
    (setf sql
	  (str+ sql 
		"WHERE
                  RIGHT(ada_code, 4) IN ('0120', /* Periodic Oral Evaluation */
                                         '0140', /* Limited Oral Evaluation */
                                         '0150', /* Comprehensive Oral Evaluation */
                                         '0160', /* Detailed and Extensive Oral Evaluation */
                                         '0180') /* Comprehensive Periodontal Evaluation */
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
