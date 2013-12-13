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
;; recreated, the call get-eaglesoft-oral-evaluations-ont with :force-create-table key set to t.

(defun get-eaglesoft-oral-evaluations-ont (&key patient-id limit-rows force-create-table)
  "Returns an ontology of the oral evaluations contained in the Eaglesoft database. The patient-id key creates an ontology based on that specific patient. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."

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
		     (#"getString" results "action_code")
		     (#"getString" results "tooth_data")
		     (#"getString" results "r21_provider_id")
		     (#"getString" results "r21_provider_type")
		     (#"getString" results "row_id")))
		(incf count))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-oral-evaluation-axioms (patient-id occurrence-date ada-code action-code tooth-data provider-id provider-type record-id)
  (let ((axioms nil)
	(patient-role-uri nil)
	(provider-role-uri nil)
	(temp-axioms nil) ; used for appending new axioms into the axioms list
	(cdt-class-uri nil)
	(cdt-uri nil)
	(eval-type nil)
	(oral-eval-name nil)
	(oral-eval-uri nil)
	(exam-uri nil)
	(finding-uri nil)
	(tooth-name nil)
	(teeth-list nil))
    
    ;; get uri of patient and patient's role
    (setf patient-uri (get-eaglesoft-dental-patient-iri patient-id))
    (setf patient-role-uri (get-eaglesoft-dental-patient-role-iri patient-id))

    ;; determine the type of oral evaluation
    (setf eval-type (get-eaglesoft-oral-evaluation-type ada-code))

    ;; declare instance of oral evaluation with annotations and date of evaluation
    ;; note: the oral evaluation is part of the visit
    (setf oral-eval-uri (get-eaglesoft-oral-evaluation-iri patient-id eval-type occurrence-date))
    (setf oral-eval-name (get-eaglesoft-oral-evaluation-name ada-code patient-id))
    (push `(declaration (named-individual ,oral-eval-uri)) axioms)
    (setf temp-axioms (get-ohd-instance-axioms oral-eval-uri eval-type))
    (setf axioms (append temp-axioms axioms))
    (push `(annotation-assertion ; label for eval
	    !rdfs:label 
	    ,oral-eval-uri 
	    ,oral-eval-name) axioms)
    (push `(data-property-assertion ; date of eval
	    !'occurrence date'@ohd
	    ,oral-eval-uri 
	    (:literal ,occurrence-date !xsd:date)) axioms)
    
        
    ;; declare instance of cdt code as identified by the ada code that is about the procedure
    (setf cdt-class-uri (get-cdt-class-iri ada-code))
    (setf cdt-uri (get-eaglesoft-cdt-instance-iri patient-id ada-code cdt-class-uri record-id))
    (push `(declaration (named-individual ,cdt-uri)) axioms)
    (setf temp-axioms (get-ohd-instance-axioms cdt-uri cdt-class-uri))
    (setf axioms (append temp-axioms axioms))

    ;; add annotion about cdt code
    (push `(annotation-assertion 
	    !rdfs:label
	    ,cdt-uri 
	    ,(str+ "billing code " ada-code " for " oral-eval-name)) axioms)
    
    ;; determine the dental exam that oral eval is part of
    ;; note: annotations for the dental exam are in the dental exam ontology
    (setf exam-uri (get-eaglesoft-dental-exam-iri patient-id occurrence-date provider-id))
    
    ;; determine the type of finding that 
    (setf action-code (format nil "~a" action-code)) ; ensure action code is a string
    (when (member action-code '("1" "2" "3" "4" "18"))
      ;; get list of teeth from tooth-data array
      (setf teeth-list (get-eaglesoft-teeth-list tooth-data))
    
      ;; for each tooth make a finding that is the ouput of exam
      (loop for tooth in teeth-list do
	   (setf tooth-name (number-to-fma-tooth tooth :return-tooth-name t))
	   (setf finding-uri 
		 (get-eaglesoft-finding-iri 
		  patient-id action-code :tooth-name tooth-name :tooth-num tooth :instance-count record-id))
	   (push `(object-property-assertion
		   !'has_specified_output'@ohd
		   ,exam-uri
		   ,finding-uri) axioms)))

    ;; if provider has been identified as a specific person use r21-provider-id; 
    ;; otherwise use record-id
    (cond
      ((equalp provider-type "person")
       (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri provider-id)))
      (t
       (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri "x" :anonymous-id record-id))))

    ;;;; relate instances ;;;;
    
    ;; oral evaluation realizes patient role
    (push `(object-property-assertion !'realizes'@ohd ,oral-eval-uri ,patient-role-uri) axioms)

    ;; oral evaluation realizes provider role
    (push `(object-property-assertion !'realizes'@ohd ,oral-eval-uri ,provider-role-uri) axioms)
        
    ;; oral evaluation processual part of a dental exam
    (push `(object-property-assertion !'is part of'@ohd ,oral-eval-uri ,exam-uri) axioms)
    
    ;; cdt code instance is about the oral evaluation
    (push `(object-property-assertion !'is about'@ohd ,cdt-uri ,oral-eval-uri) axioms)
	 
    ;;(pprint axioms)

    ;; return axioms
    axioms))


(defun get-eaglesoft-oral-evaluation-type (ada-code)
  "Returns the uri for they type/class of evaluation as determined by the ada code"
  (let ((eval-type nil))
    ;; check ada code to determine class type
    ;; note: only look at right 4 characters; e.g. D0120 -> 0120
    (setf ada-code (str-right ada-code 4))
    (cond
      ((equalp ada-code "0120") 
       (setf eval-type !'periodic oral evaluation'@ohd))
      ((equalp ada-code "0140") 
       (setf eval-type !'limited oral evaluation'@ohd))
      ((equalp ada-code "0150") 
       (setf eval-type !'comprehensive oral evaluation'@ohd))
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
       (setf eval-name "limited oral evaluation "))
      ((equalp ada-code "0150") 
       (setf eval-name "comprehensive oral evaluation "))
      ((equalp ada-code "0180") 
       (setf eval-name "comprehensive periodontal evaluation "))
      (t (setf eval-name "other oral evaluation ")))

    ;; append patient id to name of oral eval
    (setf eval-name (str+ eval-name "for patient " patient-id))

    ;; return name of evaluation
    eval-name))


(defun get-eaglesoft-oral-evaluations-query
 (&key patient-id limit-rows)
  "Returns query string for retrieving data. The patient-id key restricts records only that patient or patients.  Multiple are patients are specified using commas; e.g: \"123, 456, 789\". The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. However, the tooth key only takes a single value. The limit-rows key restricts the number of records to the number specified."

#| 
This queries the eaglesoft database for all ADA codes that indicate an oral 
evaluation has been performed for ADA codes D0120, D0140, D0150, D0180.
|#

  (let ((sql nil))
    ;; build query string
    ;; first add sql to create temp tables
    (setf sql 
	  "
SET rowcount 0

/* create temp table with just oral evals */
SELECT patient_id, table_name, tran_date, date_completed, date_entered, description, ada_code, ada_code_description, 
       tooth_data, surface_detail, r21_provider_id, r21_provider_type, practice_id, row_id
INTO #oral_evals
FROM patient_history
WHERE RIGHT(ada_code, 4) IN (
                             /* Periodic Oral Evaluation */
                             '0120',
                             /* Limited Oral Evaluation */
                             '0140',
                             /* Comprehensive Oral Evaluation */
                             '0150',
                             /* Comprehensive Periodontal Evaluation */
                             '0180')
AND LEFT(ada_code, 1) IN ('D', '0')
ORDER BY patient_id, tran_date

/* create temp table of patients with condtions whose date entered match an oral_eval transaction date */
/* note: I used the patient_history table to avoid having to do a join with patient_conditions_extra */
SELECT patient_id, date_entered, description, action_code, tooth_data, get_surface_summary_from_detail(surface_detail, tooth) AS surface
INTO #condition_findings
FROM patient_history
WHERE date_entered IN
                       (SELECT DISTINCT tran_date
                    FROM #oral_evals)
AND patient_id IN
                   (SELECT DISTINCT patient_id
                FROM #oral_evals)
AND table_name = 'patient_conditions'
  /*
  filter by the descriptions we are  using to create findings
  findings descriptions below are for:
  action codes 2,3,4 - caries
  action code 1 - missing teeth
  action code 18 - unerrupted teeth
  */
AND description IN (
                    /* caries findings */
                    'Decay', 'Decalcification', 'Dicalsification', 'Deep dentinal/cemental caries',
                    /* missing teeth findings */
                    'Missing/Extracted tooth', 'Missing Tooth', 'Missing tooth, more than a year',
                    /* unerrupted teeth */
                    'Unerupted Tooth', 'Impacted Mesial', 'Impacted Distal', 'Impacted')
  
  /* ensure that finding has tooth / surface data associated with it */
AND LENGTH(tooth_data) > 31
AND tooth_data LIKE '%Y%'

")

    ;; now build rest of query string by joining temp tables
    ;; SELECT clause
    (cond 
      (limit-rows
       (setf limit-rows (format nil "~a" limit-rows)) ;ensure that limit rows is a string
       (setf sql (str+ sql " SELECT  TOP " limit-rows "  "))) 
      (t (setf sql (str+ sql " SELECT "))))
    (setf sql  
	  (str+ sql "o.patient_id, o.table_name, o.tran_date, o.date_completed, o.date_entered, 
                     o.description, c.description AS condition_description, o.ada_code, o.ada_code_description, 
                     o.r21_provider_id, o.r21_provider_type, o.practice_id, o.row_id, c.action_code, c.tooth_data, c.surface"))

    ;; FROM clause
    (setf sql (str+ sql " FROM #oral_evals o LEFT JOIN #condition_findings c ON o.patient_id = c.patient_id AND o.tran_date = c.date_entered "))
    
    ;; WHERE clause
    ;; check for patient id
    (when patient-id
      (setf sql
	    (str+ sql " WHERE o.patient_id IN (" (get-single-quoted-list patient-id) ") ")))

    ;; ORDER BY clause
    (setf sql
	  (str+ sql " ORDER BY o.patient_id, o.tran_date "))

    ;; return query string
    ;;(pprint sql)
    sql))
