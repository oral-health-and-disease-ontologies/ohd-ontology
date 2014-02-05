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
	(id nil)
	(occurrence-date nil)
	(action-code nil)
	(ada-code nil)
	(count 0)
	(key nil)
	(eval-uri nil)
	(eval-type nil)
	(tooth nil)
	(surface nil)
	(evals-ht (make-hash-table :test #'equalp))
	(eval-info-ht (make-hash-table :test #'equalp)))
     
    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)

    ;; get query string for dental exams
    (setf query (get-eaglesoft-oral-evaluations-query 
		 :patient-id patient-id :limit-rows limit-rows))
    
    ;; as part of the onotlogy, we need to determine if a dental finding was a result
    ;; of the oral evaluation. the result set, however, is not explicit about which evaluation
    ;; matches up to a finding (f indings are determined by action codes). for example, on given
    ;; date there may be three oral evaluations listed in the record set for a given date, and
    ;; each have the same finding. E.g.:
    ;; Patient id Date      ADA Code  Action Code
    ;; ---------- --------  --------  ----------
    ;; 200        1/1/2000  D0120     2
    ;; 200        1/1/2000  D0150     2
    ;; 200        1/1/2000  D0180     2
    ;;
    ;; Thus, it not possible to determine which evaluation (D0120, D0150, D0180) had the 
    ;; dental finding 2.
    ;; 
    ;; To represent this in the onology, the evaluations are grouped using the ObjectOneOf construct
    ;; and the finding is then related to this group of evaluations the following manner:
    ;; 
    ;; (class-assertion 
    ;;  (object-some-values-from !'has_specified_output'@ohd  
    ;; 			         (object-one-of !eval1 !eval2 !eval2)) !finding)
    ;;
    ;; that is, !finding is an instance of the class of C such that C contains (eval1, eval2, eval3)
    ;; and at least one of (eval1, eval2, eval3) stand in the !has_specified_output relation to !finding
    ;;
    ;; my method for grouping evaluation togehter (i.e., eval1 ... evaln) is to first build a hash table
    ;; with the (patient id, date, action code) as key and a list of the evaluation uris that occurred on that date
    ;; for the patient with the action code as values. E.g.:
    ;;
    ;;  (patient 200, 1/1/2000, 2) => (!D0120, !D0150, !D0180)
    ;;  note: (!D0120, !D0150, !D0180) are uris
    ;;
    ;; other necessary information associated with the uri is then stored in a separate hash table
    ;; the ontology will then be built by iterating over hash table and building axioms from the uris
    

    ;; get records from eaglesoft db and fill hash table
    (with-eaglesoft  (results query)
      (loop while (#"next" results) do
	 ;; determine patient-id, action-code,ada-code, occurrence date, tooth, surface
	 ;; note: the db has already been checked that all tooth fields contains a single tooth
	   (setf id (#"getString" results "patient_id"))
	   (setf action-code (#"getString" results "action_code"))
	   (setf occurrence-date (#"getString" results "occurrence_date"))
	   (setf ada-code (#"getString" results "ada_code"))
	   (setf tooth (#"getString" results "tooth"))
	   (setf surface (#"getString" results "surface"))

	 ;; determine the type of oral evaluation
	   (setf eval-type (get-eaglesoft-oral-evaluation-type ada-code))
		
	 ;; declare instance of oral evaluation with annotations and date of evaluation
	 ;; note: the oral evaluation is part of the visit
	   (setf eval-uri (get-eaglesoft-oral-evaluation-iri patient-id eval-type occurrence-date))
	   

	 ;; fill evaluatons hash table
	   (setf key (list id occurrence-date action-code))
	   (cond
	     ;; if key is found, push new value onto value list
	     ((gethash key evals-ht)
	      (setf (gethash key evals-ht) (push eval-uri (gethash key evals-ht))))

	     ;; if value not found, create list of values
	     (t
	      (setf (gethash key evals-ht) (list eval-uri))))
	   
	 ;; now add info for that eval in a separate hash table
	   (setf (gethash eval-uri eval-info-ht)
		 (list 
		  id
		  occurrence-date
		  ada-code
		  action-code 
		  tooth
		  surface 
		  (#"getString" results "r21_provider_id")
		  (#"getString" results "r21_provider_type")
		  (#"getString" results "row_id")))
	   (incf count)))

  
    ;; build the ontology
    (with-ontology ont (:collecting t
		        :base *eaglesoft-individual-oral-evaluations-iri-base*
		        :ontology-iri  *eaglesoft-oral-evaluations-ontology-iri*)

    	( ;; import needed ontologies
	 (as (get-ohd-import-axioms))

	 ;; get axioms for declaring annotation, object, and data properties used for ohd
	 (as (get-ohd-declaration-axioms))
	 
	 ;; get records from eaglesoft db and create axioms
	 ;; now with the hash tables built, iterate over evaluatons hash table 
	 ;; (the one with multiple uris) to build axioms
	 '(loop 
	     for k being the hash-keys in evals-ht do ;; using (hash-values v) do
	     ;; get axioms
	     '(as (get-eaglesoft-oral-evaluation-axioms 
		   (#"getString" results "patient_id")
		   occurrence-date
		   (#"getString" results "ada_code")
		   (#"getString" results "action_code")
		   (#"getString" results "tooth")
		   (#"getString" results "r21_provider_id")
		   (#"getString" results "r21_provider_type")
		   (#"getString" results "row_id"))))
	 )

      (loop
	   for k being the hash-keys in evals-ht using (hash-value val) 
	   for i from 1 to 10 do
	   (when (> (length val) 1)
	     (format t "~a~%" val)
	     (loop
		for uri in val do
		  (format t "	~a ~a ~%" uri (gethash uri eval-info-ht)))))
      
      
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
    (push-instance axioms oral-eval-uri eval-type)
    
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
    (setf exam-uri (get-eaglesoft-dental-exam-iri patient-id occurrence-date))
    
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
    ;; first add sql to create temp table (#oral_evals)
    ;; creates a temp table name #oral_evals that only oral evaluations in it
    ;; I do this in order to simplfy the readability of the table joins
    (setf sql (str+ "SET rowcount 0 " (get-eaglesoft-oral-evaluations-temp-table-query)))
    
    ;; now build rest of query string by joining temp tables
    ;; SELECT clause
    (cond 
      (limit-rows
       (setf limit-rows (format nil "~a" limit-rows)) ;ensure that limit rows is a string
       (setf sql (str+ sql " SELECT TOP " limit-rows "  "))) 
      (t (setf sql (str+ sql " SELECT "))))
    
    (setf sql  
	  (str+ sql 
		" o.patient_id, o.table_name, o.occurrence_date, o.description, "
		" c.condition_description, o.ada_code, o.ada_code_description, "
		" o.r21_provider_id, o.r21_provider_type, o.practice_id, o.row_id, "
		" c.tooth, c.surface, c.action_code "))
    
    ;; FROM clause
    (setf sql (str+ sql 
		    " FROM #oral_evals o "
		    " LEFT JOIN #conditions c "
		    "   ON o.patient_id = c.patient_id "
		    "   AND o.occurrence_date = c.date_entered "))
    
    ;; WHERE clause
    ;; check for patient id
    (when patient-id
      (setf sql
	    (str+ sql " WHERE o.patient_id IN (" (get-single-quoted-list patient-id) ") ")))

    ;; ORDER BY clause
    (setf sql
	  (str+ sql " ORDER BY o.patient_id, o.occurrence_date "))

    ;; return query string
    ;;(pprint sql)
    sql))

(defun get-eaglesoft-oral-evaluations-temp-table-query ()
  "Returns sql for two temp tables named #oral_evals and #conditions.
#oral_evals that only oral evaluations in it. 
#conditions has information from the patient_condtions table that is joined with patient_conditions_extra.
I do this in order to simplfy the readability of the table joins."

  " /*create a temp table with just the oral evaluations in it */
  SELECT 
    patient_id, table_name, description, ada_code, ada_code_description, 
    r21_provider_id, r21_provider_type, practice_id, row_id, 
    occurrence_date =
    CASE
      WHEN tran_date <> 'n/a' THEN tran_date
      WHEN date_completed <> 'n/a' THEN date_completed
      ELSE date_entered
    END


  INTO 
    #oral_evals 
  FROM 
    patient_history
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


   /* create temp table with information from patient_conditions joined with patient_conditions_extra */
  SELECT
    c.patient_id, 
    c.date_entered,
    c.action_code,  
    c.description as condition_description, 
    get_tooth_from_data(ce.tooth_data) as tooth, 
    get_surface_summary_from_detail(ce.surface_detail, c.tooth) as surface 
  INTO
    #conditions 
  FROM
    patient_conditions c
  LEFT JOIN 
    patient_conditions_extra ce
  ON
    c.counter_id = ce.counter_id
  ORDER BY
    c.patient_id, c.date_entered 

")
 
 