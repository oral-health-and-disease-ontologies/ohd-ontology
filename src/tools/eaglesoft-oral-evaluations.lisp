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

;;****************************************************************
;; as part of the onotlogy, we need to determine if a dental finding was a result
;; of the oral evaluation. the result set, however, is not explicit about which evaluation
;; matches up to a finding (f indings are determined by finding description). for example, on given
;; date there may be multiple oral evaluations listed in the record set for a given date, and
;; each have the same finding.
;; For exmaple, if you run the query:
;;
;; SELECT *
;; FROM transactions
;; WHERE patient_id = 4199
;; AND tran_date = '2006-07-08'
;; AND type = 'S'
;;
;; you will find the following results:
;;
;; Patient id Date          Service code (ADA code)
;; ---------- -----------  ------------------------
;; 4199       2006-07-08    00120
;; 4199       2006-07-08    00180
;;
;; When this patient is cross-referenced in the patient conditions table:
;;
;; SELECT *
;; FROM patient_conditions
;; WHERE patient_id = 4199
;; AND date_entered = '2006-07-08'
;; 
;; you will find multiple results with an action code of "1" (missing tooth).
;;
;; thus, it not possible to determine which evaluation (D0120, D0180) resulted in a missing tooth finding.
;; 
;; to represent this in the onology, the evaluations are grouped using the ObjectOneOf construct
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
;; with the (patient id, date, description) as key and a list of the evaluation uris that occurred on that date
;; for the patient with the finding description as values. E.g.:
;;
;;  (patient 4199, 20006-07-08, "Missing tooth") => (!D0120, !D0180)
;;  note: (!D0120, !D0180) are uris
;;
;; other necessary information associated with the uri is then stored in a separate hash table
;; the ontology will then be built by iterating over hash table and building axioms from the uris

;; hash tables used to keep track oral evaluations, dental exams and findings
(defparameter *eval-uri2exam-uri* nil)
(defparameter *eval-uri2eval-info* nil)
(defparameter *finding-uri2eval-uri* nil)

(defun make-eval2exam-hash-table (&key patient-id limit-rows)
  (let (table query exam-uri eval-uri eval-type)
    ;; declare hash table
    (setf table (make-hash-table :test #'equalp))

    ;; get query string for dental exams
    (setf query (get-eaglesoft-oral-evaluations-query 
		 :patient-id patient-id :limit-rows limit-rows))

    (with-eaglesoft  (results query)
      (loop
	 while (#"next" results)
	 for id = (#"getString" results "patient_id")
	 for occurrence-date = (#"getString" results "occurrence_date")
	 for ada-code = (#"getString" results "ada_code")
	 do
	   ;; determine the type of oral evaluation
	   (setf eval-type (get-eaglesoft-oral-evaluation-type ada-code))
	   
	   ;; generate exam uri and eval uri 
	   (setf exam-uri (get-eaglesoft-dental-exam-iri id occurrence-date))
	   (setf eval-uri (get-eaglesoft-oral-evaluation-iri
			     id
			     eval-type
			     occurrence-date))

	   ;; add table enty
	   (setf (gethash eval-uri table) exam-uri))) ; end loop
    
    ;; return hash table
    table))

(defun make-eval2info-hash-table (&key patient-id limit-rows)
  (let (table query exam-uri eval-uri eval-type)
    ;; declare hash table
    (setf table (make-hash-table :test #'equalp))

    ;; get query string for dental exams
    (setf query (get-eaglesoft-oral-evaluations-query 
		 :patient-id patient-id :limit-rows limit-rows))

    (with-eaglesoft  (results query)
      (loop
	 while (#"next" results)
	 for id = (#"getString" results "patient_id")
	 for occurrence-date = (#"getString" results "occurrence_date")
	 for ada-code = (#"getString" results "ada_code")
         for tooth = (#"getString" results "tooth")
	 for provider-id = (#"getString" results "r21_provider_id")
	 for provider-type = (#"getString" results "r21_provider_type")
	 for record-id = (#"getString" results "row_id")
	 do
	   ;; determine the type of oral evaluation and eval uri
	   (setf eval-type (get-eaglesoft-oral-evaluation-type ada-code))
	   (setf eval-uri (get-eaglesoft-oral-evaluation-iri
			     id
			     eval-type
			     occurrence-date))

	   ;; some records may have empty fields recorded as empty strings instead of null
	   ;; so check for this
	   (if (< (length tooth) 1) (setf tooth nil))
	   (if (< (length ada-code) 1) (setf ada-code nil))
	   (if (< (length provider-id) 1) (setf provider-id nil))
	   (if (< (length provider-type) 1) (setf provider-type nil))
	   
	   ;; add info about evaluation to hash table
	   (setf (gethash eval-uri table)
		 (list
		    id
		    occurrence-date
		    ada-code
		    tooth
		    provider-id
		    provider-type
		    record-id))
		 )) ; end loop
    
    ;; return hash table
    table))

(defun make-finding2eval-hash-table (&key patient-id limit-rows)
  (let (table query eval-uri eval-type finding-uri)
    ;; declare hash table
    (setf table (make-hash-table :test #'equalp))

    ;; get query string for dental exams
    (setf query (get-eaglesoft-oral-evaluations-query 
		 :patient-id patient-id :limit-rows limit-rows))

    (with-eaglesoft  (results query)
      (loop
	 while (#"next" results)
	 for id = (#"getString" results "patient_id")
	 for description = (#"getString" results "description")
	 for occurrence-date = (#"getString" results "date_entered")
	 for ada-code = (#"getString" results "ada_code")
         for tooth = (#"getString" results "tooth")
	 for record-count = (#"getString" results "row_id")
	 do
	   
	   ;; determine the type of oral evaluation
	   (setf eval-type (get-eaglesoft-oral-evaluation-type ada-code))

	   ;; generate eval uri 
	   (setf eval-uri (get-eaglesoft-oral-evaluation-iri
			     patient-id
			     eval-type
			     occurrence-date))

	   ;; generate finding uri
	   (setf finding-uri
		 (get-eaglesoft-finding-iri
		    patient-id
		    description
		    :tooth-num tooth
		    :instance-count record-count))
	   
	 ;; add finding uri and eval to table
	 ;; check to see if finding already exists
	   (cond
	     ((gethash finding-uri table)
	      (setf (gethash exam-uri table)
		    (remove-duplicates
		       (push eval-uri (gethash finding-uri table)))))
	     (t
	      (setf (gethash finding-uri table) (list eval-uri)))))) ; end loop
    
    ;; return hash table
    table))
	   
(defun get-eaglesoft-oral-evaluations-ont (&key patient-id limit-rows force-create-table)
  "Returns an ontology of the oral evaluations contained in the Eaglesoft database. The patient-id key creates an ontology based on that specific patient. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."
   
    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)

    ;; build hash tables
    (setf *eval-uri2exam-uri* (make-eval2exam-hash-table :patient-id patient-id :limit-rows limit-rows))
    (setf *eval-uri2eval-info* (make-eval2info-hash-table :patient-id patient-id :limit-rows limit-rows))

    ;; ********* CURRENTLY NOT WORKING ************
    ;; the code for generating the findings that are outputs of exams is bugy
    ;;(setf *finding-uri2eval-uri* (make-finding2eval-hash-table :patient-id patient-id :limit-rows limit-rows))
    
    ;; build the ontology
    (with-ontology ont (:collecting t
		        :base *eaglesoft-individual-oral-evaluations-iri-base*
		        :ontology-iri  *eaglesoft-oral-evaluations-ontology-iri*)
	
    	( ;; import needed ontologies
	 (as (get-ohd-import-axioms))

	 ;; get axioms for declaring annotation, object, and data properties used for ohd
	 (as (get-ohd-declaration-axioms))

	 ;; build axioms about each oral evaluation
	 (as (get-eaglesoft-oral-evaluation-axioms))
	 
	 ;; build axioms about findings result from evaluation
	 ;; ***** CURRENTLY NOT WORKING ********
	 ;;(as (get-eaglesoft-oral-evaluation-finding-axioms)))
	 ;; ************************************
	 
	 )
      ;; return the ontology
      ont))

(defun get-eaglesoft-oral-evaluation-axioms ()
  (let ((axioms nil)
	(patient-role-uri nil)
	(provider-role-uri nil)
	(temp-axioms nil) ; used for appending new axioms into the axioms list
	(cdt-class-uri nil)
	(cdt-uri nil)
	(eval-type nil)
	(exam-uri nil)
	(oral-eval-name nil))

    ;; iterate over eval info hash table
    ;; for each eval uri build axioms based on the info in the value list
    ;; the value list has the structure:
    ;;   (patient-id occurrence-date ada-code tooth provider-id provider-type record-id)
    (loop
       for oral-eval-uri being the hash-keys in *eval-uri2eval-info*
           using (hash-value eval-info)
       for patient-id = (first eval-info)
       for occurrence-date = (second eval-info)
       for ada-code = (third eval-info)
       for provider-id = (fifth eval-info)
       for provider-type = (sixth eval-info)
       for record-id = (seventh eval-info)
       do
	 
         ;; get uri of patient's role
	 (setf patient-role-uri (get-eaglesoft-dental-patient-role-iri patient-id))
    
         ;; generate instance of the dental exam that the oral eval is part of
	 (setf exam-uri (gethash oral-eval-uri *eval-uri2exam-uri*))
	 (setf temp-axioms
	       (get-eaglesoft-dental-exam-axioms
		  exam-uri
		  patient-id
		  occurrence-date
		  provider-id
		  provider-type
		  record-id))
	 (setf axioms (append temp-axioms axioms))
	 
         ;; determine the type of oral evaluation
	 (setf eval-type (get-eaglesoft-oral-evaluation-type ada-code))
    
         ;; declare instance of oral evaluation with annotations and date of evaluation
         ;; note: the oral evaluation is part of the visit
	 (setf oral-eval-name
	       (get-eaglesoft-oral-evaluation-name ada-code patient-id occurrence-date))
	 (push `(declaration (named-individual ,oral-eval-uri)) axioms)
	 (setf temp-axioms (get-ohd-instance-axioms oral-eval-uri eval-type))
	 (setf axioms (append temp-axioms axioms))
    
	 (push `(annotation-assertion ; label for eval
		   !rdfs:label 
		   ,oral-eval-uri 
		   ,oral-eval-name) axioms)

         ;; date of eval
	 (push `(data-property-assertion
		   !'occurrence date'@ohd
		   ,oral-eval-uri 
		   (:literal ,occurrence-date !xsd:date)) axioms)
    
        
         ;; declare instance of cdt code as identified by the ada code that is about the procedure
	 (setf cdt-class-uri (get-cdt-class-iri ada-code))
	 (setf cdt-uri (get-eaglesoft-cdt-instance-iri patient-id ada-code cdt-class-uri record-id))
	 (push `(declaration (named-individual ,cdt-uri)) axioms)
	 (setf temp-axioms (get-ohd-instance-axioms cdt-uri cdt-class-uri))
	 (setf axioms (append temp-axioms axioms))

	 (push `(annotation-assertion ; label for cdt code
		   !rdfs:label
		   ,cdt-uri 
		   ,(str+ "billing code " ada-code " for " oral-eval-name)) axioms)
    
         ;; if provider has been identified as a specific person use r21-provider-id; 
         ;; otherwise use record-id
	 (cond
	   ((equalp provider-type "person")
	    (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri provider-id)))
	   (t
	    (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri "x" :anonymous-id record-id))))

    ;;;; relate instances ;;;;
	 
         ;; oral evaluaton is part of exam
	 (push `(object-property-assertion !'is part of'@ohd ,oral-eval-uri ,exam-uri) axioms)
    
         ;; oral evaluation realizes patient role
	 (push `(object-property-assertion !'realizes'@ohd ,oral-eval-uri ,patient-role-uri) axioms)

         ;; oral evaluation realizes provider role
	 (push `(object-property-assertion !'realizes'@ohd ,oral-eval-uri ,provider-role-uri) axioms)
        
         ;; cdt code instance is about the oral evaluation
	 (push `(object-property-assertion !'is about'@ohd ,cdt-uri ,oral-eval-uri) axioms)) ;; end loop
	 
    ;;(pprint axioms)
    
    ;; return axioms
    axioms))

(defun get-eaglesoft-oral-evaluation-finding-axioms ()
  
  ;; ***************** CURRENTLY NOT WORKING ******************
  
  (let (temp-axioms axioms exam-uri eval-uri)
    
    (loop
       for finding-uri being the hash-keys in *finding-uri2eval-uri*
           using (hash-value eval-uri-list)
       do
         ;; check the lenth of eval uri list
         ;; if 1, then the finding is the output of that eval
         ;; otherwise, use object one of syntax
	 (cond
	   ((= (length eval-uri-list) 1)
	    ;; get exam and eval uris
	    (setf eval-uri (car eval-uri-list))
	    (setf exam-uri (gethash eval-uri *eval-uri2exam-uri*))

	    ;; relate finding to eval
	    ;; check that eval is in hash table
	    (when (gethash eval-uri *eval-uri2eval-info*)
	      (push
	       `(object-property-assertion
	          !'has_specified_output'@ohd
		  ,eval-uri
		  ,finding-uri)
	       axioms)))
	   (t
	    ;; get exam uri of first eval uri in list
	    ;; find the first uri that works, since all evals are on same date
	    (loop for uri in eval-uri-list do
		 (when (gethash uri *eval-uri2exam-uri*)
		   (setf exam-uri (gethash uri *eval-uri2exam-uri*))
		   (return)))

	    ;; get object-one-of axiom and append
	    (setf temp-axioms (get-oral-eval-object-one-of-axiom eval-uri-list finding-uri))
	    (when temp-axioms
	      (setf axioms (append temp-axioms axioms))))) ;; end cond

         ;; relate finding to the dental exam that eval is part of
	 (when exam-uri
	   (push
	    `(object-property-assertion
	      !'has_specified_output'@ohd
	      ,exam-uri
	      ,finding-uri)
	    axioms))

	 ) ;; end loop

    
    ;; return axioms
    axioms))

(defun get-oral-eval-object-one-of-axiom (uri-list finding-uri)
  (let (axiom eval-uri-list)
    ;; set axiom to the class-assertion / object-one-of axiom

    ;; verify that each uri in the uri-list is in the hash table
    (loop for uri in uri-list do
	 (when (gethash uri *eval-uri2eval-info*)
	   (push uri eval-uri-list)))

    ;; now check that uri list was created
    (when eval-uri-list
      ;; check length
      (cond
	((= (length eval-uri-list) 1)
	 (setf axiom
	       `(object-property-assertion
		   !'has_specified_output'@ohd ,(car eval-uri-list) ,finding-uri)))
	 (t ;; use object-one-of axiom
	  (setf axiom
		`(class-assertion 
		    (object-some-values-from
		       !'has_specified_output'@ohd (object-one-of ,@eval-uri-list)) ,finding-uri)))))
    
    ;; return object-one-of axioms
    axiom))

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

  
(defun get-eaglesoft-oral-evaluation-name (ada-code patient-id occurrence-date)
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
    (setf eval-name (str+ eval-name "for patient " patient-id " on " occurrence-date))

    ;; return name of evaluation
    eval-name))


(defun get-eaglesoft-oral-evaluations-query
 (&key patient-id limit-rows)
  "Returns query string for retrieving data. The patient-id key restricts records only that patient or patients.  Multiple are patients are specified using commas; e.g: \"123, 456, 789\". The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. However, the tooth key only takes a single value. The limit-rows key restricts the number of records to the number specified."

#| 
This queries the eaglesoft database for all ADA codes that indicate an oral 
evaluation has been performed for ADA codes D0120, D0140, D0150, D0180.
|#
  (let (sql)

    ;; build query string
    (setf sql "SET rowcount 0 ")
    
    ;; SELECT clause
    (cond 
      (limit-rows
       (setf limit-rows (format nil "~a" limit-rows)) ;ensure that limit rows is a string
       (setf sql (str+ sql " SELECT  TOP " limit-rows "  "))) 
      (t (setf sql (str+ sql " SELECT "))))
    (setf sql (str+ sql
	       "patient_id, table_name, tran_date, date_completed, date_entered, description, ada_code, 
                get_tooth_from_data(tooth_data) as tooth, surface_detail, r21_provider_id, r21_provider_type, practice_id, row_id, 
                case
                  when tran_date <> 'n/a' then tran_date
                  when date_completed <> 'n/a' then date_completed
                  else date_entered
                 end as occurrence_date "))

    ;; FROM clause
    (setf sql (str+ sql " FROM patient_history "))

    
    (setf sql
	  (str+ sql
		"WHERE RIGHT(ada_code, 4) IN (
                             /* Periodic Oral Evaluation */
                             '0120',
                             /* Limited Oral Evaluation */
                             '0140',
                             /* Comprehensive Oral Evaluation */
                             '0150',
                             /* Comprehensive Periodontal Evaluation */
                             '0180')
                 AND LEFT(ada_code, 1) IN ('D', '0') "))

    ;; check for patient id
    (when patient-id
      (setf sql
	    (str+ sql " AND patient_id IN (" (get-single-quoted-list patient-id) ") ")))

		
    ;; ORDER BY clause
    (setf sql
	  (str+ sql "ORDER BY patient_id, tran_date "))

    ;; return query string
    sql))

(defun get-eaglesoft-findings-for-oral-evaluations-query (&key patient-id limit-rows)
  "Returns query string for retrieving data. The patient-id key restricts records only that patient or patients.  Multiple are patients are specified using commas; e.g: \"123, 456, 789\". The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. However, the tooth key only takes a single value. The limit-rows key restricts the number of records to the number specified."

#| 
This queries for all findings that have an associated oral evaluation
on the same date in the patient history table. 
|#
  (let (sql)

    ;; build query string
    (setf sql "SET rowcount 0 ")

    ;; first build a temp table with just the oral evaluations
    (setf sql
	  (str+ sql "
SELECT *
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
AND table_name = 'transactions' 

"))

    ;; now retrieve the findings that are assocated with the oral evalustions
    ;; based on the patient id and the date of finding
    ;; SELECT DISTINCT clause *** Note the use of distinct ***
    (cond 
      (limit-rows
       (setf limit-rows (format nil "~a" limit-rows)) ;ensure that limit rows is a string
       (setf sql (str+ sql " SELECT DISTINCT TOP " limit-rows "  "))) 
      (t (setf sql (str+ sql " SELECT DISTINCT "))))
    
    (setf sql (str+ sql
		    "c. patient_id, c.date_entered, get_tooth_from_data(c.tooth_data) as tooth, "
		    "c.description, o.ada_code, c.row_id "))

    ;; FROM clause
    (setf sql (str+ sql " 
FROM patient_history c
 INNER JOIN #oral_evals o
 ON o.patient_id = o.patient_id
 AND c.date_entered = o.tran_date 
 "))
    ;; WHERE clause
    
    ;; limit findings to current set of action codes
    ;; this is caries, missing, and unerupted teeth
    (setf sql
	  (str+ sql "
WHERE c.table_name = 'patient_conditions' 
AND (c.description IN ('Decay', 'Decalcification', 'Dicalsification', 'Deep dentinal/cemental caries')
                   OR c.description LIKE '%missing%'
                   OR c. description LIKE '%unerupt%' 
                   OR c.description like '%impact%') 
"))

    ;; check for patient id
    (when patient-id
      (setf sql
	    (str+ sql " AND c.patient_id IN (" (get-single-quoted-list patient-id) ") ")))

		
    ;; ORDER BY clause
    (setf sql
	  (str+ sql "ORDER BY c.patient_id, c.date_entered "))

    ;; return query string
    sql))
