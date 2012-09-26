;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-dental-providers-ont is ran, the program verifies that the 
;; action_codes and patient_history tables exist.  This is done by calling 
;; prepare-eaglesoft-db.  However, this only tests that these tables exist in the 
;; user's database. If these table need to be recreated, the call 
;; get-eaglesoft-fillings-ont with :force-create-table key set to t.

(defun get-eaglesoft-dental-providers-ont (&key r21-provider-id limit-rows force-create-table)
  "Returns an ontology of the dental providers contained in the Eaglesoft database. The r21-provider-id creates an ontology based on that specific provider as identified in the r21_provider table. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."

  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(url nil)
	(count 0))

    ;; set up connection string and query.
    (setf url (get-eaglesoft-database-url))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db url :force-create-table force-create-table)

    ;; get query string for restorations
    (setf query (get-eaglesoft-dental-providers-query
		 :r21-provider-id r21-provider-id :limit-rows limit-rows))

    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-dental-providers-iri-base*
			:ontology-iri *eaglesoft-dental-providers-ontology-iri*)
	((unwind-protect
	      (progn
		;; connect to db and get data
		(setf connection (#"getConnection" 'java.sql.DriverManager url))
		(setf statement (#"createStatement" connection))
		(setf results (#"executeQuery" statement query))
	   	
		;; import the ohd ontology
		(as `(imports ,(make-uri *ohd-ontology-iri*)))

		;; get axioms for declaring annotation, object, and data properties used for ohd
		(as (get-ohd-declaration-axioms))

		(loop while (#"next" results) do
		     ;; get axioms
		     (as (get-eaglesoft-dental-provider-axioms 
		     	  (#"getString" results "r21_provider_id")
			  (#"getString" results "r21_provider_type")
		     	  (#"getString" results "practice_id")
		     	  count))
		     (incf count)))

	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-dental-provider-axioms (r21-provider-id r21-provider-type practice-id)
  "Returns a list of axioms about a dental provider (human or organization) that is identified by the r21-provider-id."
  

)

(defun get-eaglesoft-dental-providers-query (&key r21-provider-id limit-rows)
  "Returns query string for retrieving data. The r21-provider-id key restricts records only that provider or providers as identified in the r21_provider table.  Multiple are providers are specified using commas; e.g: \"123, 456, 789\".  The limit-rows key restricts the number of records to the number specified."
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
    (setf sql (str+ sql " FROM PPM.r21_provider "))

    ;; WHERE clause
    ;; check for r21-provider-id
    (when r21
      (setf sql
	    (str+ sql " WHERE r21_provider_id IN (" (get-single-quoted-list r21-provider-id) ") ")))

    ;; ORDER BY clause
    (setf sql (str+ sql " ORDER BY r21_provider_id  "))

    ;; return query string
    ;;(pprint sql)
    sql))