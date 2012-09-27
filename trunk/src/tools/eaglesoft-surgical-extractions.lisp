;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-surgical-extractions-ont is ran, the program verifies that the 
;; action_codes and patient_history tables exist.  This is done by calling 
;; prepare-eaglesoft-db.  However, this only tests that these tables exist in the 
;; user's database. If these table need to be recreated, the call 
;; get-eaglesoft-fillings-ont with :force-create-table key set to t.

(defun get-eaglesoft-surgical-extractions-ont (&key patient-id tooth limit-rows force-create-table)
  "Returns an ontology of the surgical extractions contained in the Eaglesoft database. The patient-id key creates an ontology based on that specific patient. The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."
  (let ((results nil)
	(query nil)
	(occurrence-date nil)
	(count 0))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)

    ;; get query string for restorations
    (setf query (get-eaglesoft-surgical-extractions-query
		 :patient-id patient-id :tooth tooth :limit-rows limit-rows))

    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-surgical-extractions-iri-base* 
			:ontology-iri *eaglesoft-surgical-extractions-ontology-iri*)
	(;; import the ohd and patient ontology
	 (as `(imports ,(make-uri *ohd-ontology-iri*)))
	 (as `(imports ,(make-uri *eaglesoft-dental-patients-ontology-iri*)))
		
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
		(as (get-eaglesoft-surgical-extraction-axioms 

		     (#"getString" results "patient_id")
		     occurrence-date
		     (#"getString" results "tooth_data")
		     (#"getString" results "ada_code")
		     count))
		(incf count))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-surgical-extraction-axioms 
    (patient-id occurrence-date tooth-data ada-code record-count)
  (let ((axioms nil)
	(cdt-class-uri nil)
	(cdt-uri nil)
	(patient-uri nil)
	(extraction-role-uri nil)
	(extraction-procedure-uri nil)
	(procedure-type-uri nil)
	(tooth-name nil)
	(tooth-uri nil)
	(tooth-type-uri nil)
	(teeth-list nil))

    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-eaglesoft-teeth-list tooth-data))

    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 
	 ;; get uri of patient
	 (setf patient-uri
	       (get-eaglesoft-dental-patient-iri patient-id))
	 
         ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-with-number t))
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-uri (get-eaglesoft-tooth-iri patient-id tooth-type-uri))

	 (push `(declaration (named-individual ,tooth-uri)) axioms)
         ;; note: append puts lists together and doesn't put items in list (like push)
	 (setf axioms
	       (append (get-ohd-instance-axioms tooth-uri tooth-type-uri) axioms))
	 
        ;; add annotation about tooth
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ tooth-name
					     " of patient " patient-id)) axioms)
	 
       ;; declare instance of !ohd:'tooth to be extracted role'
	 (setf extraction-role-uri
	       (get-eaglesoft-tooth-to-be-extracted-role-iri patient-id tooth record-count))
		
	 (push `(declaration (named-individual ,extraction-role-uri)) axioms)
	 (setf axioms
	       (append (get-ohd-instance-axioms extraction-role-uri !'tooth to be extracted role'@ohd) 
		       axioms))

	 ;; add annotation about 'tooth to be extracted role'
	 (push `(annotation-assertion !rdfs:label 
				      ,extraction-role-uri
				      ,(str+ "tooth to be extracted role for " 
					     tooth-name " of patient " 
					     patient-id)) axioms)


         ;; declare instance of extraction procedure
	 (setf procedure-type-uri !'tooth extraction procedure'@ohd)
	 (setf extraction-procedure-uri
	       (get-eaglesoft-tooth-extraction-procedure-iri
		patient-id tooth-name procedure-type-uri record-count))
	 (push `(declaration (named-individual ,extraction-procedure-uri)) axioms)
	 (setf axioms
	       (append (get-ohd-instance-axioms extraction-procedure-uri procedure-type-uri) axioms))

	 ;; add annotation about this extraction procedure
	 (push `(annotation-assertion !rdfs:label 
				      ,extraction-procedure-uri
				      ,(str+ "tooth extraction procedure performed on " 
					     tooth-name " of patient " 
					     patient-id)) axioms)

	 ;; add data property !ohd:'occurrence date' to restoration
	 (push `(data-property-assertion !'occurrence date'@ohd
					 ,extraction-procedure-uri
					 (:literal ,occurrence-date !xsd:date)) axioms)

	 ;; declare instance of cdt code as identified by the ada code that is about the procedure
	 (setf cdt-class-uri (get-cdt-class-iri ada-code))
	 (setf cdt-uri (get-eaglesoft-cdt-instance-iri patient-id ada-code cdt-class-uri record-count))
	 (push `(declaration (named-individual ,cdt-uri)) axioms)
	 (setf axioms
	       (append (get-ohd-instance-axioms cdt-uri cdt-class-uri) axioms))
	 
	 ;; add annotion about cdt code
	 (push `(annotation-assertion !rdfs:label
				      ,cdt-uri
				      ,(str+ "billing code " ada-code " for tooth extraction  procedure on "
					     tooth-name " of patient " patient-id)) axioms)
	 
	  ;;;; relate instances ;;;;
	 
         ;; 'tooth to be extracted role' inheres in tooth
	 (push `(object-property-assertion !'inheres in'@ohd
					   ,extraction-role-uri ,tooth-uri) axioms)

         ;; 'tooth extraction procedure' realizes 'tooth to be extracted role'
	 (push `(object-property-assertion !'realizes'@ohd
					   ,extraction-procedure-uri
					   ,extraction-role-uri) axioms)
	 
         ;; 'tooth extraction procedure' has particpant tooth
	 (push `(object-property-assertion !'has participant'@ohd
					   ,extraction-procedure-uri ,tooth-uri) axioms)

         ;;  'tooth extraction procedure' has particpant patient
	 (push `(object-property-assertion !'has participant'@ohd
					   ,extraction-procedure-uri ,patient-uri) axioms)

	 ;; cdt code instance is about the 'crown restoration' process
	 (push `(object-property-assertion !'is about'@ohd
					   ,cdt-uri ,extraction-procedure-uri) axioms)
	 ) ;; end loop
    
    ;;(pprint axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-tooth-to-be-extracted-role-iri (patient-id tooth-name instance-count)
  "Returns an iri for a 'tooth to be extracted role' that is generated by the patient id, the name of the type of the tooth, and a count variable that used differientiate tooth role intances that have the same patient-id/tooth-name but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'tooth to be extracted role'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-tooth-extraction-procedure-iri (patient-id tooth-name
						     procedure-type-iri instance-count)
  "Returns an iri for a tooth extraction procedure identified is generated by the patient id, the name of the type of tooth, the type of procedure, and a count variable that used differientiate procedure intances that have the same patient-id/restoration-type-iri but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-surgical-extractions-iri-base*
				     :class-type procedure-type-iri
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-surgical-extractions-query (&key patient-id tooth limit-rows)
  "Returns query string for retrieving data. The patient-id key restricts records only that patient or patients.  Multiple are patients are specified using commas; e.g: \"123, 456, 789\". The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. However, the tooth key only takes a single value. The limit-rows key restricts the number of records to the number specified."

#|
Returns surgical extraction records.
Note: This has not been filtered for primary (baby) teeth.
1215 records returned
|#

  (let ((sql nil))
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
                   length(tooth_data) > 31
                 AND (
                      ada_code IN ('D7110',
                                   'D7120',
                                   'D7140',
                                   'D7210')
                 OR -- older codes begin with a '0'
                    ada_code IN ('07110',
                                 '07120',
                                 '07140',
                                 '07210')) "))

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

