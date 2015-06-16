;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-crowns-ont is ran, the program verifies that the action_codes and
;; patient_history tables exist.  This is done by calling prepare-eaglesoft-db.  However, this
;; only tests that these tables exist in the user's database. If these table need to be 
;; recreated, the call get-eaglesoft-fillings-ont with :force-create-table key set to t.

(defun get-eaglesoft-crowns-ont (&key patient-id tooth limit-rows force-create-table)
  "Returns an ontology of the crowns contained in the Eaglesoft database.  The patient-id key creates an ontology based on that specific patient. The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."

  (let ((results nil)
	(query nil)
	(occurrence-date nil)
	(count 0))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)

    ;; get query string for restorations
    (setf query (get-eaglesoft-crowns-query 
		 :patient-id patient-id :tooth tooth :limit-rows limit-rows))

    (with-ontology ont (:collecting t ;:only-return-axioms t ; used for debugging
			:base *eaglesoft-individual-crowns-iri-base* 
			:ontology-iri *eaglesoft-crowns-ontology-iri*)
	(;; import needed ontologies
	 (as (get-ohd-import-axioms))
	 
	 ;; get axioms for declaring annotation, object, and data properties used for ohd
	 (as (get-ohd-declaration-axioms))

	  (with-eaglesoft (results query)
	    (loop while (#"next" results) do
	         ;; determine this occurrence date
		 (setf occurrence-date
		       (get-eaglesoft-occurrence-date 
			(#"getString" results "table_name")
			(#"getString" results "date_entered")
			(#"getString" results "date_completed")
			(#"getString" results "tran_date")))

	         ;; get records from eaglesoft db and create axioms
		 (as (get-eaglesoft-crown-axioms 
		      (#"getString" results "patient_id")
		      occurrence-date
		      (#"getString" results "tooth_data")
		      (#"getString" results "ada_code")
		      (#"getString" results "r21_provider_id")
		      (#"getString" results "r21_provider_type")
		      (#"getString" results "row_id")))
		 (incf count))))


      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-crown-axioms 
    (patient-id occurrence-date tooth-data ada-code provider-id provider-type record-count)
  (let ((axioms nil)
	(temp-axioms nil) ; used for appending new axioms into the axioms list
	(cdt-class-uri nil)
	(cdt-uri nil)
	(patient-uri nil)
	(crown-role-uri nil)
	(crown-restoration-uri nil)
	(crown-restoration-name nil)
	(restoration-type-uri nil)
	(tooth-name nil)
	(tooth-uri nil)
	(tooth-type-uri nil)
	(teeth-list nil)
	(visit-uri nil))

    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-eaglesoft-teeth-list tooth-data))

    ;; get uri of patient
    (setf patient-uri (get-eaglesoft-dental-patient-iri patient-id))
    
    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 	 
         ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-with-number t))
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-uri (get-eaglesoft-tooth-iri patient-id tooth-type-uri))
	 (push-instance axioms tooth-uri tooth-type-uri)
	 
         ;; add annotation about tooth
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ tooth-name " of patient " patient-id)) axioms)
	 
         ;; declare instance of !ohd:'tooth to be filled role'
	 (setf crown-role-uri
	       (get-eaglesoft-tooth-to-be-crowned-role-iri patient-id tooth record-count))
	 (push-instance axioms crown-role-uri !'tooth to be crowned role'@ohd)
	 
	 ;; add annotation about 'tooth to be crowned role'
	 (push `(annotation-assertion !rdfs:label 
				      ,crown-role-uri
				      ,(str+ "tooth to be crowned role for " 
					     tooth-name " of patient " patient-id)) axioms)
	 
         ;; declare instance of restoration procedure
	 (setf restoration-type-uri (get-crown-restoraton-type-uri ada-code))
	 (setf crown-restoration-uri
	       (get-eaglesoft-crown-restoration-iri
		patient-id tooth-name restoration-type-uri record-count))
	 (push-instance axioms crown-restoration-uri restoration-type-uri)
	 
	 ;; add annotation about this restoration procedure
	 (push `(annotation-assertion !rdfs:label 
				      ,crown-restoration-uri
				      ,(str+ (get-crown-restoraton-type-name ada-code)
					     " on " tooth-name " of patient " patient-id)) axioms)

	 ;; add data property !ohd:'occurrence date' to restoration
	 (push `(data-property-assertion !'occurrence date'@ohd
					 ,crown-restoration-uri 
					 (:literal ,occurrence-date !xsd:date)) axioms)

         ;; get axioms that describe how the restoration realizes the patient and provider roles
	 (setf temp-axioms (get-eaglesoft-patient-provider-realization-axioms
			      crown-restoration-uri patient-id provider-id provider-type record-count))
	 (setf axioms (append temp-axioms axioms))

         ;; declare instance of cdt code as identified by the ada code that is about the procedure
	 (setf cdt-class-uri (get-cdt-class-iri ada-code))
	 (setf cdt-uri (get-eaglesoft-cdt-instance-iri patient-id ada-code cdt-class-uri record-count))
	 (push-instance axioms cdt-uri cdt-class-uri)
	 
	 ;; add annotion about cdt code
	 (push `(annotation-assertion !rdfs:label
				      ,cdt-uri
				      ,(str+ "billing code " ada-code " for crown restoration "
					     "on " tooth-name " of patient " patient-id)) axioms)

	  ;;;; relate instances ;;;;
	 
	 ;; tooth is part of patient
	 (push `(object-property-assertion !'is part of'@ohd
					   ,tooth-uri ,patient-uri) axioms)

         ;; 'tooth to be crowned role' inheres in tooth
	 (push `(object-property-assertion !'inheres in'@ohd
					   ,crown-role-uri ,tooth-uri) axioms)

         ;; 'crown restoration' realizes 'tooth to be crowned role'
	 (push `(object-property-assertion !'realizes'@ohd
					   ,crown-restoration-uri ,crown-role-uri) axioms)

         ;; 'crown restoration' has particpant tooth
	 (push `(object-property-assertion !'has participant'@ohd
					   ,crown-restoration-uri ,tooth-uri) axioms)
	 
         ;; cdt code instance is about the 'crown restoration' process
	 (push `(object-property-assertion !'is about'@ohd
					   ,cdt-uri ,crown-restoration-uri) axioms)

         ;; determine the visit that procedure is part of
	 (setf visit-uri (get-eaglesoft-dental-visit-iri patient-id occurrence-date))
	 (push `(object-property-assertion !'is part of'@ohd ,crown-restoration-uri ,visit-uri) axioms)

         ;; add axioms about material used in the crown restoration
	 (setf temp-axioms (crown-material-axioms patient-id tooth-name tooth-uri restoration-type-uri crown-restoration-uri record-count))
	 (setf axioms (append axioms temp-axioms))
	 
	 ) ;; end loop
    
    ;; return axioms
    axioms))

(defun crown-material-axioms (patient-id tooth-name tooth-uri restoration-type-uri restoration-uri instance-count)
  "Return list of axioms that specify the material used in the crown restoration and that the material restors the tooth."
  (let (axioms type-list name-list)
    ;; the material used in the restoration is determined by the type of restoration
    ;; some crown restoarions use more than one material, so use list
    (cond
      ((equalp restoration-type-uri !'resin crown restoration procedure'@ohd)
       (setf type-list `(!'resin dental restoration material'@ohd))
       (setf name-list '("resin dental restoration material "))
       )
      ((equalp restoration-type-uri !'resin with noble metal crown restoration procedure'@ohd)
       (setf type-list `(!'resin dental restoration material'@ohd !'noble metal dental restoration material'@ohd))
       (setf name-list '( "resin dental restoration material " "noble metal dental restoration material "))
       )
      ((equalp restoration-type-uri !'resin with high noble metal crown restoration procedure'@ohd)
       (setf type-list `(!'resin dental restoration material'@ohd !'high noble metal dental restoration material'@ohd))
       (setf name-list '("resin dental restoration material " "high noble metal dental restoration material "))
       )
      ((equalp restoration-type-uri !'resin with predominantly base metal crown restoration procedure'@ohd)
       (setf type-list `(!'resin dental restoration material'@ohd !'predominantly base metal dental restoration material'@ohd))
       (setf name-list '("resin dental restoration material " "predominantly base metal dental restoration material "))
       )
      ((equalp restoration-type-uri !'ceramic crown restoration procedure'@ohd)
       (setf type-list `(!'ceramic dental restoration material'@ohd))
       (setf name-list '("ceramic dental restoration material "))
       )
      ((equalp restoration-type-uri !'porcelain fused to noble metal crown restoration procedure'@ohd)
       (setf type-list `(!'porcelain dental restoration material'@ohd !'noble metal dental restoration material'@ohd))
       (setf name-list '("porcelain dental restoration material " "noble metal dental restoration material "))
       )
      ((equalp restoration-type-uri !'porcelain fused to high noble metal crown restoration procedure'@ohd)
       (setf type-list `(!'porcelain dental restoration material'@ohd !'high noble metal dental restoration material'@ohd))
       (setf name-list '("porcelain dental restoration material " "high noble metal dental restoration material "))
       )
      ((equalp restoration-type-uri !'porcelain fused to predominantly base metal crown restoration procedure'@ohd)
       (setf type-list `(!'porcelain dental restoration material'@ohd !'predominantly base metal dental restoration material'@ohd))
       (setf name-list '("porcelain dental restoration material " "predominantly base metal dental restoration material "))
       )
      ((equalp restoration-type-uri !'noble metal crown restoration procedure'@ohd)
       (setf type-list `(!'noble metal dental restoration material'@ohd))
       (setf name-list '("noble metal dental restoration material "))
       )
      ((equalp restoration-type-uri !'high noble metal crown restoration procedure'@ohd)
       (setf type-list `(!'high noble metal dental restoration material'@ohd))
       (setf name-list '("high noble metal dental restoration material "))
       )
      ((equalp restoration-type-uri !'predominantly base metal crown restoration procedure'@ohd)
       (setf type-list `(!'predominantly base metal dental restoration material'@ohd))
       (setf name-list '("predominantly base metal dental restoration material "))
       )
      ((equalp restoration-type-uri !'stainless steel crown restoration procedure'@ohd)
       (setf type-list `(!'stainless steel dental restoration material'@ohd))
       (setf name-list '("stainless steel dental restoration material "))
       )
      ((equalp restoration-type-uri !'stainless steel with resin window crown restoration procedure'@ohd)
       (setf type-list `(!'stainless steel dental restoration material'@ohd !'resin dental restoration material'@ohd))
       (setf name-list '("stainless steel dental restoration material " "resin dental restoration material "))
       )
      ((equalp restoration-type-uri !'titanium crown restoration procedure'@ohd)
       (setf type-list `(!'titanium dental restoration material'@ohd))
       (setf name-list '("titanium dental restoration material "))
       )
      ((equalp restoration-type-uri !'3/4 ceramic crown restoration procedure'@ohd)
       (setf type-list `(!'ceramic dental restoration material'@ohd))
       (setf name-list '("ceramic dental restoration material "))
       )
      ((equalp restoration-type-uri !'3/4 high noble metal crown restoration procedure'@ohd)
       (setf type-list `(!'high noble metal dental restoration material'@ohd))
       (setf name-list '("high noble metal dental restoration material "))
       )
      ((equalp restoration-type-uri !'3/4 noble metal crown restoration procedure'@ohd)
       (setf type-list `(!'noble metal dental restoration material'@ohd))
       (setf name-list '("noble metal dental restoration material "))
       ) 
      ((equalp restoration-type-uri !'3/4 predominantly base metal crown restoration procedure'@ohd)
       (setf type-list `(!'predominantly base metal dental restoration material'@ohd))
       (setf name-list '("predominantly based metal dental restoration material "))
       )
      ((equalp restoration-type-uri !'3/4 resin crown restoration procedure'@ohd)
       (setf type-list `(!'resin dental restoration material'@ohd))
       (setf name-list '("resin dental restoration material "))
       )
      (t
       (setf type-list `(!'dental restoration material'@ohd))
       (setf name-list `("dental restoration material "))))

    ;; loop over lists and create axioms
    (loop
       for type-uri in type-list
       for type-name in name-list
       for uri = (get-eaglesoft-crown-material-uri
		    patient-id tooth-name type-uri instance-count)
       for name = (str+ type-name "placed on tooth " tooth-name " of patient " patient-id)
       do
         ;; declare instance of material
	 (push-instance axioms uri type-uri)

         ;; add annotation label for instance
	 (push `(annotation-assertion !rdfs:label ,uri ,name) axioms)

	 ;; crown restoration has the material as a participant
	 (push `(object-property-assertion !'has participant'@ohd
					   ,restoration-uri ,uri) axioms)

         ;; material restores tooth
	 (push `(object-property-assertion
		 !'is dental restoration of'@ohd ,uri ,tooth-uri) axioms))
    
    ;; return axioms
    axioms))

  
(defun get-crown-restoraton-type-uri (ada-code)
  "Returns the uri of the type of crown."
  (let ((uri nil))
    ;; get the numeric part of code
    (setf ada-code (str-right ada-code 4))
    
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *resin-code-list* :test 'equalp)
       (setf uri !'resin crown restoration procedure'@ohd))
      ((member ada-code *resin-with-noble-metal-code-list* :test 'equalp)
       (setf uri !'resin with noble metal crown restoration procedure'@ohd))
      ((member ada-code *resin-with-high-noble-metal-code-list* :test 'equalp)
       (setf uri !'resin with high noble metal crown restoration procedure'@ohd))
      ((member ada-code *resin-with-predominantly-base-metal-code-list* :test 'equalp)
       (setf uri !'resin with predominantly base metal crown restoration procedure'@ohd))
      ((member ada-code *ceramic-code-list* :test 'equalp)
       (setf uri !'ceramic crown restoration procedure'@ohd))
      ((member ada-code *porcelain-fused-to-noble-metal-code-list* :test 'equalp)
       (setf uri !'porcelain fused to noble metal crown restoration procedure'@ohd))
      ((member ada-code *porcelain-fused-to-high-noble-metal-code-list* :test 'equalp)
       (setf uri !'porcelain fused to high noble metal crown restoration procedure'@ohd))
      ((member ada-code *porcelain-fused-to-predominantly-base-metal-code-list* :test 'equalp)
       (setf uri !'porcelain fused to predominantly base metal crown restoration procedure'@ohd))
      ((member ada-code *noble-metal-code-list* :test 'equalp)
       (setf uri !'noble metal crown restoration procedure'@ohd))
      ((member ada-code *high-noble-metal-code-list* :test 'equalp)
       (setf uri !'high noble metal crown restoration procedure'@ohd))
      ((member ada-code *predominantly-base-metal-code-list* :test 'equalp)
       (setf uri !'predominantly base metal crown restoration procedure'@ohd))
      ((member ada-code *stainless-steel-code-list* :test 'equalp)
       (setf uri !'stainless steel crown restoration procedure'@ohd))
      ((member ada-code *stainless-steel-with-resin-window-code-list* :test 'equalp)
       (setf uri !'stainless steel with resin window crown restoration procedure'@ohd))
      ((member ada-code *titanium-code-list* :test 'equalp)
       (setf uri !'titanium crown restoration procedure'@ohd))
      ((member ada-code *three-fourths-ceramic-code-list* :test #'equalp)
       (setf uri !'3/4 ceramic crown restoration procedure'@ohd))
      ((member ada-code *three-fourths-high-noble-metal-code-list* :test #'equalp)
       (setf uri !'3/4 high noble metal crown restoration procedure'@ohd))
      ((member ada-code *three-fourths-noble-metal-code-list* :test #'equalp)
       (setf uri !'3/4 noble metal crown restoration procedure'@ohd)) 
      ((member ada-code *three-fourths-predominantly-base-metal-code-list* :test #'equalp)
       (setf uri !'3/4 predominantly base metal crown restoration procedure'@ohd))
      ((member ada-code *three-fourths-resin-code-list* :test #'equalp)
       (setf uri !'3/4 resin crown restoration procedure'@ohd))
      (t
       (setf uri !'crown restoration procedure'@ohd)))

    
    ;; return uri
    uri))

(defun get-crown-restoraton-type-name (ada-code)
  "Returns the name of the type of crown restoration."
  (let ((name nil))
    ;; get the numeric part of code
    (setf ada-code (str-right ada-code 4))
    
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *resin-code-list* :test 'equalp)
       (setf name "resin crown restoration procedure"))
      ((member ada-code *resin-with-noble-metal-code-list* :test 'equalp)
       (setf name "resin with noble metal crown restoration procedure"))
      ((member ada-code *resin-with-high-noble-metal-code-list* :test 'equalp)
       (setf name "resin with high noble metal crown restoration procedure"))
      ((member ada-code *resin-with-predominantly-base-metal-code-list* :test 'equalp)
       (setf name "resin with predominantly base metal crown restoration procedure"))
      ((member ada-code *ceramic-code-list* :test 'equalp)
       (setf name "ceramic crown restoration procedure"))
      ((member ada-code *porcelain-fused-to-noble-metal-code-list* :test 'equalp)
       (setf name "porcelain fused to noble metal crown restoration procedure"))
      ((member ada-code *porcelain-fused-to-high-noble-metal-code-list* :test 'equalp)
       (setf name "porcelain fused to high noble metal crown restoration procedure"))
      ((member ada-code *porcelain-fused-to-predominantly-base-metal-code-list* :test 'equalp)
       (setf name "porcelain fused to predominantly base metal crown restoration procedure"))
      ((member ada-code *noble-metal-code-list* :test 'equalp)
       (setf name "noble metal crown restoration procedure"))
      ((member ada-code *high-noble-metal-code-list* :test 'equalp)
       (setf name "high noble metal crown restoration procedure"))
      ((member ada-code *predominantly-base-metal-code-list* :test 'equalp)
       (setf name "predominantly base metal crown restoration procedure"))
      ((member ada-code *stainless-steel-code-list* :test 'equalp)
       (setf name "stainless steel crown restoration procedure"))
      ((member ada-code *stainless-steel-with-resin-window-code-list* :test 'equalp)
       (setf name "stainless steel with resin window crown restoration procedure"))
      ((member ada-code *titanium-code-list* :test 'equalp)
       (setf name "titanium crown restoration procedure"))
      ((member ada-code *three-fourths-ceramic-code-list* :test #'equalp)
       (setf name "3/4 ceramic crown restoration procedure"))
      ((member ada-code *three-fourths-high-noble-metal-code-list* :test #'equalp)
       (setf name "3/4 high noble metal crown restoration procedure"))
      ((member ada-code *three-fourths-noble-metal-code-list* :test #'equalp)
       (setf name "3/4 noble metal crown restoration procedure")) 
      ((member ada-code *three-fourths-predominantly-base-metal-code-list* :test #'equalp)
       (setf name "3/4 predominantly base metal crown restoration procedure"))
      ((member ada-code *three-fourths-resin-code-list* :test #'equalp)
       (setf name "3/4 resin crown restoration procedure"))
      (t
       (setf name "crown restoration procedure")))

    
    ;; return name
    name))

(defun get-eaglesoft-tooth-to-be-crowned-role-iri (patient-id tooth-name instance-count)
  "Returns an iri for a 'tooth to be crowned role' that is generated by the patient id, the name of the type of the tooth, and a count variable that used differientiate tooth role intances that have the same patient-id/tooth-name but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'tooth to be crowned role'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))


(defun get-eaglesoft-crown-material-uri (patient-id tooth-name material-type-uri instance-count)
  "Returns an uri for the material used in crown restoration procedure that is generated by the patient id, the name of the type of tooth, the type of restoration's material, and a count variable that used differientiate restoration procedure intances that have the same patient-id/material-type-iri but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-fillings-iri-base*
				     :class-type material-type-uri
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-crown-restoration-iri (patient-id tooth-name
					    restoration-type-iri instance-count)
  "Returns an iri for a crown restoration procedure that is generated by the patient id, the name of the type of tooth, the type of crown restoration, and a count variable that used differientiate crown procedure intances that have the same patient-id/restoration-type-iri but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-crowns-iri-base*
				     :class-type restoration-type-iri
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))


(defun get-eaglesoft-cdt-instance-iri (patient-id ada-code cdt-class instance-count)
  "Returns an iri for a CDT code instance that is generated by the patient id, the the ADA code associated with the CDT code, the type/class of CDT code, and a count variable that used differientiate CDT code intances that have the same ADA code but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id
				     :salt *eaglesoft-salt*
				     :iri-base *cdt-iri-base*
				     :class-type cdt-class
				     :args `(,ada-code ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-crowns-query (&key patient-id tooth limit-rows)
  "Retruns query string for retrieving data. The patient-id key restricts records only that patient or patients.  Multiple are patients are specified using commas; e.g: \"123, 456, 789\". The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. However, the tooth key only takes a single value. The limit-rows key restricts the number of records to the number specified."
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
                   RIGHT(ada_code, 4) IN
                             ('2390', /* This was originally in the fillings query */
                              '2710',
                              '2712',
                              '2721',
                              '2722',
                              '2740',
                              '2750',
                              '2751',
                              '2752',
                              '2780',
                              '2781',
                              '2782',
                              '2783',
                              '2790',
                              '2791',
                              '2792',
                              '2794',
                              '2799',
                              '2931',
                              '2932',
                              '2933',
                              '2970',
 
                               /* Fixed Partial Denture Retainers - Crowns */
                              '6710',
                              '6720',
                              '6721',
                              '6722',
                              '6740',
                              '6750',
                              '6751',
                              '6752',
                              '6780',
                              '6781',
                              '6782',
                              '6783',
                              '6790',
                              '6791',
                              '6792',
                              '6793',
                              '6794',
                              '6795')

                   AND LENGTH(tooth_data) > 31 
                   /* older codes (previous to cdt4) being with a 0 
                      codes cdt4 (2003) and later begin with a D */
                   AND LEFT(ada_code, 1) IN ('D','0') "))

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

    ;;(setf sql (str+ sql " AND row_id < 136483 ")) ; used for debugging

    ;; ORDER BY clause
    (setf sql
    	  (str+ sql " ORDER BY patient_id desc "))
    
    ;; return query string
    ;;(pprint sql)
    sql))
