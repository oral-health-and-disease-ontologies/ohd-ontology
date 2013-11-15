;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-caries-findings-ont is ran, the program verifies that the 
;; action_codes and patient_history tables exist.  This is done by calling 
;; prepare-eaglesoft-db.  However, this only tests that these tables exist in the 
;; user's database. If these table need to be recreated, the call 
;; get-eaglesoft-caries-findings-ont with :force-create-table key set to t.

(defun get-eaglesoft-caries-findings-ont (&key patient-id tooth limit-rows force-create-table)
  "Returns an ontology of the caries findings contained in the Eaglesoft database.  This includes both impacted and embedded teeth. The patient-id key creates an ontology based on that specific patient. The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."
  (let ((results nil)
	(query nil)
	(occurrence-date nil)
	(count 0))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)

    ;; get query string for findings
    (setf query (get-eaglesoft-caries-findings-query 
		 :patient-id patient-id :tooth tooth :limit-rows limit-rows))

    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-caries-findings-iri-base*
			:ontology-iri *eaglesoft-caries-findings-ontology-iri*)
	(;; import needed ontologies
	 (as (get-ohd-import-axioms))

	 ;; get axioms for declaring annotation, object, and data properties used for ohd
	 (as (get-ohd-declaration-axioms))
	 
	 ;; get records from eaglesoft db and create axioms
	 (with-eaglesoft (results query)
	   (loop while (#"next" results) do
	        ;; determine the occurrence date
		(setf occurrence-date
		      (get-eaglesoft-occurrence-date 
		       (#"getString" results "table_name")
		       (#"getString" results "date_entered")
		       (#"getString" results "date_completed")
		       (#"getString" results "tran_date")))

	        ;; generate axioms
		(as (get-eaglesoft-caries-finding-axioms 
		     (#"getString" results "patient_id")
		     occurrence-date
		     (#"getString" results "tooth_data")
		     (#"getString" results "surface")
		     (#"getString" results "r21_provider_id")
		     (#"getString" results "r21_provider_type")
		     (#"getString" results "practice_id")
		     (#"getString" results "action_code")
		     (#"getString" results "row_id")))
		(incf count))))

      ;; return the ontology
      (values ont count))))


(defun get-eaglesoft-caries-finding-axioms 
    (patient-id occurrence-date tooth-data surface provider-id provider-type practice-id action-code record-count)
  (let ((axioms nil)
	(temp-axioms nil) ; used for appending new axioms into the axioms list
	(patient-uri nil)
	(finding-uri nil)
	(finding-type-uri nil)
	(lesion-uri nil)
	(surface-uri nil)
	(surface-type-uri nil)
	(surface-list nil)
	(tooth-uri nil)
	(tooth-type-uri nil)
	(tooth-name nil)
	(teeth-list nil))


    ;; get uri of patient
    (setf patient-uri (get-eaglesoft-dental-patient-iri patient-id))

    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-eaglesoft-teeth-list tooth-data))

    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 
	 ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-with-number t))
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-uri (get-eaglesoft-tooth-iri patient-id tooth-type-uri))
	 
	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 (setf temp-axioms (get-ohd-instance-axioms tooth-uri tooth-type-uri))
	 (setf axioms (append temp-axioms axioms))

         ;; get iri associated with the type 'caries finding'
	 ;;(setf finding-type-uri !'caries finding'@ohd)
	 (setf finding-type-uri (get-eaglesoft-finding-type-iri action-code))

	 
	 ;; add annotation about tooth
	 (push `(annotation-assertion !rdfs:label ,tooth-uri
				      ,(str+ tooth-name
					     " of patient " patient-id)) axioms)

         ;; declare instance of !ohd:'caries finding'
	 ;;(setf finding-uri
	   ;;    (get-eaglesoft-caries-finding-iri patient-id tooth-name record-count))

	 (setf finding-uri
	       (get-eaglesoft-individual-finding-iri 
		patient-id action-code tooth-name record-count))
	 (setf temp-axioms (get-ohd-instance-axioms finding-uri finding-type-uri))
	 (setf axioms (append temp-axioms axioms))

         ;; add annotation about caries finding
	 (push `(annotation-assertion !rdfs:label 
				      ,finding-uri
				      ,(get-eaglesoft-finding-rdfs-label 
					patient-id action-code tooth-name)) axioms)
	 
         ;; the tooth 'is part of' the patient
	 (push `(object-property-assertion !'is part of'@ohd ,tooth-uri ,patient-uri) axioms)
         
	 ;; declare instance of the carious lesion
	 (setf lesion-uri (get-eaglesoft-carious-lesion-iri patient-id tooth-name record-count))
	 (setf temp-axioms (get-ohd-instance-axioms lesion-uri !'carious lesion of tooth'@ohd))
	 (setf axioms (append temp-axioms axioms))
	 
	 ;; add annotation about lesion
	 (push `(annotation-assertion !rdfs:label ,lesion-uri
				      ,(str+ "carious lesion on " 
					     tooth-name " of patient " patient-id)) axioms)


         ;; instance of caries finding 'is about' the 'carious lesion
	 (push `(object-property-assertion !'is about'@ohd ,finding-uri ,lesion-uri) axioms)

         ;; the carious lesion is part of the tooth
	 (push `(object-property-assertion !'is part of'@ohd ,lesion-uri ,tooth-uri) axioms)

         ;; add data property !ohd:'occurrence date' of the caries finding
	 (push `(data-property-assertion !'occurrence date'@ohd
					 ,finding-uri
					 (:literal ,occurrence-date !xsd:date)) axioms)



         ;; add information about surfaces
         ;; the surface field often contains multiple surfaces; e.g., mobl
         ;; so create a list of these surfaces and add axioms about each one
	 (setf surface-list (get-eaglesoft-surface-list surface))
	 (loop 
	    for surface-name in surface-list do
	      ;; get the type of surface uri for the surface
	      (setf surface-type-uri (get-fma-surface-uri surface-name))
	      
	      ;; create and instance of this surface
	      (setf surface-uri (get-eaglesoft-surface-iri patient-id surface-type-uri tooth-name))
	      (push `(declaration (named-individual ,surface-uri)) axioms)
	      (setf temp-axioms (get-ohd-instance-axioms surface-uri surface-type-uri))
	      (setf axioms (append temp-axioms axioms))

	      
	      ;; relate surface to tooth
	      (push `(object-property-assertion !'is part of'@ohd ,surface-uri ,tooth-uri) axioms)
	      
	      ;; the carious lesion is part of the surface
	      (push `(object-property-assertion !'is part of'@ohd ,lesion-uri ,surface-uri) axioms)
	      
	      ;; add annoation about surface
	      (push `(annotation-assertion !rdfs:label
					   ,surface-uri
					   ,(str+ surface-name " surface of " tooth-name
						  " in patient " patient-id)) axioms)
	      
	      ) ;; end surface loop


         ;; insert axioms about the dental exam in which the provider (if known)
         ;; found the caries
	 (when provider-id
	   (setf temp-axioms (get-eaglesoft-dental-exam-axioms 
			      finding-uri provider-id provider-type practice-id record-count))

	   ;; ensure that axioms were returned
	   (when temp-axioms (setf axioms (append temp-axioms axioms))))
	 
	 ) ;; end loop
        ;;(pprint axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-caries-finding-iri (patient-id tooth-name instance-count)
  "Returns an iri for a 'caries finding' that is generated by the patient id, the name of the type of the tooth, and a count variable that differentiates multipe caries findings that are about the same tooth."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'caries finding'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-carious-lesion-iri (patient-id tooth-name instance-count)
  "Returns an iri for a carious lesion that is generated by the patient id, the name of the type of the tooth, and a count variable that differentiates multipe caries findings that are about the same tooth."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'carious lesion of tooth'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))


(defun get-eaglesoft-caries-findings-query (&key patient-id tooth limit-rows)
  "Returns query string for retrieving data. The patient-id key restricts records only that patient or patients.  Multiple are patients are specified using commas; e.g: \"123, 456, 789\".  The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. However, the tooth key only takes a single value. The limit-rows key restricts the number of records to the number specified."

#|
Returns records that indicate a tooth has caries
These are records that have an action code '18', but we are only looking for
description that contain:
'Decay'
'Decalcification'
'Dicalsification'
'Deep dentinal/cemental caries'

Only records that contain surface data are returned

2712 records returned.
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
                 action_code,
                 row_id,
                 get_surface_summary_from_detail(surface_detail, tooth) as surface "))

    ;; FROM clause
    (setf sql (str+ sql " FROM patient_history "))

    ;; WHERE clause
    (setf sql
	  (str+ sql
		"WHERE
                   action_code IN ('2', '3', '4')
                AND LENGTH(tooth_data) > 31
                AND description IN ('Decay',
                                    'Decalcification',
                                    'Dicalsification',
                                    'Deep dentinal/cemental caries') 
                AND tooth_data IS NOT NULL
                AND LENGTH(tooth_data) > 31
                AND tooth_data LIKE '%Y%' 
                AND surface_detail IS NOT NULL 
                AND surface_detail LIKE '%Y%' "))
    
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

    sql))