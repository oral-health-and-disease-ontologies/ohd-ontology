;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-inlays-ont is ran, the program verifies that the action_codes and
;; patient_history tables exist.  This is done by calling prepare-eaglesoft-db.  However, this
;; only tests that these tables exist in the user's database. If these table need to be 
;; recreated, the call get-eaglesoft-inlays-ont with :force-create-table key set to t.

(defun get-eaglesoft-inlays-ont (&key patient-id tooth limit-rows force-create-table)
  "Returns an ontology of the inlays contained in the Eaglesoft database. The patient-id key creates an ontology based on that specific patient. The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."

  (let ((results nil)
	(query nil)
	(occurrence-date nil)
	(count 0))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)

    ;; get query string for restorations
    (setf query (get-eaglesoft-inlays-query 
		 :patient-id patient-id :tooth tooth :limit-rows limit-rows))

    (with-ontology ont (:collecting t
			:base *eaglesoft-individual-inlays-iri-base*
			:ontology-iri  *eaglesoft-inlays-ontology-iri*)
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
		     (as (get-eaglesoft-inlays-axioms 
		     	  (#"getString" results "patient_id")
		     	  occurrence-date
		     	  (#"getString" results "tooth_data")
		     	  (#"getString" results "billed_surface")
			  (#"getString" results "charted_surface")
		     	  (#"getString" results "ada_code")
			  (#"getString" results "r21_provider_id")
			  (#"getString" results "r21_provider_type")
		     	  (#"getString" results "row_id")))
		     (incf count))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-inlays-axioms (patient-id occurrence-date tooth-data 
				     billed-surface charted-surface ada-code 
				     provider-id provider-type record-count)
  (let ((axioms nil)
	(temp-axioms nil) ; used for appending new axioms into the axioms list
	(cdt-class-uri nil)
	(cdt-uri nil)
	(material-name nil)
	(material-uri nil)
	(ohd-material-uri nil)
	(ohd-restoration-uri nil)
	(restoration-name nil)
	(restoration-uri nil)
	(patient-uri nil)
	(surface-uri nil)
	(surface-type-uri nil)
	(billed-surface-list nil)
	(charted-surface-list nil)
	(tooth-uri nil)
	(tooth-type-uri nil)
	(tooth-role-uri nil)
	(tooth-name nil)
	(teeth-list nil)
	(visit-uri nil))


    ;; alanr - parse the list since that's what's in our table 
    ;; billd - since we are using the tooth_data array, this procedure is skipped
    ;;(setf teeth-list (parse-teeth-list tooth-data)) ; commented out by billd


    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-eaglesoft-teeth-list tooth-data))
    

    ;; get uri of patient
    (setf patient-uri (get-eaglesoft-dental-patient-iri patient-id))

    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 
       
	 
         ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-with-number t))
	 (setf tooth-uri (get-eaglesoft-tooth-iri patient-id tooth-type-uri))
	 	 	 
	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 
         ;; note: append puts lists together and doesn't put items in list (like push)
	 (setf temp-axioms (get-ohd-instance-axioms tooth-uri tooth-type-uri))
	 (setf axioms (append temp-axioms axioms))
	 
         ;; add annotation about tooth
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ tooth-name
					     " of patient " patient-id)) axioms)
	 
         ;; declare instance of !ohd:'tooth to undergo inlay procedure role'
	 (setf tooth-role-uri (get-eaglesoft-tooth-to-undergo-inlay-procedure-role-iri
			       patient-id tooth-name record-count))
		
	 (push `(declaration (named-individual ,tooth-role-uri)) axioms)
	 (setf temp-axioms (get-ohd-instance-axioms tooth-role-uri 
						    !'tooth to undergo inlay procedure role'@ohd))
	 (setf axioms (append temp-axioms axioms))

	 ;; add annotation about tooth role
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-role-uri
				      ,(str+ "tooth to undergo inlay procedure role for " 
					     tooth-name " of patient " 
					     patient-id)) axioms)

         ;; declare instance of material used in tooth
	 (setf ohd-material-uri (get-ohd-material-uri ada-code))
	 (setf material-uri (get-eaglesoft-inlay-material-iri 
			     patient-id tooth-name ohd-material-uri record-count))

	 (push `(declaration (named-individual ,material-uri)) axioms)
	 (setf temp-axioms (get-ohd-instance-axioms material-uri ohd-material-uri))
	 (setf axioms (append temp-axioms axioms))

	 ;; add annotation about this instance of material
	 (setf material-name (get-ohd-material-name ada-code))
	 (push `(annotation-assertion !rdfs:label 
				      ,material-uri
				      ,(str+ material-name " placed in " tooth-name
					     " of patient " patient-id)) axioms)

         ;; declare instance of restoration 
	 (setf ohd-restoration-uri (get-eaglesoft-inlay-restoration-type-uri ada-code))
	 (setf restoration-uri (get-eaglesoft-inlay-restoration-iri 
				patient-id tooth-name ohd-restoration-uri record-count))
		     		
	 (push `(declaration (named-individual ,restoration-uri)) axioms)
	 (setf temp-axioms (get-ohd-instance-axioms restoration-uri ohd-restoration-uri))
	 (setf axioms (append temp-axioms axioms))

	 ;; add annotation about this restoration procedure
	 (setf restoration-name (get-eaglesoft-inlay-restoration-name ada-code))
	 (push `(annotation-assertion !rdfs:label 
				      ,restoration-uri
				      ,(str+ restoration-name 
					     " restoration on " 
					     tooth-name " in patient " 
					     patient-id)) axioms)

	 ;; add data property !ohd:'occurrence date' to restoration
	 (push `(data-property-assertion !'occurrence date'@ohd
					 ,restoration-uri 
					 (:literal ,occurrence-date !xsd:date)) axioms)

         ;; add information about surfaces charted in the databse
	 ;; note: this may not be the same as the surfaces that are billed for 
	 ;; the surface field often contains multiple surfaces; e.g., mobl
	 ;; so create a list of these surfaces and add axioms about each one
	 (setf charted-surface-list (get-eaglesoft-surface-list charted-surface))
	 (loop 
	    for surface-name in charted-surface-list do
	      ;; get the type of surface uri for the surface
	      (setf surface-type-uri (get-fma-surface-uri surface-name))
	      
	      ;; create and instance of this surface
	      (setf surface-uri (get-eaglesoft-surface-iri patient-id surface-type-uri tooth-name))
	      (push `(declaration (named-individual ,surface-uri)) axioms)
	      (setf temp-axioms (get-ohd-instance-axioms surface-uri surface-type-uri))
	      (setf axioms (append temp-axioms axioms))

	      
	      ;; relate surface to tooth
	      (push `(object-property-assertion !'is part of'@ohd ,surface-uri ,tooth-uri) axioms)
	      
	      ;; relate  material to surface
	      (push `(object-property-assertion !'is dental restoration of'@ohd
						,material-uri ,surface-uri) axioms)

	      ;; the surface is a participant in the restoration procedure
	      (push `(object-property-assertion !'has participant'@ohd
						,restoration-uri ,surface-uri) axioms)
	      
	      ;; add annoation about surface
	      (push `(annotation-assertion !rdfs:label
					   ,surface-uri
					   ,(str+ surface-name " surface of " tooth-name
						  " in patient " patient-id)) axioms)
	      
	      ) ;; end surface loop

         ;; get axioms that describe how the inlay procedure realizes the patient and provider roles
	 (setf temp-axioms (get-eaglesoft-patient-provider-realization-axioms restoration-uri patient-id provider-id provider-type record-count))
	 (setf axioms (append temp-axioms axioms))

         ;; declare instance of cdt code as identified by the ada code that is about the procedure
	 (setf cdt-class-uri (get-cdt-class-iri ada-code))
	 (setf cdt-uri (get-eaglesoft-cdt-instance-iri patient-id ada-code cdt-class-uri record-count))
	 (push `(declaration (named-individual ,cdt-uri)) axioms)
	 (setf temp-axioms (get-ohd-instance-axioms cdt-uri cdt-class-uri))
	 (setf axioms (append temp-axioms axioms))
	 
	 ;; add annotion about cdt code
	 (push `(annotation-assertion !rdfs:label
				      ,cdt-uri
				      ,(str+ "billing code " ada-code " for " restoration-name 
					     " inlay restoration on " tooth-name 
					     " of patient " patient-id)) axioms)

	  ;;;; relate instances ;;;;

	 ;; tooth is located in the patient
	 (push `(object-property-assertion !'is part of'@ohd
					   ,tooth-uri ,patient-uri) axioms)

         ;; tooth role inheres in tooth
	 (push `(object-property-assertion !'inheres in'@ohd
					   ,tooth-role-uri ,tooth-uri) axioms)

         ;; restoration realizes tooth role
	 (push `(object-property-assertion !'realizes'@ohd
					   ,restoration-uri ,tooth-role-uri) axioms)

         ;; restoration has particpant tooth
	 (push `(object-property-assertion !'has participant'@ohd
					   ,restoration-uri ,tooth-uri) axioms)
	 
         ;; restoration has particpant restoration material
	 (push `(object-property-assertion !'has participant'@ohd
					   ,restoration-uri ,material-uri) axioms)

         ;; restoration material is located in the tooth
	 (push `(object-property-assertion !'is located in'@ohd
					   ,material-uri ,tooth-uri) axioms)

         ;; determine the visit that procedure is part of
	 (setf visit-uri (get-eaglesoft-dental-visit-iri patient-id occurrence-date))
	 (push `(object-property-assertion !'is part of'@ohd ,restoration-uri ,visit-uri) axioms)
	 
         ;; cdt code instance is about the restoration process
	 (push `(object-property-assertion !'is about'@ohd
					   ,cdt-uri ,restoration-uri) axioms)
	 
	 ;; now for each billed surfac add annotation that the cdt code is about that surface
	 ;; this need to be done b/c there are cases in which the surface restoration that is
	 ;; billed for different from the surface restoration recorded in the chart
	 (setf billed-surface-list (get-eaglesoft-surface-list billed-surface))
	 (loop 
	    for surface-name in billed-surface-list do
	      ;; get the type of surface uri for the surface
	      (setf surface-type-uri (get-fma-surface-uri surface-name))
	      
	      ;; create and instance of this surface
	      (setf surface-uri (get-eaglesoft-surface-iri patient-id surface-type-uri tooth-name))

	      ;; relate cdt code to billed surface
	      ;; cdt code instance is about the billed surface
	      (push `(object-property-assertion !'is about'@ohd
						,cdt-uri ,surface-uri) axioms)
	      
	      ;; if the instance of surface is not in the charted surface list above
	      ;; then add the surface to the ontology (and relate it to tooth)
	      ;; as an example of this look at resin restoration of tooth 19 in patient 304
	      (when (not (member surface-name charted-surface-list :test #'equalp))
		
		;;(print-db record-count)
		;;(print-db charted-surface-list)
		;;(print-db billed-surface-list)
		;;(print-db surface-name)
		(push `(declaration (named-individual ,surface-uri)) axioms)
		(setf temp-axioms (get-ohd-instance-axioms surface-uri surface-type-uri))
		(setf axioms (append temp-axioms axioms))
		(push `(object-property-assertion !'is part of'@ohd ,surface-uri ,tooth-uri) axioms)
		
		;; add annoation about surface
		(push `(annotation-assertion !rdfs:label
					     ,surface-uri
					     ,(str+ surface-name " surface of " tooth-name
						    " in patient " patient-id)) axioms))

	      ) ;; end bill surfaces loop

	 ) ;; end loop
    
    ;;(pprint axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-tooth-to-undergo-inlay-procedure-role-iri (patient-id tooth-name instance-count)
  "Returns an iri for a 'tooth to undergo inlay procedure role' that is generated by the patient id, the name of the type of the tooth, and a count variable that used differientiate tooth role intances that have the same patient-id/tooth-name but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'tooth to undergo inlay procedure role'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-inlay-restoration-iri (patient-id tooth-name
					      restoration-type-iri instance-count)
  "Returns an iri for a restoration procedure that is generated by the patient id, the name of the type of tooth, the type of restoration's iri, and a count variable that used differientiate restoration procedure intances that have the same patient-id/restoration-type-iri but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-inlays-iri-base*
				     :class-type restoration-type-iri
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-inlay-material-iri (patient-id tooth-name
					      material-type-iri instance-count)
  "Returns an iri for the material used in restoration procedure that is generated by the patient id, the name of the type of tooth, the type of restoration's material, and a count variable that used differientiate restoration procedure intances that have the same patient-id/material-type-iri but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-inlays-iri-base*
				     :class-type material-type-iri
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))
  
(defun get-eaglesoft-inlay-restoration-name (ada-code)
  "Returns the name the type of restoration based on ada code."
  (let ((restoration-name nil))
    ;; get the numeric part of code
    (setf ada-code (str-right ada-code 4))

    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *metal-code-list* :test 'equalp)
       (setf restoration-name "metallic inlay"))
      ((member ada-code *resin-code-list* :test 'equalp)
       (setf restoration-name "resin inlay"))
      ((member ada-code *ceramic-code-list* :test 'equalp)
       (setf restoration-name "ceramic inlay"))
      (t
       (setf restoration-name "inlay")))

    ;; return material name
    restoration-name))

(defun get-eaglesoft-inlay-restoration-type-uri (ada-code)
  "Returns the uri of the restoration type based on ada code."
  (let ((restoration-uri nil))
    ;; get the numeric part of code
    (setf ada-code (str-right ada-code 4))
    
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *metal-code-list* :test 'equalp) 
       ;;(setf restoration-uri !'metallic inlay restoration procedure'@ohd))
       (setf restoration-uri !'metallic inlay restoration'@ohd)) ;; used for r21
      ((member ada-code *resin-code-list* :test 'equalp)  
       ;;(setf restoration-uri !'resin inlay restoration procedure'@ohd))
       (setf restoration-uri !'resin inlay restoration'@ohd)) ;; used for r21
      ((member ada-code *ceramic-code-list* :test 'equalp)
       ;;(setf restoration-uri !'ceramic inlay restoration procedure'@ohd))
       (setf restoration-uri !'ceramic inlay restoration'@ohd)) ;; used for r21
      (t
       (setf restoration-uri !'inlay restoration procedure'@ohd)))

    ;; return restoration
    restoration-uri))

(defun get-eaglesoft-inlays-query (&key patient-id tooth limit-rows)
  "Returns query string for retrieving data. The patient-id key restricts records only that patient or patients.  Multiple are patients are specified using commas; e.g: \"123, 456, 789\". The tooth key is used to limit results to a specific tooth, and can be used in combination with the patient-id. However, the tooth key only takes a single value. The limit-rows key restricts the number of records to the number specified."

#| 
This queries the eaglesoft database for all ADA codes that indicate an inlay procedure
has been performed.

get_surface_summary_from_detail is used with the surface_detail array in order to ensure that the 
correct surfaces names are used. For example, some records list an occlusal surface for an incisor tooth.
get_surface_summary_from_detail corrects this by recovering the correct name of the surface from
the surface_detail array.
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
                  RIGHT(ada_code, 4) IN ('2510', /* Restorative codes */
                                         '2520',
                                         '2530',
                                         '2610',
                                         '2620', 
                                         '2630',
                                         '2650',
                                         '2651',
                                         '2652',
                                         
                                         '6600', /* Fixed Partial Denture Retainers - Inlays/Onlays */
                                         '6601',
                                         '6602',
                                         '6603',
                                         '6604',
                                         '6605',
                                         '6606',
                                         '6607',
                                         '6624')
                   AND LENGTH(tooth_data) > 31
                   AND surface_detail IS NOT NULL 
                   /* older codes (previous to cdt4) being with a 0 
                      codes cdt4 (2003) and later begin with a D */
                   AND LEFT(ada_code, 1) IN ('D','0')"))


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
