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

(defun get-eaglesoft-dental-providers-ont 
    (&key dental-organization-id r21-provider-id limit-rows force-create-table)
  "Returns an ontology of the dental providers contained in the Eaglesoft database.  The dental-organization-id is needed in cases when the dental health care organization contains more than one practice.  In these cases, the dental-organization-id is used to create the larger entity that each of the practices is a part of.  Because such ids are not given in the database, it must be supplied at run time.  The r21-provider-id creates an ontology based on that specific provider as identified in the r21_provider table. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."

  (let ((results nil)
	(query nil)
	(count 0))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db :force-create-table force-create-table)

    
    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-dental-providers-iri-base*
			:ontology-iri *eaglesoft-dental-providers-ontology-iri*)
	(;; import the ohd ontology
	 (as `(imports ,(make-uri *ohd-ontology-iri*)))

	 ;; get axioms for declaring annotation, object, and data properties used for ohd
	 (as (get-ohd-declaration-axioms))
	 
	 ;; get records from eaglesoft db and create axioms

	 ;; first get axioms to describe practices
	 ;; this is needed so that a provider can be member of a practice
	 (as (get-eaglesoft-dental-practice-axioms :dental-organization-id dental-organization-id))


	 ;; get query string for known dental health care providers
	 (setf query (get-eaglesoft-dental-providers-query
		      :r21-provider-id r21-provider-id :limit-rows limit-rows))
	 ;; get axioms for known providers
	 (with-eaglesoft (results query)
	   (loop while (#"next" results) do
	        ;; get axioms
		(as (get-eaglesoft-dental-provider-axioms 
		     (#"getString" results "r21_provider_id")
		     (#"getString" results "r21_provider_type")
		     (#"getString" results "practice_id")))
		(incf count)))

	 ;; make sure to clear out results
	 (setf results nil)

	 ;; get query string for anonymous dental health care providers
	 (setf query (get-eaglesoft-anonymous-providers-query :limit-rows limit-rows))

	 ;; get axioms for anonymous providers
	 (with-eaglesoft (results query)
	   (loop while (#"next" results) do
	        ;; get axioms
		(as (get-eaglesoft-anonymous-dental-provider-axioms 
		     (#"getString" results "row_id")
		     (#"getString" results "practice_id")))
		(incf count))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-dental-provider-axioms (r21-provider-id r21-provider-type practice-id)
  "Returns a list of axioms about a dental provider (a human) that is identified by the r21-provider-id."
  (let ((axioms nil)
	(temp-axioms nil) ; used for appending instance axioms
	(provider-uri nil)
	(provider-label nil)
	(provider-role-uri nil)
	(practice-uri nil))
    
    ;; if the provider is a person; create instance and assign the person to practice (if known)
    (when (equalp r21-provider-type "person")
      
      ;; get axioms for instance of provider
      (setf provider-uri (get-eaglesoft-dental-provider-iri r21-provider-id))
      (setf temp-axioms (get-ohd-instance-axioms provider-uri !'dental health care provider'@ohd))
      (setf axioms (append temp-axioms axioms))
      
      ;; format provider's label
      (setf provider-label (format nil "dental health care provider ~a" r21-provider-id))
      (push `(annotation-assertion !rdfs:label ,provider-uri ,provider-label) axioms)


      ;; get axioms for instance of dental health care provider role
      (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri r21-provider-id))
      (setf temp-axioms (get-ohd-instance-axioms provider-role-uri !'dental health care provider role'@ohd))
      (setf axioms (append temp-axioms axioms))

      ;; add annotation about provider role
      (push `(annotation-assertion !rdfs:label
				   ,provider-role-uri
				   ,(str+ "dental health care provider role for " provider-label)) axioms)

      ;; the provider role inheres in the provider
      (push `(object-property-assertion !'inheres in'@ohd 
					,provider-role-uri ,provider-uri) axioms)

      ;; make provider member of practice (if one exists)
      (when practice-id
	(setf practice-uri (get-eaglesoft-dental-practice-iri practice-id))
	(push `(object-property-assertion !'member of'@ohd ,provider-uri ,practice-uri) axioms)))

    ;; return axioms
    axioms))

(defun get-eaglesoft-anonymous-dental-provider-axioms (row-id practice-id)
  "Return a list of axioms about anonymous providers in the patient_history table."
  (let ((axioms nil)
	(temp-axioms nil) ; used for appending instance axioms
	(provider-uri nil)
	(provider-label nil)
	(provider-role-uri nil)
	(practice-uri nil))
    
    ;; to get uri for iri for anonymous provider pass a dummy variable for the first arg
    ;; and then provide a unique identifer (in this case the row id) for the anonymous-id key
    (setf provider-uri (get-eaglesoft-dental-provider-iri "x" :anonymous-id row-id))
    (setf temp-axioms (get-ohd-instance-axioms provider-uri !'dental health care provider'@ohd))
    (setf axioms (append temp-axioms axioms))

    ;; format provider's label
    (setf provider-label (format nil "anonymous dental health care provider ~a" row-id))
    (push `(annotation-assertion !rdfs:label ,provider-uri ,provider-label) axioms)

    ;; now anonymous provider's role
    (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri "x":anonymous-id row-id))
    (setf temp-axioms (get-ohd-instance-axioms provider-role-uri !'dental health care provider role'@ohd))
    (setf axioms (append temp-axioms axioms))

    ;; add annotation about provider role
    (push `(annotation-assertion !rdfs:label
				 ,provider-role-uri
				 ,(str+ "dental health care provider role for " provider-label)) axioms)

    ;; the provider role inheres in the provider
    (push `(object-property-assertion !'inheres in'@ohd 
				      ,provider-role-uri ,provider-uri) axioms)

    ;; make provider member of practice (if one exists)
    (when practice-id
      (setf practice-uri (get-eaglesoft-dental-practice-iri practice-id))
      (push `(object-property-assertion !'member of'@ohd ,provider-uri ,practice-uri) axioms))

    ;; return axioms
    axioms))

(defun get-eaglesoft-dental-practice-axioms (&key dental-organization-id)
  (let ((axioms nil)
	(results nil)
	(practice-id nil)
	(practice-uri nil)
	(organization-uri nil)
	(query nil))
    
    ;; if a dental organization id is given, create an instance of 'dental health care organization' 
    ;; that each practice will be a part of
    (when dental-organization-id
      (setf dental-organization-id (format nil "~a" dental-organization-id)) ; ensure id is a string
      (setf organization-uri (get-eaglesoft-dental-organization-iri dental-organization-id))
      (push `(declaration (named-individual ,organization-uri)) axioms)
      (setf axioms
	    (append (get-ohd-instance-axioms organization-uri
					     !'dental health care organization'@ohd) axioms))
      (push `(annotation-assertion 
	      !rdfs:label 
	      ,organization-uri
	      ,(str+ "dental health care organization " dental-organization-id)) axioms))

    ;; get query string for returning practices
    (setf query (get-eaglesoft-dental-practices-query))

    (with-eaglesoft (results query)
      (loop while (#"next" results) do
	   (setf practice-id (#"getString" results "practice_id"))
      
	   (setf practice-uri (get-eaglesoft-dental-practice-iri practice-id))
	   (push `(declaration (named-individual ,practice-uri)) axioms)
	   (setf axioms
		 (append (get-ohd-instance-axioms practice-uri 
						  !'dental health care organization'@ohd) axioms))

	   ;; add annotation about the practice
	   (push `(annotation-assertion !rdfs:label
					,practice-uri
					,(format nil "dental practice ~a" practice-id)) axioms)
	 
	   ;; if a dental organization is given, the make the practice a part of it
	   (when organization-uri
	     (push `(object-property-assertion !'is part of'@ohd ,practice-uri ,organization-uri) axioms))))
					      
      
    ;; return axioms
    axioms))

(defun get-eaglesoft-unknown-provider-type-axioms (provider-uri)
  "Returns a list of axioms for specifying that a provider is either a dental care provider (person) or dental health care organization."
  (let ((axioms nil))
    (push `(class-assertion 
	    (object-union-of !'dental care provider'@ohd
			     !'dental health care organization'@ohd)
	    ,provider-uri) axioms)
    
    (push `(annotation-assertion !'asserted type'@ohd 
				 ,provider-uri
				 (object-union-of 
				  !'dental care provider'@ohd
				  !'dental health care organization'@ohd)) axioms)
    ;; return axioms
    axioms))

(defun get-eaglesoft-dental-organization-iri (dental-organizaton-id)
  "Returns an iri for an organization that is generated by the dental-organizaton-id."
  (let ((uri nil))
    (setf uri (get-unique-individual-iri dental-organizaton-id
					    :salt *eaglesoft-salt*
					    :iri-base *eaglesoft-individual-dental-providers-iri-base*
					    :class-type !'dental health care organization'@ohd))
    ;; return uri
    uri))

(defun get-eaglesoft-dental-providers-query (&key r21-provider-id limit-rows)
  "Returns query string for retrieving provider data. The r21-provider-id key restricts records only that provider or providers as identified in the r21_provider table.  Multiple are providers are specified using commas; e.g: \"123, 456, 789\".  The limit-rows key restricts the number of records to the number specified."
  (let ((sql nil))

    ;; build query string
    (setf sql "SET rowcount 0 ")
    
    ;; determine number of rows for SELECT clause
    (cond 
      (limit-rows
       (setf limit-rows (format nil "~a" limit-rows)) ;ensure that limit rows is a string
       (setf sql (str+ sql " SELECT  TOP " limit-rows " "))) 
      (t (setf sql (str+ sql " SELECT "))))

    ;; append field names to SELECT clause
    (setf  sql 
	   (str+ sql 
		 "r.r21_provider_id, r.provider_id, p.description, r21_provider_type, r.practice_id "))

    ;; FROM clause
    (setf sql (str+ sql " FROM r21_provider r LEFT JOIN positions p ON r.position_id = p.position_id "))

    ;; WHERE clause
    ;; check for r21-provider-id
    (when r21-provider-id
      (setf sql
	    (str+ sql " WHERE r21_provider_id IN (" (get-single-quoted-list r21-provider-id) ") ")))

    ;; ORDER BY clause
    (setf sql (str+ sql " ORDER BY r21_provider_id  "))

    ;; return query string
    ;;(pprint sql)
    sql))

(defun get-eaglesoft-dental-practices-query ()
  "Return query string for getting the dental practies."
  (let ((sql nil))
    (setf sql "SELECT * FROM practice")

    ;;return query string
    sql))

(defun get-eaglesoft-anonymous-providers-query (&key limit-rows)
  "Returns query string for the those records in the patient_history where no provider is indicated."
  (let ((sql nil))
    ;; build query string
    (setf sql "SET rowcount 0 ")
    
    ;; determine number of rows for SELECT clause
    (cond 
      (limit-rows
       (setf limit-rows (format nil "~a" limit-rows)) ;ensure that limit rows is a string
       (setf sql (str+ sql " SELECT  TOP " limit-rows " "))) 
      (t (setf sql (str+ sql " SELECT "))))

    ;; append field names to select clause
    (setf sql (str+ sql "row_id, practice_id "))
    
    ;; FROM clause
    (setf sql (str+ sql "FROM patient_history "))

    ;; WHERE clause - get those records where provide is not a person or is null
    (setf sql (str+ sql "WHERE (r21_provider_type <> 'person' or r21_provider_id IS NULL) "))
    
    ;; get codes that begin with 'D' or '0'
    (setf sql (str+ sql "AND LEFT(ada_code, 1) IN ('D', '0') "))

    ;; filter by ADA codes in R21 study
    (setf sql (str+ sql "
/*
This is a list of the ADA codes needed for R21 OHD study.
*/

AND RIGHT(ada_code, 4) IN (
             -- diagnostic codes
             '0120',
             '0140',
             '0150',
             '0180',
             
             -- preventitive codes
             '1351',
             '1352',
             
             -- fillings codes
             '2140', -- Restorative: Amalgam
             '2150',
             '2160',
             '2161',
             '2330', -- Restorative: Resin
             '2331',
             '2332',
             '2335',
             '2390',
             '2391',
             '2392',
             '2393',
             '2394',
             '2410', -- Restorative: Gold Foil
             '2420',
             '2430',
             
             -- inlay/onlay restorative
             '2510',
             '2520',
             '2530',
             '2542',
             '2543',
             '2544',
             '2610',
             '2620',
             '2630',
             '2642',
             '2643',
             '2644',
             '2650',
             '2651',
             '2652',
             '2662',
             '2663',
             '2664',
             
             -- crown codes
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
             '2940',
             '2950',
             '2952',
             '2954',
             '2960',
             '2961',
             '2962',
             '2970',
             
             -- endodontics codes
             '3220',
             '3221',
             '3222',
             '3310',
             '3320',
             '3330',
             '3346',
             '3347',
             '3348',
             '3410',
             '3421',
             '3425',
             '3450',
             '3920',
             
             -- pontics
             '6205',
             '6210',
             '6211',
             '6212',
             '6214',
             '6240',
             '6241',
             '6242',
             '6245',
             '6250',
             '6251',
             '6252',
             '6253',
             '6254',
             
             -- fixed retainers inlay/onlay
             '6545',
             '6548',
             '6600',
             '6601',
             '6602',
             '6603',
             '6604',
             '6605',
             '6606',
             '6607',
             '6608',
             '6609',
             '6610',
             '6611',
             '6612',
             '6613',
             '6614',
             '6615',
             '6624',
             '6634',
             
             -- fixed retainers crowns
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
             '6795',
             
             -- other fixed retainers
             '6970',
             '6972',
             '6973',
             
             -- surgical extractions codes
             '7110',
             '7120',
             '7140',
             '7210',
             
             -- pallative treatment
             '9110')"))))
    