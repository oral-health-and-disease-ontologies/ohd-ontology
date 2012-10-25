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

    ;; get query string for restorations
    (setf query (get-eaglesoft-dental-providers-query
		 :r21-provider-id r21-provider-id :limit-rows limit-rows))

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

	 ;; now
	 (with-eaglesoft (results query)
	   (loop while (#"next" results) do
	        ;; get axioms
		(as (get-eaglesoft-dental-provider-axioms 
		     (#"getString" results "r21_provider_id")
		     (#"getString" results "r21_provider_type")
		     (#"getString" results "practice_id")))
		(incf count))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-dental-provider-axioms (r21-provider-id r21-provider-type practice-id)
  "Returns a list of axioms about a dental provider (a human) that is identified by the r21-provider-id."
  (let ((axioms nil)
	(provider-uri nil)
	(practice-uri nil))
    
    ;; if the provider is a person; create instance and assign the person to practice (if known)
    (when (equalp r21-provider-type "person")
      
      (setf provider-uri (get-eaglesoft-dental-provider-iri r21-provider-id))
      (setf axioms
	    (append (get-ohd-instance-axioms provider-uri
					     !'dental health care provider'@ohd) axioms))
      
      (push `(annotation-assertion !rdfs:label
				   ,provider-uri
				   ,(str+ "dental provider " r21-provider-id)) axioms)

      ;; make provider member of practice (if one exists)
      (when practice-id
	(setf practice-uri (get-eaglesoft-dental-practice-iri practice-id))
	(push `(object-property-assertion !'member of'@ohd ,provider-uri ,practice-uri) axioms)))

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

