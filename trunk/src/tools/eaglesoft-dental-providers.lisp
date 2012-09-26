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
  "Returns a list of axioms about a dental provider (human or organization) that is identified by the r21-provider-id."
  (let ((axioms nil)
	(provider-uri nil)
	(provider-type-uri nil)
	(practice-uri nil))

    ;; create instance of the provider
    ;; if the provider type is unknown, then create anonymous indivual
    (cond
      (;; if the provider is person or organization create a named individual
       (or (equalp r21-provider-type "person") (equalp r21-provider-type "organization"))
       (setf provider-uri (get-eaglesoft-dental-provider-iri r21-provider-id))
       (push `(declaration (named-individual ,provider-uri)) axioms))
      
      (t ;; in this case the proviser is unknown or a temporary worker, create anonyomous individual
       (setf provider-uri `(:blank ,(format nil "anonymous provider ~a" r21-provider-id)))))

    ;; add annotation about provider
    (push `(annotation-assertion !rdfs:label
				 ,provider-uri
				 ,(format nil "provider ~a" r21-provider-id)) axioms)
       

  ;; determine type of provider
  ;; note: the unknown case will be handled in a separate conditional
  (cond
    ((equalp r21-provider-type "person") 
     (setf provider-type-uri !'dental care provider'@ohd))
    ((equalp r21-provider-type "person temporary") 
     (setf provider-type-uri !'dental care provider'@ohd))
    ((equalp r21-provider-type "organization")
      (setf provider-type-uri !'dental health care organization'@ohd)))
    
  ;; make class assertion about provider instance
  ;; if there is a provider type uri, then set type to it
  ;; otherwise, get axioms for unknown case
  (cond
    (provider-type-uri
     (setf axioms (append (get-ohd-instance-axioms provider-uri provider-type-uri) axioms)))
    (t ;this is the "unknown" case
     (setf axioms (append (get-unknown-provider-type-axioms provider-uri) axioms))))


  ;; create instance of the practice (a dental health care organization)
  ;; that provider is part of
  ;; note: do this only when there is a practice id
  (when practice-id
    (setf practice-uri (get-eaglesoft-dental-practice-iri practice-id))
    (push `(declaration (named-individual ,practice-uri)) axioms)
    (setf axioms
	  (append (get-ohd-instance-axioms practice-uri 
					   !'dental health care organization'@ohd) axioms))

    ;; add annotation about the practice
    (push `(annotation-assertion !rdfs:label
				 ,practice-uri
				 ,(format nil "practice ~a" practice-id)) axioms)

    ;; specify that the provider is part of a practice
    (push `(object-property-assertion !'is part of'@ohd
				      ,provider-uri ,practice-uri) axioms))

  ;; return axioms
  axioms))

(defun get-unknown-provider-type-axioms (provider-uri)
  "Returns a list of axioms for specifying that a provider is either a dental care provider (person) or dental health care organization."
  (let ((axioms nil))
    (push `(class-assertion 
	    (object-union-of !'dental care provider'@ohd
			     !'dental health care organization'@ohd)
	    ,provider-uri) axioms)
    
    (push `(annotation-assertion !'asserted type'@ohd ,provider-uri
				 !'dental care provider'@ohd) axioms)

    (push `(annotation-assertion !'asserted type'@ohd ,provider-uri
				 !'dental health care organization'@ohd) axioms)
    ;; return axioms
    axioms))

(defun get-eaglesoft-dental-practice-iri (practice-id)
  "Returns an iri for a practice that is generated by the practice id."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri practice-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-dental-providers-iri-base*
				     :class-type !'dental health care organization'@ohd
				     :args "eaglesoft"))
    ;; return uri
    uri))

(defun get-eaglesoft-dental-provider-iri (provider-id)
  "Returns an iri for a provider that is generated by the provider id."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri provider-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-dental-providers-iri-base*
				     :class-type !'dental care provider'@ohd
				     :args "eaglesoft"))
    ;; return uri
    uri))

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
    (when r21-provider-id
      (setf sql
	    (str+ sql " WHERE r21_provider_id IN (" (get-single-quoted-list r21-provider-id) ") ")))

    ;; ORDER BY clause
    (setf sql (str+ sql " ORDER BY r21_provider_id  "))

    ;; return query string
    ;;(pprint sql)
    sql))