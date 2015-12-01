(defun dental-encounters-to-owl (&key limit (print-count t))
  (let ((count 0))
    
    (with-ontology ont
	(:collecting t :ontology-iri "http://purl.obolibrary.org/obo/ohd/dev/r03-vdw-dental-encounters.owl")
	
	(;; build axioms of vdw pateints based on the dental-encounter file
	 (with-iterator (it :iterator-fn #'dental-encounter-iterator)
	   (loop
	      while (next it) 
	      ;; get data from file
	      for study-id = (fv "STUDY_ID")
	      for provider-id = (fv "PROVIDER_STUDY_ID")
	      for facility-code = (fv "FACILITY_CODE")
	      for encounter-id = (fv "STUDY_ENC_ID")
	      for occurrence-date = (adate-to-date (fv "ADATE"))
	      do
		;; build axioms
		(as
		 (dental-encounter-axioms
		  encounter-id 
		  study-id 
		  provider-id
		  facility-code
		  occurrence-date))
		
		(and (incf count) limit (>= count limit) (return)))))

      ;; return ontology and count
      (when print-count (print-db count))
      ont)))

(defun dental-encounter-axioms (encounter-id study-id provider-id facility-code occurrence-date)
  (let ((axioms nil)
	(label nil)
	(encounter-uri (encounter-uri encounter-id))
	(patient-role-uri (patient-role-uri study-id))
	(provider-role-uri (provider-role-uri provider-id))
	(facility-uri (facility-uri facility-code)))
    
    (with-axioms axioms

      ;; create instances of dental visit and facilty
      (instance-of encounter-uri !'dental visit'@ohd)
      (instance-of facility-uri !'dental health care organization'@ohd)

      ;; encounter realizes patient and provider roles
      (realizes encounter-uri patient-role-uri)
      (realizes encounter-uri provider-role-uri)

      ;; facilty at which the encounter took place
      (occurs-in encounter-uri facility-uri)
		 
      ;; date on which encounter happened
      (has-occurrence-date encounter-uri occurrence-date)

      ;; add label for encounter and facility
      (setf label (str+ "dental visit for patient " study-id " on " occurrence-date))
      (has-label encounter-uri label)

      (setf label (str+ "facility " facility-code))
      (has-label facility-uri label)

      ;;(print-db axioms)
      ;; return axioms
      axioms)))
