(defun providers-to-owl (&key limit (print-count t))
  (let (provider-id
	grad-year
	birth-year
	gender-code
	speciality-code
	occupation-code
	(count 0))
    
    (with-ontology ont
	(:collecting t :ontology-iri "http://purl.obolibrary.org/obo/ohd/dev/r03-vdw-providers.owl")
	
	(;; build axioms of vdw pateints based on the demographics file
	 (with-iterator (it :iterator-fn #'provider-iterator)
	   (loop
	      while (next it) do
		(setf provider-id (fv "PROVIDER_STUDY_ID"))
		(setf provider-uri (provider-uri provider-id))
		(setf grad-year (fv "YEAR_GRADUATED"))
		(setf birth-year (fv "PROVIDER_BIRTH_YEAR"))
		(setf gender-code  (fv "PROVIDER_GENDER"))
		(setf speciality-code (fv "SPECIALITY1"))
		(setf occupation--code (fv "PROVIDER_TYPE"))
		
	
	        ;; create instance of provider
		(as (provider-axioms study-id provider-uri occupation-code speciality-code))
		
	      ;; assign provider role
		(as (provider-role-axioms study-id provider-uri))
		
	      ;; assign gender to provider

	      ;; assign assign birth year to provider
		
	      ;; graduation year to vendor

	        ;; check for limit on number of axioms
		(and (incf count) limit (>= count limit) (return)))))

      ;; return ontology and count
      (when print-count (print-db count))
      ont)))

(defun provider-axioms (provider-id provider-uri occupation-code speciality-code)
  (let (axioms provider-type occupation-code-type speciality-code-type label)
    (with-axioms axioms
      
      ;; create instance
      (setf provider-type
	    (provider-type
	     (provider-role-type occupation-code-type speciality-code-type)))
      
      ;; add labels to providers
      (setf label (str+ "dental provider " provider-id))
      (has-label provider-uri label)
      
      ;; return axioms
      axioms)))

(defun provider-role-axioms (provider-id provider-uri)
  (let (axioms role-uri label)
    (with-axioms axioms
      ;; create uri for provider role
      (setf role-uri (provider-role-uri provider-id))

      ;; create instance axioms for role
      (instance-of role-uri !'provider role'@ohd)

      ;; create label for role
      (setf label (str+ "role for provider " provider-id))
      (has-label role-uri label)
    
      ;; role inheres in provider
      (inheres-in role-uri provider-uri)

      ;; return axioms
      axioms)))


(defun provider-gender-role-axioms (provider-id provider-uri gender-code)
  (let (axioms gender label)
    (with-axioms axioms
      (cond
	((equalp "F" gender-code)
	 (setf gender "female gender"))
	((equalp "M" gender-code)
	 (setf gender "male gender"))
	((equalp "O" gender-code)
	 (setf gender "other gender"))
	(t
	 (setf gender "unknown gender")))
	
      ;; add labels to providers
      (setf label (str+ "dental provider " provider-id " (" gender ")"))
      (has-label provider-uri label)
      
      ;; return axioms
      axioms)))

(defun provider-speciality-axioms (provider-id speciality1-code speciality2-code speciality3-code)
  (let
      (axioms
       speciality-uri
       (speciality-codes `(,speciality1-code ,speciality2-code ,speciality3-code)))
    
    (loop
       for speciality-code in speciality-codes
       do
	 (cond
	   ((equalp speciality-code "DEN") )
	   ((equalp speciality-code "EDO") )
	   ((equalp speciality-code "PER") )
	   ((equalp speciality-code "ORA") )
	   ((equalp speciality-code "ORD") )
	   ((equalp speciality-code "PDE") )
	   ((equalp speciality-code "PRO") )
	   ((equalp speciality-code "TMD") )
	   (t ))
	   )
	
    ;; return axioms
    axioms))

(defun povider-grad-year-axioms (provider-id provider-uri grad-year)
  (let (axioms)

    ;; return axioms
    axioms))

