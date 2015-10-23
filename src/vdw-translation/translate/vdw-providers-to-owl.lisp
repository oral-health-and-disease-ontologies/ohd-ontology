(defun providers-to-owl (&key limit (print-count t))
  (let (provider-id
	provider-uri
	role-uri
	role-type
	grad-year
	birth-year
	gender-code
	specialty-code
	specialty-code-type
	occupation-code
	occupation-code-type
	(count 0))
    
    (with-ontology ont
	(:collecting t :ontology-iri "http://purl.obolibrary.org/obo/ohd/dev/r03-vdw-providers.owl")
	
	(;; build axioms of vdw pateints based on the demographics file
	 (with-iterator (it :iterator-fn #'provider-iterator)
	   (loop
	      while (next it) do
		(setf provider-id (fv "PROVIDER_STUDY_ID"))
		(setf grad-year (fv "YEAR_GRADUATED"))
		(setf birth-year (fv "PROVIDER_BIRTH_YEAR"))
		(setf gender-code  (fv "PROVIDER_GENDER"))
		(setf specialty-code (fv "SPECIALTY"))
		(setf occupation-code (fv "PROVIDER_TYPE"))

	        ;; determine uri and types of role for provider
		(setf occupation-code-type (provider-occupation-code-type occupation-code))
		(setf specialty-code-type (specialty-code-type specialty-code))
		(setf role-type (provider-role-type occupation-code-type specialty-code-type))
		(setf role-uri (provider-role-uri provider-id occupation-code-type specialty-code-type))
		(setf provider-uri (provider-uri provider-id role-type))
		
	        ;; create instance of provider
		(as (provider-axioms provider-id provider-uri role-type))
		
	        ;; assign provider role
		(as (provider-role-axioms provider-id provider-uri role-uri role-type))
		
	        ;; assign gender to provider
		(as (provider-gender-role-axioms provider-id provider-uri gender-code))
		
	        ;; assign assign birth year to provider
		(as (provider-birth-year-axioms provider-id provider-uri birth-year))
		
	        ;; assign graduation year to vendor
		(as (provider-grad-year-axioms  provider-id provider-uri grad-year))

	        ;; assign specialty codes to provider
		(as (provider-specialty-axioms provider-id provider-uri specialty-code specialty-code-type))
		
	        ;; check for limit on number of axioms
		(and (incf count) limit (>= count limit) (return)))))

      ;; return ontology and count
      (when print-count (print-db count))
      ont)))

(defun provider-axioms (provider-id provider-uri provider-role-type)
  (let (axioms label)
    (with-axioms axioms
      ;; create instance
      (instance-of provider-uri (provider-type provider-role-type))
	          
      ;; add labels to providers
      (setf label (str+ "dental provider " provider-id))
      (has-label provider-uri label)
      
      ;; return axioms
      axioms)))

(defun provider-role-axioms (provider-id provider-uri provider-role-uri provider-role-type)
  (let (axioms label)
    (with-axioms axioms
      ;; create instance axioms for role
      (instance-of provider-role-uri provider-role-type)

      ;; create label for role
      (setf label (str+ "health care provider role for provider " provider-id))
      (has-label provider-role-uri label)
    
      ;; role inheres in provider
      (inheres-in provider-role-uri provider-uri)

      ;; return axioms
      axioms)))


(defun provider-gender-role-axioms (provider-id provider-uri gender-code)
  (let (axioms label gender-uri gender-type)
    (with-axioms axioms
      ;; create instance of gender role
      (setf gender-type (gender-role-type gender-code))
      (setf gender-uri (gender-role-uri provider-id gender-code))
      (instance-of gender-uri gender-type)
      
      ;; relate gender role to provided
      (cond
	((or (equalp gender-type !'male gender role'@ohd)
	     (equalp gender-type !'female gender role'@ohd))
	 (has-role provider-uri gender-uri))
	(t
	 (is-about gender-uri provider-uri)))
		 
      ;; determine label to provider gender role
      ;; if gender info is incomplete relate gender code to ice
      (cond
	((equalp "F" gender-code)
	 (setf label (str+ "female gender role for provider " provider-id)))
	((equalp "M" gender-code)
	 (setf label (str+ "male gender role for provider " provider-id)))
	(t
	 (setf label (str+ "record of incomplete gender information about provider " provider-id))
	 (when (> (length gender-code) 0)
	   (has-code-value gender-uri gender-code))))
	      
      ;; add label to gender role uri
      (has-label gender-uri label)
      
      ;; return axioms
      axioms)))

(defun provider-specialty-axioms (provider-id provider-uri specialty-code specialty-code-type)
  (let (axioms specialty-code-uri)
    (with-axioms axioms
      ;; create instance of specialty code
      (setf specialty-code-uri (specialty-code-uri provider-id specialty-code))
      (instance-of specialty-code-uri specialty-code-type)
      (when (> (length specialty-code) 0)
	(has-code-value specialty-code-uri specialty-code))

      ;; relate ice to provider
      (is-about specialty-code-uri provider-uri)
      
      ;; add label 
      (cond
	((equalp specialty-code-type !'record of incomplete provider specialty information'@ohd)
	 (has-label specialty-code-uri
		    (str+ "record of incomplete provider specialty information about provider " provider-id)))
	(t
	 (has-label specialty-code-uri
		    (str+ "specialty code for provider " provider-id))))
    ;; return axioms
    axioms)))

(defun provider-grad-year-axioms (provider-id provider-uri grad-year)
  (let (axioms ice-uri)
    (with-axioms axioms
      ;; verify graduation year
      (cond
	((= (length grad-year) 4)
	 (has-graduation-year provider-uri grad-year))
	(t ;; add information that grad year is incomplete
	 (setf ice-uri (make-vdw-uri provider-id :class-type !'record of incomplete graduation year information'@ohd))

	 ;; create intance of incomplete info
	 (instance-of ice-uri !'record of incomplete birth date information'@ohd)
	 (is-about ice-uri provider-uri)
	 (has-label ice-uri (str+ "record of incomplete graduation year information about provider " provider-id))

	 ;; if some info is available add it
	 (when (> (length grad-year) 0)
	   (has-code-value ice-uri grad-year))))
    
      ;; return axioms
      axioms)))

(defun provider-birth-year-axioms (provider-id provider-uri birth-year)
  (let (axioms ice-uri)
    (with-axioms axioms
      ;; verify birth year
      (cond
	((= (length birth-year) 4)
	 (has-birth-year provider-uri birth-year))
	(t ;; add information that birth year is incomplete
	 (setf ice-uri (make-vdw-uri provider-id :class-type !'record of incomplete birth date information'@ohd))

	 ;; create intance of incomplete info
	 (instance-of ice-uri !'record of incomplete birth date information'@ohd)
	 (is-about ice-uri provider-uri)
	 (has-label ice-uri (str+ "record of incomplete birth date information about provider " provider-id))

	 ;; if some info is available add it
	 (when (> (length birth-year) 0)
	   (has-code-value ice-uri birth-year))))
    
      ;; return axioms
      axioms)))

