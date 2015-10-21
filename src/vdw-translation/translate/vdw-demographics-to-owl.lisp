(defun demographics-to-owl (&key limit (print-count t))
  (let (study-id
	birth-year
	gender-code

	race-code
	ethnicity-code
	patient-uri
	(count 0))
    
    (with-ontology ont
	(:collecting t :ontology-iri "http://purl.obolibrary.org/obo/ohd/dev/r03-vdw-demographics.owl")
	(;; build axioms of vdw pateints based on the demographics file
	 
	 (with-iterator (it :iterator-fn #'demographics-iterator)
	   (loop
	      while (next it) do
		(setf study-id (fv "STUDY_ID"))
		(setf patient-uri (patient-uri study-id))
		(setf birth-year (fv "BIRTH_YEAR"))
		(setf gender-code  (fv "SEX"))
		(setf race-code (fv "RACE"))
		(setf ethnicity-code (fv "ETHNICITY"))

	        ;; create instance of patient based on gender code
		(as (patient-axioms study-id patient-uri gender-code))

	        ;; assign patient role
		(as (patient-role-axioms study-id patient-uri))

 	        ;; relate race codes to patient
		(as (patient-race-axioms study-id race-code patient-uri))

	        ;; relate ethicity codes to patient
		(as (patient-ethnicity-axioms study-id ethnicity-code patient-uri))

	      ;; add info about patient's birth year
		
		
	        ;; check for limit on number of axioms
		(and (incf count) limit (>= count limit) (return)))))

      ;; return ontology and count
      (when print-count (print-db count))
      ont)))

(defun patient-axioms (study-id patient-uri gender-code)
  (let (axioms gender label ice-uri gender-type gender-uri)
    (with-axioms axioms
      (setf gender-type (gender-role-type gender-code))
      (setf gender-uri (gender-role-uri study-id gender-code))
      
      ;; determine type and gender of patient
      ;; in cases of other or unknown gender create an ice that indicates
      ;; the gender information is not complete
      (cond
	((equalp gender-type !'male gender role'@ohd)
	 (setf gender "male gender")
	 (instance-of patient-uri !'male dental patient'@ohd))
	((equalp gender-type !'female gender role'@ohd)
	 (setf gender "female gender")
	 (instance-of patient-uri !'female dental patient'@ohd))
	(t ;; gender is other or unknown
	 (setf gender "other/unknown gender")
	 (instance-of patient-uri !'human dental patient'@ohd) ; patient is human

	 ;; create ice about patient
	 (setf ice-uri (make-vdw-uri study-id :class-type !'record of incomplete gender information'@ohd))
	 (instance-of ice-uri !'record of incomplete gender information'@ohd)
	 (is-about ice-uri patient-uri)
	 (has-label ice-uri (str+ "record of incomplete gender information about patient " study-id))

	 ;; add info about value of incomplete code
	 (when (> (length gender-code) 0)
	   (has-code-value ice-uri gender-code))))

      ;; add info about patient gender
      (when (or (equalp gender-type !'male gender role'@ohd)
		(equalp gender-type !'female gender role'@ohd))
	(instance-of gender-uri gender-type)
	(has-role patient-uri gender-uri)
	(has-label gender-uri (str+ gender " role for patient " study-id)))

      ;; add label for patient
      (setf label (str+ "dental patient " study-id " (" gender ")"))
      (has-label patient-uri label)
      
      ;; return axioms
      axioms)))

(defun patient-role-axioms (study-id patient-uri)
  (let (axioms role-uri label)
    (with-axioms axioms
      ;; create uri for patient role
      (setf role-uri (patient-role-uri study-id))
      
      ;; create instance axioms for role
      (instance-of role-uri !'dental patient role'@ohd)

      ;; create label for role
      (setf label (str+ "dental role for patient " study-id))
      (has-label role-uri label)
    
      ;; role inheres in patient
      (inheres-in role-uri patient-uri)

      ;; return axioms
      axioms)))


(defun patient-race-axioms (study-id race-code patient-uri)
  (let (axioms race-code-uri race-code-type label)
    (with-axioms axioms
      (setf race-code-uri (race-code-uri study-id race-code))
      (setf race-code-type (race-code-type race-code))

      ;; create instance of race code
      (instance-of race-code-uri race-code-type)

      ;; relate race code to patient
      (is-about race-code-uri patient-uri)

      ;; add label and code value for race code
      (cond
	((equalp race-code-type !'record of incomplete race information'@ohd)
	 (setf label (str+ "record of incomplete race information about patient " study-id)))
	(t
	 (setf label (str+ race-code " race code about patient " study-id))))

      (has-label race-code-uri label)

      ;; add info about value of code
      (when (> (length race-code) 0)
	(has-code-value race-code-uri race-code))
    
      ;; return axioms
      axioms)))

(defun patient-ethnicity-axioms (study-id ethnicity-code patient-uri)
  (let (axioms ethnicity-code-uri ethnicity-code-type label)
    (with-axioms axioms
      (setf ethnicity-code-uri (ethnicity-code-uri study-id ethnicity-code))
      (setf ethnicity-code-type (ethnicity-code-type ethnicity-code))

      ;; create instance of race code
      (instance-of ethnicity-code-uri ethnicity-code-type)

      ;; relate race code to patient
      (is-about ethnicity-code-uri patient-uri)

      ;; add label for ethnicity code
      (cond
	((equalp ethnicity-code-type !'record of incomplete ethnicity information'@ohd)
	 (setf label (str+ "record of incomplete ethnicity information about patient " study-id)))
	(t
	 (setf label (str+ "Hispanic ethnicity code about patient " study-id))))

      (has-label ethnicity-code-uri label)

      ;; add info about value of code
      (when (> (length ethnicity-code) 0)
	(has-code-value ethnicity-code-uri ethnicity-code))
        
      ;; return axioms
      axioms)))

(defun patient-birth-year-axioms (study-id patient-uri birth-year)
  (let (axioms ice-uri)
    (with-axioms axioms
      ;; verify birth year
      (cond
	((= (length birth-year) 4)
	 (has-birth-year patient-uri birth-year))
	(t ;; add information that birth year is incomplete
	 (setf ice-uri (make-vdw-uri study-id :class-type !'record of incomplete birth date information'@ohd))

	 ;; create intance of incomplete info
	 (instance-of ice-uri !'record of incomplete birth date information'@ohd)
	 (is-about ice-uri patient-uri)
	 (has-label ice-uri (str+ "record of incomplete birth date information about patient " study-id))

	 ;; if some info is available add it
	 (when (> (length birth-year) 0)
	   (has-code-value ice-uri birth-year))))
    
      ;; return axioms
      axioms)))

