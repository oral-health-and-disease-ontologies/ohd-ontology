(defun demographics-to-owl (&key limit (print-count t))
  (let (study-id
	birth-year
	gender-code
	race1-code
	race2-code
	race3-code
	race4-code
	race5-code
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
		(setf gender-code  (fv "GENDER"))
		(setf race-code (fv "RACE"))
		(setf ethnicity-code (fv "ETHNICITY"))

	        ;; create instance of patient based on gender code
		(as (patient-axioms study-id patient-uri gender-code))

	         ;; assign patient role
		(as (patient-role-axioms study-id patient-uri))
		
	        ;; check for limit on number of axioms
		(and limit (incf count) (>= count limit) (return)))))

      ;; return ontology and count
      (when print-count (print-db count))
      ont)))

(defun patient-axioms (study-id patient-uri gender-code)
  (let (axioms gender label)
    (with-axioms axioms
      (cond
	((equalp "F" gender-code)
	 (setf gender "female gender")
	 (instance-of patient-uri !'female dental patient'@ohd))
	((equalp "M" gender-code)
	 (setf gender "male gender")
	 (instance-of patient-uri !'male dental patient'@ohd))
	((equalp "O" gender-code)
	 (setf gender "other gender")
	 (instance-of patient-uri !'human dental patient'@ohd))
	(t
	 (setf gender "unknown gender")
	 (instance-of patient-uri !'human dental patient'@ohd)))

      ;; add labels to patients
      (setf label (str+ "dental patient " study-id " (" gender ")"))
      (has-label patient-uri label)
      
      ;; return axioms
      axioms)))

(defun patient-role-axioms (study-id patient-uri)
  (let (axioms role-uri label)
    (with-axioms axioms
      ;; create uri for patient role
      (setf role-uri (make-vdw-uri study-id :class-type !'patient role'@ohd))

      ;; create instance axioms for role
      (instance-of role-uri !'patient role'@ohd)

      ;; create label for role
      (setf label (str+ "role for patient " study-id))
      (has-label role-uri label)
    
      ;; role inheres in patient
      (inheres-in role-uri patient-uri)

      ;; return axioms
      axioms)))


(defun patient-race-axioms (study-id race-code)
  (let
      (axioms
       race-code)
    (cond
      ((equalp race-code "BA") )
      ((equalp race-code "WH") )
      ((equalp race-code "OT") )
      (t ))
    	
    ;; return axioms
    axioms))

(defun patient-birth-year-axioms (study-id patient-uri birth-year)
  (let (axioms)

    ;; return axioms
    axioms))

