(defun fillings-to-owl (&key limit (print-count t))
  (let ((count 0))
    
    (with-ontology ont
	(:collecting t :ontology-iri "http://purl.obolibrary.org/obo/ohd/dev/r03-vdw-fillings.owl")
	(;; build axioms of vdw pateints based on the demographics file
	 
	 (with-iterator (it :iterator-fn #'dental-procedure-diagnosis-iterator)
	   (loop
	      while (next it) do
		for study-id = fv "STUDY_ID")
		for patient-uri = (patient-uri study-id)
		for code = (fv "ADA_CODE")
		for tooth-num = = (fv "TOOTH")
		do
		
		(as (vdw-filling-axioms))
		
	        ;; check for limit on number of axioms
		(and (incf count) limit (>= count limit) (return)))))

      ;; return ontology and count
      (when print-count (print-db count))
      ont)))

(defun vdw-filling-axioms ()

  )
