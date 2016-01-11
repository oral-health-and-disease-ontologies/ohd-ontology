
(defun fillings-to-owl (&key limit (print-count t))
  (let ((count 0))
    
    (with-ontology ont
	(:collecting t :ontology-iri "http://purl.obolibrary.org/obo/ohd/dev/r03-vdw-fillings.owl")
	(;; build axioms of vdw pateints based on the demographics file
	 
	 (with-iterator (it :iterator-fn #'dental-procedure-diagnosis-iterator)
	   (loop
	      while (next it)
	      for study-id = (fv "STUDY_ID")
	      for date = (fv "ADATE")
	      for dx-code = (fv "DX")
	      for ada-code = (fv "ADA_CODE")
	      for tooth-num = (fv "TOOTH")
	      for surface-b = (fv "SURFACE_B")
	      for surface-d = (fv "SURFACE_D")
	      for surface-i = (fv "SURFACE_I")
	      for surface -l = (fv "SURFACE_L")
	      for surface-m = (fv "SURFACE_M")
	      for surface-o = (fv "SURFACE_O")
	      do
		
		(as (vdw-filling-axioms))
		
	        ;; check for limit on number of axioms
		(and (incf count) limit (>= count limit) (return)))))

      ;; return ontology and count
      (when print-count (print-db count))
      ont)))

(defun vdw-filling-axioms (study-id date ada-code tooth-num surface-b surface-d surface-i surface-l surface-m surface-o)
  (let ((patient-uri (patient-uri study-id))
	(tooth-uri (tooth-uri study-id tooth-num))
	(tooth-type (tooth-type tooth-num))
	(filling-role-uri (tooth-to-be-filled-role-uri study-id tooth-num))
	(restoration-uri (restoration-uri study-id ada-code date tooth-num))
	(restoration-type (restoration-type ada-code))
	(material-uri (material-uri 
  )
