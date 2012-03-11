(defparameter *ohd-label-source* nil)

(defmethod make-uri-from-label-source ((source (eql :ohd)) name actual)
  "URI label source for ohd. All labels in OHD or imports are available. Case insensitive matching. Complains if an ambiguous label is used, or if a label is missing, or if actual is supplied and doesn't match the looked up term"
  (unless *ohd-label-source*
    (let ((table (make-hash-table :test 'equalp)))
      (loop for file in '("ohd:imports;fma-jaws-teeth.owl" "ohd:ontology;ohd.owl" "http://purl.obolibrary.org/obo/bfo/core-classes.owl" "http://purl.obolibrary.org/obo/bfo/core-relations.owl")
	   do
	   (let ((kb (load-ontology (namestring (truename file)))))
	     (let ((labels (rdfs-labels kb)))
	       (maphash (lambda(uri label) 
			  (let ((label (car label)))
			    (if (gethash label table) 
				(unless (eq (gethash label table)  uri)
				  (setf (gethash label table) :ambiguous))
				(setf (gethash label table) uri))))
			labels))))
      (setq *ohd-label-source* table)))
  (let ((found (gethash name *ohd-label-source*)))
    (when (eq found :ambiguous)
      (progn
	(warn "Uri label ~a in ~a is ambiguous" name source)
	(setq found nil)))
    (if found
	(progn
	  (when actual
	    (assert (eq found (make-uri nil actual)) (found actual)
		    "Uri label lookup for '~a' - ~a doesn't match specified actual '~a'"
		    name found actual))
	  found)
	(if actual
	    (progn
	      (warn "Uri label lookup for '~a' failed - using provided actual: '~a'" name actual)
	      (make-uri nil actual))
	    (error "Couldn't determine which URI was meant by '~a' in ~a" name source)))))