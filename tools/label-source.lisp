(defparameter *ohd-label-source* nil)

(defmethod make-uri-from-label-source ((source (eql :ohd)) name actual)
  "URI label source for ohd. All labels in OHD or imports are available. Case insensitive matching. Complains if an ambiguous label is used, or if a label is missing, or if actual is supplied and doesn't match the looked up term"
  (unless *ohd-label-source*
    (let ((table (make-hash-table :test 'equalp)))
      (loop for file in '("ohd:imports;fma-jaws-teeth.owl" "ohd:ontology;ohd.owl" "ohd:imports;BFO2;bfo2-classes.owl" "ohd:imports;BFO2;bfo2-relations.owl")
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


(defun get-copy-ohd-label-source ()
  "Makes and returns a copy of the *ohd-label-source* hash table created in make-uri-from-label-source."
  (let ((label-copy-ht (make-hash-table :test 'equalp))
	(uri nil))
  
    ;; make sure *ohd-label-source* hash table exists)
    (when (not (hash-table-p *ohd-label-source*))
      ;; create *ohd-label-source*
      (setf uri !'Tooth'@ohd))

    ;; loop over *ohd-label-source* can copy values
    (loop 
       for k being the hash-keys in *ohd-label-source*
       using (hash-value v) do
       (setf (gethash k label-copy-ht) v))

    ;; return copy of hash table
    label-copy-ht))

(defun compare-ontology-rdfs-labels (ont1 ont2)
  "Compares the rdfs:labels for the uris in two ontologies, and prints to the screen the uris (and the uri's label) in which the labels of each ontology do not match."
  (let ((ont1-labels nil)
	(ont2-labels nil)
	(diff-labels nil))

    ;; get the (uri rdfs:label) list of each ontology
    (setf ont1-labels 
	  (loop 
	     for k being the hash-keys in (rdfs-labels ont1) using (hash-value v) 
	     collect (format nil "~a ~a" k (car v))))
	     ;;collect (format nil "~a" (car v))))

    (setf ont2-labels 
	  (loop 
	     for k being the hash-keys in (rdfs-labels ont2) using (hash-value v)
	     collect (format nil "~a ~a" k (car v))))
	     ;;collect (format nil "~a" (car v))))


    ;; get the set difference of each
    (setf diff-labels (set-difference ont1-labels ont2-labels :test 'equalp))
    
    ;; return the diff labels (if any)
    diff-labels))
	

    