(defun ohd-class-count (path-to-ontology)
  (let
      ((ohd-count 0)
       (bfo-count 0)
       (fma-count 0)
       (ogms-count 0)
       (omrse-count 0)
       (iao-count 0)
       (obi-count 0)
       (caro-count 0)
       (cdt-count 0)
       (ncbi-count 0)
       (all-classes-count 0)
       (running-total-count 0)
       (ohd-ontology nil))

    ;; load the ontology; I do this by creating a merged merged version of
    ;; the tooth restoration ontology and all its imports
    ;; e.g., set path-to-ontology to "/Users/widuncan/Desktop/ohd-merged.owl"
    (setf ohd-ontology (load-ontology path-to-ontology :reasoner :none))

    (setf all-classes-count (count-ontology-classes ohd-ontology))
    (setf ohd-count (count-ontology-classes ohd-ontology "OHD_"))
    (setf bfo-count (count-ontology-classes ohd-ontology "BFO_"))
    (setf fma-count (count-ontology-classes ohd-ontology "FMA_"))
    (setf ogms-count (count-ontology-classes ohd-ontology "OGMS_"))
    (setf omrse-count (count-ontology-classes ohd-ontology "OMRSE_"))
    (setf iao-count (count-ontology-classes ohd-ontology "IAO_"))
    (setf obi-count (count-ontology-classes ohd-ontology "OBI_"))
    (setf caro-count (count-ontology-classes ohd-ontology "CARO_"))
    (setf cdt-count (count-ontology-classes ohd-ontology "CDT_"))
    (setf ncbi-count (count-ontology-classes ohd-ontology "NCBI"))
    (setf running-total-count
	  (+ ohd-count bfo-count fma-count ogms-count omrse-count
	     iao-count obi-count caro-count cdt-count ncbi-count))

    (format t "The total number of OHD specific classes in OHD: ~a~%" ohd-count)
    (format t "The total number of BFO imported classes in OHD: ~a~%" bfo-count)
    (format t "The total number of FMA imported classes in OHD: ~a~%" fma-count)
    (format t "The total number of OGMS imported classes in OHD: ~a~%" ogms-count)
    (format t "The total number of OMRSE imported classes in OHD: ~a~%" omrse-count)
    (format t "The total number of IAO imported classes in OHD: ~a~%" iao-count)
    (format t "The total number of OBI imported classes in OHD: ~a~%" obi-count)
    (format t "The total number of CARO imported classes in OHD: ~a~%" caro-count)
    (format t "The total number of CDT imported classes in OHD: ~a~%" cdt-count)
    (format t "The total number of NCBI imported classes in OHD: ~a~%" ncbi-count)
    (format t "The total number of OHD specific and imported classes in OHD: ~a~%" running-total-count)
    (format t "The total number of all classes in OHD: ~a~%" all-classes-count)
    
    all-classes-count))
		 
		 
(defun count-ontology-classes (ont &optional prefix &key print-uri)
  "ont is ontology class (i.e, OWLAPIv3 KB);
prefix is string, e.g. 'OHD', that occurrs in the URI (e.g., !obo:OHD_000001) of the class;
when prefix is passed in, only classes that have the prefix string in their URIs will be matched;
the print-uri flag determines if the classes URI is printed to the string"
  (let ((count 0)
	(table (slot-value ont 'uri2entity)))
    (loop
       for k being the hash-keys in table using (hash-value v) 
       ;;for i from 1 to 10
       do
	 (cond 
	   (prefix
	    (when (and (all-matches (format nil "~a" k) (str+ "(?i)" prefix) 0)
		       (all-matches (format nil "~a" v) "CLASS" 0))
	      (when print-uri (pprint k))
	      (incf count)))
	   (t
	    (when (all-matches (format nil "~a" v) "CLASS" 0)
	      (when print-uri (pprint k))
	      (incf count)))))
    ;;(print-db count)
    ;;return count
    count))
