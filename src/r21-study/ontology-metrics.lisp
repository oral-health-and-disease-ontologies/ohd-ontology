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
    (print-db count)))
