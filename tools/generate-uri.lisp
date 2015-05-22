(defun get-patient-uri-sparql (patient-id dental-patients-ontology)
  (let ((uri nil))
    ;; use sparql 
    (setf uri
	  (sparql `(:select (?a) ()
		    (?a !patient_ID ,patient-id))
		  :kb dental-patients-ontology
		  :use-reasoner :pellet
		  :values t))

    ;; the uri is returned as a list in a list; i.e., ((!ohd:0000123))
    ;; so, get the inner most elment
    (setf uri (car (car uri)))

    ;; return uri
    uri))

(defun get-iri ()
  "Returns a unique iri using 'make-uri'."
  (make-uri (get-iri-string)))
  

(defun get-iri-string ()
  "Returns a unique string that is used make a unique iri."
 (let ((iri-string nil))
   ;; build iri; note cdt uri is zero padded length 7: "~7,'0d"
   (setf iri-string (str+ "OHD_" (format nil "~7,'0d" *iri-count*)))
   
   ;; increment iri count
   (incf *iri-count*)

   ;; return new iri string
   (str+ *iri* iri-string)))

(defun get-iri-num (ontology sequence-prefix)
  "Returns the sequence number that can be used to generate unique iri's.
Note: This number is associated with some prefix; e.g., OHD_."
  (let ((count nil))
    ;; find largest sequence number
    (setf count (get-largest-uri-sequence ontology sequence-prefix))
    
    ;; if count is nil, then none where found
    ;; otherwise, add 1 to count
    (cond
      ((not count) (setf count 1))
      (t (incf count)))

    ;; return the count
    count))

(defun get-largest-uri-sequence (ontology sequence-prefix)
  "Returns the largest sequence number pertaining to the uri's used in an ontology.  
Ontology uri's in the obo library typically end with a prefix folowed by an underscore '_' and 7 digit (zero padded) number.  For example, the continuant class in BFO has the uri 'http://purl.obo.library.org/obo/BFO_0000002'.  The sequence number, then, is the last 7 digits of the uri.  This procecure returns the largest numbered numbered squence relative to some prefix.  For instance, if an ontology contained the uri's 'BFO_0000005' and 'BFO_0000012', (get-largest-sequence-in-uribfo \"BFO_\" would return the integer 12. Note that the underscore '_' was included in the sequence-prefix.  When a greatest sequence is not found, nil is returned."
 
  (let ((functional-syntax nil)
	(matches nil)
	(pattern nil)
	(int-list nil)
	(largest-sequence nil))
    ;; steps 
    ;; 1. convert the ontology to owl syntax
    ;; 2. do regular expression search for sequence-prefix pattern
    ;; 3. strip off the last 7 digits of each matched item into a list of integers
    ;; 4. sort the list of integers, and store the largest sequence
    ;; 5. return the largest sequence

    ;; 1. convert the ontology to owl syntax
    (setf functional-syntax (to-owl-syntax ontology :functional))
    
    ;; 2. do regular expression search for sequence-prefix pattern + 7 digits
    (setf pattern (format nil "(~a\\d{7})" sequence-prefix))
    (setf matches (all-matches functional-syntax pattern 1))

    ;; 3. strip off the last 7 digits of each uri into a list of integers
    ;; note: first check that records where returned
    (when matches
      (loop for item in matches do
	   ;; each item in the matches is itsef an one-elment list
	   ;; so, get this element
	   (setf item (car item))

           ;; parse out the integer portion of uri
	   ;; e.g., OHD_0000123 -> "OHD_0000123"-> "0000123" -> 123
	   ;; note: this requries converting the itme to a string
	   (setf item (format nil "~a" item))
	   
	   ;; verify that the length is greater that 7
	   (when (> (length item) 7)
	     (setf item (subseq item (- (length item) 7)))
	     (setf item (parse-integer item))

	     ;; push uri integer on to list
	     (push item int-list)))

      ;; 4. sort the list of integers, and store the largest sequence
      ;; verify that list exists
      (when (listp int-list)
	(setf int-list (sort int-list #'>))
	(setf largest-sequence (car int-list))))

    ;; 5. return the largest sequence
    largest-sequence))

;; this version doesn't quite work as I intended.
;; it is difficult to retrieve all uri's for individuals, subclasses, annotation, etc.
;; but I'm keeping the code around b/c it might prove useful
;; (defun get-largest-uri-sequence (ontology sequence-prefix)
;;   "Returns the largest sequence number pertaining to the uri's used in an ontology.  
;; Ontology uri's in the obo library typically end with a prefix folowed by an underscore '_' and 7 digit (zero padded) number.  For example, the continuant class in BFO has the uri 'http://purl.obo.library.org/obo/BFO_0000002'.  The sequence number, then, is the last 7 digits of the uri.  This procecure returns the largest numbered numbered squence relative to some prefix.  For instance, if an ontology contained the uri's 'BFO_0000005' and 'BFO_0000012', (get-largest-sequence-in-uribfo \"BFO\" would return the integer 12."
 
;;   (let ((uri-list nil)
;; 	(int-list nil)
;; 	(largest-sequence nil))
;;     ;; steps 
;;     ;; 1. do a sparql query for all the uri's that match the sequence prefix
;;     ;; 2. strip off the last 7 digits of each uri into a list of integers
;;     ;; 3. sort the list of integers, and store the largest sequence
;;     ;; 4. return the largest sequence

;;     ;; 1. do a sparql query for all the uri's that match the sequence prefix
;;     (setf uri-list (sparql 
;; 		    `(:select (?a) () 
;; 			      (?a !rdf:type !owl:Thing)
;; 			      (:filter (regex (str ?a) ,sequence-prefix "i")))
;; 		    :kb ontology
;; 		    :use-reasoner :pellet
;; 		    :values t))
    
;;     ;; 2. strip off the last 7 digits of each uri into a list of integers
;;     ;; note: first check that records where returned
;;     (when uri-list
;;       (loop for item in uri-list do
;; 	   ;; each item in the uri-list is itsef an one-elment list
;; 	   ;; so, get this element
;; 	   (setf item (car item))

;;            ;; parse out the integer portion of uri
;; 	   ;; for example OHD_0000123 -> "OHD_0000123"-> "0000123" -> 123
;; 	   ;; note: this requries converting the itme to a string
;; 	   (setf item (format nil "~a" item))
;; 	   (setf item (subseq item (- (length item) 7)))
;; 	   (setf item (parse-integer item))

;; 	   ;; push uri integer on to list
;; 	   (push item int-list))

;;       ;; 3. sort the list of integers, and store the largest sequence
;;       (setf int-list (sort int-list #'>))
;;       (setf largest-sequence (car int-list)))

;;     ;; 4. return the largest sequence
;;     largest-sequence))