;; global variables for uri aliases
(def-uri-alias "female_dental_patient" !obo:OHD_0000049)
(def-uri-alias "male_dental_patient" !obo:OHD_0000054)
(def-uri-alias "inheres_in" !obo:BFO_0000052)
(def-uri-alias "realizes" !obo:BFO_0000055)
(def-uri-alias "patient_ID" !obo:OHD_0000014)
(def-uri-alias "birth_date" !obo:OHD_0000050)


;; the global variables are used to generate unique iri's
(defparameter *iri* nil)
(defparameter *iri-count* nil)

;; ensure that sql libary is loadd
(add-to-classpath "/Applications/SQLAnywhere12/System/java/sajdbc4.jar")

(defun get-dental-patient-ont (&key iri ont-iri ohd-iri-string)
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(url nil)
	(ohd-ontology nil)
	(count 0))
    
    ;; set default base and ontology iri's 
    (when (null iri) (setf iri "http://purl.obolibrary.org/obo/ohd/individuals/"))
    (when (null ont-iri) 
      (setf ont-iri "http://purl.obolibrary.org/obo/ohd/dev/patterson-partial.owl"))
    
    ;; set default iri string for the location of ohd ontology
    ;; for now this is my repository file
    (when (null ohd-iri-string)
      (setf ohd-iri-string 
	    "/Users/williamduncan/Repositories/ohd-ontology/src/ontology/penn-ub-ohsu-prototype/ohd.owl"))

    ;; set global variables 
    (setf *iri* iri)
    (setf ohd-ontology (load-ontology ohd-iri-string))
    (setf *iri-count*  (get-iri-count ohd-ontology "individuals/OHD_"))
    
    ;; set up connection string and query. Put password in ~/.pattersondbpw
    (setf url (concatenate 'string 
			   "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
			   (with-open-file (f "~/.pattersondbpw") (read-line f))))

    ;; get query string for amalgam restorations
    (setf query (get-dental-patients-query))

    (with-ontology ont (:collecting t :base iri :ontology-iri ont-iri)
	((unwind-protect
	      (progn
		;; connect to db and get data
		(setf connection (#"getConnection" 'java.sql.DriverManager url))
		(setf statement (#"createStatement" connection))
		(setf results (#"executeQuery" statement query))
	   	
		;; import the ohd ontology
		(as `(imports ,(make-uri "http://purl.obolibrary.org/obo/ohd/dev/ohd.owl")))
		
		;; declare data properties
		(as `(declaration (data-property !occurrence_date)))
		(as `(declaration (data-property !patient_ID)))
		(as `(declaration (data-property !first_name)))
		(as `(declaration (data-property !last_name)))
		(as `(declaration (data-property !birth_date)))
		(as `(declaration (data-property !patient_sex)))

		    
		(loop while (#"next" results) do
		     (as (get-dental-patient-axioms
			  (#"getString" results "patient_id")
			  (#"getString" results "first_name")
			  (#"getString" results "last_name")
			  (#"getString" results "birth_date")
			  (#"getString" results "sex")
			  ))
		     (incf count)))
	   

	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement))))

      ;; return the ontology
      (values ont count))))


(defun get-dental-patient-axioms (patient-id first-name last-name birth-date sex)
  "Returns a list of axioms about a patient that is identified by patient-id."

  (let ((axioms nil)
	(patient-uri nil))

    ;; create instance/indiviual  patient, note this dependent on the patients sex
    ;; if sex is not present; we will skip the record
    (when (or (equalp sex "F") (equalp sex "M"))
      ;; create uri; individual patient
      (setf patient-uri (get-iri))
      (push `(declaration (named-individual ,patient-uri)) axioms) 

      
      ;; declare patient to be an instance of Female or Male 
      (cond
	((equalp sex "F")
	 (push `(class-assertion !female_dental_patient ,patient-uri) axioms))
	(t
	 (push `(class-assertion !male_dental_patient ,patient-uri) axioms)))
	 
      ;; add data property 'patient id' to patient
      ;; note: the patient id is encoded
      (push `(data-property-assertion !patient_ID
				      ,patient-uri ,(encode patient-id)) axioms)

      ;; add data property about patient's birth date
      (push `(data-property-assertion !birth_date ,patient-uri ,birth-date) axioms)

    ;; add label annotation about patient
    (push `(annotation-assertion !rdfs:label 
				 ,patient-uri 
				 ,(str+ first-name " " last-name)) axioms)

    ;; add axioms about dental patient role
    ;; note: append puts lists together and doesn't put items in list (like push)
    (setf axioms (append (get-dental-patient-role-axioms patient-uri) axioms)))

    ;; return axioms
    axioms))

(defun get-dental-patient-role-axioms (patient-uri)
  "Returns a list of axioms about a dental patient's role."
  (let ((axioms nil)
	(patient-role-uri)
	(patient-string-uri nil))

    ;; create uri
    (setf patient-role-uri (get-iri))

    ;; create a string representation of the uri, this is used in the label
    (setf patient-string-uri (format nil "~a" patient-uri))
    
    ;; create instance of patient role; patient role is an instance of !obi:'patient role'
    (push `(declaration (named-individual ,patient-role-uri)) axioms)
    (push `(class-assertion !patient_role ,patient-role-uri) axioms) 

    ;; 'patient role' inheres in patient
    (push `(object-property-assertion !inheres_in
				      ,patient-role-uri ,patient-uri) axioms)

    ;; add label annotation about patient role
    (push `(annotation-assertion !rdfs:label 
				 ,patient-role-uri 
				 ,(str+ "'patient role' for patient " 
					 patient-string-uri)) axioms)
    ;; return axioms
    axioms))

(defun get-dental-patient-axioms-using-label-source
    (patient-id first-name last-name birth-date sex)
  "Returns a list of axioms about a patient that is identified by patient-id."
  (let ((axioms nil)
	(patient-uri nil))

    ;; create instance/indiviual  patient, note this dependent on the patients sex
    ;; if sex is not present; we will skip the record
    (when (or (equalp sex "F") (equalp sex "M"))
      ;; create uri; individual patient
      (setf patient-uri (get-iri))
      (push `(declaration (named-individual ,patient-uri)) axioms) 

      
      ;; declare patient to be an instance of Female or Male 
      (cond
	((equalp sex "F")
	 (push `(class-assertion !'female dental patient'@ohd ,patient-uri) axioms))
	(t
	 (push `(class-assertion !'male dental patient'@ohd ,patient-uri) axioms)))
	 
      ;; add data property 'patient id' to patient
      ;; note: the patient id is encoded
      (push `(data-property-assertion !'patient ID'@ohd 
				      ,patient-uri ,(encode patient-id)) axioms)

      ;; add data propert about patient's birth date
      (push `(data-property-assertion !'birth_date'@ohd ,patient-uri ,birth-date) axioms)

    ;; add label annotation about patient
    (push `(annotation-assertion !rdfs:label 
				 ,patient-uri 
				 ,(str+ first-name " " last-name)) axioms)

    ;; add axioms about dental patient role
    ;; note: append puts lists together and doesn't put items in list (like push)
    (setf axioms 
	  (append (get-dental-patient-role-axioms-using-label-source patient-uri) axioms)))

    ;; return axioms
    axioms))

(defun get-dental-patient-role-axioms-using-label-source (patient-uri)
  "Returns a list of axioms about a dental patient's role."
  (let ((axioms nil)
	(patient-role-uri)
	(patient-string-uri nil))

    ;; create uri
    (setf patient-role-uri (get-iri))

    ;; create a string representation of the uri, this is used in the label
    (setf patient-string-uri (format nil "~a" patient-uri))
    
    ;; create instance of patient role; patient role is an instance of !obi:'patient role'
    (push `(declaration (named-individual ,patient-role-uri)) axioms)
    (push `(class-assertion !'patient role'@ohd ,patient-role-uri) axioms) 

    ;; 'patient role' inheres in patient
    (push `(object-property-assertion !'inheres in'@ohd
				      ,patient-role-uri ,patient-uri) axioms)

    ;; add label annotation about patient role
    (push `(annotation-assertion !rdfs:label 
				 ,patient-role-uri 
				 ,(str+ "'patient role' for patient " 
					 patient-string-uri)) axioms)
    ;; return axioms
    axioms))

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

(defun get-iri-count (ontology sequence-prefix)
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

(defun get-dental-patients-query ()
"
SET rowcount 0

SELECT
  *
FROM
  PPM.patient
WHERE  LENGTH(birth_date) > 0
ORDER BY patient_id")

(defun encode (string)
  "Returns and md5 checksum of the string argument.
Note: The .pattersondbpw file is required to run this procedure."

  (let ((it nil)
	(salt nil))
    
    ;; get salt value
    (setf salt (with-open-file (f "~/.pattersondbpw") (read-line f)))
    (setf it (new 'com.hp.hpl.jena.shared.uuid.MD5 
		  (format nil "~a~a" salt string)))
    (#"processString" it)
    (#"getStringDigest" it)))


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