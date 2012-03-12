;; the global variables are used to generate unique iri's
(defparameter *iri* nil)

;; global variable used for to salt the md5 checksum
(defparameter *salt* nil)

;; ensure that sql libary is loadd
(add-to-classpath "/Applications/SQLAnywhere12/System/java/sajdbc4.jar")

(defun get-dental-patient-ont (&key iri ont-iri)
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(url nil)
	(count 0))
    
    ;; set md5 salt
    (setf *salt* (get-eaglesoft-salt))

    ;; set default base and ontology iri's 
    (when (null iri) (setf iri "http://purl.obolibrary.org/obo/ohd/"))
    (when (null ont-iri) 
      (setf ont-iri "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-dental-patients.owl"))
    
    ;; set global variables 
    (setf *iri* iri)
    
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
		(as `(declaration (data-property !birth_date)))
		(as `(declaration (data-property !patient_sex)))
		    
		(loop while (#"next" results) do
		     (as (get-dental-patient-axioms
			  (#"getString" results "patient_id")
			  (#"getString" results "birth_date")
			  (#"getString" results "sex")))
		     (incf count))) 
	   

	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement))))

      ;; return the ontology
      (values ont count))))


(defun get-dental-patient-axioms (patient-id birth-date sex)
  "Returns a list of axioms about a patient that is identified by patient-id."
  (let ((axioms nil)
	(patient-uri nil))

    ;; create instance/indiviual  patient, note this dependent on the patients sex
    ;; if sex is not present; we will skip the record
    (when (or (equalp sex "F") (equalp sex "M"))
      ;; create uri for individual patient
      (setf patient-uri (get-unique-iri patient-id 
					:salt *salt*
					:iri-base *iri*
					:class-type !'dental patient'@ohd 			
					:args "eaglesoft"))

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
				      ,patient-uri 
				      ,(encode patient-id :salt *salt*)) axioms)

      ;; add data propert about patient's birth date
      (push `(data-property-assertion !'birth_date'@ohd ,patient-uri 
				      (:literal ,birth-date !xsd:date)) axioms)

    ;; add label annotation about patient
    (push `(annotation-assertion !rdfs:label 
				 ,patient-uri 
				 ,(str+ "patient " patient-id)) axioms)

    ;; add axioms about dental patient role
    ;; note: append puts lists together and doesn't put items in list (like push)
    (setf axioms 
	  (append (get-dental-patient-role-axioms patient-uri patient-id) axioms)))

    ;;(pprint axioms)
    ;; return axioms
    axioms))

(defun get-dental-patient-role-axioms (patient-uri patient-id)
  "Returns a list of axioms about a dental patient's role."
  (let ((axioms nil)
	(patient-role-uri))
    ;; create uri
    (setf patient-role-uri (get-unique-iri patient-id
					   :salt *salt*
					   :iri-base *iri*
					   :class-type !'patient role'@ohd
					   :args "eaglesoft"))

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
					 patient-id)) axioms)
    ;; return axioms
    axioms))

(defun get-unique-iri(string &key salt class-type iri-base args)
  "Returns a unique iri by doing a md5 checksum on the string parameter. An optional md5 salt value is specified by the salt key value (i.e., :salt salt). The class-type argument (i.e., :class-type class-type) concatentates the class type to the string.  This parameter is highly suggested, since it helps guarntee that ire will be unique.  The iri-base (i.e., :iri-base iri-base) is prepended to the iri.  For example, (get-unique-iri \"test\" :iri-base \"http://test.com/\" will prepend \"http://test.com/\" to the md5 checksum of \"test\".  The args parmameter is used to specify an other information you wish to concatenate to the string paramenter.  Args can be either a single value or list; e.g., (get-unique-iri \"test\" :args \"foo\") (get-unique-iri \"test\" :args '(\"foo\" \"bar\")."
  ;; check that string param is a string
  (if (not (stringp string)) 
      (setf string (format nil "~a" string)))

  ;; prepend class type to string
  (when class-type
    (setf class-type (remove-leading-! class-type))
    (setf string (format nil "~a~a" class-type string)))
  
  ;; concatenate extra arguments to string
  (when args
    (cond 
      ((listp args)
       (loop for item in args do
	    (setf item (remove-leading-! item))
	    (setf string (format nil "~a~a" string item))))
      (t (setf string (format nil "~a~a" string args)))))

  ;; encode string
  (setf string (encode string :salt salt))
  (when iri-base
    (setf string (format nil "~a~a" iri-base string)))
  (make-uri string))

(defun remove-leading-! (iri)
  "Removes the leading '!' from a iri and returns the iri as a string.
If no leading '!' is present, the iri is simply returned as string."
  (setf iri (format nil "~a" iri))
  (when (equal "!" (subseq iri 0 1)) (setf iri (subseq iri 1)))
  ;; return iri
  iri)

(defun get-dental-patients-query ()
"
SET rowcount 0

SELECT
  *
FROM
  PPM.patient
WHERE LENGTH(birth_date) > 0
ORDER BY patient_id")

(defun get-eaglesoft-salt ()
  "Returns the salt to be used with the md5 checksum.  
Note: The .pattersondbpw file is required to run this procedure."
  (with-open-file (f "~/.pattersondbpw") (read-line f)))
    

(defun encode (string &key salt)
  "Returns and md5 checksum of the string argument.  When the salt argument is present, it used in the computing of the md5 check sum."
  (let ((it nil))
    ;; check for salt
    (when salt
      (setf string (format nil "~a~a" salt string)))
    
    (setf it (new 'com.hp.hpl.jena.shared.uuid.MD5 string))
    
    (#"processString" it)
    (#"getStringDigest" it)))

(defun str-right (string num)
	   (let ((end nil)
		 (start nil))
	     (setf end (length string))
	     (setf start (- end num))
	     (when (>= start 0)
	       (setf string (subseq string start end)))))

(defun str-left (string num)
  (when (<= num (length string))
    (setf string (subseq string 0 num))))

(defun str+ (&rest values)
  "A shortcut for concatenting a list of strings. Code found at:
http://stackoverflow.com/questions/5457346/lisp-function-to-concatenate-a-list-of-strings
Parameters:
  values:  The string to be concatenated.
Usage:
  (str+ \"string1\"   \"string1\" \"string 3\" ...)
  (str+ s1 s2 s3 ...)"

  ;; the following make use of the format functions
  ;;(format nil "~{~a~}" values)
  ;;; use (format nil "~{~a^ ~}" values) to concatenate with spaces
  ;; this may be the simplist to understand...
  (apply #'concatenate 'string values))