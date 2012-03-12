;; global variables for uri aliases
(def-uri-alias "female_dental_patient" !obo:OHD_0000049)
(def-uri-alias "male_dental_patient" !obo:OHD_0000054)
(def-uri-alias "inheres_in" !obo:BFO_0000052)
(def-uri-alias "realizes" !obo:BFO_0000055)
(def-uri-alias "patient_ID" !obo:OHD_0000014)
(def-uri-alias "birth_date" !obo:OHD_0000050)


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
			  (#"getString" results "sex")
			  ))
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
	(patient-uri nil)
	(unique-uri-string nil))

    ;; create instance/indiviual  patient, note this dependent on the patients sex
    ;; if sex is not present; we will skip the record
    (when (or (equalp sex "F") (equalp sex "M"))
      ;; create uinque string to used for generating patient uri
      ;; this is composed of the "eaglesoft" + class type + patient id
      (setf unique-uri-string 
	    (concatenate 'string
			 "eaglesoft"
			 (subseq (format nil "~a" !'dental patient'@ohd) 1)
			 patient-id))

      ;; create uri for individual patient
      (setf patient-uri (get-unique-iri unique-uri-string :iri-base *iri*))
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
	(patient-role-uri)
	(unique-uri-string nil))

    ;; create uinque string to used for generating patient uri
    ;; this is composed of the "eaglesoft" + class type + patient id
    ;; note: the leading "!" is removed from the uri
    (setf unique-uri-string 
	  (concatenate 'string
		       "eaglesoft"
		       (subseq (format nil "~a" !'patient role'@ohd) 1)
		       patient-id))
		       

    ;; create uri
    (setf patient-role-uri (get-unique-iri unique-uri-string :iri-base *iri*))

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

(defun get-unique-iri(string-uri &key iri-base)
  (setf string-uri (encode string-uri :salt *salt*))
  (when iri-base
    (setf string-uri (str+ iri-base string-uri)))
  (make-uri string-uri))

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
