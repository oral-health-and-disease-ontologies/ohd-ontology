;;****************************************************************
;; Instructions for being able to connect to database
;;
;; needs "/Applications/SQLAnywhere12/System/lib32"
;;  added to DYLD_LIBRARY_PATH so it can find libdbjdbc12.jnilib and dependencies.
;; for now add (setenv "DYLD_LIBRARY_PATH" (concatenate 'string (getenv "DYLD_LIBRARY_PATH") 
;; ":/Applications/SQLAnywhere12/System/lib32")) to your .emacs
;; (#"load" 'System "/Applications/SQLAnywhere12/System/lib32/libdbjdbc12.jnilib")  
;; fails there is no easy way to update DYLD_LIBRARY_PATH within a running java instance. POS.

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-fillings-ont is ran, the program verifies that the action_codes and
;; patient_history tables exist.  This is done by calling prepare-eaglesoft-db.  However, this
;; only tests that these tables exist in the user's database. If these table need to be 
;; recreated, the call get-eaglesoft-fillings-ont with :force-create-table key set to t.

;; the global variables are used to generate unique iri's
(defparameter *eaglesoft-dental-patients-iri* nil)

;; global variable used for to salt the md5 checksum
(defparameter *eaglesoft-salt* nil)

;; ensure that sql libary is loaded
(add-to-classpath "/Applications/SQLAnywhere12/System/java/sajdbc4.jar")

(defun get-eaglesoft-dental-patients-ont (&key iri ont-iri)
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(url nil)
	(count 0))
    
    ;; set md5 salt
    (setf *eaglesoft-salt* (get-eaglesoft-salt))

    ;; set default base and ontology iri's 
    (when (null iri) (setf iri "http://purl.obolibrary.org/obo/ohd/individuals/"))
    (when (null ont-iri) 
      (setf ont-iri "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-dental-patients.owl"))
    
    ;; set global variables 
    (setf *eaglesoft-dental-patients-iri* iri)
    
    ;; set up connection string and query.
    (setf url (get-eaglesoft-database-url))

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
		(as `(declaration (data-property !'occurence date'@ohd)))
		(as `(declaration (data-property !'patient ID'@ohd)))
		(as `(declaration (data-property !'birth_date'@ohd)))
		    
		(loop while (#"next" results) do
		     (as (get-eaglesoft-dental-patient-axioms
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


(defun get-eaglesoft-dental-patient-axioms (patient-id birth-date sex)
  "Returns a list of axioms about a patient that is identified by patient-id."
  (let ((axioms nil)
	(patient-uri nil))

    ;; create instance/indiviual  patient, note this dependent on the patients sex
    ;; if sex is not present; we will skip the record
    (when (or (equalp sex "F") (equalp sex "M"))
      ;; create uri for individual patient
      (setf patient-uri 
	    (get-unique-individual-iri patient-id 
				       :salt *eaglesoft-salt*
				       :iri-base *eaglesoft-dental-patients-iri*
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
      (push `(data-property-assertion !'patient ID'@ohd 
				      ,patient-uri 
				      ,patient-id) axioms)

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
	  (append (get-eaglesoft-dental-patient-role-axioms patient-uri patient-id) axioms)))

    ;;(pprint axioms)
    ;; return axioms
    axioms))

(defun get-eaglesoft-dental-patient-role-axioms (patient-uri patient-id)
  "Returns a list of axioms about a dental patient's role."
  (let ((axioms nil)
	(patient-role-uri))
    ;; create uri
    (setf patient-role-uri 
	  (get-unique-individual-iri patient-id
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-dental-patients-iri*
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


(defun get-dental-patients-query ()
"
SET rowcount 0

SELECT
  *
FROM
  PPM.patient
WHERE LENGTH(birth_date) > 0
ORDER BY patient_id")



