;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-dental-patients-ont is ran, the program verifies that the action_codes and
;; patient_history tables exist.  This is done by calling prepare-eaglesoft-db.  However, this
;; only tests that these tables exist in the user's database. If these table need to be 
;; recreated, the call get-eaglesoft-fillings-ont with :force-create-table key set to t.

;; the global variables are used to generate unique iri's



(defun get-eaglesoft-dental-patients-ont (&key force-create-table)
  "Returns an ontology of the dental patients contained in the Eaglesoft database.  They force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(url nil)
	(count 0))
    
    ;; set up connection string and query.
    (setf url (get-eaglesoft-database-url))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db url :force-create-table force-create-table)

    ;; get query string for amalgam restorations
    (setf query (get-eaglesoft-dental-patients-query))

    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-dental-patients-iri-base*
			:ontology-iri *eaglesoft-dental-patients-ontology-iri*)
	((unwind-protect
	      (progn
		;; connect to db and get data
		(setf connection (#"getConnection" 'java.sql.DriverManager url))
		(setf statement (#"createStatement" connection))
		(setf results (#"executeQuery" statement query))
	   	
		;; import the ohd ontology
		(as `(imports ,(make-uri *ohd-ontology-iri*)))
		
		;; get axioms for declaring annotation, object, and data properties used for ohd
		(as (get-ohd-declaration-axioms))
		
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
  "Returns a list of axioms about a patient that is identified by patient-id, birth date, and sex."
  (let ((axioms nil)
	(patient-uri nil))

    ;; create instance/indiviual  patient, note this dependent on the patients sex
    ;; if sex is not present; we will skip the record
    (when (or (equalp sex "F") (equalp sex "M"))
      ;; create uri for individual patient
      (setf patient-uri (get-eaglesoft-dental-patient-iri patient-id))
      (push `(declaration (named-individual ,patient-uri)) axioms) 
      
      ;; declare patient to be an instance of Female or Male 
      ;; note: append puts lists together and doesn't put items in list (like push)
      (cond
	((equalp sex "F")
	 (setf axioms
	       (append (get-ohd-instance-axioms patient-uri !'female dental patient'@ohd)axioms)))
	(t	 
	 (setf axioms
	       (append (get-ohd-instance-axioms patient-uri !'male dental patient'@ohd)axioms))))

	 
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
    (setf patient-role-uri (get-eaglesoft-dental-patient-role-iri patient-id))

    ;; create instance of patient role; patient role is an instance of !obi:'patient role'
    ;; note: append puts lists together and doesn't put items in list (like push)
    (push `(declaration (named-individual ,patient-role-uri)) axioms)
    ;;(push `(class-assertion !'patient role'@ohd ,patient-role-uri) axioms) 
    (setf axioms 
	  (get-ohd-instance-axioms patient-role-uri !'patient role'@ohd))

    

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

(defun get-eaglesoft-dental-patient-role-iri (patient-id)
  "Returns a uri for a patient role that is generated by the patient id."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-dental-patients-iri-base*
				     :class-type !'patient role'@ohd 			
				     :args "eaglesoft"))
    ;; return uri
    uri))

(defun get-eaglesoft-dental-patients-query ()
"
SET rowcount 0

SELECT
  --TOP 100 -- for testing
  *
FROM
  PPM.patient
WHERE LENGTH(birth_date) > 0
ORDER BY patient_id")



