;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-missing-teeth-findings-ont is ran, the program verifies that the 
;; action_codes and patient_history tables exist.  This is done by calling 
;; prepare-eaglesoft-db.  However, this only tests that these tables exist in the 
;; user's database. If these table need to be recreated, the call 
;; get-eaglesoft-fillings-ont with :force-create-table key set to t.

(defun get-eaglesoft-missing-teeth-findings-ont (&key force-create-table)
  "Returns an ontology of the missing teeth findings contained in the Eaglesoft database.  They force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(occurrence-date nil)
	(url nil)
	(count 0))

    ;; set up connection string and query.
    (setf url (get-eaglesoft-database-url))

    ;; verify that the eaglesoft db has the action_codes and patient_history tables
    (prepare-eaglesoft-db url :force-create-table force-create-table)

    ;; get query string for restorations
    (setf query (get-eaglesoft-missing-teeth-findings-query))

    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-missing-teeth-findings-iri-base*
			:ontology-iri *eaglesoft-missing-teeth-findings-ontology-iri*)
	((unwind-protect
	      (progn
		;; connect to db and get data
		(setf connection (#"getConnection" 'java.sql.DriverManager url))
		(setf statement (#"createStatement" connection))
		(setf results (#"executeQuery" statement query))
	   	
		;; import the ohd ontology
		(as `(imports ,(make-uri *ohd-ontology-iri*)))
		(as `(imports ,(make-uri *eaglesoft-dental-patients-ontology-iri*)))

		;; get axioms for declaring annotation, object, and data properties used for ohd
		(as (get-ohd-declaration-axioms))
		
		(loop while (#"next" results) do
		     ;; determine the occurrence date
		     (setf occurrence-date
			   (get-eaglesoft-occurrence-date 
			    (#"getString" results "table_name")
			    (#"getString" results "date_entered")
			    (#"getString" results "date_completed")
			    (#"getString" results "tran_date")))

		     ;; generate axioms
		     (as (get-eaglesoft-missing-tooth-finding-axioms 
		       	  (#"getString" results "patient_id")
		     	  occurrence-date
		     	  (#"getString" results "tooth_data")
			  count))
		     	  		     
		     (incf count)))

	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-missing-tooth-finding-axioms 
    (patient-id occurrence-date tooth-data record-count)
  (let ((axioms nil)
	(patient-uri nil)
	(finding-uri nil)
	(finding-type-uri nil)
	(tooth-name nil)
	(tooth-num nil)
	;;(tooth-type-uri nil)
	(dentition-uri nil)
	(dentition-type-uri nil)
	(teeth-list nil))

    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-eaglesoft-teeth-list tooth-data))

    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 
	 ;; get uri of patient
	 (setf patient-uri
	       (get-eaglesoft-dental-patient-iri patient-id))
	 
         ;; get info about the type of missing tooth
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-name t))
	 ;;(setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-num (format nil "~a" tooth)) ; converts tooth number to string
	 
	 ;; get iri associated with the type of missing tooth finding
	 ;; note: tooth-num is the string representation of the tooth number
	 ;; this can also be done using:
	 ;;(make-uri-from-label-source :OHD "missing tooth finding" nil)
	 ;; (make-uri-from-label-source :OHD 
	 ;;       (str+ "missing tooth " tooth-num " finding") nil)
	 (setf finding-type-uri 
	       (gethash (str+ "missing tooth " tooth-num " finding") *ohd-label-source*))

         ;; declare instance of !ohd:'missing tooth finding'
	 (setf finding-uri
	       (get-eaglesoft-missing-tooth-finding-iri patient-id tooth-name record-count))
	 (setf axioms
	       (append (get-ohd-instance-axioms finding-uri finding-type-uri) axioms))

         ;; add annotation about missing tooth finding
	 (push `(annotation-assertion !rdfs:label 
				      ,finding-uri
				      ,(str+ "missing tooth " tooth-num " finding "
					     "for patient " patient-id )) axioms)
         
         ;; get iri for secondary dentition
	 (setf dentition-uri
	       (get-eaglesoft-secondary-dentition-iri patient-id))
	 
         ;; make dentition instance of 'Secondary dentition'
	 (setf dentition-type-uri !'Secondary dentition'@ohd)
	 (setf axioms
	       (append (get-ohd-instance-axioms dentition-uri dentition-type-uri) axioms))

	 (push `(annotation-assertion 
		 !rdfs:label ,dentition-uri
		 ,(str+ "secondary dentition of patient " patient-id)) axioms)
	 

         ;; instance of secondary dentition is part of patient
	 (push `(object-property-assertion !'is part of'@ohd ,dentition-uri ,patient-uri) axioms)

         ;; instance of missing tooth finding 'is about' the dentition instance
	 (push `(object-property-assertion !'is about'@ohd ,finding-uri ,dentition-uri) axioms)

         ;; add data property !ohd:'occurrence date' of the missing tooth finding
	 (push `(data-property-assertion !'occurrence date'@ohd
					 ,finding-uri
					 (:literal ,occurrence-date !xsd:date)) axioms)
	 ) ;; end loop
        ;;(pprint axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-missing-tooth-finding-iri (patient-id tooth-name instance-count)
  "Returns an iri for a 'missing tooth finding' that is generated by the patient id, the name of the type of the tooth, and a count variable that differentiates multipe missing teeth finds that are about the same tooth."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'missing tooth finding'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-secondary-dentition-iri (patient-id)
  "Returns an iri for a 'secondary dentition' that is generated by the patient id."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'Secondary dentition'@ohd
				     :args `("eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-missing-teeth-findings-query ()
"
/*
Returns records that indicate a tooth has been found to be missing.
I.e., Records that have an action code '1'.
16,750 records returned.
Note:
- The query does not filter out primary (baby) teeth.
- The some multiple enteries for the same missing tooth.
*/
SELECT
  -- top 1 -- used for testing
  *
FROM
  patient_history
WHERE
  action_code = '1'
AND LENGTH(tooth_data) > 31
AND description IN ('Missing/Extracted tooth',
                    'Missing Tooth',
                    'Missing tooth, more than a year')
ORDER BY
  patient_id
"
)
