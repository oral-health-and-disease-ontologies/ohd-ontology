;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-surgical-extractions-ont is ran, the program verifies that the 
;; action_codes and patient_history tables exist.  This is done by calling 
;; prepare-eaglesoft-db.  However, this only tests that these tables exist in the 
;; user's database. If these table need to be recreated, the call 
;; get-eaglesoft-fillings-ont with :force-create-table key set to t.

(defun get-eaglesoft-surgical-extractions-ont (&key force-create-table)
  "Returns an ontology of the surgical extractions contained in the Eaglesoft database.  They force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."
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
    (setf query (get-eaglesoft-surgical-extractions-query))

    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-surgical-extractions-iri-base* 
			:ontology-iri *eaglesoft-surgical-extractions-ontology-iri*)
	((unwind-protect
	      (progn
		;; connect to db and get data
		(setf connection (#"getConnection" 'java.sql.DriverManager url))
		(setf statement (#"createStatement" connection))
		(setf results (#"executeQuery" statement query))
	   	
		;; import the ohd ontology
		(as `(imports ,(make-uri *ohd-ontology-iri*)))
		(as `(imports ,(make-uri *eaglesoft-dental-patients-ontology-iri*)))

		;; declare data properties
		(as `(declaration (data-property !'occurence date'@ohd)))
		(as `(declaration (data-property !'patient ID'@ohd)))
		    
		(loop while (#"next" results) do
		     ;; determine this occurrence date
		     (setf occurrence-date
			   (get-eaglesoft-occurrence-date 
			    (#"getString" results "table_name")
			    (#"getString" results "date_entered")
			    (#"getString" results "date_completed")
			    (#"getString" results "tran_date")))
		     
		     ;; get axioms
		     (as (get-eaglesoft-surgical-extraction-axioms 
		     	  (#"getString" results "patient_id")
		     	  occurrence-date
		     	  (#"getString" results "tooth_data")
		     	  (#"getString" results "ada_code")
		     	  count))
		     (incf count)))

	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-surgical-extraction-axioms 
    (patient-id occurrence-date tooth-data ada-code record-count)
  (let ((axioms nil)
	(patient-uri nil)
	(extraction-role-uri nil)
	(extraction-procedure-uri nil)
	(procedure-type-uri nil)
	(tooth-name nil)
	(tooth-uri nil)
	(tooth-type-uri nil)
	(teeth-list nil))

    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-eaglesoft-teeth-list tooth-data))

    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 
	 ;; get uri of patient
	 (setf patient-uri
	       (get-eaglesoft-dental-patient-iri patient-id))
	 
         ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-name t))
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-uri (get-eaglesoft-tooth-iri patient-id tooth-type-uri))

	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 (push `(class-assertion ,tooth-type-uri ,tooth-uri) axioms)	     
	 
        ;; add annotation about tooth
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ tooth-name
					     " of patient " patient-id)) axioms)
	 
       ;; declare instance of !ohd:'tooth to be extracted role'
	 (setf extraction-role-uri
	       (get-eaglesoft-tooth-to-be-extracted-role-iri patient-id tooth record-count))
		
	 (push `(declaration (named-individual ,extraction-role-uri)) axioms)
	 (push `(class-assertion !'tooth to be extracted role'@ohd ,extraction-role-uri) axioms)

	 ;; add annotation about 'tooth to be extracted role'
	 (push `(annotation-assertion !rdfs:label 
				      ,extraction-role-uri
				      ,(str+ "tooth to be extracted role for " 
					     tooth-name " of patient " 
					     patient-id)) axioms)


         ;; declare instance of extraction procedure
	 (setf procedure-type-uri !'tooth extraction procedure'@ohd)
	 (setf extraction-procedure-uri
	       (get-eaglesoft-tooth-extraction-procedure-iri
		patient-id tooth-name procedure-type-uri record-count))
	 
	 (push `(declaration (named-individual ,extraction-procedure-uri)) axioms)
	 (push `(class-assertion !'tooth extraction procedure'@ohd 
				 ,extraction-procedure-uri) axioms)

	 ;; add annotation about this extraction procedure
	 (push `(annotation-assertion !rdfs:label 
				      ,extraction-procedure-uri
				      ,(str+ "tooth extraction procedure performed on tooth " 
					     tooth-name " in patient " 
					     patient-id)) axioms)

	 ;; add data property !ohd:'occurence date' to restoration
	 (push `(data-property-assertion !'occurence date'@ohd
					 ,extraction-procedure-uri
					 (:literal ,occurrence-date !xsd:date)) axioms)
	  ;;;; relate instances ;;;;
	 
         ;; 'tooth to be extracted role' inheres in tooth
	 (push `(object-property-assertion !'inheres in'@ohd
					   ,extraction-role-uri ,tooth-uri) axioms)

         ;; 'tooth extraction procedure' realizes 'tooth to be extracted role'
	 (push `(object-property-assertion !'realizes'@ohd
					   ,extraction-procedure-uri
					   ,extraction-role-uri) axioms)
	 
         ;; 'tooth extraction procedure' has particpant tooth
	 (push `(object-property-assertion !'has participant'@ohd
					   ,extraction-procedure-uri ,tooth-uri) axioms)

         ;;  'tooth extraction procedure' has particpant patient
	 (push `(object-property-assertion !'has participant'@ohd
					   ,extraction-procedure-uri ,patient-uri) axioms)
	 ) ;; end loop
    
    ;;(pprint axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-tooth-to-be-extracted-role-iri (patient-id tooth-name instance-count)
  "Returns an iri for a 'tooth to be extracted role' that is generated by the patient id, the name of the type of the tooth, and a count variable that used differientiate tooth role intances that have the same patient-id/tooth-name but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'tooth to be extracted role'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-tooth-extraction-procedure-iri (patient-id tooth-name
						     procedure-type-iri instance-count)
  "Returns an iri for a tooth extraction procedure identified is generated by the patient id, the name of the type of tooth, the type of procedure, and a count variable that used differientiate procedure intances that have the same patient-id/restoration-type-iri but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-surgical-extractions-iri-base*
				     :class-type procedure-type-iri
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-surgical-extractions-query ()
"
/*
Returns surgical extraction records.
Note: This has not been filtered for primary (baby) teeth.
1215 records returned
*/

SET rowcount 0 

SELECT
  *
FROM
  patient_history
WHERE
 length(tooth_data) > 31
AND (
    ada_code IN ('D7110',
                 'D7120',
                 'D7140',
                 'D7210')
  OR -- older codes begin with a '0'
    ada_code IN ('07110',
                 '07120',
                 '07140',
                 '07210'))
"
)
