;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-endodontics-ont is ran, the program verifies that the 
;; action_codes and patient_history tables exist.  This is done by calling 
;; prepare-eaglesoft-db.  However, this only tests that these tables exist in the 
;; user's database. If these table need to be recreated, the call 
;; get-eaglesoft-fillings-ont with :force-create-table key set to t.

(defun get-eaglesoft-endodontics-ont (&key force-create-table)
  "Returns an ontology of the endodontic procedures contained in the Eaglesoft database.  They force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."
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
    (setf query (get-eaglesoft-endodontics-query))

    (with-ontology ont (:collecting t 
			:base *eaglesoft-individual-endodontics-iri-base*
			:ontology-iri *eaglesoft-endodontics-ontology-iri*)
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
		(as `(declaration (data-property !'occurrence date'@ohd)))
		(as `(declaration (data-property !'patient ID'@ohd)))
		
		;; declare object property relations
		(as `(declaration  (object-property !'is part of'@ohd)))
		(as `(declaration  (object-property !'inheres in'@ohd)))
		(as `(declaration  (object-property !'has participant'@ohd)))
		(as `(declaration  (object-property !'is located in'@ohd)))
		(as `(declaration  (object-property !'is about'@ohd)))

		(loop while (#"next" results) do
		     ;; determine this occurrence date
		     (setf occurrence-date
			   (get-eaglesoft-occurrence-date 
			    (#"getString" results "table_name")
			    (#"getString" results "date_entered")
			    (#"getString" results "date_completed")
			    (#"getString" results "tran_date")))
		     
		     ;; get axioms
		     (as (get-eaglesoft-endodontic-axioms 
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

(defun get-eaglesoft-endodontic-axioms 
    (patient-id occurrence-date tooth-data ada-code record-count)
  (let ((axioms nil)
	(cdt-class-uri nil)
	(cdt-uri nil)
	(patient-uri nil)
	(endodontic-role-uri nil)
	(endodontic-procedure-uri nil)
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
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-with-number t))
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-uri (get-eaglesoft-tooth-iri patient-id tooth-type-uri))

	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 (push `(class-assertion ,tooth-type-uri ,tooth-uri) axioms)	     
	 
        ;; add annotation about tooth
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ tooth-name
					     " of patient " patient-id)) axioms)
	 
       ;; declare instance of !ohd:'tooth to undergo endodontic procedure role'
	 (setf endodontic-role-uri
	       (get-eaglesoft-tooth-to-undergo-endodontic-procedure-role-iri
		patient-id tooth record-count))
		
	 (push `(declaration (named-individual ,endodontic-role-uri)) axioms)
	 (push `(class-assertion !'tooth to undergo endodontic procedure role'@ohd 
				 ,endodontic-role-uri) axioms)

	 ;; add annotation about 'tooth to undergo endodontic procedure role'
	 (push `(annotation-assertion !rdfs:label 
				      ,endodontic-role-uri
				      ,(str+ "tooth to undergo endodontic procedure role for " 
					     tooth-name " of patient " 
					     patient-id)) axioms)


         ;; declare instance of endodontic procedure
	 (setf endodontic-procedure-uri
	       (get-eaglesoft-endodontic-procedure-iri patient-id tooth-name record-count))
	 
	 (push `(declaration (named-individual ,endodontic-procedure-uri)) axioms)
	 (push `(class-assertion !'endodontic procedure'@ohd 
				 ,endodontic-procedure-uri) axioms)

	 ;; add annotation about this endodontic procedure
	 (push `(annotation-assertion !rdfs:label 
				      ,endodontic-procedure-uri
				      ,(str+ "endodontic procedure performed on " 
					     tooth-name " of patient " 
					     patient-id)) axioms)

	 ;; add data property !ohd:'occurrence date' to endodontic procedure
	 (push `(data-property-assertion !'occurrence date'@ohd
					 ,endodontic-procedure-uri
					 (:literal ,occurrence-date !xsd:date)) axioms)

         ;; declare instance of cdt code as identified by the ada code that is about the procedure
	 (setf cdt-class-uri (get-cdt-class-iri ada-code))
	 (setf cdt-uri (get-eaglesoft-cdt-instance-iri patient-id ada-code cdt-class-uri record-count))
	 (push `(declaration (named-individual ,cdt-uri)) axioms)
	 (push `(class-assertion ,cdt-class-uri ,cdt-uri) axioms)
	 
	 ;; add annotion about cdt code
	 (push `(annotation-assertion !rdfs:label
				      ,cdt-uri
				      ,(str+ "billing code " ada-code " for endodontic procedure on "
					     tooth-name " of patient " patient-id)) axioms)
	 

	  ;;;; relate instances ;;;;
	 
         ;; 'tooth to undergo endodontic procedure role' inheres in tooth
	 (push `(object-property-assertion !'inheres in'@ohd
					   ,endodontic-role-uri ,tooth-uri) axioms)

         ;; 'endodontic procedure' realizes 'tooth to undergo endodontic procedure role'
	 (push `(object-property-assertion !'realizes'@ohd
					   ,endodontic-procedure-uri
					   ,endodontic-role-uri) axioms)
	 
         ;; 'endodontic procedure' has particpant tooth
	 (push `(object-property-assertion !'has participant'@ohd
					   ,endodontic-procedure-uri ,tooth-uri) axioms)

         ;;  'endodontic procedure' has particpant patient
	 (push `(object-property-assertion !'has participant'@ohd
					   ,endodontic-procedure-uri ,patient-uri) axioms)

	  ;; cdt code instance is about the restoration process
	 (push `(object-property-assertion !'is about'@ohd
					   ,cdt-uri ,endodontic-procedure-uri) axioms)

	 ) ;; end loop
    
    ;;(pprint axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-tooth-to-undergo-endodontic-procedure-role-iri 
    (patient-id tooth-name instance-count)
  "Returns an iri for a 'tooth to undergo endodontic procedure role' that is generated by the patient id, the name of the type of the tooth, and a count variable that used differientiate tooth role intances that have the same patient-id/tooth-name but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'tooth to undergo endodontic procedure role'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-endodontic-procedure-iri (patient-id tooth-name instance-count)
  "Returns an iri for an endodontic procedure is generated by the patient id, the name of the type of tooth, and a count variable that used differientiate procedure intances that have the same patient-id/restoration-type-iri but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-endodontics-iri-base*
				     :class-type !'endodontic procedure'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-endodontics-query ()
"
/*
Returns endodontic procedure records.
Note: This has not been filtered for primary (baby) teeth.
1436 records returned
*/

SET rowcount 0

SELECT
  -- TOP 10 -- for testing
  *
FROM
  patient_history
WHERE
  (
    ada_code IN ('D3220',
                 'D3221',
                 'D3222',
                 'D3310',
                 'D3320',
                 'D3330',
                 'D3346',
                 'D3347',
                 'D3348',
                 'D3410',
                 'D3421',
                 'D3425',
                 'D3450',
                 'D3920' )
  OR
    -- Older codes begin with a '0'
    ada_code IN ('03220',
                 '03221',
                 '03222',
                 '03310',
                 '03320',
                 '03330',
                 '03346',
                 '03347',
                 '03348',
                 '03410',
                 '03421',
                 '03425',
                 '03450',
                 '03920' ) )
AND length(tooth_data) > 31
"
)
