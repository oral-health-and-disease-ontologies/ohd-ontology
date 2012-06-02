;;****************************************************************
;; Instructions for being able to connect to Eaglesoft database
;;
;; Ensure that the r21 system file is loaded.
;; For details see comment at top of r21-utilities.lisp

;;****************************************************************
;; Database preparation: 
;; When get-eaglesoft-fillings-ont is ran, the program verifies that the action_codes and
;; patient_history tables exist.  This is done by calling prepare-eaglesoft-db.  However, this
;; only tests that these tables exist in the user's database. If these table need to be 
;; recreated, the call get-eaglesoft-fillings-ont with :force-create-table key set to t.

(defun get-eaglesoft-fillings-ont (&key force-create-table)
  "Returns an ontology of the fillings contained in the Eaglesoft database.  They force-create-table key is used to force the program to recreate the actions_codes and patient_history tables."
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
    (setf query (get-eaglesoft-fillings-query))

    (with-ontology ont (:collecting t
			:base *eaglesoft-individual-fillings-iri-base*
			:ontology-iri  *eaglesoft-fillings-ontology-iri*)
	((unwind-protect
	      (progn
		;; connect to db and get data
		(setf connection (#"getConnection" 'java.sql.DriverManager url))
		(setf statement (#"createStatement" connection))
		(setf results (#"executeQuery" statement query))
	   	
		;; import the ohd ontology and dental patient ontology
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
		     (as (get-eaglesoft-filling-axioms 
		     	  (#"getString" results "patient_id")
		     	  occurrence-date
		     	  (#"getString" results "tooth_data")
		     	  (#"getString" results "surface")
		     	  (#"getString" results "ada_code")
		     	  count))
		     (incf count)))

	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement))))

      ;; return the ontology
      (values ont count))))

(defun get-eaglesoft-filling-axioms 
    (patient-id occurrence-date tooth-data surface ada-code record-count)
  (let ((axioms nil)
	(cdt-class-uri nil)
	(cdt-uri nil)
	(material-name nil)
	(material-uri nil)
	(ohd-material-uri nil)
	(ohd-restoration-uri nil)
	(restoration-name nil)
	(restoration-uri nil)
	(patient-uri nil)
	(tooth-uri nil)
	(tooth-type-uri nil)
	(tooth-role-uri nil)
	(tooth-name nil)
	(teeth-list nil))

    ;; alanr - parse the list since that's what's in our table 
    ;; billd - since we are using the tooth_data array, this procedure is skipped
    ;;(setf teeth-list (parse-teeth-list tooth-data)) ; commented out by billd


    ;; tooth_data
    ;; get list of teeth in tooth_data array
    (setf teeth-list (get-eaglesoft-teeth-list tooth-data))

    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 
	 ;; get uri of patient
	 (setf patient-uri (get-eaglesoft-dental-patient-iri patient-id))
	 
         ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-with-number t))
	 (setf tooth-uri (get-eaglesoft-tooth-iri patient-id tooth-type-uri))
	 
	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 (push `(class-assertion ,tooth-type-uri ,tooth-uri) axioms)	     
	 
	 ;; add annotation about tooth
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ tooth-name
					     " of patient " patient-id)) axioms)
	 
         ;; declare instance of !ohd:'tooth to be filled role'
	 (setf tooth-role-uri (get-eaglesoft-tooth-to-be-filled-role-iri 
			       patient-id tooth-name record-count))
		
	 (push `(declaration (named-individual ,tooth-role-uri)) axioms)
	 (push `(class-assertion !'tooth to be filled role'@ohd ,tooth-role-uri) axioms)

	 ;; add annotation about 'tooth to be filled role'
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-role-uri
				      ,(str+ "tooth to be filled role for " 
					     tooth-name " of patient " 
					     patient-id)) axioms)

         ;; declare instance of material (i.e.,  amalgam/resin/gold) used in tooth
	 (setf ohd-material-uri (get-eaglesoft-material-uri ada-code))
	 (setf material-uri (get-eaglesoft-filling-material-iri 
			     patient-id tooth-name ohd-material-uri record-count))

	 (push `(declaration (named-individual ,material-uri)) axioms)
	 (push `(class-assertion ,ohd-material-uri ,material-uri) axioms)

	 ;; add annotation about this instance of material
	 (setf material-name (get-eaglesoft-material-name ada-code))
	 (push `(annotation-assertion !rdfs:label 
				      ,material-uri
				      ,(str+ material-name " placed in " tooth-name
					     " of patient " patient-id)) axioms)

         ;; declare instance of restoration 
	 (setf ohd-restoration-uri (get-eaglesoft-restoration-uri ada-code))
	 (setf restoration-uri (get-eaglesoft-filling-restoration-iri 
				patient-id tooth-name ohd-restoration-uri record-count))
		     		
	 (push `(declaration (named-individual ,restoration-uri)) axioms)
	 (push `(class-assertion ,ohd-restoration-uri ,restoration-uri) axioms)

	 ;; add annotation about this restoration procedure
	 (setf restoration-name (get-eaglesoft-restoraton-name ada-code))
	 (push `(annotation-assertion !rdfs:label 
				      ,restoration-uri
				      ,(str+ restoration-name 
					     " restoration procedure on " 
					     tooth-name " in patient " 
					     patient-id)) axioms)

	 ;; add data property !ohd:'occurrence date' to restoration
	 (push `(data-property-assertion !'occurrence date'@ohd
					 ,restoration-uri 
					 (:literal ,occurrence-date !xsd:date)) axioms)

         ;; declare instance of cdt code as identified by the ada code that is about the procedure
	 (setf cdt-class-uri (get-cdt-class-iri ada-code))
	 (setf cdt-uri (get-eaglesoft-cdt-instance-iri patient-id ada-code cdt-class-uri record-count))
	 (push `(declaration (named-individual ,cdt-uri)) axioms)
	 (push `(class-assertion ,cdt-class-uri ,cdt-uri) axioms)
	 
	 ;; add annotion about cdt code
	 (push `(annotation-assertion !rdfs:label
				      ,cdt-uri
				      ,(str+ "billing code " ada-code " for " restoration-name 
					     " filling restoration on " tooth-name 
					     " of patient " patient-id)) axioms)

	  ;;;; relate instances ;;;;

	 ;; toot is located in the patient
	 (push `(object-property-assertion !'is part of'@ohd
					   ,tooth-uri ,patient-uri) axioms)

         ;; 'tooth to be filled role' inheres in tooth
	 (push `(object-property-assertion !'inheres in'@ohd
					   ,tooth-role-uri ,tooth-uri) axioms)

         ;; 'filling restoration' realizes 'tooth to be filled role'
	 (push `(object-property-assertion !'realizes'@ohd
					   ,restoration-uri ,tooth-role-uri) axioms)

         ;; 'filling restoration' has particpant tooth
	 (push `(object-property-assertion !'has participant'@ohd
					   ,restoration-uri ,tooth-uri) axioms)
	 
         ;; 'filling restoration' has particpant restoration material
	 (push `(object-property-assertion !'has participant'@ohd
					   ,restoration-uri ,material-uri) axioms)

         ;; 'filling restoration' has particpant patient
	 (push `(object-property-assertion !'has participant'@ohd
					   ,restoration-uri ,patient-uri) axioms)

	 ;; restoration material is located in the tooth
	 (push `(object-property-assertion !'is located in'@ohd
					   ,material-uri ,tooth-uri) axioms)

         ;; cdt code instance is about the restoration process
	 (push `(object-property-assertion !'is about'@ohd
					   ,cdt-uri ,restoration-uri) axioms)
	 ) ;; end loop
    
    ;;(pprint axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-tooth-to-be-filled-role-iri (patient-id tooth-name instance-count)
  "Returns an iri for a 'tooth to be filled role' that is generated by the patient id, the name of the type of the tooth, and a count variable that used differientiate tooth role intances that have the same patient-id/tooth-name but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type !'tooth to be filled role'@ohd
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-filling-restoration-iri (patient-id tooth-name
					      restoration-type-iri instance-count)
  "Returns an iri for a restoration filling procedure that is generated by the patient id, the name of the type of tooth, the type of restoration's iri, and a count variable that used differientiate restoration procedure intances that have the same patient-id/restoration-type-iri but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-fillings-iri-base*
				     :class-type restoration-type-iri
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-filling-material-iri (patient-id tooth-name
					      material-type-iri instance-count)
  "Returns an iri for the material used in restoration filling procedure that is generated by the patient id, the name of the type of tooth, the type of restoration's material, and a count variable that used differientiate restoration procedure intances that have the same patient-id/material-type-iri but are numerically distinct."
  (let ((uri nil))
    (setf uri 
	  (get-unique-individual-iri patient-id 
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-fillings-iri-base*
				     :class-type material-type-iri
				     :args `(,tooth-name ,instance-count "eaglesoft")))
    ;; return uri
    uri))
  
(defun get-eaglesoft-material-name (ada-code)
  "Returns the name the material used in a filling/restoration based on ada code."
  (let ((material-name nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *eaglesoft-amalgam-code-list* :test 'equal) 
       (setf material-name "amalgam"))
      ((member ada-code *eaglesoft-resin-code-list* :test 'equal) 
       (setf material-name "resin"))
      ((member ada-code *eaglesoft-gold-code-list* :test 'equal)
       (setf material-name "gold"))
      (t (setf material-name "other material")))

    ;; return material name
    material-name))

(defun get-eaglesoft-restoraton-name (ada-code)
  "Returns the name the type of restoration based on ada code."
  (let ((restoration-name nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *eaglesoft-amalgam-code-list* :test 'equalp)
       (setf restoration-name "amalgam"))
      ((member ada-code *eaglesoft-resin-code-list* :test 'equalp)
       (setf restoration-name "resin"))
      ((member ada-code *eaglesoft-gold-code-list* :test 'equalp)
       (setf restoration-name "gold"))
      (t (setf restoration-name "other")))

    ;; return material name
    restoration-name))

(defun get-eaglesoft-restoration-uri (ada-code)
  "Returns the uri of the restoration type based on ada code."
  (let ((restoration-uri nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *eaglesoft-amalgam-code-list* :test 'equalp) 
       (setf restoration-uri !'amalgam filling restoration'@ohd))
      ((member ada-code *eaglesoft-resin-code-list* :test 'equalp)  
       (setf restoration-uri !'resin filling restoration'@ohd))
      ((member ada-code *eaglesoft-gold-code-list* :test 'equalp)
       (setf restoration-uri !'gold filling restoration'@ohd))
      (t (setf restoration-uri !'filling restoration'@ohd)))

    ;; return restoration
    restoration-uri))

(defun get-eaglesoft-material-uri (ada-code)
  "Returns the uri of the material used in a filling/restoration based on ada code."
  (let ((material-uri nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *eaglesoft-amalgam-code-list* :test 'equalp)
       (setf material-uri !'amalgam'@ohd))
      ((member ada-code *eaglesoft-resin-code-list* :test 'equalp)
       (setf material-uri !'resin'@ohd))
      ((member ada-code *eaglesoft-gold-code-list* :test 'equalp)
       (setf material-uri !'gold'@ohd))
      (t 
       (setf material-uri !'restoration material'@ohd)))
    
    ;; return material uri
    material-uri))

(defun get-eaglesoft-fillings-query ()
"
/* 
This queries the eaglesoft database for all ADA codes that indicate a filling procedure
has been performed.
Note: Code D2390/02390 is a crown. Not a filling.  Also, porcelain fillings are not included.
Also, this program filters out primary/baby teeth.  
To filter out primary teeth from the query you add the line:   AND isnumeric(tooth) <> 0
*/
SET rowcount 0

SELECT
  -- TOP 10 -- for testing
   *
FROM
  patient_history
WHERE
  ada_code IN (
              'D2140', -- Restorative: Amalgam
              'D2150',
              'D2160',
              'D2161',
              'D2330', -- Restorative: Resin
              'D2332',
              'D2335',
               --D2390,
              'D2391',
              'D2392',
              'D2393',
              'D2394',
              'D2410', -- Restorative: Gold Foil
              'D2420',
              'D2430',

              -- Older codes begin with a '0'
              '02140', -- Restorative: Amalgam
              '02150',
              '02160',
              '02161',
              '02330', -- Restorative: Resin
              '02332',
              '02335',
              --'02390'
              '02391',
              '02392',
              '02393',
              '02394',
              '02410', -- Restorative: Gold Foil
              '02420',
              '02430' )

/**** This was used for testing
AND 
  patient_id IN ('930',
                 '444',
                 '790',
                 '529',
                 '112',
                 '330',
                 '1690',
                 '327',
                 '1680',
                 '304') 
******/

AND tooth_data IS NOT NULL
AND LENGTH(tooth_data) > 31
AND surface_detail IS NOT NULL
--AND table_name = 'transactions' -- This was used for testing
ORDER BY
  patient_id
"
)
