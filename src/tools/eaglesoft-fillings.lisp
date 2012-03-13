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
;; In order to set up the Patterson datase, you must first run the queries 
;; continaned in these two files in the files dropbox:
;; 1. "R21 Work/Data/Queries For Extracting Data/create action codes table.txt"
;; 2. "R21 Work/Data/Queries For Extracting Data/create patient  history table.txt"
;; It is necessarry for you to run file #1 then file #2, since the patient_history table
;; references the action_codes table.
;; After the database has been set up, you will only need to run these quereies if the table
;; definitions change.

;; the global variables are used to generate unique iri's
(defparameter *iri* nil)

;; list of ada codes for amalgam, resin, and gold fillings/restorations
(defparameter *amalgam-code-list* 
  '("D2140" "D2150" "D2160" "D2161"))
(defparameter *resin-code-list* 
  '("D2330" "D2332" "D2335" "D2390" "D2391" "D2392" "D2393" "D2394"))
(defparameter *gold-code-list* 
  '("D2410" "D2420" "D2430"))

;; global variable used for to salt the md5 checksum
(defparameter *salt* nil)

;; for ease of use, set up some aliases to reference ohd classes
(def-uri-alias "fma_tooth" !obo:FMA_12516)
(def-uri-alias "dental_patient" !obo:OHD_0000012)
(def-uri-alias "patient_role" !obo:OBI_0000093)
(def-uri-alias "dental_finding" !obo:OHD_0000010)
(def-uri-alias "caries_finding" !obo:OHD_0000024)
(def-uri-alias "fractured_tooth_finding" !obo:OHD_0000030)
(def-uri-alias "missing_tooth_finding" !obo:OHD_0000026)
(def-uri-alias "dentition" !obo:OHD_0000027)
(def-uri-alias "caries" !obo:OHD_0000021)
(def-uri-alias "fractured_tooth" !obo:OHD_0000029)
(def-uri-alias "restoration_material" !obo:OHD_0000000)
(def-uri-alias "amalgam" !obo:OHD_0000001)
(def-uri-alias "gold" !obo:OHD_0000034)
(def-uri-alias "porcelain" !obo:OHD_0000035)
(def-uri-alias "resin" !obo:OHD_0000036)
(def-uri-alias "dental_exam" !obo:OHD_0000019)
(def-uri-alias "hard_tissue_exam" !obo:OHD_0000025)
(def-uri-alias "soft_tissue_exam" !obo:OHD_0000032)
(def-uri-alias "dental_visit" !obo:OHD_0000009)
(def-uri-alias "performing_a_dental_clinical_assement" !obo:OHD_0000011)
(def-uri-alias "procedure" !obo:OHD_0000002)
(def-uri-alias "endodontic_procedure" !obo:OHD_0000003)
(def-uri-alias "restorative_procedure" !obo:OHD_0000004)
(def-uri-alias "crown_restoration" !obo:OHD_0000033)
(def-uri-alias "filling_restoration" !obo:OHD_0000006)
(def-uri-alias "direct_restoration" !obo:OHD_0000037)
(def-uri-alias "amalgam_filling_restoration" !obo:OHD_0000041)
(def-uri-alias "resin_filling_restoration" !obo:OHD_0000042)
(def-uri-alias "indirect_restoration" !obo:OHD_0000038)
(def-uri-alias "gold_filling_restoration" !obo:OHD_0000039)
(def-uri-alias "procelain_filling_restoration" !obo:OHD_0000040)
(def-uri-alias "surgical_dental_procedure" !obo:OHD_0000044)
(def-uri-alias "surgical_procedure" !obo:OHD_0000005)
(def-uri-alias "tooth_to_be_restored_role" !obo:OHD_0000007)
(def-uri-alias "tooth_to_be_filled_role" !obo:OHD_0000008)
(def-uri-alias "has_participant" !obo:BFO_0000057)
(def-uri-alias "has_role" !obo:BFO_0000087)
(def-uri-alias "has_part" !<http://www.obofoundry.org/ro/ro.owl#has_part>)
(def-uri-alias "has_specified_output" !obi:OBI_0000299)
(def-uri-alias "inheres_in" !obo:BFO_0000052)
(def-uri-alias "is_about" !obo:IAO_0000136)
(def-uri-alias "is_part_of" !obo:BFO_0000050)
(def-uri-alias "is_located_in" !obo:BFO_0000082)
(def-uri-alias "realizes" !obo:BFO_0000055)
(def-uri-alias "occurrence_date" !obo:OHD_0000015)
(def-uri-alias "patient_ID" !obo:OHD_0000014)

(add-to-classpath "/Applications/SQLAnywhere12/System/java/sajdbc4.jar")

;; Assuming that FMA uses the same numbering system, here's how the vector is computed
;; (loop with alist 
;;    for (rel tooth) in (get FMA::|Secondary dentition| :slots)
;;    nwhen (eq rel fma::|member|)
;;    do 
;;     (let ((syns (remove-if-not (lambda(e) (eq (car e) fma::|Synonym|)) (get tooth :slots))))
;;        (loop for (rel syn) in syns
;; 	  for name = (second (find fma::|name| (get syn :slots) :key 'car))
;; 	  for number = (caar (all-matches name ".*?(\\d+).*" 1))
;; 	  when number do (push (cons (parse-integer number) tooth) alist)))
;;    finally (return (coerce 
;; 		    (mapcar (lambda(e) (cons (fma-uri e) (string e)))
;; 			    (mapcar 'cdr (sort alist '< :key 'car))) 'vector)))

(defun number-to-fma-tooth (number &key return-tooth-uri return-tooth-name)
  (let ((teeth nil)
	(tooth nil))
    (setf teeth
	  #((!obo:FMA_55696 . "Right upper third secondary molar tooth") 
	    (!obo:FMA_55697 . "Right upper second secondary molar tooth")
	    (!obo:FMA_55698 . "Right upper first secondary molar tooth")
	    (!obo:FMA_55688 . "Right upper second secondary premolar tooth")
	    (!obo:FMA_55689 . "Right upper first secondary premolar tooth")
	    (!obo:FMA_55798 . "Right upper secondary canine tooth")
	    (!obo:FMA_55680 . "Right upper lateral secondary incisor tooth")
	    (!obo:FMA_55681 . "Right upper central secondary incisor tooth")
	    (!obo:FMA_55682 . "Left upper central secondary incisor tooth")
	    (!obo:FMA_55683 . "Left upper lateral secondary incisor tooth")
	    (!obo:FMA_55799 . "Left upper secondary canine tooth")
	    (!obo:FMA_55690 . "Left upper first secondary premolar tooth")
	    (!obo:FMA_55691 . "Left upper second secondary premolar tooth")
	    (!obo:FMA_55699 . "Left upper first secondary molar tooth")
	    (!obo:FMA_55700 . "Left upper second secondary molar tooth")
	    (!obo:FMA_55701 . "Left upper third secondary molar tooth")
	    (!obo:FMA_55702 . "Left lower third secondary molar tooth")
	    (!obo:FMA_55703 . "Left lower second secondary molar tooth")
	    (!obo:FMA_55704 . "Left lower first secondary molar tooth")
	    (!obo:FMA_55692 . "Left lower second secondary premolar tooth")
	    (!obo:FMA_55693 . "Left lower first secondary premolar tooth")
	    (!obo:FMA_55687 . "Left lower secondary canine tooth")
	    (!obo:FMA_57141 . "Left lower lateral secondary incisor tooth")
	    (!obo:FMA_57143 . "Left lower central secondary incisor tooth")
	    (!obo:FMA_57142 . "Right lower central secondary incisor tooth")
	    (!obo:FMA_57140 . "Right lower lateral secondary incisor tooth")
	    (!obo:FMA_55686 . "Right lower secondary canine tooth")
	    (!obo:FMA_55694 . "Right lower first secondary premolar tooth")
	    (!obo:FMA_55695 . "Right lower second secondary premolar tooth")
	    (!obo:FMA_55705 . "Right lower first secondary molar tooth")
	    (!obo:FMA_55706 . "Right lower second secondary molar tooth")
	    (!obo:FMA_55707 . "Right lower third secondary molar tooth")))

    (cond
      (return-tooth-uri (setf tooth (car (aref teeth (1- number)))))
      (return-tooth-name (setf tooth (cdr (aref teeth (1- number)))))
      (t (setf tooth (aref teeth (1- number)))))
    ;; return tooth uri/name
    tooth))
    

(defun get-filling-ont (&key iri ont-iri)
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(url nil)
	(count 0))

    ;; set md5 salt
    (setf *salt* (get-eaglesoft-salt))
    
    ;; set default base and ontology iri's 
    (when (null iri) (setf iri "http://purl.obolibrary.org/obo/ohd/individuals/"))
    (when (null ont-iri) 
      (setf ont-iri "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-fillngs.owl"))
    
    ;; set global variables 
    (setf *iri* iri)

    ;; set up connection string and query. Put password in ~/.pattersondbpw
    (setf url (concatenate 'string 
			   "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
			   (with-open-file (f "~/.pattersondbpw") (read-line f))))

    ;; get query string for restorations
    (setf query (get-fillings-query))

    (with-ontology ont (:collecting t :base iri :ontology-iri ont-iri)
	((unwind-protect
	      (progn
		;; connect to db and get data
		(setf connection (#"getConnection" 'java.sql.DriverManager url))
		(setf statement (#"createStatement" connection))
		(setf results (#"executeQuery" statement query))
	   	
		;; import the ohd ontology
		(as `(imports ,(make-uri "http://purl.obolibrary.org/obo/ohd/dev/ohd.owl")))
		(as `(imports ,(make-uri "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-dental-patients.owl")))

		;; declare data properties
		(as `(declaration (data-property !'occurence date'@ohd)))
		(as `(declaration (data-property !'patient ID'@ohd)))
		    
		(loop while (#"next" results) do
		     (as (get-filling-axioms 
			  (#"getString" results "patient_id")
	 		  (#"getString" results "tran_date")
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

(defun get-filling-axioms (patient-id occurrence-date tooth-data surface ada-code record-count)
  (let ((axioms nil)
	(material-name nil)
	(material-uri nil)
	(obo-material-uri nil)
	(obo-restoration-uri nil)
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
    (setf teeth-list (get-teeth-list tooth-data))

    (loop for tooth in teeth-list do
         ;;;;  declare instances of participating entities ;;;;
	 
	 ;; get uri of patient
	 (setf patient-uri 
	       (get-unique-iri patient-id 
			       :salt *salt*
			       :iri-base *iri*
			       :class-type !'dental patient'@ohd 			
			       :args "eaglesoft"))
	 
         ;; declare tooth instance; for now each tooth will be and instance of !fma:tooth
	 (setf tooth-type-uri (number-to-fma-tooth tooth :return-tooth-uri t))
	 (setf tooth-uri 
	       (get-unique-iri patient-id
			       :salt *salt*
			       :iri-base *iri*
			       :class-type tooth-type-uri
			       :args `(,tooth "eaglesoft")))					 
	 (push `(declaration (named-individual ,tooth-uri)) axioms)
	 (push `(class-assertion ,tooth-type-uri ,tooth-uri) axioms)	     
	 
	 ;; add annotation about tooth
	 (setf tooth-name (number-to-fma-tooth tooth :return-tooth-name t))
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-uri
				      ,(str+ tooth-name
					     " of patient " patient-id)) axioms)
	 
         ;; declare instance of !ohd:'tooth to be filled role'
	 (setf tooth-role-uri
	       (get-unique-iri patient-id
			       :salt *salt*
			       :iri-base *iri*
			       :class-type !'tooth to be filled role'@ohd
			       :args `(,tooth ,record-count "eaglesoft")))
		
	 (push `(declaration (named-individual ,tooth-role-uri)) axioms)
	 (push `(class-assertion !'tooth to be filled role'@ohd ,tooth-role-uri) axioms)

	 ;; add annotation about 'tooth to be filled role'
	 (push `(annotation-assertion !rdfs:label 
				      ,tooth-role-uri
				      ,(str+ "tooth to be filled role for " 
					     tooth-name " of patient " 
					     patient-id)) axioms)

         ;; declare instance of material (i.e.,  amalgam/resin/gold) used in tooth
	 (setf obo-material-uri (get-obo-material-uri ada-code))
	 (setf material-uri 
	       (get-unique-iri patient-id
			       :salt *salt*
			       :iri-base *iri*
			       :class-type obo-material-uri
			       :args `(,tooth ,record-count "eaglesoft")))
	 (push `(declaration (named-individual ,material-uri)) axioms)
	 (push `(class-assertion ,obo-material-uri ,material-uri) axioms)

	 ;; add annotation about this instance of material
	 (setf material-name (get-material-name ada-code))
	 (push `(annotation-assertion !rdfs:label 
				      ,material-uri
				      ,(str+ material-name " placed in " tooth-name
					     " of patient " patient-id)) axioms)

         ;; declare instance of restoration 
	 (setf obo-restoration-uri (get-obo-restoration-uri ada-code))
	 (setf restoration-uri
	       (get-unique-iri patient-id
			       :salt *salt*
			       :iri-base *iri*
			       :class-type obo-restoration-uri
			       :args `(,tooth ,record-count "eaglesoft")))
		
	 (push `(declaration (named-individual ,restoration-uri)) axioms)
	 (push `(class-assertion ,obo-restoration-uri ,restoration-uri) axioms)

	 ;; add annotation about this restoration procedure
	 (setf restoration-name (get-restoraton-name ada-code))
	 (push `(annotation-assertion !rdfs:label 
				      ,restoration-uri
				      ,(str+ restoration-name 
					     " restoration procedure on tooth " 
					     tooth-name " in patient " 
					     patient-id)) axioms)

	 ;; add data property !ohd:'occurence date' to restoration
	 (push `(data-property-assertion !'occurence date'@ohd
					 ,restoration-uri 
					 (:literal ,occurrence-date !xsd:date)) axioms)

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

	 ;; restoration material is located in the tooth
	 (push `(object-property-assertion !'is located in'@ohd
					   ,material-uri ,tooth-uri) axioms)
	 ) ;; end loop
    
    ;;(pprint axioms)

    ;; return axioms
    axioms))
  
(defun get-material-name (ada-code)
  "Returns the name the material used in a filling/restoration based on ada code."
  (let ((material-name nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equal) (setf material-name "amalgam"))
      ((member ada-code *resin-code-list* :test 'equal) (setf material-name "resin"))
      ((member ada-code *gold-code-list* :test 'equal) (setf material-name "gold"))
      (t (setf material-name "other material")))

    ;; return material name
    material-name))

(defun get-restoraton-name (ada-code)
  "Returns the name the type of restoration based on ada code."
  (let ((restoration-name nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equalp) (setf restoration-name "amalgam"))
      ((member ada-code *resin-code-list* :test 'equalp) (setf restoration-name "resin"))
      ((member ada-code *gold-code-list* :test 'equalp) (setf restoration-name "gold"))
      (t (setf restoration-name "other")))

    ;; return material name
    restoration-name))

(defun get-obo-restoration-uri (ada-code)
  "Returns the uri of the restoration type based on ada code."
  (let ((obo-restoration-uri nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equalp) 
       (setf obo-restoration-uri !'amalgam filling restoration'@ohd))
      ((member ada-code *resin-code-list* :test 'equalp)  
       (setf obo-restoration-uri !'resin filling restoration'@ohd))
      ((member ada-code *gold-code-list* :test 'equalp)
       (setf restoration-uri !'gold filling restoration'@ohd))
      (t (setf obo-restoration-uri !'filling restoration'@ohd)))

    ;; return restoration
    obo-restoration-uri))

(defun get-obo-material-uri (ada-code)
  "Returns the uri of the material used in a filling/restoration based on ada code."
  (let ((obo-material-uri nil))
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equalp)
       (setf obo-material-uri !'amalgam'@ohd))
      ((member ada-code *resin-code-list* :test 'equalp)
       (setf obo-material-uri !'resin'@ohd))
      ((member ada-code *gold-code-list* :test 'equalp)
       (setf obo-material-uri !'gold'@ohd))
      (t 
       (setf obo-material-uri !'restoration material'@ohd)))
    
    ;; return obo material uri
    obo-material-uri))
		     
(defun get-teeth-list (tooth-data)
  "Reads the tooth_data array and returns a list of tooth numbers referenced in the tooth_data (i.e., field) array."
  (let ((teeth-list nil))

    ;; verify that there are at least 32 tooth items in tooth-data
    (when (>= (length tooth-data) 32)
      (loop 
	 for tooth from 0 to 31 do
	   ;; when a postion in tooth-data is marked 'Y' add to teeth list
	   (when (or (equal (char tooth-data tooth) #\Y) 
		     (equal (char tooth-data tooth) #\y))
	     (push (1+ tooth) teeth-list))))

    ;; return list of teeth
    teeth-list))

;; alanr
;; billd: this was intended to read data that has tooth ranges of the form "3-6".
;; however, we are reading the tooth_data array, which is processed using get-teeth-list
(defun parse-teeth-list (teeth-description)
  "Parses the tooth list format '-' is range, ',' separates, into a list of teeth numbers"
  (let ((teeth-list nil))

    (let ((pieces (split-at-char teeth-description #\,)))
      (loop for piece in pieces do
	   (cond ((find #\- piece) 
		  (destructuring-bind (start end) 
		      (mapcar 'parse-integer 
			      (car (all-matches piece "^(\\d+)-(\\d+)" 1 2)))
		    (loop for i from start to end do (push i teeth-list))))
		 ;; otherwise it is a single number
		 (t (push (parse-integer piece) teeth-list)))))
    ;; return list of teeth
    teeth-list))

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

(defun get-fillings-query ()
"
SET rowcount 0

SELECT 
  *
FROM
  patient_history
WHERE
  ada_code IN ('D2140', -- Restorative: Amalgam
              'D2150',
              'D2160',
              'D2161',
              'D2330', -- Restorative: Resin
              'D2332',
              'D2335',
              'D2390',
              'D2391',
              'D2392',
              'D2393',
              'D2394',
              'D2410', -- Restorative: Gold Foil
              'D2420',
              'D2430' )
/****
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
AND surface_detail IS NOT NULL
AND 
  table_name = 'transactions'
ORDER BY
  patient_id
"
)

(defun encode (string &key salt)
  "Returns and md5 checksum of the string argument.  When the salt argument is present, it used in the computing of the md5 check sum."
  (let ((it nil))
    ;; check for salt
    (when salt
      (setf string (format nil "~a~a" salt string)))
    
    (setf it (new 'com.hp.hpl.jena.shared.uuid.MD5 string))
    
    (#"processString" it)
    (#"getStringDigest" it)))

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


(defun create-table (table-name &optional force-create-table)
  (let ((url nil)
	(connection nil)
	(statement nil)
	(query nil)
	(success nil)
	(table-found))

    ;; check to see if the table is in the db
    (setf table-found (table-exists table-name))
    
    ;; create table if it does not exist or there is a force create flag
    (when (or (not table-found) force-create-table)    
      ;; create connection string
      (setf url (concatenate 
		 'string 
		 "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
		 (with-open-file (f "~/.pattersondbpw") (read-line f))))

      ;; create query string for creating table
      (cond
	((equal table-name "action_codes") 
	 (setf query (get-create-action-codes-query)))
	((equal table-name "patient_history")
	 (setf query (get-create-patient-history-query))))
      
      ;; verfiy that there is a query
      (when query
	(unwind-protect
	     (progn
	       ;; connect to db and execute query
	       (setf connection (#"getConnection" 'java.sql.DriverManager url))
	       (setf statement (#"createStatement" connection))
	       
	       ;; if the table is already in the db, delete all the rows
	       ;; this is to ensure that the created table will not have
	       ;; any duplicate rows
	       (when table-found (delete-table table-name))
	       
	       ;; create table
	       ;; on success 1 is returned; otherwise nil
	       (setf success (#"executeUpdate" statement query)))

	  ;; database cleanup
	  (and connection (#"close" connection))
	  (and statement (#"close" statement)))))

    ;; return success indicator
    success))


(defun delete-table (table-name)
  "Deletes all the rows in table-name."
  (let ((url nil)
	(connection nil)
	(statement nil)
	(query nil)
	(success nil))
    
  ;; create connection string
    (setf url (concatenate 
	       'string 
	       "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
	       (with-open-file (f "~/.pattersondbpw") (read-line f))))

    (setf query (str+ "DELETE " table-name))

    (unwind-protect
	 (progn
	   ;; connect to db and execute query
	   (setf connection (#"getConnection" 'java.sql.DriverManager url))
	   (setf statement (#"createStatement" connection))
	       
	   ;; create table
	   ;; on success 1 is returned; otherwise nil
	   (setf success (#"executeUpdate" statement query)))

      ;; database cleanup
      (and connection (#"close" connection))
      (and statement (#"close" statement)))

    ;; return success indicator
    success))

(defun get-create-action-codes-query ()
"
DROP TABLE IF EXISTS PattersonPM.PPM.action_codes; 
CREATE TABLE PattersonPM.PPM.action_codes (action_code_id int NOT NULL, description varchar(50) NOT NULL, PRIMARY KEY (action_code_id));
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (1, 'Missing Tooth'); 
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (2, 'Caries');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (3, 'Dental Caries');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (4, 'Recurring Caries');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (5, 'Abscess/Lesions');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (6, 'Open Contact');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (7, 'Non Functional Tooth');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (8, 'Fractured Restoration');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (9, 'Fractured Tooth');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (10,'Restoration With Poor Marginal Integrity');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (11,'Wear Facets');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (12,'Tori');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (13,'Malposition');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (14,'Erosion');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (15,'Extrusion');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (16,'Root Amputation');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (17,'Root Canal');
insert into PattersonPM.PPM.action_codes (action_code_id, description) values (18,'Impaction');")

(defun get-create-patient-history-query ()
"
/* This line is needed to force Sybase to return all rows when populating the patient_history table. */
SET rowcount 0

DROP TABLE IF EXISTS PattersonPM.PPM.patient_history

/* Create and define the column names for the patient_history table. */
CREATE TABLE
  PattersonPM.PPM.patient_history
  (
    /* Define field names of patient_history. */
    patient_id INT,
    table_name VARCHAR(20),
    date_completed VARCHAR(15) NULL,
    date_entered VARCHAR(15),
    tran_date VARCHAR(15) NULL,
    description VARCHAR(40) NULL,
    tooth VARCHAR(10) NULL,
    surface VARCHAR(10) NULL,
    action_code VARCHAR(5) NULL,
    action_code_description VARCHAR(50) NULL,
    service_code VARCHAR(5) NULL,
    ada_code VARCHAR(5) NULL,
    ada_code_description VARCHAR(50) NULL,
    tooth_data CHAR(55) NULL,
    surface_detail CHAR(23) NULL
  )


/*
Put values of union query into patient_history table.

A field with the value 'n/a' means that the table/view being queried does not have a corresponding
field.  This is necessary because when doing a union of multiple queries the number of columns
returned by each query must match.  Putting the 'n/a' in as a place holder ensures this.

Note: Many of the field are cast to type VARCHAR.  This is necessary in order to allow an 'n/a' to
be the value of a non-character data field (such as a date).
*/
INSERT
INTO
  patient_history
  (
    patient_id,
    table_name,
    date_completed,
    date_entered,
    tran_date,
    description,
    tooth,
    surface,
    action_code,
    action_code_description,
    service_code,
    ada_code,
    ada_code_description,
    tooth_data,
    surface_detail
  )
/*
The existing_services table is the table for all findings in ES that are, essentially
procedures completed by entities outside the current dental office.
*/

SELECT DISTINCT
  existing_services.patient_id,
  table_name = 'existing_services', -- table_name value
  /* date_completed is the date (usually reported by the patient) that something was done */
  CAST(existing_services.date_completed AS VARCHAR),
  /* date_entered is the date that the record was entered into the system */
  CAST(existing_services.date_entered AS VARCHAR),
  tran_date = 'n/a', -- tran_date is not recorded in the existing_services table
  existing_services.description,
  existing_services.tooth,
  existing_services.surface,
  action_code = 'n/a', -- action_code in not recorded in the existing_services table
  action_code_description = 'n/a', -- there is no action code description in the existing_services table
  existing_services.service_code,
  
  ada_code =
  (
    -- This subquery finds the ada_code (if any) associated with a service code.
    SELECT
      services.ada_code
    FROM
      services
    WHERE
      services.service_code = existing_services.service_code),
  
  ada_code_descriiption =
  (
    -- This subquery finds the ada code description (if any) associated with a service code
    SELECT
      services.description
    FROM
      services
    WHERE
      services.service_code = existing_services.service_code),
  
  tooth_data =
  (
    SELECT
      existing_services_extra.tooth_data
    FROM
      existing_services_extra
    WHERE
      existing_services.patient_id = existing_services_extra.patient_id
    AND existing_services.line_number = existing_services_extra.line_number),
  
  surface_detail =
  (
    SELECT
      surface_detail
    FROM
      existing_services_extra
    WHERE
      existing_services.patient_id = existing_services_extra.patient_id
    AND existing_services.line_number = existing_services_extra.line_number)
FROM
  existing_services
WHERE
  patient_id IN
  (
    SELECT
      patient_id
    FROM
      patient
    WHERE
      LENGTH(birth_date) > 0)

UNION ALL
/*
The patient_conditions table is the table for all conditions in ES that do not result from a
procedure, i.e. all things that happen one their own (fracture, caries, etc.)
*/
SELECT DISTINCT
  patient_conditions.patient_id,
  table_name = 'patient_conditions', -- table_name value
  date_completed = 'n/a', -- date_completed is not recorded in the patient_conditions table
  CAST(patient_conditions.date_entered AS VARCHAR), -- date_entered is the date that the record was entered into the system
  tran_date = 'n/a', -- tran_date is not recorded in the patient_conditions table
  patient_conditions.description,
  patient_conditions.tooth,
  /* The case statement, here, is used to return empty strings for null values. */
  surface =
  CASE
    WHEN patient_conditions.surface IS NULL
    THEN ''
    ELSE patient_conditions.surface
  END,
  
  --see 'EagleSoft queries_123011.xls' spreadsheet for a list of action codes
  CAST(patient_conditions.action_code AS VARCHAR),
  action_code_description =
  (
    -- This subquery finds the desciption associate with an action code.
    SELECT
      action_codes.description
    FROM
      action_codes
    WHERE
      action_codes.action_code_id = patient_conditions.action_code),
  
  -- service codes, ada codes, and ada code descriptions are not recorded in the patient_conditions table
  service_code = 'n/a',
  ada_code = 'n/a',
  ada_code_description = 'n/a',
  
  tooth_data =
  (
    SELECT
      tooth_data
    FROM
      patient_conditions_extra
    WHERE
      patient_conditions.counter_id = patient_conditions_extra.counter_id),
  
  surface_detail =
  (
    SELECT
      surface_detail
    FROM
      patient_conditions_extra
    WHERE
      patient_conditions.counter_id = patient_conditions_extra.counter_id)
FROM
  patient_conditions
WHERE
  patient_id IN
  (
    SELECT
      patient_id
    FROM
      patient
    WHERE
      LENGTH(birth_date) > 0)

UNION ALL
/*
The transactions view records transactions between the provider and the patient.
*/
SELECT DISTINCT
  transactions.patient_id,
  table_name = 'transactions', -- table_name value (although transactions is a viiew)
  date_completed = 'n/a', -- date_completed is not recorded in the transactons view
  date_entered = 'n/a', -- date_entered is not recorded in the transactons view
  CAST(transactions.tran_date AS VARCHAR),
  transactions.description,
  transactions.tooth,
  /* The case statement, here, is used to return empty strings for null values. */
  surface =
  CASE
    WHEN transactions.surface IS NULL
    THEN ''
    ELSE transactions.surface
  END,
  action_code = 'n/a', -- action_code in not recroded in the transactions view
  action_code_description = 'n/a', -- there are no action code descriptions in the transactions view
  transactions.service_code,
  
  ada_code =
  (
    -- This subquery finds the ada_code (if any) associated with a service code.
    SELECT
      services.ada_code
    FROM
      services
    WHERE
      services.service_code = transactions.service_code),
  
  ada_code_description =
  (
    -- This subquery finds the ada code description (if any) associated with a service code
    SELECT
      services.description
    FROM
      services
    WHERE
      services.service_code = transactions.service_code),
  
  tooth_data =
  (
    SELECT
      tooth_data
    FROM
      transactions_extra
    WHERE
      transactions.tran_num = transactions_extra.tran_num),
  
  surface_detail =
  (
    SELECT
      surface_detail
    FROM
      transactions_extra
    WHERE
      transactions.tran_num = transactions_extra.tran_num)
FROM
  transactions
WHERE
  type = 'S'
AND
  patient_id IN
  (
    SELECT
      patient_id
    FROM
      patient
    WHERE
      LENGTH(birth_date) > 0)
  
  /* To use an order by clause in a union query, you reference the column number. */
ORDER BY
  1, -- patient_id
  2, -- table_name
  3, -- date_completed
  4, -- date_entered
  5, -- tran_date
  7 -- tooth
")


(defun table-exists (table-name)
  "Returns t if table-name exists in db; nil otherwise."
  (let ((url nil)
	(connection nil)
	;;(meta nil)
	(statement nil)
	(results nil)
	(query nil)
	(answer nil))
  
    ;; create connection string
    (setf url (concatenate 
	       'string 
	       "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
	       (with-open-file (f "~/.pattersondbpw") (read-line f))))

    ;; create query string for finding table name in sysobjects
    (setf query (concatenate 
		 'string
		 "SELECT * FROM dbo.sysobjects "
		 "WHERE name = '" table-name "' AND type = 'U'"))

    (unwind-protect
	 (progn
	   ;; connect to db and get data
	   (setf connection (#"getConnection" 'java.sql.DriverManager url))
	   (setf statement (#"createStatement" connection))
	   (setf results (#"executeQuery" statement query))

	   ;; ** could not get this to work :(
	   ;;(setf meta (#"getMetaData" connection)) ;; col
	   ;;(setf results (#"getTables" meta nil nil "action_codes" "TABLE"))

	   (when (#"next" results) (setf answer t)))
      ;; database cleanup
      (and connection (#"close" connection))
      (and results (#"close" results))
      (and statement (#"close" statement)))))

;;;; a number of funcitons for testing various aspects of database functionality ;;;;
;;;; such as using stored procedures, creating tables, and testing existence of a table ;;;

(defun test-stored-produre ()
  (let ((url nil)
	(connection nil)
	(statement nil)
	(results nil)
	(query nil))

    (setf url (concatenate 
	       'string 
	       "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
	       (with-open-file (f "~/.pattersondbpw") (read-line f))))

    ;; get query string for amalgam restorations
    (setf query "{call uspGetPatientById(?)}")
    

    (unwind-protect
	 (progn
	   ;; connect to db and get data
	   (setf connection (#"getConnection" 'java.sql.DriverManager url))
	   (setf statement (#"prepareCall" connection query))
	   (#"setInt" statement 1 3000)
	   (setf results (#"executeQuery" statement))
	   
	   (#"next" results)
	   (pprint (#"getString" results "first_name"))
	   (pprint (#"getString" results "last_name")))

      ;; database cleanup
      (and connection (#"close" connection))
      (and results (#"close" results))
      (and statement (#"close" statement)))))

(defun test-create-table ()
  (let ((url nil)
	(connection nil)
	(statement nil)
	(success nil)
	(query nil))
  
    (setf url (concatenate 
	       'string 
	       "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
	       (with-open-file (f "~/.pattersondbpw") (read-line f))))

    (unwind-protect
	 (progn
	   ;; connect to db and get data
	   (setf connection (#"getConnection" 'java.sql.DriverManager url))
	   (setf statement (#"createStatement" connection))

	   (setf query 
		 (concatenate
		  'string 
		  "create table mytest (test varchar(20)); "
		  "insert into mytest (test) values('this is a test'); "
		  "insert into mytest (test) values('this is another test');"))
	   
	   ;; just using "execute" returns nil on both success and failure
	   ;;(#"execute" statement query)
	   
	   ;; "executeUpdate" returns 1 on successl; nil on failure
	   ;; note: there is not a result to return or close during db cleanup
	   (setf success (#"executeUpdate" statement query)))

      ;; database cleanup
      (and connection (#"close" connection))
      (and statement (#"close" statement)))
    
    ;; return success of query
    success
    ))

(defun test-table-exists (table-name)
  (let ((url nil)
	(connection nil)
	(meta nil)
	(statement nil)
	(results nil)
	(query nil)
	(answer nil)
	(javanull nil))
  
    ;; create connection string
    (setf url (concatenate 
	       'string 
	       "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
	       (with-open-file (f "~/.pattersondbpw") (read-line f))))

    (unwind-protect
	 (progn
	   
	   ;; create a null object to be used in with java calss
	   (setf javanull (make-immediate-object nil :ref))
	   
	   ;; connect to db and get data
	   (setf connection (#"getConnection" 'java.sql.DriverManager url))
	   (setf statement (#"createStatement" connection))

	   ;; note the use of javanull 
	   (setf meta (#"getMetaData" connection)) 
	   (setf results (#"getTables" meta javanull javanull "action_codes" "TABLE"))

	   (when (#"next" results) (setf answer t)))

      ;; database cleanup
      (and connection (#"close" connection))
      (and results (#"close" results))
     (and statement (#"close" statement)))))

