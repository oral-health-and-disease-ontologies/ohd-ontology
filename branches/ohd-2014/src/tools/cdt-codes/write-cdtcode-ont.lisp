(defparameter *parent-class-ht* nil
  "Global hash table that contains child/parent class information.")

(defparameter *meta-class-id-ht*
  "Global hash table to store the auto-generated id's of the cdt categories.")

;; *** use 'editor note' for this. 12-19-2011
;;(defparameter *cdt-definition-uri* nil
;;  "Global variable to hold the uri of the cdt_definition annotation.")

(defparameter *cdt-label-uri* nil
  "Global variable to hold the uri of the cdt_label annotation.")

(defparameter *cdt-version-uri* nil
  "Global variable to hold the uri of the cdt_version annotation.")

;;;; main driver function
(defun get-cdtcode-ont (xmlfile &key iri ont-iri import-iris
			(cdt-codes-as-classes t) ada-code-list)
  "This functions Builds an ontology of the CDT Codes.
Returns:
   An ontology of the CDT codes.
Parmaters: 
  iri: The IRI for entities in the ontology.
  xmlfile: The xml file containing the CDT codes
  ont-iri: an optional iri that can be assigned to the ontology itself.
           i.e., the ontology-iri parmater to with-ontology
  import-iris: An optional list of iris that the ontology of will import. By default the 
               the ontology will import IAO at <purl.obolibrary.org/obo/iao/dev/iao.owl>
  cdt-codes-as-classes: If true (by default), the cdt codes are repesented as classes.
          If false (nil), the cdt classes are named individuals.
  ada-code-list: A list of ADA codes that are to be included in the ontology.  This is
                 used for creating a paired down ontology that only has a subset of the
                 ADA codes, e.g., the store of a tooth codes used in the r21 study.
Usage:
  (get-cdtcode-ont \"http://test.com\" \"CDTCodes.xml\")
  (get-cdtcode-ont \"http://test.com\" \"CDTCodes.xml\" \"http://example.com/foo.owl\")

To create cdt-imports ontology for OHD use:
  (get-cdtcode-ont \"CDTCodes8.xml\" :ont-iri \"http://purl.obolibrary.org/obo/ohd/r21cdtcodes.owl\"
                                     :import-iris '(\"http://purl.obolibrary.org/obo/dev/iao-imports.owl\")
                                     :ada-code-list ada-code-list)"

  (let ((xmls-parse nil)
	(meta-class-name-list nil)
	(temp-list nil)
	(cdt-code nil)
	(cdt-list nil))
    
    ;; set default base iri
    (when (null iri) 
      (setf iri "http://purl.obolibrary.org/obo/"))

    ;; verify the iri ends with "/"
    (unless (equal (subseq iri (- (length iri) 1)) "/")
      (setf iri (str+ iri "/")))

    ;; set default ontology iri
    (when (null ont-iri)
      (setf ont-iri "http://purl.obolibrary.org/obo/iao/cdt.owl"))

    ;; build xmlse parse list
    (setf xmls-parse (get-cdtcode-xmls xmlfile))
    
    ;; get list of tags
    (setf meta-class-name-list (what-tags xmls-parse))

    ;; remove non-meta tags from meta-class-name-list
    ;; that is, remove "code" "label" "comment" tags
    (setf meta-class-name-list 
	  (reduce-meta-class-name-list meta-class-name-list
				       :xmls-parse xmls-parse :ada-code-list ada-code-list))

    ;; make id associated with meta classes
    (make-meta-class-id-hash-table meta-class-name-list)

    ;; create hash table for child/parent class info
    (make-parent-class-hash-table meta-class-name-list xmls-parse :ada-code-list ada-code-list)
    

    (with-ontology ont (:collecting t :base iri :ontology-iri ont-iri)
	( ;; check for imports
	 (cond
	   (import-iris
	    (loop for u in import-iris do
		 (as `(imports ,(make-uri u)))))
	   (t ;; the default is to import IAO
	    (as `(imports !<http://purl.obolibrary.org/obo/dev/iao.owl>))))

	 ;; add ontology annotations
	 (as `(annotation !dc:creator "American Dental Association; see www.ada.org/dentalcode"))
	 (as `(annotation !dc:contributor "Bill Duncan"))
	 (as `(annotation !dc:contributor "Alan Ruttenberg"))
	 (as `(declaration (annotation-property !dc:creator)))
	 (as `(declaration (annotation-property !dc:contributor)))

	 (as `(declaration (annotation-property !dc:identifier)))
	 
	 ;; make uri for cdt description annotation property
	 ;; this will be used to annotate cdt codes (and categories) with
	 ;; the descriptions provided by the ADA
	 ;; *** use 'edit note' for cdt definition
	 ;;(setf *cdt-definition-uri* (make-uri (str+ iri "cdt_definition"))) 

	 ;; make uri for cdt label annotation property
	 ;; this will be used to annotate cdt codes (and categories) with
	 ;; the labels provided by the ADA
	 (setf *cdt-label-uri* (make-uri (str+ iri "cdt_label")))
	 
	 
	 ;; add cdt_definition and cdt_label annotations as a sub-annotation 
	 ;;of IAO's "alternative term"  annotation; i.e., IAO_0000118
	 (as `(declaration (annotation-property ,*cdt-label-uri*)))
	 (as `(annotation-assertion !rdfs:comment ,*cdt-label-uri* 
				    "This annotation is used for the labels of the CDT codes provided by the American Dental Association."))
	 (as `(subannotationpropertyof ,*cdt-label-uri*
				       !<http://purl.obolibrary.org/obo/IAO_0000118>))
	 (as `(annotation-property-range ,*cdt-label-uri* !xsd:string))
	 (as `(annotation-property-domain ,*cdt-label-uri* 
					  ,(make-uri (str+ iri 
							   (get-meta-class-id "cdt-code")))))
	 #|
	 ;; *** use 'editor note' for the cdt definition. 12-19-2011
	 (as `(declaration (annotation-property ,*cdt-definition-uri*)))
	 (as `(annotation-assertion !rdfs:comment ,*cdt-definition-uri* 
				    "This annotation is used for the definitions (and descriptions) of the CDT codes provided by the American Dental Association."))
	 (as `(subannotationpropertyof ,*cdt-definition-uri*
				       !<http://purl.obolibrary.org/obo/IAO_0000118>))
	 (as `(annotation-property-range ,*cdt-definition-uri* !xsd:string))
	 (as `(annotation-property-domain ,*cdt-definition-uri* 
					  ,(make-uri (str+ iri 
							   (get-meta-class-id "cdt-code")))))
	 |#
	 
	 ;; add top level cdt code class (i.e., most general class) axioms
	 (as (get-top-level-cdt-class-axioms iri))

	 ;; add meta class axioms
	 (loop 
	    for item in meta-class-name-list do
	      (setf cdt-list (find-element-with-tag xmls-parse item))
	      
	    ;; create a temp list from meta class
	    ;; meta class has form:
	    ;; (CLASS-NAME NIL (CLASS-LABEL ...) (CDTCOMMENT ..) (.......)
	    ;; so, for efficiency I am getting the first four items
	      (setf temp-list 
		    (list (first cdt-list) (second cdt-list) 
			  (third cdt-list) (fourth cdt-list)))
	      ;;(print-db temp-list)
	      (as (get-meta-class-axioms iri temp-list)))

	 ;; add cdt code axoms
	 (setf cdt-list (find-elements-with-tag xmls-parse "CDTCode"))
	 
	 (loop 
	    for item in cdt-list do	    
	      ;; get cdt code from item in cdt-list
	      ;; the item has form  ("CDTCode" NIL ("Code" NIL "D1234") ("CDTLabel" nil "...") ("CDTComment" nil "...")
	      (setf cdt-code (third (third item)))
	      
	      ;; if an ada code list has been specified, check to see if code is in the list
	      ;; otherwise, create code
	      (cond
		(ada-code-list
		 (when (member cdt-code ada-code-list :test #'equalp)
		   ;; test whether cdt codes should be classes or individuals
		   (if cdt-codes-as-classes
		       (as (get-cdt-code-as-classes-axioms iri item)) ;; as class
		       (as (get-cdt-code-axioms iri item))))) ;; as individuals
		(t
		 ;; test whether cdt codes should be classes or individuals
		 (if cdt-codes-as-classes
		     (as (get-cdt-code-as-classes-axioms iri item)) ;; as class
		     (as (get-cdt-code-axioms iri item)))))) ;; as individuals
	 
	 ;; make meta-level classes disjoint
	 ;; not doing this -- 1/5/2013; billd
	 ;;(as (get-disjoint-class-axioms iri))

	 ;; add historical codes
	 (if cdt-codes-as-classes
	     (as (get-historical-code-class-axoms iri))
	     (as (get-historical-code-individual-axoms iri)))

	 ;; ********* add disjoint cdt class or different individual axioms
	 ;; test whether cdt codes should be a list of 
	 ;; disjoint classes or different individuals
	 (if cdt-codes-as-classes
	     (as (get-disjoint-cdt-classes-axioms iri))
	     (as (get-different-individuals-axioms iri))))
      
      ;; return the ontology
      ont)))

;;;;;;;;;;;;; Functions for getting axioms of meta and cdt classes ;;;;;;;;;;;;

(defun get-historical-code-class-axoms (iri)
  "Creates axioms for historical codes D7110 and D7120 that were part of CDT-3, but were replaced in CDT-4 with D7140."
  (let ((axioms nil)
	(uri-d7110 nil)
	(uri-d7120 nil)
	(uri-d7140 nil))
    
    ;; create uri's for D7110, D7120, D7140
    (setf uri-d7110 (make-uri (str+ iri "CDT_0007110")))
    (setf uri-d7120 (make-uri (str+ iri "CDT_0007120")))
    (setf uri-d7140 (make-uri (str+ iri "CDT_0007140")))
    
    ;; create classes D7110 and D7120
    (push `(declaration (class ,uri-d7110)) axioms)
    (push `(declaration (class ,uri-d7120)) axioms)

    ;; add annotations for D7110
    (push `(annotation-assertion !rdfs:label ,uri-d7110 "billing code D7110: single tooth") axioms)
    (push `(annotation-assertion ,*cdt-label-uri* ,uri-d7110 "single tooth") axioms)
    (push `(annotation-assertion !dc:identifier ,uri-d7110 "D7110") axioms)
    
    ;; add info about when code D7110 was retired annotation "retired from use of"
    ;; for info about xsd:datetimestamp see http://www.schemacentral.com/sc/xsd11/t-xsd_dateTimeStamp.html
    (push `(annotation-assertion 
	    !<http://purl.obolibrary.org/obo/IAO_0000604>
	    ,uri-d7110
	    (:literal "2003-01-01T00:00:00-5:00" !xsd:datetimestamp)) axioms)
    
    ;; add editor note about date time stamp format
    (push `(annotation-assertion 
	    !<http://purl.obolibrary.org/obo/IAO_0000116>
	    ,uri-d7110
	    "For information on the datetimestamp formate see: http://www.schemacentral.com/sc/xsd11/t-xsd_dateTimeStamp.html")
	  axioms)

    ;; add annotations for D7120
    (push `(annotation-assertion !rdfs:label ,uri-d7120 "billing code D7120: each additional tooth") axioms)
    (push `(annotation-assertion ,*cdt-label-uri* ,uri-d7120 "each additional tooth") axioms)
    (push `(annotation-assertion !dc:identifier ,uri-d7120 "D7120") axioms)

    ;; add info about when code D7120 was retired
    ;; for info about xsd:datetimestamp see http://www.schemacentral.com/sc/xsd11/t-xsd_dateTimeStamp.html
    (push `(annotation-assertion 
	    !<http://purl.obolibrary.org/obo/IAO_0000604>
	    ,uri-d7120
	    (:literal "2003-01-01T00:00:00-5:00" !xsd:datetimestamp)) axioms)

    ;; add editor note about date time stamp format
    (push `(annotation-assertion 
	    !<http://purl.obolibrary.org/obo/IAO_0000116>
	    ,uri-d7120
	    "For information on the datetimestamp formate see: http://www.schemacentral.com/sc/xsd11/t-xsd_dateTimeStamp.html")
	  axioms)

    ;; add editor note about D7120
    (push `(annotation-assertion 
	    !<http://purl.obolibrary.org/obo/IAO_0000116>
	    ,uri-d7120
	    ,(format nil "To be reported for an additional extraction in the same quadrant at the same visit. ~%~%cdt3 users manual: current dental terminology version 2000"))
	  axioms)
     

    ;; D7110 and D7120 are subclasses of D7140
    (push `(subclass-of ,uri-d7110 ,uri-d7140) axioms)
    (push `(subclass-of ,uri-d7120 ,uri-d7140) axioms)

    ;; return axioms
    axioms))

(defun get-historical-code-individual-axoms (iri)
  "Creates axioms for historical codes D7110 and D7120 that were part of CDT-3, but were replaced in CDT-4 with D7140."
  (let ((axioms nil)
	(uri-d7110 nil)
	(uri-d7120 nil)
	(parent-class nil)
	(parent-class-uri nil))
    
    ;; create uri's for D7110, D7120, D7140
    (setf uri-d7110 (make-uri (str+ iri "CDT_0007110")))
    (setf uri-d7120 (make-uri (str+ iri "CDT_0007120")))
        
    ;; create named-individuals D7110 and D7120 and make them of type "billing code for extractions (includes local anesthesia, suturing, if needed, and routine postoperative care)"
    ;; which is the same parent class of D7140
    (setf parent-class (get-meta-class-id (get-parent-class "D7140")))
    (setf parent-class-uri (make-uri (str+ iri parent-class)))
	
    (push `(declaration (named-individual ,uri-d7110)) axioms)
    (push `(declaration (named-individual ,uri-d7120)) axioms)
    (push `(class-assertion ,parent-class-uri ,uri-d7110) axioms)
    (push `(class-assertion ,parent-class-uri ,uri-d7120) axioms)

    ;; add annotations for D7110
    (push `(annotation-assertion !rdfs:label ,uri-d7110 "billing code D7110: single tooth") axioms)
    (push `(annotation-assertion ,*cdt-label-uri* ,uri-d7110 "single tooth") axioms)
    (push `(annotation-assertion !dc:identifier ,uri-d7110 "D7110") axioms)
    
    ;; add info about when code D7110 was retired
    (push `(annotation-assertion 
	    !<http://purl.obolibrary.org/obo/IAO_0000604>
	    ,uri-d7110
	    (:literal "2003-01-01" !xsd:datetimestamp)) axioms)
	    
    ;; add annotations for D7120
    (push `(annotation-assertion !rdfs:label ,uri-d7120 "billing code D7120: each additional tooth") axioms)
    (push `(annotation-assertion ,*cdt-label-uri* ,uri-d7120 "each additional tooth") axioms)
    (push `(annotation-assertion !dc:identifier ,uri-d7120 "D7120") axioms)

    ;; add info about when code D7120 was retired
    (push `(annotation-assertion 
	    !<http://purl.obolibrary.org/obo/IAO_0000604>
	    ,uri-d7120
	    (:literal "2003-01-01" !xsd:datetimestamp)) axioms)

    ;; add editor note about D7120
    (push `(annotation-assertion 
	    !<http://purl.obolibrary.org/obo/IAO_0000116>
	    ,uri-d7120
	    ,(format nil "To be reported for an additional extraction in the same quadrant at the same visit. ~%~%cdt3 users manual: current dental terminology version 2000"))
	  axioms)
     
    ;; return axioms
    axioms))
	 
(defun get-top-level-cdt-class-axioms (iri)
  (let 
      ((axioms nil)
       (uri nil))
    
    (setf uri (make-uri (str+ iri (get-meta-class-id "cdt-code"))))
    (push `(declaration (class ,uri)) axioms)
    (push `(annotation-assertion !rdfs:label ,uri "current dental terminology code") axioms)
    (push `(annotation-assertion !rdfs:comment ,uri
     "A current dental terminology code is a centrally registered identifier that is maintained by the American Dental Association and used for recording dental services provided. It is typically used on the patient record, and when reporting procedures on a paper or electronic submission. ") axioms)

    ;; make cdt-code a subclass of IAO:crid (centrally registered identifier)
    (push `(subclassof ,uri
		       !<http://purl.obolibrary.org/obo/IAO_0000578>) axioms)
          
    (return-from get-top-level-cdt-class-axioms axioms)))

(defun get-meta-class-axioms (iri meta-list)
  (let 
      ((axioms nil)
       (uri nil)
       (parent-class nil)
       (parent-class-uri nil)
       (class-name nil)
       (label nil) ;; used for the rdfs:label
       (class-label nil) ;; used for ADA's lable of the class
       (class-comment nil))

    ;;(print-db meta-list)

    ;; gather necassary info about tag
    (setf class-name (first meta-list))
    (setf class-label (string-downcase (third (third meta-list))))
    (setf class-comment (third (fourth meta-list)))
    
    ;; get the parent class
    (setf parent-class (get-parent-class class-name))

    ;; clean up the ADA's label (i.e., class-label)
    (if (or (null class-label) (equal class-label "NIL"))
	(setf class-label(regex-replace-all "_" (string-downcase class-name) " ")))

    ;; assign rdfs label info
    (setf label (str+ "billing code for " class-label))
 
    ;; for testing
    ;;(print-db class-label)
    ;;(print-db class-comment)

    ;; build uri's
    ;; note: uri's are built using id's: CDT_0000???
    (setf uri (make-uri (str+ iri (get-meta-class-id class-name))))
    (setf parent-class-uri (make-uri (str+ iri (get-meta-class-id parent-class))))
        
    ;; add axioms about meta code to axiom list
    (push `(declaration (class ,uri)) axioms)
    (push `(subclass-of ,uri ,parent-class-uri) axioms)
    (push `(annotation-assertion !rdfs:label ,uri ,label) axioms)
    (push `(annotation-assertion ,*cdt-label-uri* ,uri ,class-label) axioms)

    #|
    ;; *** use 'editor note' for this. 12-19-2011
    ;; if the ADA has provided a definition/description add the definition and its source
    (if (not (null class-comment))
	(when (not (equal class-comment "NIL"))
	  ;; *** use 'editor note' for this. 12-19-2011
	  ;;(push `(annotation-assertion ,*cdt-definition-uri* ,uri ,class-comment) axioms)
	  (push `(annotation-assertion 
		  !<http://purl.obolibrary.org/obo/IAO_0000119>
		  ,uri
		  "ISBN:1935201387#CDT 2011-2012 Current Dental Terminology, Chapter 1")
		axioms)))
    |#

    ;; if the ADA has provided a definition/description add the definition and its source
    ;; as an 'editor note' (IAO_0000116) annotation
    (if (not (null class-comment))
	(when (not (equal class-comment "NIL"))
	  (push `(annotation-assertion 
		  !<http://purl.obolibrary.org/obo/IAO_0000116>
		  ,uri
		  ,(format nil "~a ~%~% ~a"
			   class-comment
			   "ISBN:1935201387#CDT 2011-2012 Current Dental Terminology, Chapter 1"))
		axioms)))

    (return-from get-meta-class-axioms axioms)))

(defun get-cdt-code-axioms (iri cdt-code-list)
  (let ((axioms nil)
	(cdt-code nil)
	(label nil) ;; used for the rdfs label
	(cdt-label nil) ;; used for the labels provided by the ADA
	(cdt-comment nil)
	(cdt-uri nil)
	(cdt-num nil)
	(parent-class nil)
	(parent-class-uri))

    ;; gather necassary info about tag
    ;; cdt codes have the form:
    ;; ("CDTCode" NIL ("Code" NIL "D1234") ("CDTLabel" nil "...") ("CDTComment" nil "...")
    (setf cdt-code (third (third cdt-code-list)))
    (setf cdt-label (third (fourth cdt-code-list)))
    (setf cdt-comment (third (fifth cdt-code-list)))



    ;; for testing
    ;;(print-db cdt-label)
    ;;(print-db cdt-comment)
    ;;(print-db cdt-code)
    ;;(print-db parent-class)

    ;; clean up cdt label info
    (if (or (null cdt-label) (equal cdt-label "NIL")) 
	(setf cdt-label cdt-code))


    ;; create rdfs label
    (if (or (null cdt-label) (equal cdt-label "NIL"))
	(setf label (str+ "billing code " cdt-code))
	(setf label (str+ "billing code " cdt-code ": " cdt-label)))

    ;; build uri's; note cdt uri is zero padded length 7: "~7,'0d"
    (setf cdt-num (parse-integer  (subseq cdt-code 1)))
    (setf cdt-uri (make-uri (str+ iri "CDT_" (format nil "~7,'0d" cdt-num))))
    (setf parent-class (get-meta-class-id (get-parent-class cdt-code)))
    (setf parent-class-uri (make-uri (str+ iri parent-class)))

    ;; add axioms about cdt code to axiom list
    (push `(declaration (named-individual ,cdt-uri)) axioms)
    (push `(class-assertion ,parent-class-uri ,cdt-uri) axioms)
    (push `(annotation-assertion !rdfs:label ,cdt-uri ,label) axioms)
    (push `(annotation-assertion ,*cdt-label-uri* ,cdt-uri ,cdt-label) axioms)
    (push `(annotation-assertion !dc:identifier ,cdt-uri ,cdt-code) axioms)
    
    #|
    ;; *** for now I will used 'editor note' for this. 12-19-2011
    ;; add cdt description if description is present
    (if (not (null cdt-comment))
	(when (not (equal cdt-comment "NIL"))
	    (push `(annotation-assertion ,*cdt-definition-uri* ,cdt-uri ,cdt-comment) axioms)
	    (push `(annotation-assertion 
		     !<http://purl.obolibrary.org/obo/IAO_0000119>
		     ,cdt-uri
		    "ISBN:1935201387#CDT 2011-2012 Current Dental Terminology, Chapter 1") 
                   axioms)))
    |#

    ;; if the ADA has provided a definition/description add the definition and its source
    ;; as an 'editor note' (IAO_0000116) annotation
    (if (not (null cdt-comment))
	(when (not (equal cdt-comment "NIL"))
	  (push `(annotation-assertion 
		  !<http://purl.obolibrary.org/obo/IAO_0000116>
		  ,cdt-uri
		  ,(format nil "~a ~%~% ~a"
			   cdt-comment
			   "ISBN:1935201387#CDT 2011-2012 Current Dental Terminology, Chapter 1"))
		axioms)))

    ;; if the cdt code ends with "by report" add editor note (IAO_0000116) 
    ;; specifying that a narrative explaining the treatment must be provided 
    ;; with the code submission
    (when (> (length cdt-label) 9)
      (when (equal (subseq cdt-label (- (length cdt-label) 9)) "by report")
	(push `(annotation-assertion 
		!<http://purl.obolibrary.org/obo/IAO_0000116>
		,cdt-uri
		,(format nil "A narrative explaining the treatment provided must be included with a claim submission that uses this code.  ~%~% ISBN:1935201387#CDT 2011-2012 Current Dental Terminology, Chapter 1"))
	      axioms)))

    (return-from get-cdt-code-axioms axioms)))

(defun get-cdt-code-as-classes-axioms (iri cdt-code-list)
  (let ((axioms nil)
	(cdt-code nil)
	(label nil) ;; used for the rdfs label
	(cdt-label nil) ;; used for the labels provided by the ADA
	(cdt-comment nil)
	(cdt-uri nil)
	(cdt-num nil)
	(parent-class nil)
	(parent-class-uri))

    ;; gather necassary info about tag
    ;; cdt codes have the form:
    ;; ("CDTCode" NIL ("Code" NIL "D1234") ("CDTLabel" nil "...") ("CDTComment" nil "...")
    (setf cdt-code (third (third cdt-code-list)))
    (setf cdt-label (third (fourth cdt-code-list)))
    (setf cdt-comment (third (fifth cdt-code-list)))

    ;; for testing
    ;;(print-db cdt-label)
    ;;(print-db cdt-comment)
    ;;(print-db cdt-code)
    ;;(print-db parent-class)

    ;; clean up cdt label info
    (if (or (null cdt-label) (equal cdt-label "NIL")) 
	(setf cdt-label cdt-code))


    ;; create rdfs label
    (if (or (null cdt-label) (equal cdt-label "NIL"))
	(setf label (str+ "billing code " cdt-code))
	(setf label (str+ "billing code " cdt-code ": " cdt-label)))

    ;; build uri's; note cdt uri is zero padded length 7: "~7,'0d"
    (setf cdt-num (parse-integer  (subseq cdt-code 1)))
    (setf cdt-uri (make-uri (str+ iri "CDT_" (format nil "~7,'0d" cdt-num))))
    (setf parent-class (get-meta-class-id (get-parent-class cdt-code)))
    (setf parent-class-uri (make-uri (str+ iri parent-class)))

    ;; add axioms about cdt code to axiom list
    (push `(declaration (class ,cdt-uri)) axioms)
    (push `(subclass-of ,cdt-uri ,parent-class-uri) axioms)
    (push `(annotation-assertion !rdfs:label ,cdt-uri ,label) axioms)
    (push `(annotation-assertion ,*cdt-label-uri* ,cdt-uri ,cdt-label) axioms)
    (push `(annotation-assertion !dc:identifier ,cdt-uri ,cdt-code) axioms)
    

    ;; if the ADA has provided a definition/description add the definition and its source
    ;; as an 'editor note' (IAO_0000116) annotation
    (if (not (null cdt-comment))
	(when (not (equal cdt-comment "NIL"))
	  (push `(annotation-assertion 
		  !<http://purl.obolibrary.org/obo/IAO_0000116>
		  ,cdt-uri
		  ,(format nil "~a ~%~% ~a"
			   cdt-comment
			   "ISBN:1935201387#CDT 2011-2012 Current Dental Terminology, Chapter 1"))
		axioms)))

    ;; if the cdt code ends with "by report" add editor note (IAO_0000116) 
    ;; specifying that a narrative explaining the treatment must be provided 
    ;; with the code submission
    (when (> (length cdt-label) 9)
      (when (equal (subseq cdt-label (- (length cdt-label) 9)) "by report")
	(push `(annotation-assertion 
		!<http://purl.obolibrary.org/obo/IAO_0000116>
		,cdt-uri
		,(format nil "A narrative explaining the treatment provided must be included with a claim submission that uses this code.  ~%~% ISBN:1935201387#CDT 2011-2012 Current Dental Terminology, Chapter 1"))
	      axioms)))

    (return-from get-cdt-code-as-classes-axioms axioms)))

(defun get-disjoint-class-axioms (iri)
  "Returns a list of disjoint classes."
  (let ((axioms nil)
	(uri nil)
	(uri-list nil)
	(level2-list nil)
	(level3-list nil)
	(parent-class nil))


    ;; the cdt-code hierarchy goes four deep; so procede by iterating over the 
    ;; parent class hash table three times, and during each iteration create
    ;; a list of the disjoint classes

    ;; add classes who's parent is cdt-code (first level) to disjoint list;
    ;; i.e., add the immediate/second level under cdt-code
    (loop 
       for child-class being the hash-keys of *parent-class-ht* do
	 (when (equal (get-parent-class child-class) "cdt-code")
	   ;; build uri
	   (setf uri (make-uri (str+ iri (get-meta-class-id child-class))))
	   
	   ;; add uri to list
	   (push uri uri-list)
	   (push child-class level2-list)))
    
    ;; spefify that all classes are disjoint
    ;; NB: a disjoint list of length one (i.e. DisjointClasses(cdt-class))
    ;;     will cause a parse error.  so, check length of axiom list.
    (when (> (length uri-list) 1)
      (push `(disjoint-classes ,@uri-list) axioms))
    (setf uri-list nil)

    ;; now add classes who parents are in second level
    (loop 
       for item in level2-list do
	 (loop 
	    for child-class being the hash-keys of *parent-class-ht* do
	      ;; check parent class
	      ;; note: individual cdt codes (e.g., D1234) are in hash table
	      ;;       so, make sure to exclude them
	      (setf parent-class (get-parent-class child-class))
	      (when (and (equal item parent-class)
			 (not (all-matches child-class "(D\\d{4})" 1)))
		;; build uri
		(setf uri (make-uri (str+ iri (get-meta-class-id child-class))))
		
		;; add uri to list
		(push uri uri-list)
		(push child-class level3-list)))
	 
	 ;; add disjoint classes to axioms
	 (when (> (length uri-list) 1)
	   (push `(disjoint-classes ,@uri-list) axioms))
	 (setf uri-list nil))
    
    ;; now add classes who parents are in second level
    (loop 
       for item in level3-list do
	 (loop 
	    for child-class being the hash-keys of *parent-class-ht* do
	      ;; check parent 
       	      ;; note: individual cdt codes (e.g., D1234) are in hash table
	      ;;       so, make sure to exclude them
	      (setf parent-class (get-parent-class child-class))
	      (when (and (equal item parent-class)
			 (not (all-matches child-class "(D\\d{4})" 1)))
		;; build uri
		(setf uri (make-uri (str+ iri (get-meta-class-id child-class))))

		;; add uri to list
		(push uri uri-list)))
	 
       ;; add disjoint classes to axioms
	 (when (> (length uri-list) 1)
	   (push `(disjoint-classes ,@uri-list) axioms))
	 (setf uri-list nil))

    ;; return axioms
    axioms))

(defun get-different-individuals-axioms (iri)
  (let ((axioms nil)
	(uri nil)
	(uri-list nil)
	(cdt-id nil)
	(cdt-num nil)
	(uri-d7110 nil) ; D7110 and D7120 are historical codes
	(uri-d7120 nil))
    
    ;; loop for child/parent hash table; if the name of the child class
    ;; has form "D\\d{4}" (i.e., a "D" followed by four numbers) it is 
    ;; a cdt code (i.e., individual cdt code)
    (loop 
       for child-class being the hash-keys in *parent-class-ht* do
	 (when (all-matches child-class "(D\\d{4})" 1)
	   ;; build uri; note cdt uri is zero padded length 7: "~7,'0d"
	   (setf cdt-num (parse-integer (subseq child-class 1)))
	   (setf cdt-id (str+ "CDT_" (format nil "~7,'0d" cdt-num)))
	   (setf uri (make-uri (str+ iri cdt-id)))
	   (push uri uri-list)))
    
    ;; add axioms for historical codes
    ;; NB: this is a hack!
    (setf uri-d7110 (make-uri (str+ iri "CDT_0007110")))
    (setf uri-d7120 (make-uri (str+ iri "CDT_0007120")))
    (push uri-d7110 uri-list)
    (push uri-d7120 uri-list)

    ;; add different individual axioms
    (when (> (length uri-list) 1)
      (push `(different-individuals ,@uri-list) axioms))

    ;; return axioms
    axioms))

(defun get-disjoint-cdt-classes-axioms (iri)
  "Returns a list of disjoint cdt code classes."
  (let ((axioms nil)
	(uri nil)
	(uri-list nil)
	(cdt-num nil)
	(cdt-id nil)
	;;(cdt-list nil) ; no longer needed as of 1/8/2013
	;;(parent-class nil) ; no longer needed as of 1/8/2013
	(uri-d7110 nil) ; D7110 and D7120 are historical codes
	(uri-d7120 nil))


    
    #| billd 1/8/2013:
       Rather than looping through the hierarachy to create disjoint
       lists for each heirarchical group, I will simply iterate over
       hash table to create the list of disjoint classes 
    (loop 
       for child-class being the hash-keys of *parent-class-ht* do
	 ;; test for cdt code that has not been processed
	 (when (and (all-matches child-class "(D\\d{4})" 1)
		    (not (member child-class cdt-list)))
	   ;; get that cdt code's parent class
	   (setf parent-class (get-parent-class child-class))
	   
	   (loop
	      for cdt-code being the hash-keys of *parent-class-ht* do
		;; test for cdt code's that are subclasses of the above parent class
		(when (and (all-matches cdt-code "(D\\d{4})" 1)
			   (equal parent-class  (get-parent-class cdt-code)))
	     
		  ;; build uri's; note cdt uri is zero padded length 7: "~7,'0d"
		  (setf cdt-num (parse-integer  (subseq cdt-code 1)))
		  (setf uri (make-uri (str+ iri "CDT_" (format nil "~7,'0d" cdt-num))))
		
		  ;; add uri to list
		  (push uri uri-list)))
	   
	   ;; add disjoint cdt classes to axioms
           (when (> (length uri-list) 1)
	     (push `(disjoint-classes ,@uri-list) axioms))
	   (setf uri-list nil)
	   
	   (push child-class cdt-list)))
    |#
    
    ;; loop for child/parent hash table; if the name of the child class
    ;; has form "D\\d{4}" (i.e., a "D" followed by four numbers) it is 
    ;; a cdt code (i.e., individual cdt code)
    (loop 
       for child-class being the hash-keys in *parent-class-ht* do
	 (when (all-matches child-class "(D\\d{4})" 1)
	   ;; build uri; note cdt uri is zero padded length 7: "~7,'0d"
	   (setf cdt-num (parse-integer (subseq child-class 1)))
	   (setf cdt-id (str+ "CDT_" (format nil "~7,'0d" cdt-num)))
	   (setf uri (make-uri (str+ iri cdt-id)))
	   (push uri uri-list)))
    
    ;; add axioms for historical codes
    ;; NB: this is a hack!
    (setf uri-d7110 (make-uri (str+ iri "CDT_0007110")))
    (setf uri-d7120 (make-uri (str+ iri "CDT_0007120")))
    (push uri-d7110 uri-list)
    (push uri-d7120 uri-list)

    ;; add disjoint cdt classes to axioms
    (when (> (length uri-list) 1)
      (push `(disjoint-classes ,@uri-list) axioms))

    ;; return axioms
    axioms))

;;;;;;;;;;;;;; Functions for parsing CDT code xml file ;;;;;;;;;;;;;;;

(defun get-cdtcode-xmls (xmlfile)
  "This function does an xmln:parse on the file specified by xmlfile"
  (let ((p nil))
    (with-open-file (f xmlfile :direction :input)
      (unwind-protect 
	   (setf p (xmls:parse f))))
    (return-from get-cdtcode-xmls p)))

;;;;;;;;;;;;;;;;; Other helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reduce-meta-class-name-list (meta-class-name-list &key xmls-parse ada-code-list)
  (let ((codes nil)
	(common-codes nil))
    ;; remove non-meta tags; i.e., remove tags that pertain to codes, labels, and comments
    ;; I am only intersted in the upper level grouping of cdt codes
    (loop for item in meta-class-name-list do
	 (when (member item (list "CDTCode-List" "CDTCode" 
				  "Code" "CDTLabel" "CDTComment") :test 'equal)
	   (setf meta-class-name-list (remove item meta-class-name-list))))

    ;; if only a subset of ada codes are to be used remove the meta tags that
    ;; do not contain any of the ada code subset
    (when ada-code-list
      (loop for item in meta-class-name-list do
	   ;; get list of codes for the meta tag
	   ;; the inner find-element gets a list for the meta tag item
	   ;; the outter find-elements gets a list of code tags for the met tag
	   (setf codes 
		 (find-elements-with-tag (find-element-with-tag xmls-parse item) "Code"))

	   ;; check whether there any codes in common in the meta tag cods and ada code list 
	   ;; codes is a list of lists of the form (("Code" NIL "D1234")("Code" NIL "D2345")...)
	   ;; so, I use mapcar to check each sublist (i.e., ("Code" NIL "D1234"))
	   ;; against the ada-code-list
	   (setf common-codes 
		 (mapcar #'(lambda(e) (intersection e ada-code-list :test #'equalp)) codes))

	   ;; remove any nils from the common-codes list
	   (setf common-codes (remove nil common-codes))
	   
	   ;; if there are no common-codes then remove the meta tag
	   (when (not common-codes)
	     (setf meta-class-name-list (remove item meta-class-name-list)))))
	 
    meta-class-name-list))
    

(defun get-meta-class-id (meta-class)
  "Returns the uri id for a class (e.g., CDT_0001234)."
  (gethash meta-class *meta-class-id-ht*))

(defun get-parent-class (class-name)
  "Returns the parent class of the class-name parameter."
  (gethash class-name *parent-class-ht*))

(defun make-meta-class-id-hash-table (meta-class-name-list)
  "Creates a hash table that associates an id with each meta class."
  (let ((cdt-num 1000001)
	(cdt-id nil))

    (setf *meta-class-id-ht* (make-hash-table :test 'equal))
    
    ;; add top-level cdt code id
    (setf cdt-id "CDT_1000001")
    (setf (gethash "cdt-code" *meta-class-id-ht*) cdt-id)

    (loop for item in meta-class-name-list do
	 (incf cdt-num)
	 (setf cdt-id (str+ "CDT_" (format nil "~7,'0d" cdt-num)))
	 (setf (gethash item *meta-class-id-ht*) cdt-id))))

(defun make-parent-class-hash-table (meta-class-name-list xmls-parse &key ada-code-list)
  "Creates a hash table that associates cdt code or meta class with a parent class"
  (let ((child-list nil)
	(class-list nil)
	(code nil))

    ;; declare hash table
    (setf *parent-class-ht* (make-hash-table :test 'equal))

    ;;;;;;;;;; add meta classes (i.e., not cdt codes ) to hash ;;;;;;;;;
    ;; iterate over meta class name list
    (loop 
       for class in meta-class-name-list do
         ;; create a class list of each meta class name
	 (setf class-list (find-element-with-tag xmls-parse class))
	 
	 (loop 
	    for subclass in meta-class-name-list do
	      ;; get the "immediate children" of each meta class name in the class list
	      (setf child-list (find-immediate-children-with-tag class-list subclass))
	 
	      ;; if it found and immediate child then the class is a parent of the subclass
	      (when child-list
		(setf (gethash subclass *parent-class-ht*) class))))

    ;; now add those meta class that have cdt-code as their parent class
    (loop 
       for class in meta-class-name-list do
	 (unless (gethash class *parent-class-ht*)
	   (setf (gethash class *parent-class-ht*) "cdt-code")))

    ;;;;;;;;;;;;;; add cdt codes (e.g., D1234) info to hash ;;;;;;;;;;;;;
    
    (loop
       for class in meta-class-name-list do
	 ;; find the meta class elements
	 (setf class-list (find-element-with-tag xmls-parse class))
	 
	 ;; get the "immediate children" of each meta class name that are cdt codes
	 (setf child-list (find-immediate-children-with-tag class-list "CDTCode"))
	 
	 ;; iterate over the children and add to hash table
	 (loop
	    for cdt-code in child-list do
	      ;; cdt codes have the form:
	      ;; ("CDTCode" NIL ("Code" NIL "D1234") ("CDTLabel"...) ("CDTComment"...)
	      ;; the code is the third item of the third element in the list
	       (setf code (third (third cdt-code)))

	      ;; add code to hash table with the class name as parent
	      ;; check to see if only a subset of codes are specified in ada-code-list
	      (cond 
		(ada-code-list
		 ;; check to see if code is in the ada-code-list
		 (when (member code ada-code-list :test #'equalp)
		   (setf (gethash code *parent-class-ht*) class)))
		(t
		 (setf (gethash code *parent-class-ht*) class)))))
	      
    ;; return hash table
    *parent-class-ht*))

    
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

(defun test-cdt-ont (xmlfile &key iri ont-iri cdt-codes-as-classes
		                  print-ont save-ont filename filepath)
  (let ((ont nil))
    (setf ont (get-cdtcode-ont xmlfile :iri iri :ont-iri ont-iri
			       :cdt-codes-as-classes cdt-codes-as-classes))
    
    (when (not (null print-ont))
      (pprint (to-owl-syntax ont :functional)))
    
    (when (not (null save-ont))
      (when (null filepath) (setf filepath "~/Desktop/"))
      (when (null filename) 
	(if cdt-codes-as-classes
	    (setf filename "CDTCodes as Classes.owl")
	    (setf filename "CDTCodes.owl")))
      (write-rdfxml ont (str+ filepath filename)))
      
    ;; return the ontology
    ont
))


(defun test-axioms (axiom-list)
  "This function is used for testing list of axioms."
  (with-ontology foo (:collecting t)
      ((as axiom-list))
    (to-owl-syntax foo :functional)))

(defun print-parent-ht (hash)
  (maphash #'(lambda (key val)
	       (format t "child: ~A ~% parent: ~A ~%~%" key val)) hash))

(defun print-meta-id-ht (hash)
  (maphash #'(lambda (key val)
	       (format t "class: ~A ~% id: ~A ~%~%" key val)) hash))