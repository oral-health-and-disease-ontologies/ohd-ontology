(defparameter *parent-class-ht* nil
  "Global hash table that contains child/parent class information.")

(defparameter *meta-class-id-ht*
  "Global hash table to store the auto-generated id's of the cdt categories.")

(defparameter *cdt-description-uri* nil
  "Global variable to hold the uri of the cdt_label annotation.")

;;;; main driver function
(defun get-cdtcode-ont (iri xmlfile)
  "This functions Builds an ontology of the CDT Codes.
Returns:
   An ontology of the CDT codes.
Parmaters: 
  iri: The IRI for the ontology.
  xmlfile: The xml file containing the CDT codes
Usage:
  (get-cdtcode-ont \"http://test.com\" \"CDTCodes.xml\")"

  (let ((xmls-parse nil)
	(meta-class-name-list nil)
	(temp-list nil)
	(cdt-list nil))

    ;; build xmlse parse list
    (setf xmls-parse (get-cdtcode-xmls xmlfile))
    
    ;; get list of tags
    (setf meta-class-name-list (what-tags xmls-parse))

    ;; remove non-meta tags; i.e., remove tags that pertain to codes, labels, and comments
    ;; I am only intersted in the upper level grouping of cdt codes
    (loop for item in meta-class-name-list do
	 (when (member item (list "CDTCode-List" "CDTCode" 
				  "Code" "CDTLabel" "CDTComment") :test 'equal)
	   (setf meta-class-name-list (remove item meta-class-name-list))))	 
    
    ;; make id associated with meta classes
    (make-meta-class-id-hash-table meta-class-name-list)

    ;; create hash table for child/parent class info
    (make-parent-class-hash-table meta-class-name-list xmls-parse)
    
    ;; verify the iri ends with "/"
    (unless (equal (subseq iri (- (length iri) 1)) "/")
      (setf iri (str+ iri "/")))

    (with-ontology ont (:collecting t :base iri :ontology-iri (str+ iri "cdt-codes.owl"))
	( ;;import IAO meta data	 
	 (as `(imports !<http://purl.obolibrary.org/obo/iao/ontology-metadata.owl>))

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
	 (setf *cdt-description-uri* (make-uri (str+ iri "cdt_description")))

	 ;; add cdt_label annotation as a sub-annotation of IAO's "alternative term" 
	 ;; annotation; i.e., IAO_0000118
	 (as `(declaration (annotation-property ,*cdt-description-uri*)))
	 (as `(annotation-assertion !rdfs:comment ,*cdt-description-uri* 
				    "This annotation is used for the descriptions of the CDT codes provided by the American Dental Association."))
	 (as `(subannotationpropertyof ,*cdt-description-uri*
				       !<http://purl.obolibrary.org/obo/IAO_0000118>))
	 (as `(annotation-property-range ,*cdt-description-uri* !xsd:string))
	 (as `(annotation-property-domain ,*cdt-description-uri* 
					  ,(make-uri (str+ iri "cdt-code"))))

	 
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
	    (as (get-cdt-code-axioms iri item)))

	 ;; ********* add disjoint class info for meta and cdt codes
	 (as (get-disjoint-class-axioms iri))

	 )
      ;; return the ontology
      ont
      )))

;;;;;;;;;;;;; Functions for getting axioms of meta and cdt classes ;;;;;;;;;;;;
	 
(defun get-top-level-cdt-class-axioms (iri)
  (let 
      ((axioms nil)
       (uri nil))
    
    (setf uri (make-uri (str+ iri (get-meta-class-id "cdt-code"))))
    (push `(declaration (class ,uri)) axioms)
    (push `(annotation-assertion !rdfs:label ,uri "cdt code") axioms)
    (push `(annotation-assertion !rdfs:comment ,uri
				 "This is the top level class of all CDT code classes.") axioms)
          
    (return-from get-top-level-cdt-class-axioms axioms)))

(defun get-meta-class-axioms (iri meta-list)
  (let 
      ((axioms nil)
       (uri nil)
       (parent-class nil)
       (parent-class-uri nil)
       (class-name nil)
       (class-label nil)
       (class-comment nil))

    ;;(print-db meta-list)

    ;; gather necassary info about tag
    (setf class-name (first meta-list))
    (setf class-label (string-downcase (third (third meta-list))))
    (setf class-comment (third (fourth meta-list)))
    
    ;; get the parent class
    (setf parent-class (get-parent-class class-name))

    ;; clean up tag info
    (if (or (null class-label) (equal class-label "NIL"))
	(setf class-label (str+ "billing category for " 
				(regex-replace-all "_" (string-downcase class-name) " ")))
	(setf class-label (str+ "billing category for " class-label)))
    
    ;; for testing
    ;;(print-db class-label)
    ;;(print-db class-comment)

    ;; build uri's
    ;; note: uri's are built using id's: CDTC_0000???
    (setf uri (make-uri (str+ iri (get-meta-class-id class-name))))
    (setf parent-class-uri (make-uri (str+ iri (get-meta-class-id parent-class))))
        
    ;; add axioms about meta code to axiom list
    (push `(declaration (class ,uri)) axioms)
    (push `(subclass-of ,uri ,parent-class-uri) axioms)
    (push `(annotation-assertion !rdfs:label ,uri ,class-label) axioms)

    ;; add cdt description if description is present
    (if (not (null class-comment))
	(if (not (equal class-comment "NIL"))
	    (push `(annotation-assertion ,*cdt-description-uri* ,uri ,class-comment) axioms)))

    (return-from get-meta-class-axioms axioms)))

(defun get-cdt-code-axioms (iri cdt-code-list)
  (let ((axioms nil)
	(cdt-code nil)
	(cdt-label nil)
	(cdt-comment nil)
	(cdt-uri nil)
	(parent-class nil)
	(parent-class-uri))

    ;; gather necassary info about tag
    ;; cdt codes have the form:
    ;; ("CDTCode" NIL ("Code" NIL "D1234") ("CDTLabel" nil "...") ("CDTComment" nil "...")
    (setf cdt-code (third (third cdt-code-list)))
    (setf cdt-label (third (fourth cdt-code-list)))
    (setf cdt-comment (third (fifth cdt-code-list)))

    ;; get the parent class of cdt-code and the parent class id
    (setf parent-class (get-meta-class-id (get-parent-class cdt-code)))

    ;; for testing
    ;;(print-db cdt-label)
    ;;(print-db cdt-comment)
    ;;(print-db cdt-code)
    ;;(print-db parent-class)

    ;; clean up label info
    (if (or (null cdt-label) (equal cdt-label "NIL"))
	(setf cdt-label (str+ "billing code " cdt-code))
	(setf cdt-label (str+ "billing code " cdt-code " for: " cdt-label)))

    ;; build uri's; note cdt uri is zero padded length 7: "~7,'0d"
    (setf cdt-uri (make-uri (str+ iri "CDTD_" (format nil "~7,'0d" (subseq cdt-code 1)))))
    (setf parent-class-uri (make-uri (str+ iri parent-class)))

    ;; add axioms about cdt code to axiom list
    (push `(declaration (named-individual ,cdt-uri)) axioms)
    (push `(class-assertion ,parent-class-uri ,cdt-uri) axioms)
    (push `(annotation-assertion !rdfs:label ,cdt-uri ,cdt-label) axioms)
    (push `(annotation-assertion !dc:identifier ,cdt-uri ,cdt-code) axioms)

    ;; add cdt description if description is present
    (if (not (null cdt-comment))
	(if (not (equal cdt-comment "NIL"))
	    (push `(annotation-assertion ,*cdt-description-uri* ,cdt-uri ,cdt-comment) axioms)))

    (return-from get-cdt-code-axioms axioms)))    

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
      (push `(disjoint-classes ,@uri-list) axioms)
      (setf uri-list nil))
    
    ;; now add classes who parents are in second level
    (loop 
       for item in level2-list do
	 (loop 
	    for child-class being the hash-keys of *parent-class-ht* do
	      ;; check parent class
	      (setf parent-class (get-parent-class child-class))
	      (when (equal item parent-class)
		;; build uri
		(setf uri (make-uri (str+ iri (get-meta-class-id child-class))))

		;; add uri to list
		(push uri uri-list)
		(push child-class level3-list)))
	 
	 ;; add disjoint classes to axioms
	 (when (> (length uri-list) 1)
	   (push `(disjoint-classes ,@uri-list) axioms)
	   (setf uri-list nil)))
	 
     ;; now add classes who parents are in second level
    (loop 
       for item in level3-list do
	 (loop 
	    for child-class being the hash-keys of *parent-class-ht* do
	      ;; check parent class
	      (setf parent-class (get-parent-class child-class))
	      (when (equal item parent-class)
		;; build uri
		(setf uri (make-uri (str+ iri (get-meta-class-id child-class))))

		;; add uri to list
		(push uri uri-list)
		(push child-class level3-list)))
	 
	 ;; add disjoint classes to axioms
	 (when (> (length uri-list) 1)
	   (push `(disjoint-classes ,@uri-list) axioms)
	   (setf uri-list nil)))

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

(defun get-meta-class-id (meta-class)
  "Returns the uri id for a class (e.g., CDTC_0001234)."
  (gethash meta-class *meta-class-id-ht*))

(defun get-parent-class (class-name)
  "Returns the parent class of the class-name parameter."
  (gethash class-name *parent-class-ht*))

(defun make-meta-class-id-hash-table (meta-class-name-list)
  "Creates a hash table that associates an id with each meta class."
  (let ((cdt-num 1)
	(cdt-id nil))

    (setf *meta-class-id-ht* (make-hash-table :test 'equal))
    
    ;; add top-level cdt code id
    (setf cdt-id "CDTC_0000001")
    (setf (gethash "cdt-code" *meta-class-id-ht*) cdt-id)

    (loop for item in meta-class-name-list do
	 (incf cdt-num)
	 (setf cdt-id (str+ "CDTC_" (format nil "~7,'0d" cdt-num)))
	 (setf (gethash item *meta-class-id-ht*) cdt-id))))

(defun make-parent-class-hash-table (meta-class-name-list xmls-parse)
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
	      (setf (gethash code *parent-class-ht*) class)))
	      
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
  ;; this may be the simplist to understant...
  (apply #'concatenate 'string values))

(defun test-cdt-ont (iri xmlfile &key print-ont save-ont filename filepath)
  (let ((ont nil))
    (setf ont (get-cdtcode-ont iri xmlfile))
    
    (when (not (null print-ont))
      (pprint (to-owl-syntax ont :functional)))
    
    (when (not (null save-ont))
      (if (null filepath) (setf filepath "~/Desktop/"))
      (if (null filename) (setf filename "CDTCodes.owl"))
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