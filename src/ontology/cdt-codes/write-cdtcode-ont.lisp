(defparameter *parent-class-ht* nil
  "Global hash table that contains child/parent class information.")

;;;; main driver function
(defun get-cdtcode-ont (iri xmlfile)
  "This functions Builds an ontology of the CDT Codes.
Returns:
   An ontology of the CDT codes.
Parmaters: 
  iri: The IRI for the ontology.
  xmlfile: The xml file containing the CDT codes
Usage:
  (get-cdtcode-ont \"http://test.com\" \"CDTCodes8.xml\")"

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
    
    ;; create hash table for child/parent class info
    (make-parent-class-hash-table meta-class-name-list xmls-parse)

    (with-ontology ont (:collecting t :base iri)
	(
	 (as (import "purl.obolib"
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
	      (as (get-meta-class-axioms iri temp-list)))
	 
	 ;; add cdt code axoms
	 (setf cdt-list (find-elements-with-tag xmls-parse "CDTCode"))	 
	 (loop 
	    for item in cdt-list do
	      (as (get-cdt-code-axioms iri item)))

	 ;; ********* add disjoint class info for meta and cdt codes
	 (as (get-disjoint-class-axioms iri)))
	 
      (return-from get-cdtcode-ont ont))))


;;;;;;;;;;;;; Functions for getting axioms of meta and cdt classes ;;;;;;;;;;;;
	 
(defun get-top-level-cdt-class-axioms (iri)
  (let 
      ((axioms nil)
       (uri nil))
    
    
    (setf uri (make-uri (str+ iri "CDTCode")))
    (push `(declaration (class ,uri)) axioms)
    (push `(annotation-assertion !rdfs:label ,uri "CDT Code") axioms)
    (push `(annotation-assertion !rdfs:comment ,uri
				 "This is the top level class of all CDT Codes.") axioms)
          
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
    
    ;; gather necassary info about tag
    (setf class-name (first meta-list))
    (setf class-label (third (third meta-list)))
    (setf class-comment (third (fourth meta-list)))
    (setf parent-class (gethash class-name *parent-class-ht*))

    ;; clean up tag info
    (if (or (not class-label) (equal class-label "NIL"))
	(setf class-label ""))
    (if (or (not class-comment) (equal class-comment "NIL"))
	(setf class-comment ""))

    ;; for testing
    ;;(print-db class-label)
    ;;(print-db class-comment)

    ;; build uri's
    (setf uri (make-uri (str+ iri class-name)))
    (setf parent-class-uri (make-uri (str+ iri parent-class)))
	 
    ;; add axioms about meta code to axiom list
    (push `(declaration (class ,uri)) axioms)
    (push `(subclass-of ,uri ,parent-class-uri) axioms)
    (push `(annotation-assertion !rdfs:label ,uri ,class-label) axioms)
    (push `(annotation-assertion !rdfs:comment ,uri ,class-comment) axioms)

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
    (setf parent-class (gethash cdt-code *parent-class-ht*))

    ;; clean up tag info
    (if (or (not cdt-label) (equal cdt-label "NIL"))
	(setf cdt-label ""))
    (if (or (not cdt-comment) (equal cdt-comment "NIL"))
	(setf cdt-comment ""))

    ;; for testing
    ;;(print-db cdt-label)
    ;;(print-db cdt-comment)
    ;;(print-db cdt-code)
    ;;(print-db parent-class)

    ;; build uri's
    (setf cdt-uri (make-uri (str+ iri cdt-code)))
    (setf parent-class-uri (make-uri (str+ iri parent-class)))
	 
    ;; add axioms about meta code to axiom list
    (push `(declaration (class ,cdt-uri)) axioms)
    (push `(subclass-of ,cdt-uri ,parent-class-uri) axioms)
    (push `(annotation-assertion !rdfs:label ,cdt-uri ,cdt-label) axioms)
    (push `(annotation-assertion !rdfs:comment ,cdt-uri ,(str+ "CDT Code: " cdt-code)) axioms)
    (push `(annotation-assertion !rdfs:comment ,cdt-uri ,cdt-comment) axioms)

    (return-from get-cdt-code-axioms axioms)))
    
(defun get-disjoint-class-axioms (iri)
  (let ((axioms nil)
	(uri nil)
	(uri-list nil)
	(processed-list nil))
  
    ;; *parent-class-ht* has the structure (key: child-class) (value: parent-class)
    ;; so, iterate over hash table and:
    ;; 1. get each parent-class (i.e., value)
    ;; 2. for each child-class (i.e., key)  of that parent make a disjoint axiom list
    (loop 
       for parent-class being the hash-values of *parent-class-ht* do
         ;; check to see if the parent class had already been processed
	 (unless (member parent-class processed-list :test 'equal)
	   ;; reset uri list
	   (setf uri-list nil)
	   
	   ;; now do step 2
	   (loop 
	      for child-class being the hash-keys of *parent-class-ht* do
		;; if this is a child class of the parent, add uri to disjoint list
		(when (equal (gethash child-class *parent-class-ht*) parent-class)
		  ;; build and add uri
		  (setf uri (make-uri (str+ iri child-class)))
		  (push uri uri-list)))
	   
	   ;; spefify that all classes are disjoint
	   ;; NB: a disjoint list of length one (i.e. DisjointClasses(cdt-class))
	   ;;     will cause a parse error.  so, check length of axiom list.
	   (when (> (length axioms) 1)
	     (push `(disjoint-classes ,@uri-list) axioms))
	   
	   ;; add parent class to process list to keep from doing class twice
	   (push parent-class processed-list)))

    ;; NB: a disjoint list of length one (i.e. DisjointClasses(cdt-class))
    ;;     will cause a parse error.  so, check length of axiom list.
    (when (> (length axioms) 1)
      (return-from get-disjoint-class-axioms axioms))    
    ))

;;;;;;;;;;;;;; Functions for parsing CDT code xml file ;;;;;;;;;;;;;;;

(defun get-cdtcode-xmls (xmlfile)
  "This function does an xmln:parse on the file specified by xmlfile"
  (let ((p nil))
    (with-open-file (f xmlfile :direction :input)
      (unwind-protect 
	   (setf p (xmls:parse f))))
    (return-from get-cdtcode-xmls p)))

;;;;;;;;;;;;;;;;; Other helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-parent-class-hash-table (meta-class-name-list xmls-parse)
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

    ;; now add those meta class that have CDTCode as their parent class
    (loop 
       for class in meta-class-name-list do
	 (unless (gethash class *parent-class-ht*)
	   (setf (gethash class *parent-class-ht*) "CDTCode")))

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
    
    (when print-ont
      (pprint (to-owl-syntax ont :functional)))

    (when save-ont
      (if (null filepath) (setf filepath "~/Desktop/"))
      (if (null filename) (setf filename "CDTCodes.owl"))
      (write-rdfxml ont (str+ filepath filename)))


    ;; return the ontology
    ont))


(defun test-axioms (axiom-list)
  "This function is used for testing list of axioms."
  (with-ontology foo (:collecting t)
      ((as axiom-list))
    (to-owl-syntax foo :functional)))

(defun print-parent-ht (hash)
  (maphash #'(lambda (key val)
	       (format t "child: ~A ~% parent: ~A ~%~%" key val)) hash))