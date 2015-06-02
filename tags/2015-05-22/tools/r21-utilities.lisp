;;****************************************************************
;; Required components on user's system:
;;
;; 1. Must have an instanct of the Eaglesoft datbase and necessary drivers (see below)
;;    installed on your machine.
;; 2. A file named .pattersondbpw (containing the password of Eaglesoft database) located in
;;    your home directory.

;; Instructions for being able to connect to Eaglesoft database
;;
;; Needs "/Applications/SQLAnywhere12/System/lib32"
;;  added to DYLD_LIBRARY_PATH so it can find libdbjdbc12.jnilib and dependencies.
;; for now add (setenv "DYLD_LIBRARY_PATH" (concatenate 'string (getenv "DYLD_LIBRARY_PATH")
;; ":/Applications/SQLAnywhere12/System/lib32")) to your .emacs
;; (#"load" 'System "/Applications/SQLAnywhere12/System/lib32/libdbjdbc12.jnilib")
;; fails there is no easy way to update DYLD_LIBRARY_PATH within a running java instance. POS.
;;
;; The conection string for connecting to the database is retrieved by calling
;; (get-eaglesoft-database-url).
;;****************************************************************

;;;; ensure that sql libary is loaded ;;;;
(add-to-classpath "/Applications/SQLAnywhere12/System/java/sajdbc4.jar")

;;****************************************************************
;;; general shared functions ;;;;

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


(defmacro push-items (place &rest items)
"macro used for pushing muliple items onto a list.
For example, instead of doing multiple push operations like so:
  (push `(declaration (class !A)) axioms)
  (push `(declaration (class !B)) axioms)
  (push `(declaration (class !C)) axioms)

push-items can be called like so:
  (push-items axioms
              `(declaration (class !A))
              `(declaration (class !B))
              `(declaration (class !C))

created by Alan Ruttenberg 11/26/2013"
`(setf ,place (append (list ,@items) ,place)))


(defmacro push-instance (axioms instance class)
  "macro used for creating instances of OHD data.
example usage: (push-instance x !a !A)
created by: Bill Duncan 1/8/2014"
  `(progn 
     (push (list 'declaration (list 'named-individual ,instance)) ,axioms)
     (push (list 'class-assertion ,class ,instance) ,axioms)
     (push (list 'annotation-assertion !'asserted type'@ohd ,instance ,class) ,axioms)))

;; THIS ALSO WORKS
;; (defmacro push-instance (axioms instance class)
;;   "macro used for creating instances of OHD data.
;; example usage: (push-instance x !a !A)
;; created by: Bill Duncan 1/8/2014"
;;   `(push-items ,axioms
;; 	       (list 'declaration (list 'named-individual ,instance))
;; 	       (list 'class-assertion ,class ,instance)
;; 	       (list 'annotation-assertion !'asserted type'@ohd ,instance ,class)))

;; THIS ALSO WORKS
;; (defmacro push-instance (axioms instance class)
;;   "macro used for creating instances of OHD data.
;; example usage: (push-instance x !a !A)
;; created by: Bill Duncan 1/8/2014"
;;   `(setf ,axioms
;; 	 (append 
;; 	  (list (list 'declaration (list 'named-individual ,instance)))
;; 	  (list (list 'class-assertion ,class ,instance))
;; 	  (list (list 'annotation-assertion !'asserted type'@ohd ,instance ,class)) ,axioms)))

(defun number-to-fma-tooth (number &key return-tooth-uri return-tooth-name return-tooth-with-number)
  "Translates a tooth number into corresponding fma class.  By default, a cons is returned consisting of the (fma-class . name).  The :return-tooth-uri key specifies that only the uri is returned.  The :return-tooth-name key specifies that only the name is returned."
  (let ((teeth nil)
	(tooth nil))
    (setf teeth
	  #((!obo:FMA_55696 "Right upper third secondary molar tooth" "tooth 1")
	    (!obo:FMA_55697 "Right upper second secondary molar tooth" "tooth 2")
	    (!obo:FMA_55698 "Right upper first secondary molar tooth" "tooth 3")
	    (!obo:FMA_55688 "Right upper second secondary premolar tooth" "tooth 4")
	    (!obo:FMA_55689 "Right upper first secondary premolar tooth" "tooth 5")
	    (!obo:FMA_55798 "Right upper secondary canine tooth" "tooth 6")
	    (!obo:FMA_55680 "Right upper lateral secondary incisor tooth" "tooth 7")
	    (!obo:FMA_55681 "Right upper central secondary incisor tooth" "tooth 8")
	    (!obo:FMA_55682 "Left upper central secondary incisor tooth" "tooth 9")
	    (!obo:FMA_55683 "Left upper lateral secondary incisor tooth" "tooth 10")
	    (!obo:FMA_55799 "Left upper secondary canine tooth" "tooth 11")
	    (!obo:FMA_55690 "Left upper first secondary premolar tooth" "tooth 12")
	    (!obo:FMA_55691 "Left upper second secondary premolar tooth" "tooth 13")
	    (!obo:FMA_55699 "Left upper first secondary molar tooth" "tooth 14")
	    (!obo:FMA_55700 "Left upper second secondary molar tooth" "tooth 15")
	    (!obo:FMA_55701 "Left upper third secondary molar tooth" "tooth 16")
	    (!obo:FMA_55702 "Left lower third secondary molar tooth" "tooth 17")
	    (!obo:FMA_55703 "Left lower second secondary molar tooth" "tooth 18")
	    (!obo:FMA_55704 "Left lower first secondary molar tooth" "tooth 19")
	    (!obo:FMA_55692 "Left lower second secondary premolar tooth" "tooth 20")
	    (!obo:FMA_55693 "Left lower first secondary premolar tooth" "tooth 21")
	    (!obo:FMA_55687 "Left lower secondary canine tooth" "tooth 22")
	    (!obo:FMA_57141 "Left lower lateral secondary incisor tooth" "tooth 23")
	    (!obo:FMA_57143 "Left lower central secondary incisor tooth" "tooth 24")
	    (!obo:FMA_57142 "Right lower central secondary incisor tooth" "tooth 25")
	    (!obo:FMA_57140 "Right lower lateral secondary incisor tooth" "tooth 26")
	    (!obo:FMA_55686 "Right lower secondary canine tooth" "tooth 27")
	    (!obo:FMA_55694 "Right lower first secondary premolar tooth" "tooth 28")
	    (!obo:FMA_55695 "Right lower second secondary premolar tooth" "tooth 29")
	    (!obo:FMA_55705 "Right lower first secondary molar tooth" "tooth 30")
	    (!obo:FMA_55706 "Right lower second secondary molar tooth" "tooth 31")
	    (!obo:FMA_55707 "Right lower third secondary molar tooth" "tooth 32")))

    (cond
      (return-tooth-uri (setf tooth (first (aref teeth (1- number)))))
      (return-tooth-name (setf tooth (second (aref teeth (1- number)))))
      (return-tooth-with-number (setf tooth (third (aref teeth (1- number)))))
      (t (setf tooth (aref teeth (1- number)))))
    ;; return tooth uri/name
    tooth))

(defun get-fma-surface-uri (surface-name)
  "Return the uri for a tooth surface, based on the FMA ontology, for a given surface name."
  (let ((uri nil))
    (cond
      ((equalp surface-name "buccal") (setf uri !obo:FMA_no_fmaid_Buccal_surface_enamel_of_tooth))
      ((equalp surface-name "distal") (setf uri !obo:FMA_no_fmaid_Distal_surface_enamel_of_tooth))
      ((equalp surface-name "incisal") (setf uri !obo:FMA_no_fmaid_Incisal_surface_enamel_of_tooth))
      ((equalp surface-name "labial") (setf uri !obo:FMA_no_fmaid_Labial_surface_enamel_of_tooth))
      ((equalp surface-name "lingual") (setf uri !obo:FMA_no_fmaid_Lingual_surface_enamel_of_tooth))
      ((equalp surface-name "mesial") (setf uri !obo:FMA_no_fmaid_Mesial_surface_enamel_of_tooth))
      ((equalp surface-name "occlusial") (setf uri !obo:FMA_no_fmaid_Occlusial_surface_enamel_of_tooth)) ; occlusal was mispelled in previous OHD versions
      ((equalp surface-name "occlusal") (setf uri !obo:FMA_no_fmaid_Occlusial_surface_enamel_of_tooth)))

    ;; return suface uri
    uri))

(defun get-surface-name-from-surface-letter (surface-letter)
  "Returns the name of the tooth surface associated with a letter (e.g 'b' -> 'buccal')."
  (let ((surface-name nil))
    ;; remove any leading and trailing spaces from letter
    (setf surface-letter (string-trim " " surface-letter))

    (cond
      ((equalp surface-letter "b") (setf surface-name "buccal"))
      ((equalp surface-letter "d") (setf surface-name "distal"))
      ((equalp surface-letter "i") (setf surface-name "incisal"))
      ((equalp surface-letter "f") (setf surface-name "labial"))
      ((equalp surface-letter "l") (setf surface-name "lingual"))
      ((equalp surface-letter "m") (setf surface-name "mesial"))
      ((equalp surface-letter "o") (setf surface-name "occlusial")) ; occlusal was mispelled in previous OHD versions
      ((equalp surface-letter "o") (setf surface-name "occlusal")))

    ;; return surface name
    surface-name))

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

(defun get-eaglesoft-surface-iri (patient-id surface-type-iri tooth-name)
  "Returns an iri for the tooth surface that was involved in a restoration procedure.  The iri is generated by the patient id, the type of surface, the name of the type of tooth."
  (let ((uri nil))
    (setf uri
	  (get-unique-individual-iri patient-id
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-surfaces-iri-base*
				     :class-type surface-type-iri
				     :args `(,tooth-name "eaglesoft")))
    ;; return uri
    uri))

(defun get-unique-individual-iri(string &key salt class-type iri-base args)
  "Returns a unique iri for individuals in the ontology by doing a md5 checksum on the string parameter. An optional md5 salt value is specified by the salt key value (i.e., :salt salt). The class-type argument (i.e., :class-type class-type) concatentates the class type to the string.  This parameter is highly suggested, since it helps guarntee that iri will be unique.  The iri-base (i.e., :iri-base iri-base) is prepended to the iri.  For example, (get-unique-iri \"test\" :iri-base \"http://test.com/\" will prepend \"http://test.com/\" to the md5 checksum of \"test\".  The args parmameter is used to specify an other information you wish to concatenate to the string paramenter.  Args can be either a single value or list; e.g., (get-unique-iri \"test\" :args \"foo\") (get-unique-iri \"test\" :args '(\"foo\" \"bar\")."
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
    (setf string (format nil "~aI_~a" iri-base string)))
  (make-uri string))

(defun remove-leading-! (iri)
  "Removes the leading '!' from a iri and returns the iri as a string.
If no leading '!' is present, the iri is simply returned as string."
  (setf iri (format nil "~a" iri))
  (when (equal "!" (subseq iri 0 1)) (setf iri (subseq iri 1)))
  ;; return iri
  iri)

(defun encode (string &key salt)
  "Returns and md5 checksum of the string argument.  When the salt argument is present, it used in the computing of the md5 check sum."
  (let ((it nil))
    ;; check for salt
    (when salt
      (setf string (format nil "~a~a" salt string)))

    (setf it (new 'com.hp.hpl.jena.shared.uuid.MD5 string))

    (#"processString" it)
    (#"getStringDigest" it)))

(defun print-hash-table (hash-table &key limit)
  "prints out the key/value pairs in a hash table; the param limit limits the number of entries printed."
  (let ((count 0))
    (loop 
       for k being the hash-keys in hash-table using (hash-value v) do
	 (when limit 
	   (if (>= count limit) (return)))
         (format t "~a ~a ~%" k v)
	 (incf count))))

(defmacro setf+ (string1 &rest string2)
  "A shortcut for str+:
 (setf x (str+ y z)) is the same as (setf+ x y z)"
  `(setf ,string1 (str+ ,string1 ,@string2)))

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

(defun str-right (string num)
  "Returns num characters from the right of string. E.g. (str-right \"test\" 2) => \"st\""
  (let ((end nil)
	(start nil))
    (setf end (length string))
    (setf start (- end num))
    (when (>= start 0)
      (setf string (subseq string start end)))))

(defun str-left (string num)
  "Returns num characters from the left of string. E.g. (str-right \"test\" 2) => \"te\""
  (when (<= num (length string))
    (setf string (subseq string 0 num))))

(defun get-cdt-class-iri (ada-code)
  "Returns the IRI for the CDT class that is associated with an ADA code. The function reads the last 4 charcters.  For example if the ADA code is 'D2390', the CDT class is determine by reading the '2390'. If the ada-code is less than 4 characters long, nil is returned."
  (let ((cdt-iri nil))
    ;; get last 4 characters of ada code
    (setf ada-code (str-right ada-code 4))
    (when ada-code
      (setf cdt-iri (make-uri (str+ *cdt-iri-base* "CDT_000" ada-code))))
    ;; return the cdt iri
    cdt-iri))

(defun get-ohd-import-axioms ()
  "Returns a list of axioms for importing ontologies that are needed to link procedures and finding to patients, providers, and visits.  For example, if a crown procedure was performed, we need to know the patient it was performed on, provider who did the procedure, and when the procedure occurred.  This requires the we import patient, provider, and visit ontologies."
  (let ((axioms nil))
    ;; import the ohd, patient, provider, dental visits, and dental exam ontologies
    (push `(imports ,(make-uri *ohd-ontology-iri*)) axioms)
    (push `(imports ,(make-uri *eaglesoft-dental-patients-ontology-iri*)) axioms)
    ;;(push `(imports ,(make-uri *eaglesoft-dental-providers-ontology-iri*)) axioms)
    ;;(push `(imports ,(make-uri *eaglesoft-dental-visits-ontology-iri*)) axioms)
    ;;(push `(imports ,(make-uri *eaglesoft-dental-exams-ontology-iri*)) axioms)

    ;; return axioms
    axioms))

(defun get-ohd-declaration-axioms ()
  "Returns a list of declaration axioms for data properties, object properties, and annotations that are used in the ohd ontology."
  (let ((axioms nil))

    ;; declare data properties
    (push `(declaration (data-property !'occurrence date'@ohd)) axioms)
    (push `(declaration (data-property !'patient ID'@ohd)) axioms)

    ;; declare object property relations
    (push `(declaration  (object-property !'is part of'@ohd)) axioms)
    (push `(declaration  (object-property !'inheres in'@ohd)) axioms)
    (push `(declaration  (object-property !'has participant'@ohd)) axioms)
    (push `(declaration  (object-property !'is located in'@ohd)) axioms)
    (push `(declaration  (object-property !'is about'@ohd)) axioms)

    ;; declare custom ohd annation
    (push `(declaration (annotation-property !'asserted type'@ohd)) axioms)

    ;; return axioms
    axioms))


(defun get-ohd-instance-axioms (instance class)
  "Returns a list of axioms that 1) asserts the 'instance' to be a member of 'class'; 2) annotates that 'class' is a 'asserted type' of 'instance'."
  (let ((axioms nil))
    (push `(declaration (named-individual , instance)) axioms)
    (push `(class-assertion ,class ,instance) axioms)
    (push `(annotation-assertion !'asserted type'@ohd ,instance ,class) axioms)

    ;;return axioms
    axioms))

(defun get-single-quoted-list (items)
  "Returns the value of items with single quotes around the value.  For example if the value of items is \"test\", this function returns \"'test'\".  Or if the value of items is \"one, two, three\", then \"'one', 'two', 'three'\" is returned."

  (let ((replace-value nil)
	(value-list nil))
    ;; ensure items is a string
    (setf items (format nil "~a" items))

    ;; get a list of values
    ;; note: all-matches returns a list of lists, so do (fist list-item) to get value
    (loop
       for list-item in (all-matches items "\\b\\w+\\b" 0) do
	 (push (first list-item) value-list))

    ;; remove any duplicates
    (setf value-list (remove-duplicates value-list :test 'equalp))


    ;; replace each value in list with a quoted value
    (loop
       for list-item in value-list do
	 (setf replace-value (str+ "'" list-item "'"))
	 (setf items (regex-replace-all list-item items replace-value)))

    ;; retun list of single-quoted items
    items))

(defun get-ohd-material-name (ada-code)
  "Returns the name the material used in a restoration based on ada code."
  (let ((material-name nil))
    ;; get the numeric part of code
    (setf ada-code (str-right ada-code 4))

    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equal)
       (setf material-name "amalgam"))
      ((member ada-code *resin-code-list* :test 'equal)
       (setf material-name "resin"))
      ((member ada-code *resin-with-noble-metal-code-list* :test 'equalp)
       (setf material-name "resin with noble metal"))
      ((member ada-code *resin-with-high-noble-metal-code-list* :test 'equalp)
       (setf material-name "resin with high noble metal"))
      ((member ada-code *resin-with-predominantly-base-metal-code-list* :test 'equalp)
       (setf material-name "resin with predominantly base metal"))
      ((member ada-code *gold-code-list* :test 'equal)
       (setf material-name "gold"))
      ((member ada-code *metal-code-list* :test 'equal)
       (setf material-name "metal"))
      ((member ada-code *ceramic-code-list* :test 'equal)
       (setf material-name "ceramic"))
      ((member ada-code *porcelain-code-list* :test 'equalp)
       (setf material-name "porcelain"))
      ((member ada-code *porcelain-fused-to-noble-metal-code-list* :test 'equalp)
       (setf material-name "porcelain fused to noble metal"))
      ((member ada-code *porcelain-fused-to-high-noble-metal-code-list* :test 'equalp)
       (setf material-name "porcelain fused to high noble metal"))
      ((member ada-code *porcelain-fused-to-predominantly-base-metal-code-list* :test 'equalp)
       (setf material-name "porcelain fused to predominantly base metal"))
      ((member ada-code *noble-metal-code-list* :test 'equalp)
       (setf material-name "noble metal"))
      ((member ada-code *high-noble-metal-code-list* :test 'equalp)
       (setf material-name "high noble metal"))
      ((member ada-code *predominantly-base-metal-code-list* :test 'equalp)
       (setf material-name "predominantly base metal"))
      ((member ada-code *stainless-steel-code-list* :test 'equalp)
       (setf material-name "stainless steel"))
      ((member ada-code *stainless-steel-with-resin-window-code-list* :test 'equalp)
       (setf material-name "stainless steel with resin window"))
      ((member ada-code *titanium-code-list* :test 'equalp)
       (setf material-name "titanium"))
      ((member ada-code *three-fourths-ceramic-code-list* :test #'equalp)
       (setf material-name "ceramic"))
      ((member ada-code *three-fourths-high-noble-metal-code-list* :test #'equalp)
       (setf material-name "predominantly high noble metal"))
      ((member ada-code *three-fourths-noble-metal-code-list* :test #'equalp)
       (setf material-name "predominantly noble metal"))
      ((member ada-code *three-fourths-predominantly-base-metal-code-list* :test #'equalp)
       (setf material-name "predominantly base metal"))
      ((member ada-code *three-fourths-resin-code-list* :test #'equalp)
       (setf material-name "resin"))
      (t (setf material-name "dental restoration material")))

    ;; return material name
    material-name))

(defun get-ohd-material-uri (ada-code)
  "Returns the uri of the material used in a restoration based on ada code."
  (let ((material-uri nil))
    ;; get the numeric part of code
    (setf ada-code (str-right ada-code 4))
    
    ;; compare ada code to respective global code lists
    (cond
      ((member ada-code *amalgam-code-list* :test 'equalp)
       (setf material-uri !'amalgam dental restoration material'@ohd))
      ((member ada-code *resin-code-list* :test 'equalp)
       (setf material-uri !'resin dental restoration material'@ohd))
      ((member ada-code *resin-with-noble-metal-code-list* :test 'equalp)
       (setf material-uri !'resin with noble metal dental restoration material'@ohd))
      ((member ada-code *resin-with-high-noble-metal-code-list* :test 'equalp)
       (setf material-uri !'resin with high noble metal dental restoration material'@ohd))
      ((member ada-code *resin-with-predominantly-base-metal-code-list* :test 'equalp)
       (setf material-uri !'resin with predominantly base metal dental restoration material'@ohd))
      ((member ada-code *gold-code-list* :test 'equalp)
       (setf material-uri !'gold dental restoration material'@ohd))
      ((member ada-code *metal-code-list* :test 'equalp)
       (setf material-uri !'metal dental restoration material'@ohd))
      ((member ada-code *ceramic-code-list* :test 'equalp)
       (setf material-uri !'ceramic dental restoration material'@ohd))
      ((member ada-code *porcelain-code-list* :test 'equalp)
       (setf material-uri !'porcelain dental restoration material'@ohd))
      ((member ada-code *porcelain-fused-to-noble-metal-code-list* :test 'equalp)
       (setf material-uri !'porcelain fused to noble metal dental restoration material'@ohd))
      ((member ada-code *porcelain-fused-to-high-noble-metal-code-list* :test 'equalp)
       (setf material-uri !'porcelain fused to high noble metal dental restoration material'@ohd))
      ((member ada-code *porcelain-fused-to-predominantly-base-metal-code-list* :test 'equalp)
       (setf material-uri !'porcelain fused to predominantly base metal dental restoration material'@ohd))
      ((member ada-code *noble-metal-code-list* :test 'equalp)
       (setf material-uri !'noble metal dental restoration material'@ohd))
      ((member ada-code *high-noble-metal-code-list* :test 'equalp)
       (setf material-uri !'high noble metal dental restoration material'@ohd))
      ((member ada-code *predominantly-base-metal-code-list* :test 'equalp)
       (setf material-uri !'predominantly base metal dental restoration material'@ohd))
      ((member ada-code *stainless-steel-code-list* :test 'equalp)
       (setf material-uri !'stainless steel dental restoration material'@ohd))
      ((member ada-code *stainless-steel-with-resin-window-code-list* :test 'equalp)
       (setf material-uri !'stainless steel with resin window dental restoration material'@ohd))
      ((member ada-code *titanium-code-list* :test 'equalp)
       (setf material-uri !'titanium dental restoration material'@ohd))
      ((member ada-code *three-fourths-ceramic-code-list* :test #'equalp)
       (setf material-uri !'ceramic dental restoration material'@ohd))
      ((member ada-code *three-fourths-high-noble-metal-code-list* :test #'equalp)
       (setf material-uri !'high noble metal dental restoration material'@ohd))
      ((member ada-code *three-fourths-noble-metal-code-list* :test #'equalp)
       (setf material-uri !'noble metal dental restoration material'@ohd))
      ((member ada-code *three-fourths-predominantly-base-metal-code-list* :test #'equalp)
       (setf material-uri !'predominantly base metal dental restoration material'@ohd))
      ((member ada-code *three-fourths-resin-code-list* :test #'equalp)
       (setf material-uri !'resin dental restoration material'@ohd))
      (t
       (setf material-uri !'dental restoration material'@ohd)))

    
    ;; return material uri
    material-uri))

(defun get-eaglesoft-finding-type-iri (description &key tooth-num)
  "Determine the iri of the type of finding based on eaglesoft's description of findings."
  (let ((finding-type-uri nil))

    ;; NOTE: the action codes are not reliable for certain findings. For example, I found a number
    ;; of records in which an unerupted tooth (action code 18) was entered in the database using 
    ;; the action code for missing teeth (action code 1)
    
    ;; make sure action-code is a string
    ;;(setf action-code (string-trim " " (format nil "~a" action-code)))

    (cond
      ((all-matches description "(?i)missing+" 0) ;; missing tooth finding
      ;;((equalp action-code "1") 
      
       ;; get iri associated with the type of missing tooth finding
       ;; note: tooth-num is the string representation of the tooth number
       ;; this can also be done using:
       ;;(make-uri-from-label-source :OHD "missing tooth finding" nil)
       ;; (make-uri-from-label-source :OHD
       ;;       (str+ "missing tooth " tooth-num " finding") nil)
       (setf tooth-num (string-trim " " (format nil "~a" tooth-num)))
       (setf finding-type-uri
	     (gethash (str+ "missing tooth " tooth-num " finding") *ohd-label-source*)))
      ((equalp action-code "18") ;; unerupted tooth finding
       (setf finding-type-uri !'unerupted tooth finding'@ohd))
      ((all-matches description "(?i)(caries+|Decay|Decalcification+|Dicalsification+)" 1)  ;; caries finding
       ;;(member action-code '("2" "3" "4") :test #'equalp)
       (setf finding-type-uri !'caries finding'@ohd))
      ((equalp description "none")
       (setf finding-type-uri !'no oral health issues reported'@ohd))
      (t
       (setf finding-type-uri !'dental finding'@ohd))) ;; dental finding


    ;; return uri
    finding-type-uri))

(defun get-eaglesoft-individual-finding-iri-base (description)
  "Return the IRI base for finding based on the description of the finding."
  (let ((iri-base nil))
    
    (cond
      ((all-matches description "(?i)missing+" 0) ;; missing tooth finding
       (setf iri-base *eaglesoft-individual-missing-teeth-findings-iri-base* ))
      ((all-matches description "(?i)(unerupted+|impacted+)" 1) ;; unerupted tooth finding
       (setf iri-base *eaglesoft-individual-unerupted-teeth-findings-iri-base*))
      ((all-matches description "(?i)(caries+|Decay|Decalcification+|Dicalsification+)" 1)  ;; caries finding
       (setf iri-base *eaglesoft-individual-caries-findings-iri-base*))
      (t
       (setf iri-base *eaglesoft-individual-dental-findings-iri-base*))) ;; dental finding

    ;; return iri base
    iri-base))

(defun get-eaglesoft-finding-rdfs-label (patient-id description &key tooth)
  "Determine the rdfs:label for a finding based on one of eaglesoft's action codes."
  (let ((label nil))
    ;; make sure tooth is a string
    (setf tooth (string-trim " " (format nil "~a" tooth)))

    (cond
      ((all-matches description "(?i)missing+" 0) ;; missing tooth finding
       (setf label (str+ "missing tooth " tooth " finding for patient " patient-id )))
      ((all-matches description "(?i)(unerupted+|impacted+)" 1) ;; unerupted tooth finding
       (setf label (str+ "unerupted tooth " tooth " finding for patient " patient-id )))
      ((all-matches description "(?i)(caries+|Decay|Decalcification+|Dicalsification+)" 1)  ;; caries finding
       (setf label (str+ "caries finding on tooth " tooth " of patient " patient-id )))
      ((equalp description "none")
       (setf label (str+ "no oral health issues reported for patient " patient-id )))
      (t
       (setf label (str+ "dental finding on tooth " tooth " of patient " patient-id )))) ;; dental finding

    ;; return rdfs:label
    label))

(defun get-eaglesoft-finding-iri (patient-id description 
				  &key tooth-name tooth-num instance-count finding-type occurrence-date)
  "Determine the iri of an individual/instance of finding"
  (let ((finding-uri nil))
    ;; get uri for instance of finding
    (setf finding-uri
	  (get-unique-individual-iri patient-id
				     :salt *eaglesoft-salt*
				     :iri-base (get-eaglesoft-individual-finding-iri-base description)
				     :class-type finding-type
				     :args `(,tooth-name ,instance-count ,occurrence-date "eaglesoft")))
    ;; return uri
    finding-uri))

;;;; database functions ;;;;

(defun table-exists (table-name url)
  "Returns t if table-name exists in db; nil otherwise; url specifies the connection string for the database."
  (let ((connection nil)
	;;(meta nil)
	(statement nil)
	(results nil)
	(query nil)
	(answer nil))

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
      (and statement (#"close" statement)))

    ;; return whether the table exists
    answer))

(defun delete-table (table-name url)
  "Deletes all the rows in table-name; url specificies the connection string for connecting to the database."
  (let ((connection nil)
	(statement nil)
	(query nil)
	(success nil))

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


;;;; specific to eagle soft ;;;;

(defmacro with-eaglesoft ((results query) &body body)
  "This macro is used for connecting to Eaglesoft database and retrieving records. The results parameter is the recordset that holds the retrieved records.  The query parameter is the query string that is applied to the database."
  (let ((connection (make-symbol "CONNECTION-"))
	 (statement (make-symbol "STATEMENT-"))
	 (url (make-symbol "DB-URL-")))
    `(let ((,connection nil)
	   (,statement nil)
	   (,url nil))
       (unwind-protect
	  (progn
	    (find-java-class "com.microsoft.sqlserver.jdbc.SQLServerDriver")
	    (setf ,url (get-eaglesoft-database-url))
	    (setf ,connection (#"getConnection" 'java.sql.DriverManager ,url))
	    (setf ,statement (#"createStatement" ,connection))
	    (setf ,results (#"executeQuery" ,statement ,query))

	    ,@body

	    )
       (and ,connection (#"close" ,connection))
       (and ,results (#"close" ,results))
       (and ,statement (#"close" ,statement))))))

(defun create-eaglesoft-ontologies (&key patient-id r21-provider-id
				    limit-rows save-to-path force-create-table)
  "Creates the suite of ontologies based on the Eaglesoft database. These ontologies are then returned in an associated list. The patient-id key creates the ontologies based on that specific patient. The r21-provider-id key creates a provider ontology based on the provider identified by the id. The limit-rows key restricts the number of records returned from the database.  It is primarily used for testing. When the save-to-path key (as a string) is provided, the created ontologies will saved to the specified path.  The force-create-table key is used to force the program to recreate the actions_codes and patient_history tables.
As an example of use, this call would create the ontologies for patient 3000: (create-eaglesoft-ontologies :patient-id 3000 :save-to-path \"/Users/williamduncan/Desktop/patient-3000/patient-3000-\").
And this call would create all the ontologies with the prefix \"r21-eaglesoft-\" in the ontologies directory:
    (create-eaglesoft-ontologies :save-to-path \"/Users/williamduncan/repos/svn/ohd-origninal/src/ontology/pitt-ub-ohsu-r21/r21-eaglesoft-\")"
  (let ((crowns nil)
	(dental-patients nil)
	(dental-providers nil)
	(endodontics nil)
	(fillings nil)
	(missing-teeth nil)
	(unerupted-teeth nil)
	(caries nil)
	(surgical-extractions nil)
	(inlays nil)
	(onlays nil)
	(veneers nil)
	(oral-evaluations nil)
	(visits nil))


    ;; create ontologies
    (setf crowns (get-eaglesoft-crowns-ont
		  :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf dental-patients (get-eaglesoft-dental-patients-ont
		  :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf dental-providers (get-eaglesoft-dental-providers-ont
		  :r21-provider-id r21-provider-id
		  :limit-rows limit-rows :force-create-table force-create-table))
    (setf endodontics (get-eaglesoft-endodontics-ont
		  :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf fillings (get-eaglesoft-fillings-ont
		  :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf missing-teeth (get-eaglesoft-missing-teeth-findings-ont
		   :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf unerupted-teeth (get-eaglesoft-unerupted-teeth-findings-ont
		  :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf caries (get-eaglesoft-caries-findings-ont
		  :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf surgical-extractions (get-eaglesoft-surgical-extractions-ont
		  :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf inlays (get-eaglesoft-inlays-ont
       	          :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf onlays (get-eaglesoft-onlays-ont
	          :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf veneers (get-eaglesoft-veneers-ont
	          :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf oral-evaluations (get-eaglesoft-oral-evaluations-ont
	          :patient-id patient-id :limit-rows limit-rows :force-create-table force-create-table))
    (setf visits (get-eaglesoft-dental-visits-ont
		  :patient-id patient-id :r21-provider-id r21-provider-id
		  :limit-rows limit-rows :force-create-table force-create-table))

    ;; check to save
    (when save-to-path
      ;; ensure save-to-path is a string
      (setf save-to-path (format nil "~a" save-to-path))

      ;; billd: I decided the commented out code below was too limiting
      ;;        for the various ways I might want to save files
      ;; ;; make sure save-to-path ends with a "/"
      ;; (when (not (equal (str-right save-to-path 1) "/"))
      ;;   (setf save-to-path
      ;; 	  (str+ save-to-path "/")))

      ;; ;; if a patient-id has been given, append it to directory
      ;; ;; this has the affect of prepennding the patient-id to the file name
      ;; (when patient-id
      ;;   ;; ensure patient-id is a string
      ;;   (setf patient-id (format nil "~a" patient-id))
      ;;   (setf save-to-path
      ;; 	  (str+ save-to-path "patient-" patient-id "-")))

      ;; save each ontology
      (write-rdfxml crowns (str+ save-to-path "crowns.owl"))
      (write-rdfxml dental-patients (str+ save-to-path "dental-patients.owl"))
      (write-rdfxml dental-providers (str+ save-to-path "dental-providers.owl"))
      (write-rdfxml endodontics (str+ save-to-path "endodontics.owl"))
      (write-rdfxml fillings (str+ save-to-path "fillings.owl"))
      (write-rdfxml missing-teeth (str+ save-to-path "missing-teeth-findings.owl"))
      (write-rdfxml unerupted-teeth (str+ save-to-path "unerupted-teeth-findings.owl"))
      (write-rdfxml caries (str+ save-to-path "caries-findings.owl"))
      (write-rdfxml surgical-extractions (str+ save-to-path "surgical-extractions.owl"))
      (write-rdfxml inlays (str+ save-to-path "inlays.owl"))
      (write-rdfxml onlays (str+ save-to-path "onlays.owl"))
      (write-rdfxml veneers (str+ save-to-path "veneers.owl"))
      (write-rdfxml oral-evaluations (str+ save-to-path "oral-evaluations.owl"))
      (write-rdfxml visits (str+ save-to-path "dental-visits.owl")))

    ;; place ontologies in asscoiated list and return
    `((crowns . ,crowns)
      (dental-patients . ,dental-patients)
      (dental-providers . ,dental-providers)
      (endodontics . ,endodontics)
      (fillings . ,fillings)
      (missing-teeth . ,missing-teeth)
      (unerupted-teeth . ,unerupted-teeth)
      (caries . ,caries)
      (surgical-extractions . ,surgical-extractions)
      (inlays . ,inlays)
      (onlays . ,onlays)
      (veneers . ,veneers)
      (oral-evaluations . ,oral-evaluations)
      (visits . ,visits))))

(defun  search-eaglesoft-database-schema (&key table-name field-name)
  "Searches the Eaglesoft database schema for specified table and field names, and prints the results to the screen.  If not parameters are given, all the results are printed to sceen."
  (let ((connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(url nil))

    ;; set up connection string and query.
    (setf url (get-eaglesoft-database-url))

    (setf query
"SELECT
  dbo.sysobjects.name AS table_name,
  dbo.syscolumns.name AS field_name
FROM
  dbo.sysobjects
LEFT JOIN
  dbo.syscolumns
ON
  dbo.sysobjects.id = dbo.syscolumns.id
WHERE
  dbo.sysobjects.type = 'U'
OR dbo.sysobjects.type = 'V' ")

    ;; append additional search criteria to query string
    (when table-name
      (setf query (str+ query " AND table_name LIKE '%" table-name "%' ")))

    (when field-name
      (setf query (str+ query " AND field_name LIKE '%" field-name "%' ")))

    ;; append order info
    (setf query (str+ query " ORDER BY table_name, field_name "))
    ;;(pprint query)

    (unwind-protect
	 (progn
	   ;; connect to db and get data
	   (setf connection (#"getConnection" 'java.sql.DriverManager url))
	   (setf statement (#"createStatement" connection))
	   (setf results (#"executeQuery" statement query))

	   (loop while (#"next" results) do
		(format t "~a~a~a~%"
			(#"getString" results "table_name")
			#\tab
			(#"getString" results "field_name"))))

      	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement)))))

(defun get-eaglesoft-field-type-count (table-name &key field-name  print)
  "Returns a list that contains the totals (counts) of each distinct value in each field in the table specified by the table-name paramater.  The returned list has the following form: ((field1 ((value1 total) (value2 total) (value3 total)))).  The field-name paramater specifies the totals will only be for that field.  The print parameter specifies that the results will be printed to the screen."
  (let ((field-list nil)
	(count-list nil)
	(summary-list nil)
	(connection nil)
	(statement nil)
	(results nil)
	(query nil)
	(url nil))

    ;; set up connection string and query.
    (setf url (get-eaglesoft-database-url))

    ;; if no field name supplied given, get a list of the
    ;; of all field names; otherwise, if a field name is given
    ;; just create a list with that field
    (cond
      ((or (not field-name) (functionp field-name) (symbolp field-name))

       ;; build query string to get the first record from the table
       (setf query (str+ "SELECT TOP 1 * FROM " table-name))

       (unwind-protect
	    (progn
	      ;; connect to db and get data
	      (setf connection (#"getConnection" 'java.sql.DriverManager url))
	      (setf statement (#"createStatement" connection))
	      (setf results (#"executeQuery" statement query))

	      ;; build field name list based on the resultset's column names
	      (loop for column from 1 to (#"getColumnCount" (#"getMetaData" results))
		 for name = (#"getColumnName" (#"getMetaData" results) column) do
		   (if (or (not field-name) (and field-name (funcall field-name name)))
		   (push (#"getColumnName" (#"getMetaData" results) column) field-list))))

	 ;; database cleanup
	 (and connection (#"close" connection))
	 (and results (#"close" results))
	 (and statement (#"close" statement))))
      (t (push field-name field-list)))

    ;; sort field name list
    ;; note: I sort from greatest to least so that the fields will be in
    ;; alphabetical order when I push the count summaries on the list
    (setf field-list (sort field-list #'string-greaterp))

    ;; iterate over field name list and for each field get a count of each distinct value
    (loop
       for field in field-list do
	 (setf query
	       (str+ "SELECT DISTINCT " field ", COUNT(*) AS total "
		     " FROM " table-name
		     " GROUP BY " field
		     " ORDER BY total DESC "))

	 (unwind-protect
	      (progn
		;; connect to db and get data
		(setf connection (#"getConnection" 'java.sql.DriverManager url))
		(setf statement (#"createStatement" connection))
		(setf results (#"executeQuery" statement query))

		;; buld list of data value counts
		;; column 1 is the value of in the field
		;; column 2 is the count of the values
		(setf count-list
		      (loop while (#"next" results)
			 collect (list (#"getString" results 1) (#"getString" results 2))))

		(push (list field count-list) summary-list)

	   ;; database cleanup
	   (and connection (#"close" connection))
	   (and results (#"close" results))
	   (and statement (#"close" statement)))))

    ;; check to print out summary list
    (when print
      (loop
	 for field-data in summary-list do
	   ;; dispaly field name
	   (format t "~%~a~%" (first field-data))
	   (format t "----------------------------------------~%")

	   (loop
	      for data in (second field-data) do
				  (format t "~a~a~a~a~%"
					  #\tab
					  (first data) ; first is value in the field
					  #\tab
					  (second data))))) ; second is the total
    ;; return summary list of
    ;; field names and counts
    summary-list))

(defun get-eaglesoft-database-url ()
  "Returns the url string for connecting to Eaglesoft dabase.
Note: The file ~/.pattersondbpw must be on your system."
  (concatenate 'string
	       "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
	       (with-open-file (f "~/.pattersondbpw") (read-line f))))

(defun get-eaglesoft-salt ()
  "Returns the salt to be used with the md5 checksum.
Note: The ~/.pattersondbpw file is required to run this procedure."
  (with-open-file (f "~/.pattersondbpw") (read-line f)))

(defun get-eaglesoft-occurrence-date (table-name date-entered date-completed tran-date)
  "Returns the occurrence date as determined by the name of the table."
  (let ((occurrence-date nil))
    ;; all records in the paient_history table have either a date_entered,
    ;; date_completed, or tran_date value
    (cond
      ;; the transactions table only has a tran_date
      ((equalp table-name "transactions") (setf occurrence-date tran-date))
      (t
       ;; for existing_services and patient_conditions table
       ;; default to date-completed
       (setf occurrence-date date-completed)
       ;; if no date_completed, the set occurrence-date to date-entered
       (when (equalp date-completed (string-trim " " "n/a"))
	 (setf occurrence-date date-entered))))

    ;; return the occurrence date
    occurrence-date))

(defun get-eaglesoft-teeth-list (tooth-data)
  "Reads the tooth_data array and returns a list of tooth numbers referenced in the tooth_data (i.e., field) array. Note: This only returns a list of permanent teeth (i.e., teeth 1 - 32)."
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

(defun get-eaglesoft-surface-list (surface)
  "Reads the surface data field (a string of characters) and returns a list of the surfaces."
  (let ((surface-list nil)
	(surface-matches nil)
	(surface-letter nil)
	(surface-name nil))

    ;; surfaces sometimes contain numbers to indicate different kinds of caries (e.g., B5)
    ;; so, use regular expression to get a list of just the letters (e.g., "m" "f")
    (setf surface-matches  (all-matches surface "[a-z]|[A-Z]" 0))

    (loop
       for item in surface-matches do
	 ;; all-matches returns a list of single item list items that matched:
	 ;; e.g., "mf" -> (("m") ("f"))
	 ;; so, pull out the individual list items
	 (setf surface-letter (first item))

	 ;; get surface name associated with letter
	 (setf surface-name
	       (get-surface-name-from-surface-letter surface-letter))

	 ;; push surface name on to the list
	 (push surface-name surface-list))

    ;; return list of tooth surfaces
    surface-list))

(defun get-eaglesoft-dental-patient-iri (patient-id)
  "Returns an iri for a patient that is generated by the patient id."
  (let ((uri nil))
    (setf uri
	  (get-unique-individual-iri patient-id
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-dental-patients-iri-base*
				     :class-type !'human dental patient'@ohd
				     :args "eaglesoft"))
    ;; return uri
    uri))

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

(defun get-eaglesoft-mouth-iri (patient-id)
  "Returns a uri for a patient's mouth that is generated by the patient id."
  (let ((uri nil))
    (setf uri
	  (get-unique-individual-iri patient-id
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-fma-iri-base*
				     :class-type !'Mouth'@ohd
				     :args "eaglesoft"))
    ;; return uri
    uri))


(defun get-eaglesoft-secondary-dentition-iri (patient-id)
  "Returns an iri for a 'secondary dentition' that is generated by the patient id."
  (let ((uri nil))
    (setf uri
	  (get-unique-individual-iri patient-id
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-fma-iri-base*
				     :class-type !'Secondary dentition'@ohd
				     :args `("eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-dental-visit-iri (patient-id occurrence-date)
  "Returns a uri for a dental visit that is generated by the patient id and the date of the visit. Note: I use the patient-id / occurrence-date combo because for a given visit, multiple entries may exist in the database.  For example, when a patient has more than one tooth restored, each tooth restoration is a record in the database.  This way generating the iri prohibits generating iris for cases in which a patient might have visited the provider twice in the same day.  However, my guess is that these cases are rare."
  (let ((uri nil))
    (setf uri
	  (get-unique-individual-iri patient-id
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-dental-visits-iri-base*
				     :class-type !'dental visit'@ohd
				     :args `(,occurrence-date "dental visit" "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-dental-visit-name (patient-id occurrence-date)
  "Returns a string that is used as the label of a dental visit iri."
  (let ((visit-name nil))
    (setf visit-name (str+ "dental visit for patient " patient-id " on " occurrence-date))

    ;; return name of visit
    visit-name))

(defun get-eaglesoft-patient-provider-realization-axioms (process-uri patient-id provider-id provider-type record-id)
  "Returns a list of axioms that specify how a particular process (e.g., visit or exam) realizes patient and provider roles"
  (let ((axioms nil)
	(patient-uri nil)
	(patient-role-uri nil)
	(provider-uri nil)
	(provider-role-uri nil))

    ;; get patient iri and patient-role iri
    (setf patient-uri (get-eaglesoft-dental-patient-iri patient-id))
    (setf patient-role-uri (get-eaglesoft-dental-patient-role-iri patient-id))

    ;; if provider has been identified as a specific person use r21-provider-id;
    ;; otherwise use record-id
    (cond
      ((equalp provider-type "person")
       (setf provider-uri (get-eaglesoft-dental-provider-iri provider-id))
       (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri provider-id)))
      (t
       (setf provider-uri (get-eaglesoft-dental-provider-iri "x" :anonymous-id record-id))
       (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri "x" :anonymous-id record-id))))

    ;; process realizes patient and provider roles
    (push `(object-property-assertion !'realizes'@ohd ,process-uri ,patient-role-uri) axioms)
    (push `(object-property-assertion !'realizes'@ohd ,process-uri ,provider-role-uri) axioms)

    ;; return axioms
    axioms))

(defun get-eaglesoft-dental-exam-iri (patient-id occurrence-date)
  "Returns an iri for a dental exam that is generated by the pateint-id and date of exam. Note: I use the patient-id/occurrence-date combo because for a given exam, multiple entries may exist in the database.  For example, when it is discovered that a patient has caries on multiple teeth, each finding is a record in the database.  This way generating the iri prohibits generating iris for cases in which a patient might have two exams by the same provider on the same day.  However, my guess is that these cases are rare."
  (let ((uri nil))
    ;; check to see if provider-id is nil
    (setf uri (get-unique-individual-iri patient-id
					    :salt *eaglesoft-salt*
					    :iri-base *eaglesoft-individual-dental-exams-iri-base*
					    :class-type !'dental exam'@ohd
					    :args `(,occurrence-date "dental exam" "eaglesoft")))
    ;; return uri
    uri))

(defun get-eaglesoft-dental-exam-name (patient-id occurrence-date)
  "Returns a string that is used as the label of a dental exam iri."
  (let ((visit-name nil))
    (setf visit-name (str+ "dental exam for patient " patient-id " on " occurrence-date))

    ;; return name of visit
    visit-name))

(defun get-eaglesoft-dental-exam-axioms (exam-uri patient-id occurrence-date provider-id provider-type record-id)
  (let (axioms exam-name visit-uri patient-role-uri provider-role-uri)
    ;; declare instance of dental exam
    (push `(declaration (named-individual ,exam-uri)) axioms)
    (setf axioms (get-ohd-instance-axioms exam-uri !'dental exam'@ohd))

    ;; add occurrent date of exam
    (push `(data-property-assertion 
	    !'occurrence date'@ohd
	    ,exam-uri 
	    (:literal ,occurrence-date !xsd:date)) axioms)
    
    ;; add annotation about exam
    (setf exam-name (get-eaglesoft-dental-exam-name patient-id occurrence-date))
    (push `(annotation-assertion !rdfs:label ,exam-uri ,exam-name) axioms)
    
    ;; the visit that the dental exam is part of
    (setf visit-uri (get-eaglesoft-dental-visit-iri patient-id occurrence-date))
    (push `(object-property-assertion !'is part of'@ohd ,exam-uri ,visit-uri) axioms)

    ;; generate role uris for patient and provider
    (setf patient-role-uri (get-eaglesoft-dental-patient-role-iri patient-id))
    
    ;; if provider has been identified as a specific person use r21-provider-id; 
    ;; otherwise use record-id
    (cond
      ((equalp provider-type "person")
       (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri provider-id)))
      (t
       (setf provider-role-uri (get-eaglesoft-dental-provider-role-iri "x" :anonymous-id record-id))))

    ;; exams realizes roles
    (push `(object-property-assertion !'realizes'@ohd ,exam-uri ,patient-role-uri) axioms)
    (push `(object-property-assertion !'realizes'@ohd ,exam-uri ,provider-role-uri) axioms)
    
    ;; return axioms
    axioms))

(defun get-eaglesoft-dental-provider-iri (r21-provider-id &key anonymous-id)
  "Returns an iri for a provider that is generated by the r21-provider-id. The anonymous-id key is used to generate iri for anonymous providers."
  (let ((uri nil))
    ;; check for anonymous provider
    (when anonymous-id
      ;; ensure that anonymous-id is a string
      (setf anonymous-id (format nil "~a" anonymous-id))
      (setf r21-provider-id (str+ "anonymous eaglesoft dental health care provider" anonymous-id)))

    (setf uri (get-unique-individual-iri r21-provider-id
					    :salt *eaglesoft-salt*
					    :iri-base *eaglesoft-individual-dental-providers-iri-base*
					    :class-type !'dental health care provider'@ohd))
    ;; return uri
    uri))

(defun get-eaglesoft-dental-provider-role-iri (r21-provider-id &key anonymous-id)
  "Returns an iri for a provider roel that is generated by the r21-provider-id. The anonymous-id key is used to generate iri for anonymous providers."
  (let ((uri nil))
    ;; check for anonymous provider
    (when anonymous-id
      (setf anonymous-id (format nil "~a" anonymous-id)) ; ensure that anonymous-id is a string
      (setf r21-provider-id (str+ "anonymous eaglesoft dental provider role " anonymous-id)))

    (setf uri (get-unique-individual-iri r21-provider-id
					    :salt *eaglesoft-salt*
					    :iri-base *eaglesoft-individual-dental-providers-iri-base*
					    :class-type !'dental health care provider role'@ohd))
    ;; return uri
    uri))

(defun get-eaglesoft-dental-practice-iri (practice-id)
  "Returns an iri for a practice that is generated by the practice id."
  (let ((uri nil))
    (setf uri
	  (get-unique-individual-iri practice-id
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-dental-providers-iri-base*
				     :class-type !'dental health care organization'@ohd
				     :args "eaglesoft dental practice"))
    ;; return uri
    uri))


(defun get-eaglesoft-tooth-iri (patient-id tooth-type-iri)
  "Returns an iri for a patient's tooth that is generated by the patient id and the type of the tooth."
  (let ((uri nil))
    (setf uri
	  (get-unique-individual-iri patient-id
				     :salt *eaglesoft-salt*
				     :iri-base *eaglesoft-individual-teeth-iri-base*
				     :class-type tooth-type-iri
				     :args "eaglesoft"))
    ;; return uri
    uri))

(defun prepare-eaglesoft-db (&key force-create-table)
  "Tests whether the action_codes and patient_history tables exist in the Eaglesoft database. If the force-create-db key is given, then the tables tables are created regardless of whether they exist. This is needed when the table definitions change."
  (let ((url nil))
    ;; get connection info
    (setf url (get-eaglesoft-database-url))

    ;; test to see if action_codes table exists
    ;; if not then create it
    (when (or (not (table-exists "action_codes" url)) force-create-table)
      (create-eaglesoft-action-codes-or-patient-history-table "action_codes" url))

    ;; test to see if patient history table exists
    (when (or (not (table-exists "patient_history" url)) force-create-table)
      (create-eaglesoft-action-codes-or-patient-history-table "patient_history" url))))

(defun create-eaglesoft-action-codes-or-patient-history-table (table-name url)
  "Creates either the action_codes or patient_history table, as determined by the table-name parameter.  The url parameter specifies the connection string to the database."
  (let ((connection nil)
	(statement nil)
	(query nil)
	(success nil)
	(table-found nil))

    ;; check to see if the table is in the db
    (setf table-found (table-exists table-name url))

    ;; create query string for creating table
    (cond
      ((equal table-name "action_codes")
       (setf query (get-create-eaglesoft-action-codes-table-query)))
      ((equal table-name "patient_history")
       (setf query (get-create-eaglesoft-patient-history-table-query))))

    (unwind-protect
	 (progn
	   ;; connect to db and execute query
	   (setf connection (#"getConnection" 'java.sql.DriverManager url))
	   (setf statement (#"createStatement" connection))

	   ;; if the table is already in the db, delete all the rows
	   ;; this is to ensure that the created table will not have
	   ;; any duplicate rows
	   (when table-found (delete-table table-name url))

	   ;; create table
	   ;; on success a non-nil value is returned (usually the number of rows created)
	   ;; on failure nil is returned
	   (setf success (#"executeUpdate" statement query)))

      ;; database cleanup
      (and connection (#"close" connection))
      (and statement (#"close" statement)))

    ;; return success indicator
    success))

(defun get-create-eaglesoft-action-codes-table-query ()
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

(defun get-create-eaglesoft-patient-history-table-query ()
"
DROP TABLE IF EXISTS PattersonPM.PPM.patient_history

/* This line is needed to force Sybase to return all rows when populating the patient_history table. */
SET rowcount 0


/* Create and define the column names for the patient_history table. */
CREATE TABLE
  PattersonPM.PPM.patient_history
  (
    /* Define field names of patient_history. */
    row_id INT IDENTITY,
    patient_id INT,
    birth_date DATE,
    sex VARCHAR(1),
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
    surface_detail CHAR(23) NULL,
    provider_id CHAR(3) NULL,
    r21_provider_id SMALLINT NULL,
    r21_provider_type VARCHAR(20) NULL,
    practice_id SMALLINT NULL
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
    birth_date,
    sex,
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
    surface_detail,
    provider_id,
    r21_provider_id,
    r21_provider_type,
    practice_id
  )
/*
The existing_services table is the table for all findings in ES that are, essentially
procedures completed by entities outside the current dental office.
*/

SELECT
  existing_services.patient_id,
  birth_date =
  (
    -- This subquery finds the patient's birth date
    SELECT
      birth_date
    FROM
      patient
    WHERE
      patient.patient_id = existing_services.patient_id),
  sex =
  (
    -- This subquery finds the patient's sex
    SELECT
      sex
    FROM
      patient
    WHERE
      patient.patient_id = existing_services.patient_id),
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
    AND existing_services.line_number = existing_services_extra.line_number),

  provider_id,

  r21_provider_id =
  (
    SELECT
      r21_provider_id
    FROM
      r21_provider
    WHERE
      existing_services.provider_id = r21_provider.provider_id),

  r21_provider_type =
  (
    SELECT
      r21_provider_type
    FROM
      r21_provider
    WHERE
      existing_services.provider_id = r21_provider.provider_id),

  practice_id =
  (
    SELECT
      practice_id
    FROM
      r21_provider
    WHERE
      existing_services.provider_id = r21_provider.provider_id)
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
SELECT
  patient_conditions.patient_id,
  birth_date =
  (
    -- This subquery find the patient's birht date
    SELECT
      birth_date
    FROM
      patient
    WHERE
      patient.patient_id = patient_conditions.patient_id),
  sex =
  (
    -- This subquery finds the patient's sex
    SELECT
      sex
    FROM
      patient
    WHERE
      patient.patient_id = patient_conditions.patient_id),
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
      patient_conditions.counter_id = patient_conditions_extra.counter_id),

  provider_id,

  r21_provider_id =
  (
    SELECT
      r21_provider_id
    FROM
      r21_provider
    WHERE
      patient_conditions.provider_id = r21_provider.provider_id),

  r21_provider_type =
  (
    SELECT
      r21_provider_type
    FROM
      r21_provider
    WHERE
      patient_conditions.provider_id = r21_provider.provider_id),

  practice_id =
  (
    SELECT
      practice_id
    FROM
      r21_provider
    WHERE
      patient_conditions.provider_id = r21_provider.provider_id)
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
SELECT
  transactions.patient_id,
  birth_date =
  (
    -- This subquery find the patient's birht date
    SELECT
      birth_date
    FROM
      patient
    WHERE
      patient.patient_id = transactions.patient_id),
  sex =
  (
    -- This subquery finds the patient's sex
    SELECT
      sex
    FROM
      patient
    WHERE
      patient.patient_id = transactions.patient_id),
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
      transactions.tran_num = transactions_extra.tran_num),

  provider_id,

  r21_provider_id =
  (
    SELECT
      r21_provider_id
    FROM
      r21_provider
    WHERE
      transactions.provider_id = r21_provider.provider_id),

  r21_provider_type =
  (
    SELECT
      r21_provider_type
    FROM
      r21_provider
    WHERE
      transactions.provider_id = r21_provider.provider_id),

  practice_id =
  (
    SELECT
      practice_id
    FROM
      r21_provider
    WHERE
      transactions.provider_id = r21_provider.provider_id)
FROM
  transactions
WHERE
  type = 'S'
AND
  status = 'A'
AND
  -- There are some wacky dates in the db, so limit dates to 1999-2011
  YEAR(tran_date) BETWEEN 1999 AND 2011
AND patient_id IN
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
"
)

(defun how-to-create-the-r21-provider-table ()
  "Describes the process for creating the r21_provider table."

  "In order to create the provider ontology (i.e., the r21-eaglesoft-dental-providers.owl file), it was necessary to clean up the providers table, for the provider table contained a number of junk entries that didn't make sense, and individuals were entered twice (once for each practice), but there was no information indicating these entries were about the same person.

The steps for creating the r21_provider table are as follows:

1. Copy the provider table into a new table named 'r21_provider'.

2. Add the following columns to r21_provider:
  a. r21_provider_id as INT
  b. r21_provider_type as VARCHAR(20)

3. Delete from the r21_provider table records in which the provider_id does not appear in the patient_history table; i.e., provider's that have not been involved in a patient's history.  This has the effect of elimatiing a number of junk records.

4. Sort the r21_provider table by last_name and first_name, and manually assign a r21_provider_id to each provider (starting at 1).  In cases where two records appear to be the same person (e.g., the records contain the same last_name and first_name), give both of the records the same r21_provider_id number.

5. Assign a r21_provider_type to each entity using the following scheme.
  a. If the provider is identifiable as a named person, use the value 'person'.
  b. If the provider is temporory worker that is a person, use the value 'person temporary'.
  c. If the provider is a practice (as indentified in the practice table), use the value 'practice'.
  d. If none of a-c is applicable, use the value 'unknown'.
")

;;****************************************************************
;;; global variables ;;;;

;; Note: These must be loaded at the end of the file in order to call functions that
;; load the intitial values of the globlal variable.
;; An example of this is the *eaglesoft-salt* variable.  It is initialized by calling
;; the get-eaglesoft-salt procedure (above).

;; global variable used for to salt the md5 checksum for eaglesoft entities
;; it is loaded in get-eaglesoft-salt above
(defparameter *eaglesoft-salt* (get-eaglesoft-salt))

;; lists of ada codes that indicate a particual kind of material is used in a procedure
;; NB: only the numeric portion of the codes is given. this is b/c the current
;;     codes start with a "D" but older codes start with a "0"
(defparameter *amalgam-code-list*
  '("2140" "2150" "2160" "2161"))

(defparameter *resin-code-list*
  '("2330" "2331" "2332" "2335" "2390" "2391" "2392" "2393" "2394"
    "2650" "2651" "2652" "2662" "2663" "2664" "2710" "2932" "2960" "2961" "6710"))

(defparameter *three-fourths-resin-code-list* '("2712"))

(defparameter *resin-with-noble-metal-code-list* '("2722" "6722"))

(defparameter *resin-with-high-noble-metal-code-list* '("6720"))

(defparameter *resin-with-predominantly-base-metal-code-list* '("2721" "6721"))

(defparameter *gold-code-list* '("2410" "2420" "2430"))

(defparameter *metal-code-list* '("2510" "2520" "2530" "2542" "2543" "2544"))

(defparameter *ceramic-code-list* '("2610" "2620" "2630" "2642" "2643" "2644" "2740"
				    "6600" "6601" "6608" "6609" "6740"))

(defparameter *three-fourths-ceramic-code-list* '("2783" "6783"))

(defparameter *porcelain-code-list* '("2962"))

(defparameter *porcelain-fused-to-noble-metal-code-list* '("2752" "6752"))

(defparameter *porcelain-fused-to-high-noble-metal-code-list* '("2750" "6750"))

(defparameter *porcelain-fused-to-predominantly-base-metal-code-list* '("2751" "6751"))

(defparameter *noble-metal-code-list* '("2792" "6606" "6607" "6614" "6615" "6792"))

(defparameter *three-fourths-noble-metal-code-list* '("2782" "6782"))

(defparameter *high-noble-metal-code-list* '("2790" "6602" "6603" "6610" "6611" "6790"))

(defparameter *three-fourths-high-noble-metal-code-list* '("2780" "6780"))

(defparameter *predominantly-base-metal-code-list* '("2791" "6604" "6605" "6612" "6613" "6791"))

(defparameter *three-fourths-predominantly-base-metal-code-list* '("2781" "6781"))

(defparameter *titanium-code-list* '("2794" "6624" "6634" "6794"))

(defparameter *stainless-steel-code-list* '("2931"))

(defparameter *stainless-steel-with-resin-window-code-list* '("2933"))




;; global iri variables

;; ohd ontology
(defparameter *ohd-iri-base* "http://purl.obolibrary.org/obo/ohd/")
(defparameter *ohd-ontology-iri* "http://purl.obolibrary.org/obo/ohd/dev/ohd.owl")

;; CDT code ontology
(defparameter *cdt-iri-base* "http://purl.obolibrary.org/obo/")
(defparameter *cdt-ontology-iri* "http://purl.obolibrary.org/obo/cdt.owl")

;; base iri used for tooth surfaces in eaglesoft
(defparameter *eaglesoft-individual-surfaces-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")

;; base iri used for individual teeth in eaglesoft
(defparameter *eaglesoft-individual-teeth-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")

;; base iri used for anatomical parts in eaglesoft
(defparameter *eaglesoft-individual-fma-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")

;; eaglesoft dental patients ontology
(defparameter *eaglesoft-individual-dental-patients-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-dental-patients-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-dental-patients.owl")

;; eaglesoft fillings ontology
(defparameter *eaglesoft-individual-fillings-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-fillings-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-fillngs.owl")

;; eaglesoft inlays ontology
(defparameter *eaglesoft-individual-inlays-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-inlays-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-inlays.owl")

;; eaglesoft onlays ontology
(defparameter *eaglesoft-individual-onlays-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-onlays-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-onlays.owl")

;; eaglesoft crowns ontology
(defparameter *eaglesoft-individual-crowns-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-crowns-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-crowns.owl")

;; eaglesoft veneers ontology
(defparameter *eaglesoft-individual-veneers-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-veneers-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-veneers.owl")

;; eaglesoft surgical extractions ontology
(defparameter *eaglesoft-individual-surgical-extractions-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-surgical-extractions-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-surgical-extractions.owl")

;; eaglesoft endodontics ontology
(defparameter *eaglesoft-individual-endodontics-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-endodontics-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-endodontics.owl")

;; eaglesoft dental findings (in general)
(defparameter *eaglesoft-individual-dental-findings-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-missing-dental-findings-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-dental-findings.owl")

;; eaglesoft missing teeth findings
(defparameter *eaglesoft-individual-missing-teeth-findings-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-missing-teeth-findings-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-missing-teeth-findings.owl")

;; eaglesoft unerupted teeth findings
(defparameter *eaglesoft-individual-unerupted-teeth-findings-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-unerupted-teeth-findings-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-unerupted-teeth-findings.owl")

;; eaglesoft caries findings
(defparameter *eaglesoft-individual-caries-findings-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-caries-findings-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-caries-findings.owl")

;; eaglesoft dental providers
(defparameter *eaglesoft-individual-dental-providers-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-dental-providers-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-dental-providers.owl")

;; eaglesoft oral evaluations
(defparameter *eaglesoft-individual-oral-evaluations-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-oral-evaluations-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-oral-evaluations.owl")

;; eaglesoft dental visits
(defparameter *eaglesoft-individual-dental-visits-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-dental-visits-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-dental-visits.owl")

;; eaglesoft dental exams
(defparameter *eaglesoft-individual-dental-exams-iri-base*
  "http://purl.obolibrary.org/obo/ohd/individuals/")
(defparameter *eaglesoft-dental-exams-ontology-iri*
  "http://purl.obolibrary.org/obo/ohd/dev/r21-eaglesoft-dental-exams.owl")
