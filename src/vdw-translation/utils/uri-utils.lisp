;; global variable used for to salt the md5 checksum for vdw instances
;; Note: The ~/vdw-data/vdw-salt file is required to generate *vdw-salt*
(defparameter *vdw-salt* (with-open-file (f "~/vdw-data/vdw-salt") (read-line f)))

(defun vdw-uri-base-string ()
  "http://purl.obolibrary.org/obo/vdw/OHD_")

(defun vdw-ontlogy-iri-string ()
   "http://purl.obolibrary.org/obo/vdw/ohd-vdw.owl")

;; (defun vdw-ontlogy-iri ()
;;   (make-uri (vdw-ontlogy-iri-string)))

(defun make-vdw-uri (id &key base)
  ;; zero pad the id
  (setf id (str-right (str+ "000000" id) 7))

  ;; if a base string is provide use it;
  ;; otherwise use vdw-uri-base-string
  (if base
      (make-uri (str+ base id))
      (make-uri (str+ (vdw-uri-base-string) id))))

(defun make-obo-uri (id)
  (make-uri (str+ "http://purl.obolibrary.org/obo/" id)))

;;(defun vdw-import-axioms ()
;;  `((imports ,(vdw-ontlogy-iri-string))))

(defun vdw-declaration-axioms ()
  
  )

(defun instance-of (instance class)
  (when (and instance class)
  `((declaration (named-individual ,instance))
    (class-assertion ,class ,instance)
    (annotation-assertion !'asserted type'@ohd ,instance ,class))))

(defun has-label (uri label)
  (when (and uri label)
    `(annotation-assertion !rdfs:label ,uri ,label)))

(defun participates-in (continuant process)
  (when (and continuant process)
    `(object-property-assertion !'participates in'@ohd ,continuant ,process)))

(defun realizes (process realizable)
  (when (and process realizable)
    `(object-property-assertion !'realizes'@ohd ,process ,realizable)))

(defun located-in (entity location)
  (when (and entity location)
    `(object-property-assertion !'is located in'@ohd ,entity ,location)))

(defun part-of (entity1 entity2)
  (when (and entity1 entity2)
    `(object-property-assertion !'is part of'@ohd ,entity1 ,entity2)))

(defun is-dental-restoration-of (entity1 entity2)

  )

(defun uri-id (uri)
  "Return numeric (or id part) of an uri.
e.g.: (uri-id http://www.ex.com/1234) -> 1234"
  (let (position)
    ;; reverse the full uri
    ;; e.g.: (reverse (uri-full !obo:test))
    ;;       -> "tset/obo/gro.yrarbilobo.lrup//:ptth"
    (setf uri (reverse (uri-full uri)))

    ;; search for first "/" in uri
    (setf position (search "/" uri :test #'equalp))

    ;; grab part of uri up to postion
    (setf uri (str-left uri position))

    ;; re-reverse uri and return
    (reverse uri)))


(defun remove-leading-! (uri)
  "Removes the leading '!' from a uri and returns the uri as a string.
If no leading '!' is present, the uri is simply returned as string."
  (setf uri (format nil "~a" uri))
  (when (equal "!" (subseq uri 0 1)) (setf uri (subseq uri 1)))
  ;; return uri
  uri)

(defun encode (string &key salt)
  "Returns and md5 checksum of the string argument.  When the salt argument is present, it used in the computing of the md5 check sum."
  (let ((it nil))
    ;; check for salt
    (when salt
      (setf string (format nil "~a~a" salt string)))
    
    (setf it (new 'com.hp.hpl.jena.shared.uuid.MD5 string))
    
    (#"processString" it)
    (#"getStringDigest" it)))

(defun make-uri-id (string &key (salt *vdw-salt*) class-type uri-base args)
  "Returns a unique uri identifier for instances in the ontology by doing a md5 checksum on the string parameter. 
An optional md5 salt value is specified by the salt key value (i.e., :salt salt). 
The class-type argument (i.e., :class-type class-type) concatentates the class type to the string.  This parameter is highly suggested, since it helps guarntee that uri will be unique.  
The uri-base (i.e., :uri-base uri-base) is prepended to the uri.  For example, (get-unique-uri \"test\" :uri-base \"http://test.com/\" will prepend \"http://test.com/\" to the md5 checksum of \"test\".  
The args parmameter is used to specify an other information you wish to concatenate to the string paramenter.  Args can be either a single value or list; e.g., (get-unique-uri \"test\" :args \"foo\") (get-unique-uri \"test\" :args '(\"foo\" \"bar\")."
  
  ;; ensure that string param is a string
  (setf string (format nil "~a" string))

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
  (when uri-base
    (setf string (format nil "~aI_~a" uri-base string)))
  (make-uri string))
