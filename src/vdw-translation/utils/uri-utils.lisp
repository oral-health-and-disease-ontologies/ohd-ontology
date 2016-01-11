;; global variable used for to salt the md5 checksum for vdw instances
;; Note: The ~/vdw-data/vdw-salt file is required to generate *vdw-salt*
(defparameter *vdw-salt* (with-open-file (f "~/vdw-data/vdw-salt") (read-line f)))

(defun vdw-uri-base-string ()
  "http://purl.obolibrary.org/obo/vdw/OHD_VDW_")

(defun vdw-ontlogy-iri-string ()
   "http://purl.obolibrary.org/obo/vdw/ohd-vdw.owl")

;; (defun vdw-ontlogy-iri ()
;;   (make-uri (vdw-ontlogy-iri-string)))

(defun make-vdw-uri (id &key
			  (instance-uri t)
			  (class-uri nil)
			  (uri-base (vdw-uri-base-string))
			  (salt *vdw-salt*)
			  (uri-base (vdw-uri-base-string))
			  class-type
			  args)
  (let (uri)
    ;; determine what method to use for generating uri
    (cond
      (instance-uri
       (setf id (format nil "~a" id))
       (setf uri
	     (make-vdw-instance-uri id
				    :salt salt
				    :uri-base uri-base
				    :class-type class-type
				    :args args)))
      (t
       (setf uri (make-vdw-class-uri id :uri-base uri-base))))
	       
    ;; return uri
    uri))

(defun make-obo-uri (id)
  (make-uri (str+ "http://purl.obolibrary.org/obo/" id)))

(defun make-icd9-uri (code)
  "Takes an ICD9 code and returns a uri.
e.g. (make-icd9-uri \"123.4\") -> <http://purl.org/NET/regenstrief/ICD9_123.4>"

  ;; ensure the code is string
  (setf code (format nil "~a" code))

  ;; make uri by appending code to icd9 base
  (make-uri (format nil "http://purl.org/NET/regenstrief/ICD9_~a" code)))

(defun make-vdw-cdt-code-uri (code)
  ;; zero pad code
  (setf code (str-right (str+ "000000" code) 7))

  ;; determine url part of uri
  ;; skip codes "99203" and "T1013"; they are not CDT codes and not being used
  (when (and (not (equalp code "99203"))
	     (not (equalp code "T1013")))
    (setf code (str+ "http://purl.obolibrary.org/obo/ohd/VDW_CDT_" code)))

  ;; return uri
  (make-uri code))

;;(defun vdw-import-axioms ()
;;  `((imports ,(vdw-ontlogy-iri-string))))


(defun patient-uri(study-id)
  (when (not *study-id2uri*)
    (load-study-id2uri-table))
  (gethash study-id *study-id2uri*))

(defun patient-role-uri (study-id)
  (make-vdw-uri study-id :class-type !'dental patient role'@ohd))

(defun provider-role-uri (provider-id)
  (make-vdw-uri provider-id :class-type !'dental health care provider role'@ohd))

(defun race-code-uri (id race-code)
  (make-vdw-uri
   id :class-type (race-code-type race-code)))

(defun restoration-type (code)
  ;; make sure code to procedure mapping table is loaded
  (when (not *code2proc-uri*)
    (load-code2proc-uri-table))

  ;; find procedure type
  (gethash code *code2proc-uri*))

(defun restoration-uri (study-id code date tooth-num)
  ;; make sure tooth-num and date are strings
  (setf date (str+ date))
  (setf tooth-num (str+ tooth-num))
  (make-vdw-uri
   study-id :class-type (restoration-type code) :args `(,code date tooth-num)))

  
(defun tooth-to-be-restored-role-uri (study-id tooth-num)
  ;; make sure tooth-num is a string
  (setf tooth-num (str+ tooth-num))
  (make-vdw-uri
   study-id :class-type !'tooth to be restored role'@ohd :args (tooth-type tooth-num)))

(defun tooth-uri (study-id tooth-num)
  ;; make sure tooth-num is a string
  (setf tooth-num (str+ tooth-num))
  (make-vdw-uri
   study-id :class-type (tooth-type tooth-num)))

(defun tooth-type (tooth-num)
  (let (tooth-type tooth-string)
    ;; form tooth string (e.g., "Tooth 1")
    (setf tooth-string (str+ "Tooth " tooth-num))
    
    ;; use the tooth string to look up tooth type
    (setf tooth-type (gethash tooth-string (label2uri *ohd-label-source*)))

    ;; return tooth type
    tooth-type))

(defun surface-uri (study-id tooth-num surface)
  ;; make sure tooth-num is a string
  (setf tooth-num (str+ tooth-num))

  ;; make sure surface is a string
  (setf surface (str+ surface))

  (make-vdw-uri
   study-id :class-type (surface-type surface) :args (tooth-uri study-id tooth-num)))

(defun surface-type (surface)
  (let ((uri nil))
    (cond
      ;; check for surface letters
      ((equalp surface-name "b") (setf uri !obo:FMA_no_fmaid_Buccal_surface_enamel_of_tooth))
      ((equalp surface-name "d") (setf uri !obo:FMA_no_fmaid_Distal_surface_enamel_of_tooth))
      ((equalp surface-name "i") (setf uri !obo:FMA_no_fmaid_Incisal_surface_enamel_of_tooth))
      ((equalp surface-name "f") (setf uri !obo:FMA_no_fmaid_Labial_surface_enamel_of_tooth))
      ((equalp surface-name "l") (setf uri !obo:FMA_no_fmaid_Lingual_surface_enamel_of_tooth))
      ((equalp surface-name "m") (setf uri !obo:FMA_no_fmaid_Mesial_surface_enamel_of_tooth))
      ((equalp surface-name "o") (setf uri !obo:FMA_no_fmaid_Occlusial_surface_enamel_of_tooth))

      ;; check for surface names
      ((equalp surface-name "buccal") (setf uri !obo:FMA_no_fmaid_Buccal_surface_enamel_of_tooth))
      ((equalp surface-name "distal") (setf uri !obo:FMA_no_fmaid_Distal_surface_enamel_of_tooth))
      ((equalp surface-name "incisal") (setf uri !obo:FMA_no_fmaid_Incisal_surface_enamel_of_tooth))
      ((equalp surface-name "labial") (setf uri !obo:FMA_no_fmaid_Labial_surface_enamel_of_tooth))
      ((equalp surface-name "facial") (setf uri !obo:FMA_no_fmaid_Labial_surface_enamel_of_tooth))
      ((equalp surface-name "lingual") (setf uri !obo:FMA_no_fmaid_Lingual_surface_enamel_of_tooth))
      ((equalp surface-name "mesial") (setf uri !obo:FMA_no_fmaid_Mesial_surface_enamel_of_tooth))
      ((equalp surface-name "occlusial") (setf uri !obo:FMA_no_fmaid_Occlusial_surface_enamel_of_tooth)) ; occlusal was mispelled in previous OHD versions
      ((equalp surface-name "occlusal") (setf uri !obo:FMA_no_fmaid_Occlusial_surface_enamel_of_tooth)))

    ;; return suface uri
    uri))

(defun surface-name (surface-letter)
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
      ((equalp surface-letter "o") (setf surface-name "occlusal")))

    ;; return surface name
    surface-name))


(defun race-code-type (race-code)
  (let (type)
    (cond
      ((equalp race-code "BA")
       (setf type !'VDW African American race code'@ohd))
      ((equalp race-code "AS")
       (setf type !'VDW Asian race code'@ohd))
      ((equalp race-code "MU")
       (setf type !'VDW more than one race race code'@ohd))
      ((equalp race-code "IN")
       (setf type !'VDW Native American race code'@ohd))
      ((equalp race-code "HP")
       (setf type !'VDW Pacific Islander race code'@ohd))
      ((equalp race-code "WH")
       (setf type !'VDW white race code'@ohd))
      ((equalp race-code "OT")
       (setf type !'VDW other race code'@ohd))
      ((equalp race-code "UN")
       (setf type !'VDW unknown race code'@ohd))
      (t
       (setf type !'record of incomplete race information'@ohd)))
    
    type))
  
(defun ethnicity-code-uri (id ethnicity-code)
  (make-vdw-uri id :class-type (ethnicity-code-type ethnicity-code)))

(defun ethnicity-code-type (ethnicity-code)
  (let (type)
    (cond
      ((equalp ethnicity-code "Y")
       (setf type !'VDW Hispanic ethnicity code'@ohd))
      (t
       (setf type !'record of incomplete ethnicity information'@ohd)))
    
    type))

(defun gender-role-uri (id gender-code)
  (make-vdw-uri id :class-type (gender-role-type gender-code)))

(defun gender-role-type (gender-code)
  (let (type)
    (cond
      ((equalp gender-code "M")
       (setf type !'male gender role'@ohd))
      ((equalp gender-code "F")
       (setf type !'female gender role'@ohd))
      (t
       (setf type !'record of incomplete gender information'@ohd)))
    
    type))

(defun provider-uri(provider-id)
  (when (not *provider-id2uri*)
    (load-provider-id2uri-table))
  (gethash provider-id *provider-id2uri*))

(defun specialty-code-uri (id specialty-code)
  (make-vdw-uri id :class-type (specialty-code-type specialty-code)))

(defun specialty-code-type (specialty-code)
  (let (type)
    (cond
      ((equalp specialty-code "EDO")
       (setf type !'VDW endodontics specialty code'@ohd))
      ((equalp specialty-code "DEN")
       (setf type !'VDW general dentistry specialty code'@ohd))
      ((equalp specialty-code "ORA")
       (setf type !'VDW oral surgery specialty code'@ohd))
      ((equalp specialty-code "TMD")
       (setf type !'VDW orofacial pain specialty code'@ohd))
      ((equalp specialty-code "ORD")
       (setf type !'VDW orthodontics specialty code'@ohd))
      ((equalp specialty-code "PDE")
       (setf type !'VDW pediatric dentistry specialty code'@ohd))
      ((equalp specialty-code "PER")
       (setf type !'VDW periodontics specialty code'@ohd))
      ((equalp specialty-code "PRO")
       (setf type !'VDW prosthetics specialty code'@ohd))
      (t
       (setf type !'record of incomplete provider specialty information'@ohd)))

    type))

(defun provider-occupation-code-uri (id type-code)
  (make-vdw-uri id :class-type (provider-occupation-code-type type-code)))

(defun provider-occupation-code-type (type-code)
  (let (type)
    (cond
      ((equalp type-code "17")
       (setf type !'VDW dental assistant type code'@ohd))
      ((equalp type-code "18")
       (setf type !'VDW dentist type code'@ohd))
      ((equalp type-code "30")
       (setf type !'VDW dental hygienist type code'@ohd))
      ((equalp type-code "42")
       (setf type !'VDW dental therapist type code'@ohd))
      ((equalp type-code "73")
       (setf type !'VDW registered nurse type code'@ohd))
      (t
       (setf type !'record of incomplete provider type information'@ohd)))

    type))

(defun provider-role-uri (id)
  (make-vdw-uri id :class-type !'dental health care provider role'@ohd))

(defun provider-role-type (provider-occupation-code-type specialty-code-type)
  (let (type)
    ;; determine the occupational role of provider
    ;; in case of the dentist, look for specialty role
    (cond
      ((equalp provider-occupation-code-type !'VDW dental assistant type code'@ohd)
       (setf type !'dental assistant role'@ohd))
      ((equalp provider-occupation-code-type !'VDW dental hygienist type code'@ohd)
       (setf type !'dental hygienist role'@ohd))
      ((equalp provider-occupation-code-type !'VDW dental therapist type code'@ohd)
       (setf type !'dental therapist role'@ohd))
      ((equalp provider-occupation-code-type !'VDW dentist type code'@ohd)
       ;; determine specialty of dentist
       (cond
	 ((equalp specialty-code-type !'VDW endodontics specialty code'@ohd)
	  (setf type !'endodontist role'@ohd))
	 ((equalp specialty-code-type !'VDW oral surgery specialty code'@ohd)
	  (setf type !'oral surgeon role'@ohd))
	 ((equalp specialty-code-type !'VDW orofacial pain specialty code'@ohd)
	  (setf type !'orofacial pain dentist role'@ohd))
	 ((equalp specialty-code-type !'VDW orthodontics specialty code'@ohd)
	  (setf type !'orthodontist role'@ohd))
	 ((equalp specialty-code-type !'VDW pediatric dentistry specialty code'@ohd)
	  (setf type !'pediatric dentist role'@ohd))
	 ((equalp specialty-code-type !'VDW periodontics specialty code'@ohd)
	  (setf type !'periodontist role'@ohd))
	 ((equalp specialty-code-type !'VDW prosthetics specialty code'@ohd)
	  (setf type !'prosthodontist role'@ohd))
	 (t ;; default for specialties type
	  (setf type !'dentist role'@ohd))))
       
      (t ;; default provider role type
       (setf type !'dental health care provider role'@ohd)))
    
    type))

(defun provider-uri (id)
  (make-vdw-uri id :class-type !'dental health care provider'@ohd))

(defun encounter-uri (id)
  (make-vdw-uri id :class-type !'dental visit'@ohd))

(defun facility-uri (facility-code)
  (make-vdw-uri facility-code :class-type !'dental health care organization'@ohd))

(defun provider-type (provider-role-type)
  (let (type)
    (cond
      ((equalp provider-role-type !'dental assistant role'@ohd)
       (setf type !'dental assistant'@ohd))
      ((equalp provider-role-type !'dental hygienist role'@ohd)
       (setf type !'dental hygienist'@ohd))
      ((equalp provider-role-type !'dental therapist role'@ohd)
       (setf type !'dental therapist'@ohd))
      ((equalp provider-role-type !'dentist role'@ohd)
       (setf type !'dentist'@ohd))
      ((equalp provider-role-type !'endodontist role'@ohd)
       (setf type !'endodontist'@ohd))
      ((equalp provider-role-type !'oral surgeon role'@ohd)
       (setf type !'oral surgeon'@ohd))
      ((equalp provider-role-type !'orofacial pain dentist role'@ohd)
       (setf type !'orofacial pain dentist'@ohd))
      ((equalp provider-role-type !'orthodontist role'@ohd)
       (setf type !'orthodontist'@ohd))
      ((equalp provider-role-type !'pediatric dentist role'@ohd)
       (setf type !'pediatric dentist'@ohd))
      ((equalp provider-role-type !'periodontist role'@ohd)
       (setf type !'periodontist'@ohd))
      ((equalp provider-role-type !'prosthodontist role'@ohd)
       (setf type !'prosthodontist'@ohd))
      (t ;; default provider type
       (setf type !'dental health care provider'@ohd)))

    type))

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

(defun make-vdw-instance-uri (string &key
				       (salt *vdw-salt*)
				       (uri-base (vdw-uri-base-string))
				       class-type
				       args)
  "Returns a unique uri identifier for instances in the ontology by doing a md5 checksum on the string parameter. 
An optional md5 salt value is specified by the salt key value (i.e., :salt salt). 
The class-type argument (i.e., :class-type class-type) concatentates the class type to the string.  This parameter is highly suggested, since it helps guarntee that uri will be unique.  
The uri-base (i.e., :uri-base uri-base) is prepended to the uri.  For example, (get-unique-uri \"test\" :uri-base \"http://test.com/\" will prepend \"http://test.com/\" to the md5 checksum of \"test\".  
The args parmameter is used to specify an other information you wish to concatenate to the string paramenter.  Args can be either a single value or list; e.g., (make-vdw-instance-uri \"test\" :args \"foo\") (make-vdw-instance-uri \"test\" :args '(\"foo\" \"bar\")."
  
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

(defun make-vdw-class-uri (id &key (uri-base (vdw-uri-base-string)))
  ;; zero pad the id
  (setf id (str-right (str+ "000000" id) 7))

  ;; if a uri-base string is provide use it;
  ;; otherwise use vdw-uri-base-string
  (if uri-base
      (make-uri (str+ uri-base id))
      (make-uri (str+ (vdw-uri-base-string) id))))
