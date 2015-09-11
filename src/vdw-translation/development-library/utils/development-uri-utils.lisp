;; note: consider using (full-uri-string uri) e.g. (full-uri-string !obo:) => "http://purl.obolibrary.org/obo/"

(defun make-individual-uri(uri-base string &key salt class-type args)
  "Returns a unique uri for individuals in the ontology by doing a md5 checksum on the string parameter. 
The uri-base is prepended to the uri.  For example, (make-individual-uri \"test\" :uri-base \"http://test.com/\" will prepend \"http://test.com/\" to the md5 checksum of \"test\". When this argument is specified, the returned uri has the form \"http://uri-base.com/I_XXX\"; where \"http:uri-base.com\" is the value of the uri-base arument, and \"I_XXX\" is the value of the encoded string.
An optional md5 salt value is specified by the salt key value. 
The class-type argument concatentates the class type to the string.  This parameter is highly suggested, since it helps guarntee that uri will be unique.  
The args parmameter is used to specify an other information you wish to concatenate to the string paramenter. This done in order to help ensure that the checksum is unique.
Args can be either a single value or list; e.g., (make-individual-uri \"test\" :args \"foo\") (make-individual-uri \"test\" :args '(\"foo\" \"bar\")."
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

  ;; encode string and prepend uri-base
  (setf string (encode-string string :salt salt))
  (setf string (format nil "~aI_~a" uri-base string))
  (make-uri string))

(defun remove-leading-! (uri)
  "Removes the leading '!' from a uri and returns the uri as a string.
If no leading '!' is present, the uri is simply returned as string."
  (setf uri (format nil "~a" uri))
  (when (equal "!" (subseq uri 0 1)) (setf uri (subseq uri 1)))
  ;; return uri
  uri)

(defun encode-string (string &key salt)
  "Returns and md5 checksum of the string argument.  When the salt argument is present, it used in the computing of the md5 check sum."
  (let ((it nil))
    ;; check for salt
    (when salt
      (setf string (format nil "~a~a" salt string)))
    
    (setf it (new 'com.hp.hpl.jena.shared.uuid.MD5 string))
    
    (#"processString" it)
    (#"getStringDigest" it)))
