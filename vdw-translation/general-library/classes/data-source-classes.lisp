(defclass data-source (data-resource)
  ((stream
    :accessor stream
    :initform nil)
   (iri 
    :accessor iri
    :initform nil)))

(defclass file-data-source (data-source)
  ((iri
    :accessor filespec
    :initarg :filespec
    :initform nil)
   (headers-line
    :accessor headers-line
    :initarg :headers-line
    :initform nil)
   (data-start-line
    :accessor data-start-line
    :initarg :data-start-line
    :initform nil)
   (delimiter
    :accessor delimiter
    :initarg :delimiter
    :initform 'tab))) ; default to tab delimited

(defclass jdbc-data-source (data-source) 
  ((iri
    :accessor jdbc-url
    :initarg :jdbc-url
    :initform nil)
   (stream 
    :accessor connection
    :initarg :connection
    :initform nil)
   (statement 
    :accessor statement 
    :initarg :statement
    :initform nil)))    

