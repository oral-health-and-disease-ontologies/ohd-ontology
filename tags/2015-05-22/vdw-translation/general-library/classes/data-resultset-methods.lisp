;;; generic methods for data-resultset

(defgeneric initialize-instance ((rs data-resultset) &key &allow-other-keys)
  (:documentation "Initializes the data-resultset."))

(defgeneric get-jdbc-resultset (&key &allow-other-keys)
  (:documentation "Returns a jdbc data-resultset."))

(defgeneric get-file-resultset (&key &allow-other-keys)
  (:documentation "Returns a file based data-resultset."))

(defgeneric open-resultset ((rs data-resultset) &key &allow-other-keys)
  (:documentation "Opens the data-resultset."))

(defgeneric value ((dr data-resource) field-id &key &allow-other-keys)
  (:documentation "Returns the value associated with current result's field-id in the data-resultset"))

(defgeneric read-record ((rs data-resultset) &key &allow-other-keys)
  (:documentation "Reads the current record from the result set"))

;;; methods for file resultset

(defmethod open-resultset ((rs file-resultset) &key &allow-other-keys)
  ;; create records to hold data
  (setf (line rs) (make-instance 'file-record :resultset rs))
  (setf (next-line rs) (make-instance 'file-record :resultset rs))
   
  ;; check for headers 
  (when (headers-line (data-source rs))
    (setf (headers rs) (get-headers rs)))
	    
  ;; check if line is give at which data starts
  (when (data-start-line (data-source rs))
    (move-to-data-start-line rs))
    
      
  ;; populate the contents of the next line of the file
  ;; this is used to enable the has-next() method of the iterator
  (split-record 
   (next-line rs) :data (read-record rs))
  
  ;; return resultset
  rs)

(defmethod get-headers ((rs file-resultset))
  (let ((headers nil)
	(headers-line (headers-line (data-source rs))))

    ;; check that header line is valid
    (when (not (plusp headers-line))
      (error "you must enter a positive integer for the headers line"))

    ;; move file pointer to 
    (loop 
       for i from 1 to headers-line 
       do 
	 (setf headers (read-record rs)))

    ;; return headers split into list
    (split-data headers :delimiter (delimiter (data-source rs)))))

(defmethod move-to-data-start-line ((rs file-resultset))
  (let ((offset nil)
	(headers-line (headers-line (data-source rs)))
	(data-start-line (data-start-line (data-source rs))))

    ;; check that data start line is valid
    (when (not (plusp data-start-line))
      (error "you must enter a positive integer for the data start line"))

    ;; if no headers-line then set it to 0
    (when (not headers-line) (setf headers-line 0))

    ;; check that the data starts after the headers
    (when (not (< headers-line data-start-line))
      (error "the data start line must come after the headers line"))

    ;; find the offset between headers and data
    (setf offset (- data-start-line headers-line))
    
    ;; now substract 1 from offset
    ;; this is so a call to next() will get the first line of data
    (decf offset)
    
    ;; move resultset record to line before the data start line
    (loop 
       for i from 1 to offset 
       do
	 (read-record rs))
    
    ;; return resultset
    rs))
			

(defmethod read-record ((rs file-resultset) &key &allow-other-keys)
  (read-line (stream (data-source rs)) nil :eof))


(defmethod split-record ((r file-record) &key data)
  ;; if data key arg not given, set data var to contents of record
  (when (not data)
    (setf data (contents r)))

  ;; split the data contents of the record accordingly
  (setf (contents r)	
	(split-data data :delimiter (delimiter (data-source (resultset r)))))
  
  ;; return the record
  r)

(defmethod split-data (data &key delimiter)
  ;; use split-at-regex to return a list of data elements
  ;; split according to the delimiter specification
  (cond
    ( ;; check for eof
     (eq :eof data) :eof)
    ( ;; custom delimiter string
     ;; note: this overrides the delimiter key
     (stringp delimiter)
     (split-at-regex data delimiter))
    ( ;; tab delimited file
     (eq 'tab delimiter)
     (split-at-regex data "\\t"))
    ( ;; comma delimited file
     (eq 'comma delimiter)
     (split-at-regex data "\\,"))
    ( ;; pipe delimited file
     (eq 'pipe delimiter)
     (split-at-regex data "\\|"))
    (t ;; otherwise just return the data
     data)))

(defmethod get-file-resultset (&key data-source filespec &allow-other-keys)
  ;; make instance of resultset
  (let ((rs (make-instance 'file-resultset)))
    ;; if filespec given, open that file
    (when filespec
      (setf data-source (open-data-source (make-instance 'file-data-source)
							 :filespec filespec)))
    
      ;; set references of key args
      (when data-source (setf (data-source rs) data-source))
  
      ;; before opening resultset verify that resultset has 
      ;; a data source and iterator
      ;; open the resultset
      (when (not (data-source rs)) (error "you must supply a data source for the resultset"))

      ;; open the resultset and create an iterator
      (open-resultset rs)
      (setf (iterator rs) (open-resultset-iterator rs))
    
      ;;(print-db (iterator rs))    
      ;; return resultset 
      rs))

(defmethod open-resultset-iterator ((rs file-resultset) &key &allow-other-keys)
  ;; if no iterator in resultset, create one and set its reference
  ;; back to the rs
  (when (not (iterator rs))
    (setf (iterator rs) (make-instance 'file-iterator)))

  ;; verify references 
  (setf (resultset (iterator rs)) rs)

  ;; return iterator
  (iterator rs))

(defmethod value ((rs file-resultset) field-id  &key &allow-other-keys)
  ;; return value for the current result of the resultset's iterator
  (value (line rs) field-id))

(defmethod close-resultset ((rs file-resultset))
  ;; close the resultset
  (close-data-source (data-source rs)))

;;; methods for jdbc resultset

(defmethod get-jdbc-resultset (&key data-source jdbc-url sql-string &allow-other-keys)
  ;; make instance of resultset
  (let ((rs (make-instance 'jdbc-resultset)))
    ;; if jdbc-url given create a connection to new data source
    (when jdbc-url
      (setf data-source (open-data-source (make-instance 'jdbc-data-source)
					  :jdbc-url jdbc-url)))
    
    ;; set references of key args
    (when data-source (setf (data-source rs) data-source))
    (when sql-string (setf (sql-string rs) sql-string))
  
    ;; before opening resultset verify that resultset has 
    ;; a data source, sql-string, and iterator
    ;; open the resultset
    (when (not (data-source rs)) (error "you must supply a data source for the resultset"))
    (when (not (sql-string rs)) (error "you must supply an sql string"))

    ;; open the resultset and create an iterator
    (open-resultset rs)
    (setf (iterator rs) (open-resultset-iterator rs))
    
    ;;(print-db (iterator rs))    
    ;; return resultset 
    rs))

(defmethod open-resultset ((rs jdbc-resultset) &key &allow-other-keys)
  ;; determine what kind of resultset to use
  ;; and populate the resultset records
  (cond
    ( ;; if the resultset is being returned as a list don't open the connection
     ;; the sql-query funciton handles opening/closing the connection
     (with-resultset-as-list rs)
     ;; create instance slots to hold records
     (setf (current-results rs) (make-instance 'record-list :resultset rs))
     (setf (next-results rs) (make-instance 'record-list :resultset rs))
     (setf (resultset-list rs)
	   (open-jdbc-resultset-list rs)))
    (t 
     ;; otherwise use jdbc resultset
     (cond
       ( ;; check if records are stored as lists
	(with-results-as-list rs)
	(setf (current-results rs) (make-instance 'record-list :resultset rs))
	(setf (next-results rs) (make-instance 'record-list :resultset rs))
	(setf (cached-results rs) (make-instance 'jdbc-results :resultset rs))
	
	;; cached-results will be used for db calls
	(setf (contents (cached-results rs))  (open-jdbc-resultset rs)))
       (t
	;; otherwise use jdbc results
	(setf (current-results rs) (make-instance 'jdbc-results :resultset rs))
	(setf (next-results rs) (make-instance 'jdbc-results :resultset rs))
	(setf (contents (current-results rs))  (open-jdbc-resultset rs))))
     ))
  
  ;; return resultset
  rs)
 
(defmethod open-resultset-iterator ((rs jdbc-resultset) &key &allow-other-keys)
  ;; if no iterator in resultset, create one and set its reference
  ;; back to the rs
  (when (not (iterator rs))
    (setf (iterator rs) (make-instance 'jdbc-iterator)))

  ;; verify references 
  (setf (resultset (iterator rs)) rs)

  ;; bind iterator closures functions
  (setf (iterator rs)
	(set-jdbc-iterator-closure-functions (iterator rs))))

   
(defmethod open-jdbc-resultset-list ((rs jdbc-resultset))
  (let (results-to-return)
    ;; resultset lists are retrieved using sql-query function
    ;; this returns a list of lists
    (unwind-protect
	 (progn
	   (multiple-value-bind (resultset-list field-names)
	       (sql-query (sql-string rs) (connection (data-source rs)))
	     ;; set the resultset and field names to values returned by sql-query
	     (setf results-to-return resultset-list)
	     (setf (field-names rs) field-names)))
      ;; data source is no longer needed
      (close-data-source (data-source rs)))

    ;; return results
    results-to-return))


(defmethod open-jdbc-resultset ((rs jdbc-resultset))
  (let (results-to-return)
    (setf (statement (data-source rs))
	  (#"createStatement" (connection (data-source rs))))
  
    ;; determine kind of jdbc resultset
    (cond 
      ( ;; use cached resultset
       (with-cached-results rs)
       (setf results-to-return (new 'CachedRowSetImpl))
       (#"populate" results-to-return 
		    (#"executeQuery" (statement (data-source rs)) (sql-string rs))))
      (t 
       ;; otherwise use jdbc resultset
       (setf results-to-return
	     (#"executeQuery" (statement (data-source rs)) (sql-string rs)))))

    ;; set resultset field names
    (setf (field-names rs)
	  (jdbc-resultset-field-names results-to-return))

    ;; return resultset
    results-to-return))

(defmethod close-resultset ((rs jdbc-resultset))
  ;; close the resultset unless it is a list
  (when (not (with-resultset-as-list rs))
    ;; check is results have been converted to list
    (cond
      ((with-results-as-list rs)
       (and (contents (cached-results rs))
	    (#"close" (contents (cached-results rs))))
       )
      (t 
       (and (contents (current-results rs))
	    (#"close" (contents (current-results rs)))))
    (close-data-source (data-source rs)))))


(defmethod value ((rs jdbc-resultset) field-id  &key &allow-other-keys)
  ;; return value for the current result of the resultset's iterator
  (value (current-results rs) field-id))

