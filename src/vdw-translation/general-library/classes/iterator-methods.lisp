;;; generic methods

(defgeneric open-iterator ((it iterator) &key &allow-other-keys)
  (:documentation "Connects the iterator to the jdbc resultset."))

(defgeneric get-file-iterator (&key &allow-other-keys)
  (:documentation "Returns a file iterator."))

(defgeneric get-jdbc-iterator (&key &allow-other-keys)
  (:documentation "Returns a jdbc iterator."))

;;; methods for file-iterator

(defmethod next ((it file-iterator))
  ;; set current line to next-line, move next-line forward; return current line
  (setf (contents (line (resultset it)))
	(contents (next-line (resultset it))))

  ;; if eof has been reached return nil
  (if (not (has-current it)) (return-from next nil))

  ;; populate the contents of the next line of the file
  ;; this is used to enable the has-next() method of the iterator
  (split-record 
   (next-line (resultset it)) :data (read-record (resultset it)))

  ;; finally return the current line of file
  (line (resultset it)))
  
(defmethod has-current ((it file-iterator))
   "Returns a boolean indicating if the current line is eof."
   (not (eq :eof (contents (line (resultset it))))))

(defmethod has-next ((it file-iterator))
   "Returns a boolean indicating if the next line is eof."
   (not (eq :eof (contents (next-line (resultset it))))))

(defmethod get-file-iterator (&key data-source filespec resultset &allow-other-keys)
  ;; make an instance of iterator
  (let ((it (make-instance 'file-iterator)))
    
    ;; if filespec given create a connection to new data source
    (when filespec
      (setf data-source 
	    (open-data-source (make-instance 'file-data-source :filespec filespec))))
    
    ;; verify data-source
    (when (not data-source) (error "you must supply a data-source or filespec"))

    ;; check of resultset needs to be created
    (when (not resultset)
      (setf resultset (make-instance 'file-resultset :data-source data-source)))
        
    ;; at this point resultset must have either been passed in or created 
    (when (not resultset) (error "no resultset found for iterator"))

    ;; set referencess to iterator's resultset
    (setf (resultset it) resultset)
    (setf (iterator resultset) it)
    
    ;; open the iterator's resultset: i.e., set up the resultset's records for iteration
    ;; this places the first line of data in the next-line slot
    (open-resultset resultset)
    
    ;;return iterator
    it))

(defmethod value ((it file-iterator) field-id  &key &allow-other-keys)
  ;; return value for the current result of the resultset's iterator
  (value (line (resultset it)) field-id))

(defmethod close-iterator ((it file-iterator))
  (when (resultset it)
    (close-resultset (resultset it))))

;;; methods for jdbc-iterator

(defmethod get-jdbc-iterator (&key data-source jdbc-url resultset sql-string  &allow-other-keys)
  ;; make an instance of iterator
  (let ((it (make-instance 'jdbc-iterator)))
    ;;(it nil)) ; used to set reference to resultset

    ;; if jdbc-url given create a connection to new data source
    (when jdbc-url
      (setf data-source (open-data-source (make-instance 'jdbc-data-source)
					  :jdbc-url jdbc-url)))

    ;; check it the data source is in resultset
    (when resultset (setf data-source (data-source resultset)))

    ;; verify data-source
    (when (not data-source) (error "you must supply a data-source or jdbc-url"))

    ;; if sql string is given, create a new resultset 
    (when sql-string
      (setf resultset (get-jdbc-resultset 
		       :data-source data-source 
		       :sql-string sql-string)))

    ;; at this point resultset must have either been passed in
    ;; or created using sql-string, so check this
    (when (not resultset) (error "no resultset found for iterator"))
    
    ;; check that resultset is has been opened
    ;; if it is closed there will be no contents 
    ;; in the current or results
    ;; note: opening the resultset also calls
    ;;       open-iterator (below)
    (when (not (current-results resultset))
      (setf resulset (open-resultset resultset)))

    ;; now set reference to for iterator to pass over
    ;; set the resultset's iterator to this iterator
    (setf it (open-resultset-iterator resultset))
        
    it))

(defmethod open-iterator ((it jdbc-iterator) &key &allow-other-keys)
  "Connect the iterator to the resultset."
  (setf it (set-jdbc-iterator-closure-functions it)))
  
(defmethod next ((it jdbc-iterator))
  "Returns the next record in the iterator resultset."
  (let ((record nil))
    ;; get next record
    (setf record (funcall (next-fn it)))
    
    ;; check to make sure there is a current record
    ;; this will NOT be the case when the iterator has moved past the last row
    ;; otherwise return the record
    (if (not (has-current it)) 
	nil
	record)))



(defmethod has-next ((it jdbc-iterator))
  "Returns a boolean indicating if there is a next result.
Note: non-cached java resultsets are not supported."
  (funcall (has-next-fn it)))

(defmethod has-current ((it jdbc-iterator))
  "Returns a boolean indicating if the iterator's current result is empty.
This will be false after the result has been iterated over."
  (funcall (has-current-fn it)))

(defmethod close-iterator ((it jdbc-iterator))
  (when (resultset it)
    (close-resultset (resultset it))))

(defmethod value ((it jdbc-iterator) field-id  &key &allow-other-keys)
  ;; return value for the current result of the resultset's jdbc iterator
  (value (current-results (resultset it)) field-id))
