
;;; generic methods for data-source class

(defgeneric initialize-instance ((ds data-source) &key &allow-other-keys)
  (:documentation "Initializes the data source."))

(defgeneric open-data-source ((ds data-source) &key &allow-other-keys) 
  (:documentation "Opens a stream (or connection) the data source."))

;;; methods for file-data-source

(defmethod open-data-source ((fs file-data-source) &key filespec &allow-other-keys)
  "Opens a file stream to the file specified by the data source's filespec slot.
If the filespec param is given, the filespec is set to that value."
  (when filespec
    (setf (filespec fs) filespec))

  (when (not (filespec fs))
	     (error "you must provide a file specificaiton for the data source."))

  ;; open a file stream
  (setf (stream fs) (open (filespec fs)))

  ;; return the data-source
  fs)


(defmethod close-data-source ((fs file-data-source))
  "Closes the file stream of the file data source."
  (when (stream fs)
    (close (stream fs))
    (setf (stream fs) nil)))

;;; methods for jdbc-data-source

(defmethod open-data-source ((js jdbc-data-source) &key jdbc-url &allow-other-keys)
  "Opens a connection to the jdbc data source."

  ;; set key value
  (when jdbc-url
    (setf (jdbc-url js) jdbc-url))


  ;; check for jdbc-url
  (when (not (jdbc-url js)) 
    (error "you must supply a jdbc url (i.e., connection string)"))

  ;; set connction to jdbc data source
  (when (search "jdbc:sqlite" (jdbc-url js))
    (#"forName" 'Class "org.sqlite.JDBC"))
  (setf (connection js) 
	(#"getConnection" 'java.sql.DriverManager (jdbc-url js)))

  ;;return the data source
  js)

(defmethod open-data-source-resultset ((js jdbc-data-source) &key sql-string &allow-other-keys)
  ;; although it listed as a key, the sql-string must be supplied
  (let ((rs (make-instance 'jdbc-results :sql-string sql-string)))
    (setf rs (open-resultset rs))))

(defmethod close-data-source ((js jdbc-data-source))
   "Closes the connection to the jdbc data source."
   (and (connection js) (#"close" (connection js)))
   (and (statement js) (#"close" (statement js))))

   



  
