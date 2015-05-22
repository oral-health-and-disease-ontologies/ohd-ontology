;;;; a number of funcitons for testing various aspects of database functionality ;;;;
;;;; such as using stored procedures, creating tables, and testing existence of a table ;;;

(defun test-stored-produre ()
  (let ((url nil)
	(connection nil)
	(statement nil)
	(results nil)
	(query nil))

    (setf url (concatenate 
	       'string 
	       "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
	       (with-open-file (f "~/.pattersondbpw") (read-line f))))

    ;; get query string for amalgam restorations
    (setf query "{call uspGetPatientById(?)}")
    

    (unwind-protect
	 (progn
	   ;; connect to db and get data
	   (setf connection (#"getConnection" 'java.sql.DriverManager url))
	   (setf statement (#"prepareCall" connection query))
	   (#"setInt" statement 1 3000)
	   (setf results (#"executeQuery" statement))
	   
	   (#"next" results)
	   (pprint (#"getString" results "first_name"))
	   (pprint (#"getString" results "last_name")))

      ;; database cleanup
      (and connection (#"close" connection))
      (and results (#"close" results))
      (and statement (#"close" statement)))))

(defun test-create-table ()
  (let ((url nil)
	(connection nil)
	(statement nil)
	(success nil)
	(query nil))
  
    (setf url (concatenate 
	       'string 
	       "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
	       (with-open-file (f "~/.pattersondbpw") (read-line f))))
    (unwind-protect
	 (progn
	   ;; connect to db and get data
	   (setf connection (#"getConnection" 'java.sql.DriverManager url))
	   (setf statement (#"createStatement" connection))

	   (setf query 
		 (concatenate
		  'string 
		  "create table mytest (test varchar(20)); "
		  "insert into mytest (test) values('this is a test'); "
		  "insert into mytest (test) values('this is another test');"))
	   
	   ;; just using "execute" returns nil on both success and failure
	   ;;(#"execute" statement query)
	   
	   ;; "executeUpdate" returns 1 on successl; nil on failure
	   ;; note: there is not a result to return or close during db cleanup
	   (setf success (#"executeUpdate" statement query)))

      ;; database cleanup
      (and connection (#"close" connection))
      (and statement (#"close" statement)))
    
    ;; return success of query
    success
    ))

(defun test-table-exists (table-name url)
  (let ((url nil)
	(connection nil)
	(meta nil)
	(statement nil)
	(results nil)
	(query nil)
	(answer nil)
	(javanull nil))
  
    (unwind-protect
	 (progn
	   
	   ;; create a null object to be used in with java calss
	   (setf javanull (make-immediate-object nil :ref))
	   
	   ;; connect to db and get data
	   (setf connection (#"getConnection" 'java.sql.DriverManager url))
	   (setf statement (#"createStatement" connection))

	   ;; note the use of javanull 
	   (setf meta (#"getMetaData" connection)) 
	   (setf results (#"getTables" meta javanull javanull "action_codes" "TABLE"))

	   (when (#"next" results) (setf answer t)))

      ;; database cleanup
      (and connection (#"close" connection))
      (and results (#"close" results))
     (and statement (#"close" statement)))))
