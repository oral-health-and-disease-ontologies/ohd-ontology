(defun test-jdbc-iterator-1 ()
  "Simple test using the jdbc resultset. note: the has-next method cannot be called."
  (let ((it nil)
	(rs nil)
	(sql-string "select top 3 patient_id, sex, birth_date from patient")
	;;(sql-string "select top 3 patient_id from patient where patient_id = 1208948557654"
	(jdbc-url (str+ "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
			(with-open-file (f "~/.pattersondbpw") (read-line f)))))
    (unwind-protect
	 (progn
	   ;; this way of opening the iterator can only use the resultsets default properties
	   ;; currently, the default resultset type is jdbc resultset
	   (setf it (get-jdbc-iterator
		     :jdbc-url jdbc-url 
		     :sql-string sql-string))

	   ;;(setf rs (get-jdbc-resultset :jdbc-url jdbc-url :sql-string sql-string))
	   ;;(setf it (iterator rs))

	   ;;(print-db it)
	   ;;(print-db (resultset it))
	   ;;(print-db (contents (current-results (resultset it))))
	   ;;(print-db (next it))
	   
	   (loop 
	      for rs = (next it)
	      while (has-current it)
	      for i from 1 to 5 do 
	        ;;(pprint (#"getString" rs "patient_id"))
		(pprint (str+ :space 
			      (value rs "patient_id")
			      (value rs "sex")
			      (value rs "birth_date")))
		)
	   ) ;; end progn
      (when it
	(close-iterator it))
      (when rs
	(close-resultset rs)))
    ))

(defun test-jdbc-iterator-2 ()
  "Simple test using the cached resultset. This allows has-next to be called."
  (let ((ds nil)
	(rs nil)
	(it nil)
	(sql-string "select top 3 patient_id, sex, birth_date from patient")
	;;(sql-string "select top 3 patient_id from patient where patient_id = 1208948557654"
	(jdbc-url (str+ "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
			(with-open-file (f "~/.pattersondbpw") (read-line f)))))
    (unwind-protect
	 (progn
	   ;; to use non-default resultset properties you must explicitly build the resultset
	   (setf ds (open-data-source (make-instance 'jdbc-data-source :jdbc-url jdbc-url)))
	   (setf rs (make-instance 'jdbc-resultset
				   :data-source ds
				   :with-cached-results t
				   :sql-string sql-string))
	   (setf it (get-jdbc-iterator :resultset rs))
	   ;;(print-db it)
	   ;;g(print-db (has-next it))
	   (loop 
	      while (has-next it)
	      for r = (next it)
	      for i from 1 to 5 do 
		;;(pprint (#"getString" (results it) "patient_id"))
		;;(pprint (#"getString" r "patient_id"))
		;;(pprint (value it "patient_id"))
		(pprint (str+ :space 
			      (value rs "patient_id") 
			      (value rs "sex")
			      (value rs "birth_date")))
		)
	   ) ;; end progn
      (when it
	(close-iterator it))
      (when rs
	(close-resultset rs))
      (when ds
	(close-data-source ds)))
    ))

(defun test-jdbc-iterator-3 ()
  "Simple test using resultset as a list (i.e., results returned using sql-query)."
  (let ((it nil)
	(rs nil)
	(ds nil)
	(sql-string "select top 3 patient_id, sex, birth_date from patient")
	;;(sql-string "select top 3 patient_id from patient where patient_id = 1208948557654"
	(jdbc-url (str+ "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
			(with-open-file (f "~/.pattersondbpw") (read-line f)))))
    (unwind-protect
	 (progn
	   ;; to use non-default resultset properties you must explicitly build the resultset
	   (setf ds (open-data-source (make-instance 'jdbc-data-source :jdbc-url jdbc-url)))
	   (setf rs (open-resultset 
		     (make-instance 'jdbc-resultset
				    :data-source ds
				    :with-resultset-as-list t
				    :sql-string sql-string)))
	   (setf it (get-jdbc-iterator :resultset rs))
	   
	   (loop 
	      while (has-next it)
	      for r = (next it)
	      for i from 1 to 4 do
	        ;;(print-db r)
		(pprint (str+ :space 
			      (value r "patient_id") 
			      (value r "sex")
			      (value r "birth_date")))
		)
	   ) ;; end progn
      (when it
	(close-iterator it))
      (when rs
	(close-resultset rs))
      (when ds
	(close-data-source ds)))

    ))

(defun test-jdbc-iterator-4 ()
  "Simple test using the resutls as list, but only having the results stored as a list.
The resultset (as a whole) is still based on calls to the data source."
  (let ((it nil)
	(rs nil)
	(ds nil)
	(sql-string "select top 3 patient_id, sex, birth_date from patient")
	;;(sql-string "select top 3 patient_id from patient where patient_id = 1208948557654"
	(jdbc-url (str+ "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
			(with-open-file (f "~/.pattersondbpw") (read-line f)))))
    (unwind-protect
	 (progn
	   ;; to use non-default resultset properties you must explicitly build the resultset
	   (setf ds (open-data-source (make-instance 'jdbc-data-source :jdbc-url jdbc-url)))
	   (setf rs (open-resultset 
		     (make-instance 'jdbc-resultset
				    :data-source ds
				    :with-results-as-list t
				    :sql-string sql-string)))
	   (setf it (get-jdbc-iterator :resultset rs))
	   ;;(print-db (has-next it))
	   (print-db (next it))
	   (loop 
	      while (has-next it)
	      for r = (next it)
	      for i from 1 to 4 do
		;;(print-db (contentes r))
		(pprint (str+ :space
			     (value it "patient_id")
			     (value it "sex")
			     (value it "birth_date")))
		)
	   ) ;; end progn
      (when it
	(close-iterator it))
      (when rs
	(close-resultset rs))
      (when ds
	(close-data-source ds)))
      ))


;; test the jdbc iterator by passing the jdbc-url to with-iterator
(defun test-with-jdbc-iterator-1 ()
  ;; test simple jdbc iterator; note: has-next() fails for this
  (with-iterator (it :jdbc-url (str+ "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
				     (with-open-file (f "~/.pattersondbpw") (read-line f)))
		     :sql-string "select top 3 patient_id, sex, birth_date from patient"
		     :with-resultset-as-list nil
		     :with-results-as-list nil
		     :iterator-fn nil)
    ;;(print-db it)
    (loop 
       for r = (next it)
       while (has-current it)
       for i from 1 to 4
       do
	 ;(print-db r)
	 (pprint (str+ :space 
		       (value r 0)
		       (value r 1)
		       (value r 2))))))

(defun test-with-jdbc-iterator-2 ()
  ;; test the CachedRowSetImpl option -> set :with-cached-results true
  (with-iterator (it :jdbc-url (str+ "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
				     (with-open-file (f "~/.pattersondbpw") (read-line f)))
		     :sql-string "select top 3 patient_id, sex, birth_date from patient"
		     :with-resultset-as-list nil
		     :with-results-as-list nil
		     :with-cached-results t
		     :iterator-fn nil)

    ;;(print-db it)
    (loop 
       while (has-next it)
       for r = (next it) 
       for i from 1 to 4
       do
         ;;(print-db (contents r))
	 ;;(print-db (type-of r))
	 (pprint (str+ :space 
		       (value r "patient_id")
		       (value r "sex")
		       (value r "birth_date"))))))

(defun test-with-jdbc-iterator-3 ()
  ;; test using resultset as list (this is the default option)
  (with-iterator (it :jdbc-url (str+ "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
				     (with-open-file (f "~/.pattersondbpw") (read-line f)))
		     :sql-string "select top 3 patient_id, sex, birth_date from patient"
		     :iterator-fn nil)
    ;;(print-db it)
    (loop 
       while (has-next it)
       for r = (next it) 
       for i from 1 to 4
       do
	 ;;(print-db r)
	 (pprint (str+ :space 
		       (value r "patient_id")
		       (value r "sex")
		       (value r "birth_date"))))))

(defun test-with-jdbc-iterator-4 ()
  ;; test using results as list but w/o the whole resultset in a list
  (with-iterator (it :jdbc-url (str+ "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
				     (with-open-file (f "~/.pattersondbpw") (read-line f)))
		     :sql-string "select top 3 patient_id, sex, birth_date from patient"
		     :with-resultset-as-list nil
		     :with-results-as-list t
		     :with-cached-results nil
		     :iterator-fn nil)
    ;;(print-db it)
    ;;(print-db (next it))
    ;;(print-db (has-next it))
    (loop 
       while (has-next it)
       for r = (next it) 
       for i from 1 to 4
       do
	 ;;(print-db r)
	 (pprint (str+ :space 
		       (value r "patient_id")
		       (value r "sex")
		       (value r "birth_date"))))))
	 

(defun test-with-jdbc-iterator-5 ()
  ;; create iterator using a custom funciton instead of keyword options
  (with-iterator (it :iterator-fn #'test-jdbc-iterator-fn-5)
    ;;(print-db it)
    (loop 
       while (has-next it)
       for r = (next it) 
       for i from 1 to 4
       do
       ;;(print-db r)
	 (pprint (str+ :space 
		       (value r "patient_id")
		       (value r "sex")
		       (value r "birth_date"))))))

(defun test-jdbc-iterator-fn-5 ()
  (let (data-source resultset iterator jdbc-url sql-string)
    (setf jdbc-url (str+ "jdbc:sqlanywhere:Server=PattersonPM;UserID=PDBA;password="
			 (with-open-file (f "~/.pattersondbpw") (read-line f))))
    (setf sql-string "select top 3 patient_id, sex, birth_date from patient")
	  
    ;; create connection to jdbc database
    (setf data-source (open-data-source (make-instance 'jdbc-data-source
						       :jdbc-url jdbc-url)))
       
    ;; create resultset for iterator
    (setf resultset (make-instance 'jdbc-resultset
				   :data-source data-source
				   :with-resultset-as-list t
				   :with-results-as-list t
				   :sql-string sql-string))
    ;; build iterator
    (setf iterator 
	  (get-jdbc-iterator :resultset resultset))

  ;; return iterator
  iterator))


(defun test-with-jdbc-iterator-6 ()
  ;; test fv "field-value" function
  (with-iterator (it :iterator-fn #'test-jdbc-iterator-fn-5)
    ;;(print-db it)
    (loop 
       while (next it)
       for i from 1 to 4
       do
       	 (pprint (str+ :space 
		       (fv "patient_id")
		       (fv "sex")
		       (fv "birth_date"))))))

(defun test-with-jdbc-iterator-7 ()
  ;; test with-jdbc-iterator
  (with-jdbc-iterator 
      (it :iterator-fn #'project-jdbc-iterator)
    ;;(print-db it)
    (loop 
       while (has-next it)
       for r = (next it) 
       for i from 1 to 4
       do
	 ;;(print-db r)
	 (pprint (str+ :space 
		       (value r "patient_id")
		       (value r "sex")
		       (value r "birth_date"))))))

(defun display-jdbc-resultset-properties (rs &key num-next-calls)
  (pprint "resultset properties before next calls")

  (print-db rs)
  (print-db (#"getRow" rs))
  (print-db (#"isBeforeFirst" rs))
  (print-db (#"isAfterLast" rs))
  (print-db (#"isClosed" rs))

  (pprint "-------------------------------")

  (when (and nil (> num-next-calls 0))
    (loop 
	 repeat num-next-calls do
	 (#"next" rs))

    (pprint (str+ "resultset properties after " num-next-calls " next calls"))
    
      (print-db rs)
      (print-db (#"getRow" rs))
      (print-db (#"isBeforeFirst" rs))
      (print-db (#"isAfterLast" rs))
      (print-db (#"isClosed" rs))))

    
