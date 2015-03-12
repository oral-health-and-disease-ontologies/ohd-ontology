
;; this macro passes the body in a lambda function to the function with-iterator-1
(defmacro with-iterator 
    ((iterator  &key 
		filespec		; options for opening file
		headers-line
		data-start-line
		jdbc-url     ; options for connecting to jdbc database
		sql-string 
		(with-resultset-as-list t)
		(with-results-as-list t)
		with-cached-results
		iterator-fn)
		  ;; removing the default option ... causing problems
		  ;;(iterator-fn '#'project-iterator)) ; function to create the iterator
     &body body)
  
  `(progn
     ;; test if iterator is not bound
     ;; this happend when with-iterator is called without defining an iterator variable
     ;; e.g. (with-iterator (it :filespec ...) ...)
     (handler-case 
	 (progn
	   ;;(setf ,iterator-1 ,iterator)
	   (eval ,iterator))
       (unbound-variable ()   
	 (progn 
	   ;;(print-db 'caught-error)
	   (setf ,iterator ',iterator))))
      
     (with-iterator-1		     ; the function to with-iterator-1
	 ,iterator	
       ,filespec			; options for opening file
       ,headers-line
       ,data-start-line
       ,jdbc-url	     ; options for connecting to jdbc database
       ,sql-string 
       ,with-resultset-as-list
       ,with-results-as-list
       ,with-cached-results
       ,iterator-fn		    ; function used to create iterator
       (lambda (,iterator) 
	 ;; define fv (i.e. "field value shortcut for accessing the value of a record
	 (flet ((fv (field-id)
		  (value ,iterator field-id)))
	   ,@body)))))


;; macro specifically for file iterators, the only change is 
;; the keys and default project-iterator function
(defmacro with-file-iterator
    ((iterator &key 
	       filespec			; options for opening file
	       headers-line
	       data-start-line
	       iterator-fn)
     &body body)
  `(progn
     ;; test if iterator is not bound
     ;; this happend when with-file-iterator is called without defining an iterator variable
     ;; e.g. (with-file-iterator (it :filespec ...) ...)
     (handler-case 
	 (progn
	   ;;(setf ,iterator-1 ,iterator)
	   (eval ,iterator))
       (unbound-variable ()   
	 (progn 
	   (setf ,iterator ',iterator))))

     (with-iterator-1
	 ,iterator 
       ,filespec	; options for opening file
       ,headers-line
       ,data-start-line
       nil ;; ,jdbc-url		     ; these keys are not needed for file iterator
       nil ;; ,sql-string 
       nil ;; ,with-resultset-as-list
       nil ;; ,with-results-as-list
       nil ;; ,with-cached-results
       ,iterator-fn		    ; function used to create iterator
       (lambda (,iterator) 
	 ;; define fv (i.e. "field value shortcut for accessing the value of a record
	 (flet ((fv (field-id)
		  (value ,iterator field-id)))
	 ,@body)))))

;; macro specifically for jdbc iterators, the only change is 
;; the keys and default project-iterator function
(defmacro with-jdbc-iterator
    ((iterator &key 
	       jdbc-url			; options for opening jdbc database
	       sql-string
	       (with-resultset-as-list t)
	       (with-results-as-list t)
	       with-cached-results
	       iterator-fn)
     &body body)
  `(progn
     ;; test if iterator is not bound
     ;; this happend when with-jdbc-iterator is called without defining an iterator variable
     ;; e.g. (with-jdbc-iterator (it :filespec ...) ...)
     (handler-case 
	 (progn
	   ;;(setf ,iterator-1 ,iterator)
	   (eval ,iterator))
       (unbound-variable ()   
	 (progn 
	   (setf ,iterator ',iterator))))

     (with-iterator-1
	 ,iterator                                         
       nil ;; ,filespec       ; these keys are not needed for jdbc iterator
       nil ;; ,headers-line
       nil ;; ,data-start-line
       ,jdbc-url			; options for jdbc iterator
       ,sql-string 
       ,with-resultset-as-list
       ,with-results-as-list
       ,with-cached-results
       ,iterator-fn		    ; function used to create iterator
       (lambda (,iterator) 
	 ;; define fv (i.e. "field value shortcut for accessing the value of a record
	 (flet ((fv (field-id)
		  (value ,iterator field-id)))
	 ,@body)))))


(defun with-iterator-1 (iterator 
			filespec      ; options for opening file
			headers-line
			data-start-line
			jdbc-url    ; options for connecting to jdbc database
			sql-string 
			with-resultset-as-list
			with-results-as-list
			with-cached-results
			iterator-fn ;function used to create iterator
			lambda-function)
  (unwind-protect 
       (progn 
	 ;; if a funciton is specified for building the iterator
	 (when iterator-fn
	   (setf iterator (funcall iterator-fn)))

	 ;; check if iterator was created
	 (when (or (not iterator)
		   (not (typep iterator 'iterator)))
	   (setf iterator 
		 (get-with-iterator-1 
		  filespec		; options for opening file
		  headers-line
		  data-start-line
		  jdbc-url   ; options for connecting to jdbc database
		  sql-string 
		  with-resultset-as-list
		  with-results-as-list
		  with-cached-results)))
	 ;; call lambda funciton on the body of the macro using the iterator
	 (funcall lambda-function iterator))

    ;; shut down connections
    (close-iterator iterator)
    (when (resultset iterator) (close-resultset (resultset iterator)))
    (when (data-source (resultset iterator)) 
      (close-data-source (data-source (resultset iterator))))))
	  


(defun get-with-iterator-1 (filespec	; options for opening file
			    headers-line
			    data-start-line
			    jdbc-url ; options for connecting to jdbc database
			    sql-string 
			    with-resultset-as-list
			    with-results-as-list
			    with-cached-results)
  ;; define variables for data resources
  ;; I do this in order to make debugging easier
  (let (data-source resultset iterator)
    
    ;; check to see what info was passed in
    (cond 
      (;; create iterator for file
       filespec
       ;; create connection to data-source
       (setf data-source (open-data-source (make-instance 'file-data-source
							  :filespec filespec
							  :headers-line headers-line
							  :data-start-line data-start-line)))
       ;; build iterator
       ;; note: get-file-iterator creates resultset for iterator
       (setf iterator 
	     (get-file-iterator :data-source data-source)))
		       
      (;; create iterator for jdbc database
       jdbc-url
       (when (not sql-string)
	 (error "you must also supply an sql string for the jdbc iterator"))
       
       ;; create connection to jdbc database
       (setf data-source (open-data-source (make-instance 'jdbc-data-source
							  :jdbc-url jdbc-url)))
       
       ;; create resultset for iterator
       (setf resultset (make-instance 'jdbc-resultset
				      :data-source data-source
				      :with-resultset-as-list with-resultset-as-list
				      :with-results-as-list with-results-as-list
				      :with-cached-results with-cached-results
				      :sql-string sql-string))
       ;; build iterator
       (setf iterator 
	     (get-jdbc-iterator :resultset resultset))))
    
    ;;return iterator
    iterator))






