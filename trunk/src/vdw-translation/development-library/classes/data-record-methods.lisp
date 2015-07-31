
(defmethod value ((r jdbc-results) field-id  &key &allow-other-keys)
  ;; get data field from the result located/identified by field-id
  ;; if field is a string get position number of the string
  (when (stringp field-id)
    (setf field-id 
	  (position field-id 
		    (field-names (resultset r)) :test #'equalp))
      ;; check that field was found
    (when (not field-id)
      (error "the specified field was not found")))
  
  ;; get the data from jdbc results
  ;; note: jdbc columns are 1 based
  (#"getString" (contents r) (1+ field-id)))

(defmethod value ((r record-list) field-id &key &allow-other-keys)
  ;; get data field from the result located/identified by field-id
  ;; if field is a string get position number of the string
  (when (stringp field-id)
    (setf field-id 
	  (position field-id 
		    (field-names (resultset r)) :test #'equalp))
    ;; check that field was found
    (when (not field-id)
      (error "the specified field was not found")))
  
  ;; get item in list
  ;; note: first element is at 0 position
  (nth field-id (contents r)))


(defmethod value ((r file-record) field-id &key &allow-other-keys)
  ;; get data associated with header/field-id of the file's record/line
  ;; if field is a string get position number of the string
  
  ;; check that the record is in list form
  (cond
    (;; if it isn't a list simply return the contents
     (not (listp (contents r)))
     (contents r))
    
    (t ;; otherwise get element in list
     (when (stringp field-id)
       (setf field-id 
	     (position field-id 
		       (field-names (resultset r)) :test #'equalp))
       ;; check that field was found
       (when (not field-id)
	 (error "the specified field was not found")))

     ;; get item in list 
     (nth field-id (contents r)))))
 
