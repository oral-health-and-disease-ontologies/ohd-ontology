(defmethod set-jdbc-iterator-closure-functions ((it jdbc-iterator))
  ;; set up closure method for has-next
  (setf (has-next-fn it)
	(set-jdbc-iterator-has-next-function-1 it))

  ;; set up closure method for has-current-result
  (setf (has-current-fn it)
	(set-jdbc-iterator-has-current-result-function-1 it))
       
  ;; set up closure for next function
  (setf (next-fn it)
	(set-jdbc-iterator-next-function-1 it))

  ;; if the results are operating on lists, be sure to set the 
  ;; with-results-as-list flag, and make a call to next in order
  ;; to set the next-results
  (when (or (with-resultset-as-list (resultset it))
	    (with-records-as-list (resultset it)))
    (setf (with-results-as-list (resultset it)) t))
  
  ;; return the iterator
  it)


;;; closure mehtods for accessing iterator results

(defun set-jdbc-iterator-next-function-1 (iterator)
  (lambda()
    (let ((rs (resultset iterator))
	  (current-rs (current-results (resultset iterator)))
	  (next-rs (next-results (resultset iterator)))
	  (cached-rs (cached-results (resultset iterator))))

      (cond
	( ;; using a resultset list or results as lists
	 (with-resultset-as-list rs)
	 
	 ;; check if the iterator has been called
	 ;; if not set place numbers and
	 ;; move next results forward
	 (when (not (called-next? iterator))
	   (setf (called-next? iterator) t) ; update called flag
	   (setf (current-place-number rs) 0)
	   (setf (next-place-number rs) 0)
	   (setf (contents next-rs)   ; next results is now at 
		 (nth (next-place-number rs) ; first iem in list
		      (resultset-list rs))))
     
	 ;; otherwise update both place numbers
	 (incf (current-place-number rs))
	 (incf (next-place-number rs))

	;; set current results to next
	(setf (contents current-rs) (contents next-rs))
	 
	;; set next result contents to next place in resultset list
	(setf (contents next-rs)
	      (nth (next-place-number rs)
		   (resultset-list rs)))

	 ;; return current results
	 current-rs)

	( ;; using results as list
	 ;; ths is not identical with resultset as list
	 ;; this flag just means that the results in the
	 ;; jdbc result are converted to a list
	 (with-results-as-list rs)
	 
	 ;; check to see if next() has already been called
	 ;; if not the next-rs will have to be moved one ahead of current
	 (when (not (called-next? iterator))
	   (setf (called-next? iterator) t) ; update called flag
	   (#"next" (contents cached-rs)) ; move the cached-rs to first record
	   
	   ;; covert next-results into a list using cached-results
	   (setf (contents next-rs)
		 (jdbc-results-to-list (contents cached-rs))))

	 
	 ;; set current results to next & update next
	 (setf (contents current-rs) (contents next-rs))

	 ;; call next() on cached-results
	 (#"next" (contents cached-rs))
	 
	 ;; covert next-results into a list using cached-results
	 (setf (contents next-rs)
	       (jdbc-results-to-list (contents cached-rs)))

	 ;; return current results
	 current-rs)
	(t 
	 ;; otherwise call next method on jdbc resultset
	 ;; return nil if we have moved past last results
	 ;; note: next() returns true/false, we have return
	 ;; the iterator result
	 ;;(print-db (contents current-rs))
	 (when (not (#"isAfterLast" (contents current-rs)))
	   (#"next" (contents current-rs)))
	 
	 current-rs)))))

(defun set-jdbc-iterator-has-next-function-1 (iterator)
  (lambda ()
    (let ((rs (resultset iterator))
	  (current-rs (current-results (resultset iterator)))
	  (next-rs (next-results (resultset iterator)))
	  (cached-rs (cached-results (resultset iterator))))

      ;; determine what type of resultset is used
      (cond
	( ;; using a resultset list or records as list
	 (with-resultset-as-list rs)
	 ;; if the iterator has not been called
	 ;; check first item in results list
	 (if (not (called-next? iterator))
	     (> (length (car (resultset-list rs))) 0)
	     (> (length (contents next-rs)) 0)))
	 
	((with-results-as-list rs)
	 ;; check to see if next() has already been called
	 ;; if not the next-rs will have to be moved one ahead of current
	 (when (not (called-next? iterator))
	   (setf (called-next? iterator) t) ; update called flag
	   (#"next" (contents cached-rs))
	   
	   ;; covert next-results into a list using cached-results
	   (setf (contents next-rs)
		 (jdbc-results-to-list (contents cached-rs))))
	 
	 ;; determine if next result in list exists
	 (> (length (contents next-rs)) 0))
    
	( ;; cached results can use the isLast function
	 (with-cached-results (resultset iterator))
	 (not (#"isLast" (contents current-rs))))
	(t 
	 ;; otherwise throw an error
	 (error "has-next is not supported for this type of resultset"))))))

(defun set-jdbc-iterator-has-current-result-function-1 (iterator)
  (lambda ()
    (let ((rs (resultset iterator))
	  (current-rs (current-results (resultset iterator))))
      ;; determine what type of resultset is used
      (cond
	( ;; using a resultset let or results as list
	 (or (with-results-as-list rs)
	     (with-resultset-as-list rs))
	 (> (length (contents current-rs)) 0)) ; check for empty results
	(t 
	 ;; otherwise examine the following:
	 ;; has-current-returns returns t if either:
	 ;; 1. isAfterLast() returns nil
	 ;; 2. isBeforeFirst() returns row
	 ;; 3.  getRow > 0 (i.e., the row number > 0)
	 ;;(print-db (contents current-rs))
	 (and (not (#"isAfterLast" (contents current-rs)))
	      (not (#"isBeforeFirst" (contents current-rs)))
	      (> (#"getRow" (contents current-rs)) 0)))))))

    
