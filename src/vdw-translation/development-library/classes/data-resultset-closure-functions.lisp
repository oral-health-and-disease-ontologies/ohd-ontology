(defmethod set-jdbci-iterator-closure-functions ((it iterator))
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
  ;; results-as-list flag, and make a call to next in order
  ;; to set the next-results
  (when (or (return-resultset-as-list (resultset it))
	    (results-as-list (resultset it)))
    (setf (results-as-list (resultset it)) t)
    (funcall (next-fn it)))
    
  ;; return the iterator
  it)


;;; closure mehtods for accessing iterator results

(defun set-jdbc-iterator-next-function-1 (iterator)
  (lambda()
    (cond
      ( ;; using a resultset list or results as lists
       (return-resultset-as-list (resultset iterator))
       
       ;; on first call to next the list place number will 
       ;; not be set, so set it and move the next-result
       (when (not (resultset-list-place-number iterator))
	 (setf (resultset-list-place-number iterator) -1))
       
       ;; set current results to next
       (setf (results iterator) (next-results iterator))

       ;; update the place to point to in results list
       (incf (resultset-list-place-number iterator))

       ;; set next result to next place in resultset list
       (setf (next-results iterator)
	     (nth (resultset-list-place-number iterator)
		  (results-list iterator)))
       ;; return current 
       (results iterator))

      ( ;; using results as list
       ;; ths is not idential with resultset as list
       ;; this flag just means that the results in the
       ;; jdbc result are converted to a list
       (results-as-list (resultset iterator))

       ;; on the first call cached-results will be nil
       ;; set cached-results to results, and use it
       ;; to populate next-results
       (when (not (cached-results iterator))
	 ;;(#"next" (results iterator))
	 (setf (cached-results iterator) (results iterator)))
       
       ;; set current results to next & update next
       (setf (results iterator) (next-results iterator))

       ;; call next() on cached-results
       (#"next" (cached-results iterator))
       
       ;; covert next-results into a list using cached-results
       (setf (next-results iterator) 
	     (jdbc-results-to-list (cached-results iterator)))

       ;; return results
       (results iterator))
      (t 
       ;; otherwise call next method on jdbc resultset
       ;; return nil if we have moved past last results
       ;; note: next() returns true/false, we have return
       ;; the iterator result
       (when (not (#"isAfterLast" (results iterator)))
	 (#"next" (results iterator))
	 (results iterator))))))


(defun set-jdbc-iterator-has-next-function-1 (iterator)
  (lambda ()
    ;; determine what type of resultset is used
    (cond
      ( ;; using a resultset list or results as list
       (or (results-as-list  (resultset iterator))
	   (return-resultset-as-list (resultset iterator)))
       ;; determine if next result in list exists
       (> (length (next-results iterator)) 0))
    
      ( ;; cached results can use the isLast function
       (use-cached-results (resultset iterator))
       (not (#"isLast" (results iterator))))
      (t 
       ;; otherwise throw an error
       (error "has-next is not supported for this type of resultset")))))

(defun set-jdbc-iterator-has-current-result-function-1 (iterator)
  (lambda ()
    ;; determine what type of resultset is used
    (cond
      ( ;; using a resultset let or results as list
       (or (results-as-list (resultset iterator))
	   (return-resultset-as-list (resultset iterator)))
       (> (length (results iterator)) 0)) ; check for empty results
      (t 
       ;; otherwise examine the following:
       ;; has-current-returns returns t if either:
       ;; 1. isAfterLast() returns nil
       ;; 2. isBeforeFirst() returns row
       ;; 3.  getRow > 0 (i.e., the row number > 0)
       (and (not (#"isAfterLast" (results iterator)))
	    (not (#"isBeforeFirst"(results iterator)))
	    (> (#"getRow" (results iterator)) 0))))))

    
