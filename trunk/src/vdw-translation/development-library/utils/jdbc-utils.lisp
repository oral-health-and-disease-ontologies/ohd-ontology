(defun jdbc-results-to-list (results)

  ;; check that results are on a current row
  (when  (not (or (#"isBeforeFirst" results)
		  (#"isAfterLast" results)))
    (loop 
       for i from 1 to (#"getColumnCount" (#"getMetaData" results)) 
       collect (#"getString" results i))))

(defun jdbc-resultset-field-names (results)
  (loop 
     for i from 1 to (#"getColumnCount" (#"getMetaData" results))
     collect (#"getColumnName" (#"getMetaData" results) i)))
