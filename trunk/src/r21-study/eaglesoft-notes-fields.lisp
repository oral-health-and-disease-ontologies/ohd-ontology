(defun check-eaglesoftnotes ()
  (let (results query percent-non-null)
    (flet ((total-number-of-recs (table_name)
	     (let((results nil) 
		  (total nil)
		  (query (format nil "select count(*) as total from ~a" table_name)))
	       (with-eaglesoft (results query)
		 (#"next" results)
		 (setf total (#"getString" results "total")))
	       (parse-integer total)))
		    
	   (non-null-total-number-of-recs (table_name field_name)
	     (let((results nil) 
		  (non-null-total nil)
		  (query (format nil "select count(*) as total from ~a where length(~a) > 0" table_name field_name)))
	       (with-eaglesoft (results query)
		 (#"next" results)
		 (setf non-null-total (#"getString" results "total")))
	       (parse-integer non-null-total))))
	       
      (setf query "
SELECT
  dbo.syscolumns.id,
  dbo.syscolumns.name AS field_name
INTO
  #fields
FROM
  dbo.syscolumns
WHERE
  field_name LIKE '%note%'
AND
  field_name NOT LIKE '@%'
AND 
  field_name NOT LIKE '%_id'
  
SELECT
  so.name AS table_name, f.field_name
FROM
  dbo.sysobjects so
INNER JOIN
  #fields f
ON
  so.id = f.id
WHERE
  (so.type = 'U' OR so.type = 'V')
ORDER BY
  table_name, field_name
")
      (with-eaglesoft (results query)
	(loop
	   while (#"next" results)
	   for table_name = (#"getString" results "table_name")
	   for field_name = (#"getString" results "field_name")
	   for total = (total-number-of-recs table_name)
	   for non-null-total = (non-null-total-number-of-recs table_name field_name)
	   do
	     (cond
	       ((> total 0)
		(setf percent-non-null (floor (float (* (/ non-null-total total) 100)))))
	       (t
		(setf percent-non-null 0)))
	     (format t "~a ~a ~a~%" table_name field_name percent-non-null)
	     ) ; end loop
	))))
