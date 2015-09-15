(defun trim-field (value)
  "Removes spaces and quotes (i.e., \") from beginning and end of value."
  ;; check that value is not nil (i.e., length > 0)
  ;; if you don't, the string "NIL" will be returned
  (when value 
    (setf value (string-trim " " value))
    (setf value (string-trim "\"" value)))
  
  ;; if not empty after trim, return value
  ;; otherwise return nil
  (cond 
    ((> (length value) 0)
     value)
    (t ;; otherwise 
     nil)))

(defun format-comment (comment &key name)
  "Formats a comment so that it has a name and date in it."
  (when (not name) (setf name "Bill Duncan"))

  ;; build comment string, note the line break after the colon
  (str+
   name " " (current-date-string) ":
" comment))

(defun current-date-string ()
  "Returns current date as a string."
  (multiple-value-bind
	(sec min hr day mon yr dow dst-p tz)
        (get-decoded-time)
    (format nil "~2,'0d/~2,'0d/~4,'0d" mon day yr)))

