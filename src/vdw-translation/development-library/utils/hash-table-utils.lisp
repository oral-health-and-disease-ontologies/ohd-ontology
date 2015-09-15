(defun print-hash-table (hash-table &key limit print-key print-value)
  "Prints out the key/value pairs in a hash table.
The limit parameter limits the number of entries printed.
The print-key parameter causes only the keys to be printed.
The print-value parameter causes only the values the to be printed."

  (let ((count 0))
    (loop 
       for k being the hash-keys in hash-table using (hash-value v) do
	 (when limit 
	   (if (>= count limit) (return)))
	 (cond
	   (print-key ; print only the key
	    (format t "~a~%" k))
	   (print-value ; print only the value
	    (format t "~a~%" v))
	   (t
	      ;; note: there is a tab character between the key and value
	     (format t "~a	~a ~%" k v)))
	 (incf count))))

(defun save-hash-table (hash-table file-name &key limit save-key save-value)
  "Saves the hash table specifed by the hash-table parameter to he file specified by file-name.
The limit parameter limits the number of entries saved.
The save-key parameter causes only the keys to be saved.
The save-value parameter causes only the values the to be saved."

  (let ((count 0))
    (with-open-file (stream file-name
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
      (loop 
	 for k being the hash-keys in hash-table using (hash-value v) do
	   (when limit 
	     (if (>= count limit) (return)))
	   (cond
	     (save-key			; print only the key
	      (format stream "~a~%" k))
	     (save-value		; print only the value
	      (format stream "~a~%" v))
	     (t
	      ;; note: there is a tab character between the key and value
	      (format stream "~a	~a ~%" k v)))
	   (incf count)))))

(defun copy-hash-table (hash-table)
  "Returns a copy of hash-table. Code adapted from:
http://stackoverflow.com/questions/26045442/copy-hash-table-in-lisp"
  (let ((ht (make-hash-table 
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    
    (loop
       for key being the hash-keys in hash-table
           using (hash-value value)
       do
	 (setf (gethash key ht) value))

    ;; return copy of hash-table
    ht))
