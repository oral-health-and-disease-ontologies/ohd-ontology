(defun print-hash-table (hash-table &key limit)
  "prints out the key/value pairs in a hash table; the param limit limits the number of entries printed."
  (let ((count 0))
    (loop 
       for k being the hash-keys in hash-table using (hash-value v) do
	 (when limit 
	   (if (>= count limit) (return)))
         (format t "~a ~a ~%" k v)
	 (incf count))))
