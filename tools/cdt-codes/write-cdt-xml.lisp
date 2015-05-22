(defun write-cdt-xml (file-in file-out)
  (setf cdt-ht nil)
  (setf cdt-ht (get-cdt-ht cdt-ht))

  (with-open-file 
      (out file-out :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-open-file 
	(in file-in :direction :input)
      (loop for line = (read-line in nil :eof) until (eq line :eof) do
	   (cond
	     ((equal (subseq line 0 1) "D")
	      (setf code (subseq line 0 5))
	      (setf label (string-trim " " (subseq line 5)))
	      (setf cdt-comment (gethash code cdt-ht))
	      (setf output 
		    (concatenate 
		     'string
		     "  <CDTCode id='~A'>~%"
		     "     <Code>~A</Code>~%"
		     "     <CDTLabel>~A</CDTLabel>~%"
		     "     <CDTComment>~A</CDTComment>~%"
		     "  </CDTCode>~%"))
	      (format out output code code label cdt-comment))
	     (t (format out "~A~%" line)))
	   ))))

(defun get-cdt-ht (hash)
  (setf hash (make-hash-table :test 'equal))
  (setf code nil)
  (setf cdt-comment nil)
  
  (with-open-file (in "CDTCodeComments3.txt" :direction :input)
    (loop for line = (read-line in nil :eof) until (eq line :eof) do
	 (cond
	   ((equal (subseq line 0 5) "[CDT]")
	    ;; check to se if comment should be added
	    (cond 
	      ((> (length code) 0)
	       (setf (gethash code hash) (format nil "~A" cdt-comment))
	       (setf cdt-comment nil)))

	    ;; grab code
	    (setf code (subseq line 5 10))
	    (setf code (format nil "~A" code)))
	   
	   (t ;; otherwise build comment
	    (cond
	      ((> (length line) 0)
	       (setf cdt-comment 
		     (concatenate 'string cdt-comment 
				  (format nil "~A~%" line)))))))))
  ;; add last entry
  (setf (gethash code hash) (format nil "~A" expl))
  hash)
  
  


(defun print-ht (hash)
  (maphash #'(lambda (key val)
	       (format t "~A    ~A ~%" key val)) hash))

(defun save-ht (hash_table file_name)
  ;; open file for saving hash_table
  (with-open-file (f file_name :direction :output :if-exists :supersede
		     :if-does-not-exist :create)
    (format f "~S ~S~%" (hash-table-test hash_table) (hash-table-size hash_table))
    (let ((*print-readably* t)
	  (*print-circle* t)
	  (*print-escape* t))
      
      (maphash #'(lambda(key value)
		   (format-key-value f key value))
		   hash_table))))

(defun read-ht (file_name)
  (with-open-file (f file_name :direction :input)
    (let* ((test (read f))
	   (size (read f))
	   (hash (make-hash-table :test test :size size))
	   (eof (make-symbol "EOF")))
      (loop for key = (read f nil eof)
	 for value = (read f nil eof)
	 until (or (eq key eof) (eq value eof))
	 do (setf (gethash key hash) value))
      hash)))
