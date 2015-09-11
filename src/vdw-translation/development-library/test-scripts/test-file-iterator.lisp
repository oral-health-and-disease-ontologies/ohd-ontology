;; test the file line iterator by explicitly making a file data source and iterator
(defun test-file-iterator-1 ()	
  (let ((fs nil)
	(it nil))
    
    ;; (setf fs (open-data-source
    ;; 	      (make-instance 
    ;; 	       'file-data-source 
    ;; 	       :filespec (truename "general-library:test-scripts;test-input.txt"))))

    (setf it (get-file-iterator
	      :filespec (truename "general-library:test-scripts;test-input.txt")))

    ;;(pprint (type-of it))
    ;;(print-db (typep it 'iterator))
    ;;(print-db (contents (next-line (resultset it))))
    ;;(print-db (has-next it))

    (loop 
       while (has-next it) 
       for line = (next it) 
       for i from 1 to 5 do 
	 ;;(print-db (contents line))
	 ;;(value line 0)
	 (pprint (str+ :space 
		       (value line 0)
		       (value line 1)
		       (value line 2)))

	 ;; this also works
	 '(pprint (str+ :space 
		       (value it 0)
		       (value it 1)
		       (value it 2))))

    ;;(close-data-source fs)
    (close-iterator it)))

(defun test-file-iterator-2 ()	
  ;; test file with headers
  (let ((fs nil)
	(it nil))

    (setf fs (open-data-source
	      (make-instance 
	       'file-data-source 
	       :headers-line 1
	       :filespec (truename "general-library:test-scripts;test-input-headers-1.txt"))))

    (setf it (get-file-iterator :data-source fs))
    ;;(print-db (contents (next-line (resultset it))))
    ;;(print-db (has-next it))
    (print-db (headers (resultset it)))

    (loop 
       while (has-next it) 
       for line = (next it) 
       for i from 1 to 5 do 
	 ;;(print-db (contents line))
	 ;;(value line 0)
	 (pprint (str+ :space 
		       (value line "patient id")
		       (value line "sex")
		       (value line "birth date"))))

    (close-data-source fs)))

(defun test-file-iterator-3 ()
  ;; test file with headers
  (let ((fs nil)
	(it nil))

    (setf fs (open-data-source
	      (make-instance 
	       'file-data-source 
	       :headers-line 1
	       :data-start-line 3
	       :filespec (truename "general-library:test-scripts;test-input-headers-2.txt"))))

    (setf it (get-file-iterator :data-source fs))
    ;;(print-db (contents (next-line (resultset it))))
    ;;(print-db (has-next it))

    (print-db (headers (resultset it)))
    (loop 
       while (has-next it) 
       for line = (next it) 
       for i from 1 to 5 do 
	 ;;(print-db (contents line))
	 ;;(value line 0)
	 (pprint (str+ :space 
		       (value line "patient id")
		       (value line "sex")
		       (value line "birth date"))))

    (close-data-source fs)))

(defun test-file-iterator-4 ()	
  ;; test file with headers
  (let ((fs nil)
	(it nil))

    (setf fs (open-data-source
	      (make-instance 
	       'file-data-source 
	       :headers-line 2
	       :data-start-line 5
	       :filespec (truename "general-library:test-scripts;test-input-headers-3.txt"))))

    (setf it (get-file-iterator :data-source fs))
    ;;(print-db (contents (next-line (resultset it))))
    ;;(print-db (has-next it))

    (print-db (headers (resultset it)))
    (loop 
       while (has-next it) 
       for line = (next it) 
       for i from 1 to 5 do 
	 ;;(print-db (contents line))
	 ;;(value line 0)
	 (pprint (str+ :space 
		       (value line "patient id")
		       (value line "sex")
		       (value line "birth date"))))

    (close-data-source fs)))

;; test the file line iterator by passing the filespec to with-iterator
(defun test-with-file-iterator-1 ()
  (with-iterator (it :filespec (truename "general-library:test-scripts;test-input.txt"))
    ;;(print-db it)
    (loop 
       while (has-next it)
       for line = (next it) 
       for i from 1 to 4
       do
	 ;(print-db line)
	 (pprint (str+ :space 
		       (value line 0)
		       (value line 1)
		       (value line 2))))))

(defun test-with-file-iterator-2 ()
  (with-iterator (it :filespec (truename "general-library:test-scripts;test-input-headers-1.txt")
		     :headers-line 1)
    ;;(print-db it)
    (loop 
       while (has-next it)
       for line = (next it) 
       for i from 1 to 4
       do
	 ;;(print-db line)
	 (pprint (str+ :space 
		       (value line "patient id")
		       (value line "sex")
		       (value line "birth date"))))))

(defun test-with-file-iterator-3 ()
  (with-iterator (it :filespec (truename "general-library:test-scripts;test-input-headers-2.txt")
		     :headers-line 1
		     :data-start-line 3)
    ;;(print-db it)
    (loop 
       while (has-next it)
       for line = (next it) 
       for i from 1 to 4
       do
	 ;;(print-db line)
	 (pprint (str+ :space 
		       (value line "patient id")
		       (value line "sex")
		       (value line "birth date"))))))

(defun test-with-file-iterator-4 ()
  (with-iterator (it :filespec (truename "general-library:test-scripts;test-input-headers-3.txt")
		     :headers-line 2
		     :data-start-line 5)
    ;;(print-db it)
    (loop 
       while (has-next it)
       for line = (next it) 
       for i from 1 to 4
       do
	 ;;(print-db line)
	 (pprint (str+ :space 
		       (value line "patient id")
		       (value line "sex")
		       (value line "birth date"))))))
	 

(defun test-with-file-iterator-5 ()
  ;; create iterator using a custom funciton instead of keyword options
  (with-iterator (it :iterator-fn #'test-file-iterator-fn-5)
    ;;(print-db it)
    (loop 
       while (has-next it)
       for line = (next it) 
       for i from 1 to 4
       do
       ;;(print-db line)
	 (pprint (str+ :space 
		       (value line 0)
		       (value line 1)
		       (value line 2))))))

(defun test-file-iterator-fn-5 ()
  (get-file-iterator 
   :filespec (truename "general-library:test-scripts;test-input.txt")))

;; removed this option
;; (defun test-with-file-iterator-6 ()
;;   ;; create iterator using default project-iterator function
;;   (with-iterator (it)
;;     ;;(print-db it)
;;     (loop 
;;        while (has-next it)
;;        for line = (next it) 
;;        for i from 1 to 4
;;        do
;;        ;;(print-db line)
;; 	 (pprint (str+ :space 
;; 		       (value line "patient id")
;; 		       (value line "sex")
;; 		       (value line "birth date"))))))

(defun test-with-file-iterator-7 ()
  ;; test overriding the project-iterator function
  (with-iterator (it :filespec (truename "general-library:test-scripts;test-input-headers-1.txt")
		     :headers-line 1
		     :iterator-fn nil)
    ;;(print-db it)
    (loop 
       while (has-next it)
       for line = (next it) 
       for i from 1 to 4
       do
	 ;;(print-db line)
	 (pprint (str+ :space 
		       (value line "patient id")
		       (value line "sex")
		       (value line "birth date"))))))

(defun test-with-file-iterator-8 ()
  ;; test with-file-iterator macro
  (with-file-iterator 
      (it :filespec (truename "general-library:test-scripts;test-input-headers-1.txt")
	  :headers-line 1
	  :iterator-fn nil)
    ;;(print-db it)
    (loop 
       while (has-next it)
       for line = (next it) 
       for i from 1 to 4
       do
	 ;;(print-db line)
	 (pprint (str+ :space 
		       (value line "patient id")
		       (value line "sex")
		       (value line "birth date"))))))


(defun test-with-file-iterator-9 ()
  ;; test setting the iterator prior to calling with-iterator
  ;; then pass the iterator to macro as arg
  (let ((it nil))
    (setf it (get-file-iterator 
	      :filespec (truename "general-library:test-scripts;test-input.txt")))
        
    (with-iterator (it)
      ;;(print-db it)
      (loop 
	 while (has-next it)
	 for line = (next it) 
	 for i from 1 to 4
	 do
	 ;;(print-db line)
	   (pprint (str+ :space 
			 (value line 0)
			 (value line 1)
			 (value line 2)))))))

(defun test-with-file-iterator-10 ()
  ;; test fv local funciton
  (let ((it nil))
    (setf it (get-file-iterator 
	      :filespec (truename "general-library:test-scripts;test-input.txt")))
        
    (with-iterator (it)
      ;;(print-db it)
      (loop 
	 while (next it)
	 for i from 1 to 4
	 do
	 ;;(print-db line)
	   (pprint (str+ :space 
			 (fv 0)
			 (fv 1)
			 (fv 2)))))))

(defun test-with-file-iterator-11 ()
  ;; test fv local funciton with string names as field values
  (with-iterator 
      (it :filespec (truename "general-library:test-scripts;test-input-headers-1.txt")
	  :headers-line 1
	  :iterator-fn nil)
    ;;(print-db it)
    (loop 
       while (next it)
       for i from 1 to 4
       do
	 ;;(print-db it)
	 (pprint (str+ :space 
		       (fv "patient id")
		       (fv "sex" )
		       (fv "birth date"))))))

;; just for fun: here is a next function using closure
(defun next-line-fn (src)
  (let ((line nil)
	(stream (open src)))
        
    (lambda ()
      (unwind-protect
	   (setf line (read-line stream nil :eof))
	(close stream)))
	))

(defun test-next-line-fn ()
  (let ((fn nil))
    (setf fn (next-line-fn (truename "general-library:test-scripts;test-input.txt")))
    (loop repeat 3 do
	 (pprint (funcall fn)))))
