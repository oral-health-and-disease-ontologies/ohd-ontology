(defun vdw-extra-ada-codes-to-text ()
  (let ((file-name (str+ (truename "ohd:data") "vdw-extra-ada-codes.txt")))
    ;; check for hash table
    (when (not *ada-code2uri*)
      (load-ada-code2uri-table))

    ;; save the keys from the hash table
    (save-hash-table *ada-code2uri* file-name :save-key)))

(defun vdw-missing-icd9-codes-to-text ()
  (let ((file-name (str+ (truename "ohd:data") "vdw-icd9-codes.txt")))
    ;; check for hash table
    (when (not *icd9-code2uri*)
      (load-icd9-code2uri-table))

    ;; save the keys from the hash table
    (save-hash-table *icd9-code2uri* file-name :save-key)))

(defun vdw-extra-ada-codes-counts ()
  (let ((counts-table (make-hash-table :test #'equalp))
	(file-name (str+ (truename "ohd:data") "vdw-extra-ada-codes-with-counts.txt")))

    ;; check for hash table of extra ada codes
    (when (not *vdw-extra-ada-codes*) (load-vdw-extra-ada-codes))

    ;; copy keys from extra codes table to counts table
    ;; set the count value 0
    (loop
       for code being the hash-key in *vdw-extra-ada-codes* do
	 (setf (gethash code counts-table) 0))
	 

    ;; iterater over dental-procedure-diagnosis.txt
    ;; and increment count
    (with-iterator
	
	)
    ;; save the keys from the hash table
    (save-hash-table counts-table file-name)

    ;; return counts table
    counts-table))
