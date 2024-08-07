;; declare global hash table variables
(defparameter *study-id2uri* nil)
(defparameter *study-id-with-race2uri* nil)
(defparameter *study-id-with-ethnicity2uri* nil)
(defparameter *encounter-id2uri* nil)
(defparameter *provider-id2uri* nil)
(defparameter *icd9-code2uri* nil)
(defparameter *vdw-extra-dx-codes* nil)
(defparameter *ada-code2uri* nil)
(defparameter *vdw-extra-ada-codes* nil)
(defparameter *code2proc-uri* nil)
(defparameter *code2proc-name* nil)
(defparameter *proc-uri2proc-name* nil)
(defparameter *filling-codes* nil)
(defparameter *code2material-uri* nil)
(defparameter *code2material-name* nil)

(defun load-study-id2uri-table ()
  (let (uri)
    (setf *study-id2uri* (make-hash-table :test #'equalp))
    (with-iterator (it :iterator-fn #'demographics-iterator)
      (loop
	 while (next it)
	 for study-id = (fv "STUDY_ID")
	 for uri = (make-vdw-uri
		    study-id
		    :class-type !'human dental patient'@ohd)
	 do
	   ;;(pprint uri)
	   (setf (gethash study-id *study-id2uri*) uri)))
    ;; return hash table
    *study-id2uri*))

(defun load-encounter-id2uri-table ()
  (let (uri)
    (setf *encounter-id2uri* (make-hash-table :test #'equalp))
    (with-iterator (it :iterator-fn #'dental-encounter-iterator)
      (loop
	 while (next it)
	 for encounter-id = (fv "STUDY_ENC_ID")
	 for uri = (make-vdw-uri
		    encounter-id
		    :class-type !'health care encounter'@ohd)
	 do
	   ;;(pprint uri)
	   (setf (gethash encounter-id *encounter-id2uri*) uri)))
    ;; return hash table
    *encounter-id2uri*))

(defun load-provider-id2uri-table ()
  (let (uri)
    (setf *provider-id2uri* (make-hash-table :test #'equalp))
    (with-iterator (it :iterator-fn #'provider-iterator)
      (loop
	 while (next it)
	 for provider-id = (fv "PROVIDER_STUDY_ID")
	 for uri = (make-vdw-uri
		    provider-id
		    :class-type !'dental health care provider'@ohd)
	 do
	   ;;(pprint uri)
	   (setf (gethash provider-id *provider-id2uri*) uri)))
    ;; return hash table
    *provider-id2uri*))

(defun load-code2proc-uri-table ()
  (let (uri)
    (setf *code2proc-uri* (make-hash-table :test #'equalp))
    (with-iterator (it :iterator-fn #'code-to-procedure-iterator)
      (loop
	 while (next it)
	 for code = (fv "code")
	 for uri = (make-uri (fv "procedure uri"))
	 do
	   ;;(pprint uri)
	   (setf (gethash code *code2proc-uri*) uri)))
    ;; return hash table
    *code2proc-uri*))

(defun load-code2proc-name-table ()
  (let (uri)
    (setf *code2proc-name* (make-hash-table :test #'equalp))
    (with-iterator (it :iterator-fn #'code-to-procedure-iterator)
      (loop
	 while (next it)
	 for code = (fv "code")
	 for name =  (fv "procedure name")
	 do
	   ;;(pprint uri)
	   (setf (gethash code *code2proc-name*) name)))
    ;; return hash table
    *code2proc-name*))

(defun load-proc-uri2proc-name-table ()
  (let (uri)
    (setf *proc-uri2proc-name* (make-hash-table :test #'equalp))
    (with-iterator (it :iterator-fn #'code-to-procedure-iterator)
      (loop
	 while (next it)
	 for uri = (make-uri (fv "procedure uri"))
	 for name = (fv "procedure name")
	 do
	   ;;(pprint uri)
	   (setf (gethash uri *proc-uri2proc-name*) name)))
    ;; return hash table
    *proc-uri2proc-name*))


(defun load-icd9-code2uri-table ()
  (let (uri2label icd9-ont)
    ;; declare hash table
    (setf *icd9-code2uri* (make-hash-table :test #'equalp))

    ;; load icd9 ontology
    (setf icd9-ont (load-ontology (str+ (truename "ohd:imports;icd9-import.owl"))))

    ;; get hash table in icd9 ontology that maps the icd9 uris to its lablel
    (setf uri2label (rdfs-labels icd9-ont))

    ;; the loop below find the "id" portion of uri, which the last part of uri
    ;; e.g., e.g.: (uri-id http://www.ex.com/ICD_123.4) -> ICD_123.4
    ;; it then reverse the id and find the position of the underscore
    ;; e.g., ICD_123.4 -> 4.321_CDI -> postion = 5
    ;; the charcters before the underscore are then re-reversed to create the icd code
    ;;  4.321_CDI -> 123.4
    (loop
       for uri being the hash-keys in uri2label
       for id = (uri-id uri) ;; id portion of uri
       for reverse-id = (reverse id) ;; reverse the id 
       for position = (search "_" reverse-id :test #'equalp) ;; find the first "_" 
       for icd-code = (reverse (str-left reverse-id position)) ;; re-reverse the icd code
       do
       ;; add icd code and uri to hash table
	 (setf (gethash icd-code *icd9-code2uri*) uri))
    ;; return hash table
    *icd9-code2uri*))

(defun load-vdw-extra-dx-codes ()
  ;; declare extra ada code table
  (setf *vdw-extra-dx-codes* (make-hash-table :test #'equalp))
  
  ;; check for ada code hash table
  (when (not *icd9-code2uri*) (load-ada-code2uri-table))

  ;; iterate over dental-procedure-diagnosis.txt
  ;; and look for dx codes that do exist
  (with-iterator (it :iterator-fn #'dental-procedure-diagnosis-iterator)
    (loop
       while (next it)
       for code = (trim-field (fv "DX"))
       do
         ;; check if code is in the icd9 table and code exists
	 (when (and code (not (gethash code *icd9-code2uri*)))
	   (setf (gethash code *vdw-extra-dx-codes*) t))))
  ;; return hash table
  *vdw-extra-dx-codes*)

(defun load-ada-code2uri-table ()
  (let (uri2label ada-code-ont)
    ;; declare hash table
    (setf *ada-code2uri* (make-hash-table :test #'equalp))

    ;; load ada code ontology
    (setf ada-code-ont (load-ontology (str+ (truename "ohd:imports;cdt-imports.owl"))))

    ;; get hash table in ada ontology that maps the ada code uris to its lablel
    (setf uri2label (rdfs-labels ada-code-ont))
    
    ;; the loop below finds the ada code part of the label
    ;; the labels have the form "billing code Dwxyz: description"
    ;; e.g., "billing code D3410: apicoectomy/periradicular surgery - anterior"
    ;; To extract the ada code:
    ;; 1. get the subseq of the label between positions 13 and 18
    ;; 2. check that it has the form "Dwxyz" where w, x, y, and z are numbers
    (loop
       for uri being the hash-keys in uri2label using (hash-value code-label-list)
       for code-label = (car code-label-list)
       ;; get "Dwxyz" part of label, note: the length needs to be checked
       for ada-code = (if (not (< (length code-label) 18)) (subseq code-label 13 18))
       do
       ;; check that ada code is a "D" followed by digits
	 (when (all-matches ada-code "D\\d+")
	   ;; add ada code and uri to hash table
	   (setf (gethash ada-code *ada-code2uri*) uri)))
    
    ;; return hash table
    *ada-code2uri*))

(defun load-vdw-extra-ada-codes ()
  ;; declare extra ada code table
  (setf *vdw-extra-ada-codes* (make-hash-table :test #'equalp))
  
  ;; check for ada code hash table
  (when (not *ada-code2uri*)
    (load-ada-code2uri-table))

  ;; iterate over dental-procedure-diagnosis.txt
  ;; and look for ada codes that do exist
  (with-iterator (it :iterator-fn #'dental-procedure-diagnosis-iterator)
    (loop
       while (next it)
       for code = (trim-field (fv "ADA_CODE"))
       do
         ;; check if code is in the ada table and code exists
	 (when (and code (not (gethash code *ada-code2uri*)))
	   (setf (gethash code *vdw-extra-ada-codes*) t))))
  ;; return hash table
  *vdw-extra-ada-codes*)

(defun load-filling-codes-table()
  (let (iterator-function filespec)
    ;; declare hash table
    (setf *filling-codes* (make-hash-table :test #'equalp))

    ;; set filespec to file
    (setf filespec (str+ (truename "vdw:data;filling-codes.txt")))
    
    ;; load hash table from file
    (with-iterator (it :iterator-fn (lambda () (vdw-file-iterator filespec)))
      (loop
	 while (next it)
	 for code = (fv "code")
	 do
	   (setf (gethash code *filling-codes*) t)))
    ;;return hash table
    *filling-codes*))


(defun load-code2material-uri-table()
  (let (iterator-function filespec)
    ;; declare hash table
    (setf *code2material-uri* (make-hash-table :test #'equalp))

    ;; set filespec to file
    (setf filespec (str+ (truename "vdw:data;restoration-material-codes.txt")))
    
    ;; load hash table from file
    (with-iterator (it :iterator-fn (lambda () (vdw-file-iterator filespec)))
      (loop
	 while (next it)
	 for code = (fv "code")
	 for uri = (fv "material uri")
	 for uri-list = (split-at-regex uri "\\|")  ; split uri field on pipe character
	 do
	   ;; replace strings in uri-list with uris
	   (loop for n from 0 to (- (length uri-list) 1) do
		(setf (nth n uri-list) (make-uri (nth n uri-list))))
	   
	   (setf (gethash code *code2material-uri*) uri-list)))
    ;;return hash table
    *code2material-uri*))

(defun load-code2material-name-table()
  (let (iterator-function filespec)
    ;; declare hash table
    (setf *code2material-name* (make-hash-table :test #'equalp))

    ;; set filespec to file
    (setf filespec (str+ (truename "vdw:data;restoration-material-codes.txt")))
    
    ;; load hash table from file
    (with-iterator (it :iterator-fn (lambda () (vdw-file-iterator filespec)))
      (loop
	 while (next it)
	 for code = (fv "code")
	 for name = (fv "material name")
	 for name-list = (split-at-regex name "\\|")  ; split name field on pipe character
	 do
	   (setf (gethash code *code2material-name*) name-list)))
    ;;return hash table
    *code2material-name*))

;; load hash tables
;;(load-study-id2uri-table)
;;(load-encounter-id2uri-table)
;;(load-provider-id2uri-table)
;;(load-icd9-code2uri-table)
;;(load-ada-code2uri-table)
