;; declare global hash table variables
(defparameter *study-id2uri* nil)
(defparameter *encounter-id2uri* nil)
(defparameter *provider-id2uri* nil)
(defparameter *icd9-code2uri* nil)
(defparameter *ada-code2uri* nil)
 
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

;; load hash tables
(load-study-id2uri-table)
(load-encounter-id2uri-table)
(load-provider-id2uri-table)
(load-icd9-code2uri-table)
(load-ada-code2uri-table)


