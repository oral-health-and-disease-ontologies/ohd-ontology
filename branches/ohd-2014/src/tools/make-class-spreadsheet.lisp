;;(defparameter 
;;    ohd-end-point "http://localhost:8080/openrdf-workbench/repositories/ohd/query")

;; (defparameter 
;;     ohd-end-point "http://localhost:8080/openrdf-sesame/repositories/ohd")

;; (defparameter 
;;     ohd-remote "http://365-imac.sdm.buffalo.edu:8080/openrdf-workbench/repositories/ohd-r21-nightly/query")

(defun write-class-spreadsheet (ont pattern &key file-path print)
  (let ((results nil)
	(headers nil))

    ;; fetch results
    ;; note: the IAO annotation relations can be figured out by looking at the variable name
    ;; for now I am going to ignore the editor notes
    (setf results
	  ;;(sparql `(:select (?uri ?label ?alternative_term ?definition ?term_editor)
	  (sparql `(:select (?uri ?label ?alternative_term ?definition)
			    ;;(:limit 10)
			    ()
			    (?uri !rdfs:label ?label)
			    (:optional (?uri !obo:IAO_0000118 ?alternative_term))
			    (:optional (?uri !obo:IAO_0000115 ?definition))
			    ;;(:optional (?uri !obo:IAO_0000116 ?editor_note))
			    ;;(:optional (?uri !obo:IAO_0000117 ?term_editor)) ; not going to worry this for now
			    ;;(:filter (regex (str ?editor) "Pedro")))
			    ;;(:filter (regex (str ?uri) "OHD_0000010")))
			    (:filter (regex (str ?uri) ,pattern)))
			    
		  :kb ont
		  :use-reasoner :pellet
		  :values t))

    ;; write results to file
    ;;(setf headers "uri	label	alternative term	definition	editor note	term editor")
    ;;(setf headers "uri	label	alternative term	definition	term editor")
    (setf headers "uri	label	alternative term	definition")
    (when file-path (write-results-to-spreadsheet results file-path :headers headers))
    
	
    ;; print result to screen
    (when print (print-results-to-screen results))))

(defun write-ohd-spreadsheet (ohd &key file-path print)  
       (let ((results nil)
	     (headers nil))

    ;; fetch results
    ;; note: the IAO annotation relations can be figured out by looking at the variable name
    ;; for now I am going to ignore the editor notes
    (setf results
	  (sparql `(:select (?uri ?label ?alternative_term ?definition ?term_editor)
			    ;;(:limit 10)
			    ()
			    (?uri !rdfs:label ?label)
			    (:optional (?uri !obo:IAO_0000118 ?alternative_term))
			    (:optional (?uri !obo:IAO_0000115 ?definition))
			    ;;(:optional (?uri !obo:IAO_0000116 ?editor_note))
			    (:optional (?uri !obo:IAO_0000117 ?term_editor))
			    ;;(:filter (regex (str ?editor) "Pedro")))
			    ;;(:filter (regex (str ?uri) "OHD_0000010")))
			    (:filter (regex (str ?uri) "OHD_")))
			    
		  :use-reasoner ohd
		  :values t))

    ;; write results to file
    ;;(setf headers "uri	label	alternative term	definition	editor note	term editor")
    (setf headers "uri	label	alternative term	definition	term editor")
    (when file-path (write-results-to-spreadsheet results file-path :headers headers))
    
	
    ;; print result to screen
    (when print (print-results-to-screen results))))

(defun write-results-to-spreadsheet (results file-path &key headers)
  (with-open-file (file file-path 
			:direction :output 
			:if-exists :supersede 
			:if-does-not-exist :create 
			:external-format :utf8)
	
    ;; write headers
    (when headers (format file "~a~%" headers))
	
    (loop 
       for r in results do
       ;; remove nils
       (setf r
	     (loop for e in r do
		  (if (not e) (setf e " ")) collecting e))
       ;;(format t "~{~a~^	~}~%" r)
       (format file "~{~a~^	~}~%" r))))

(defun print-results-to-screen (results &key headers)
  (when headers (format t "~a~%" headers))
  (loop 
     for r in results do
     ;; remove nils
     (setf r
	   (loop for e in r do
		(if (not e) (setf e " ")) collecting e))
     (format t "~{~a~^	~}~%" r)))

(defun foo (ont &key pattern)
  (let ((query nil))
    (cond 
      (pattern
       (setf query
	     `(:select (?uri ?label ?definition)
		       (:limit 10)
		       (?uri !rdfs:label ?label)
		       (:optional (?uri !obo:IAO_0000115 ?definition))
		       (:filter (regex (str ?uri) ,pattern))))) 
      (t 
       (setf query 
	     `(:select (?uri ?label ?definition)
			     (:limit 10)
			     (?uri !rdfs:label ?label)
			     (:optional (?uri !obo:IAO_0000115 ?definition))))))
    ;;query
    (sparql query
	    :kb ont
	    :use-reasoner :pellet
	    :values t)))
