(defvar stardog-r21 "http://127.0.0.1:5822/r21db/query")

(defun r21query (query  &rest args &key (expressivity "EL") &allow-other-keys)
  "Do a query against the r21 store. expressivity is nil, EL, RL, QL, DL(?)"
  (if expressivity
      (apply 'sparql query  :use-reasoner stardog-r21 :geturl-options (and expressivity `(:extra-headers (("SD-Connection-String" ,(format nil "reasoning=~a" expressivity))))) args)
      (apply 'sparql query  :use-reasoner stardog-r21 args)))


(defun explain-r21query (query &key (expressivity "EL") geturl-options &aux (url stardog-r21))
  (destructuring-bind (protocol site path) (car (all-matches stardog-r21 "(http)://([^/]*)(.*)" 1 2 3))
    (let ((query-uri (clean-uri site (#"replaceAll" path "query" "explain") protocol nil (format nil "query=~a" query))))

      (apply 'get-url query-uri
	     ;:post `(("query" ,query ))
	     (append geturl-options (list :extra-headers `(("SD-Connection-String" ,(format nil "reasoning=~a" expressivity)) ("Accept" "text/plain"))
				      :dont-cache t :force-refetch t))))))

(defun stardog-db-consistent (&key (expressivity "EL") geturl-options &aux (url stardog-r21))
  (destructuring-bind (protocol site path) (car (all-matches stardog-r21 "(http)://([^/]*)(.*)" 1 2 3))
    (let ((query-uri (clean-uri site (#"replaceAll" path "query" "reasoning/consistency") protocol (make-immediate-object nil :ref) (make-immediate-object nil :ref))))
      (apply 'get-url query-uri
	     
;:post `(("query" ,query ))
	     (append geturl-options (list :extra-headers `(("SD-Connection-String" ,(format nil "reasoning=~a" expressivity)) ("Accept" "text/boolean"))
				      :dont-cache t :force-refetch t))))))



(defvar *cdt2string* nil)

(defun cdt2string (uri)
  "Should be able to do this as part of the whole query but x rdf:type ?type is very slow, so its easier to just build this table"
  (if *cdt2string* 
      (gethash uri *cdt2string*)
      (loop with hash = (make-hash-table)
	 for (class code) in 
	 (r21query '(:select (?class ?code) ()
		     (?class !rdf:type !owl:Class)
		     (?class !dc:identifier ?code)) 
		   :expressivity nil)
	 do (setf (gethash class hash) code)
	 finally  (setq *cdt2string* hash)
	 (return-from cdt2string (cdt2string uri)))))

;; patients are asserted to be male organism or female organism.
(defun build-spreadsheet ()
  "Start of the query. Next need to figure out how ?code is to be bound - it isn't in this version. It also appears to return incorrect answers."
  (r21query '(:select (?person  ?bdate ?tooth ?procedure ?procedurei ?procedure_type ?date ?toothn ?code) (:limit 10 :order-by (?person ?toothn ?date))
	      (?procedurei !rdf:type !'restorative procedure'@ohd) ; procedure instances we are looking for are restorative procedures
	      (:optional (?not !rdfs:subClassOf ?procedurei))
	      (?procedurei !'occurrence date'@ohd ?date) ; echo of which occurs on ?date
	      (?procedurei !'has participant'@ohd ?toothi) ; that involve an instance
	      (?toothi !rdf:type ?toothtype) ; of some type
	      (?toothi !rdf:type !'tooth'@ohd) ; that is a tooth
	      (?toothtype !'ADA universal tooth number'@ohd ?toothn) ; and we want the tooth number 
	      (?toothi !rdfs:label ?tooth) ; and the label of the tooth
	      (?personi !rdf:type !'homo sapiens'@ohd) ; Now there is a person involved
	      (?toothi !'is part of'@ohd ?personi) ; that that tooth is part of
	      (?personi !'birth_date'@ohd ?bdate) ; we want their birth date
	      (?personi !rdfs:label ?person) ; their label 
	      (?procedurei !rdfs:label ?procedure)
	      (?procedurei !rdf:type ?procedure_type)
	      (:optional (?code !'is about'@ohd ?procedure_type))
	      (:filter (bound ?not))
	      )
	    :expressivity "RL" :trace "story of some teeth")
  nil) ;; RL fastest for this?

