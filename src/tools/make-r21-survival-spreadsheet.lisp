(defvar stardog-r21 "http://127.0.0.1:5822/r21db/query")

(defun r21query (query  &rest args &key (expressivity "EL") &allow-other-keys t)
  "Do a query against the r21 store. expressivity is nil, EL, RL, QL, DL(?)"
  (if expressivity
      (apply 'sparql query  :use-reasoner stardog-r21 :geturl-options `(:extra-headers (("SD-Connection-String" ,(format nil "reasoning=~a" expressivity)))) args)
      (apply 'sparql query  :use-reasoner stardog-r21 args)))

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
	 do (setf (gethash uri hash) code)
	 finally  (setq *cdt2string* hash)
	 (return-from cdt2string (cdt2string uri)))))

(defun build-spreadsheet ()
  "Start of the query. Next need to figure out how ?code is to be bound - it isn't in this version. It also appears to return incorrect answers."
  (r21query '(:select (?patient ?tooth ?procedure ?date ?code) (:limit 10)
	      (?toothi !rdf:type !'tooth'@ohd)
	      (?procedurei !rdf:type !'restorative procedure'@ohd)
	      (?procedurei !'occurence date'@ohd ?date)
	      (?toothi !'is part of'@ohd ?personi)
	      (?personi !rdf:type !'homo sapiens'@ohd)
	      (?personi !rdfs:label ?person)
	      (?toothi !rdfs:label ?tooth)
	      (?procedurei !rdfs:label ?procedure)
	      (:optional (?code !'is about'@ohd ?procedurei))
	      )
	    :expressivity "RL" :trace "story of some teeth")
  nil) ;; RL fastest for this?

