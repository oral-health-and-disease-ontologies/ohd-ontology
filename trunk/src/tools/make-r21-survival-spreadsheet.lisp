(defvar stardog-r21 "http://127.0.0.1:5822/r21db/query")
;;(defvar owlim-lite-r21 "http://localhost:8080/openrdf-workbench/repositories/ohd/query)
;;(defparameter
;;    owlim-lite-r21 "http://localhost:8080/openrdf-workbench/repositories/ohd-top-10-patients/query")
(defparameter
    owlim-lite-r21 "http://localhost:8080/openrdf-workbench/repositories/owlim-5-se-september/query")

(defun r21query (query  &rest args &key (expressivity "RL") (reasoner 'stardog-r21) &allow-other-keys)
  "Do a query against the r21 store. expressivity is nil, EL, RL, QL, DL(?)"
  (if expressivity
      (apply 'sparql query  :use-reasoner (eval reasoner) :geturl-options 
	     (and expressivity `(:extra-headers (("SD-Connection-String" 
						  ,(format nil "reasoning=~a" expressivity))))) args)
      (apply 'sparql query  :use-reasoner (eval reasoner))))

(def-uri-alias "stardog-asserted-subclassof" !<http://pellet.owldl.com/ns/sdle#directSubClassOf>)
;(def-uri-alias "stardog-asserted-type" !<http://pellet.owldl.com/ns/sdle#directType>)
(def-uri-alias "stardog-asserted-subPropertyOf" !<http://pellet.owldl.com/ns/sdle#directSubPropertyOf>)

(defun explain-r21query (query &key (expressivity "EL") geturl-options &allow-other-keys &aux (url stardog-r21))
  (destructuring-bind (protocol site path) (car (all-matches stardog-r21 "(http)://([^/]*)(.*)" 1 2 3))
    (setq query (sparql-stringify query))
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
(defun build-spreadsheet (&key explain translate (reasoner 'stardog-r21))
  "Start of the query. Next need to figure out how ?code is to be bound - it isn't in this version. It also appears to return incorrect answers."
  (let ((res 
	 (funcall (if explain 'explain-r21query (if translate 'sparql-stringify 'r21query) )
		  '(:select (?person  
			     ?bdate 
			     ;;?tooth 
			     ?toothn 
			     ?procedure 
			     ?date 
			     ;;?code
			     ?surface			     
			     ) 
		    (:limit 10 :order-by (?person ?toothn ?date) )

		    ;; billd: general not about 'asserted type'
		    ;; the 'assert type' relation is used to restrict matching so that
		    ;; instances are matched only with the most direct parent.
		    ;; for example, an instance of tooth 1 is matched not only with the
		    ;; type 'Tooth 1', but also with the types 'Secondary upper molar' and
		    ;; the most general type 'Tooth'.
		    ;; we, however, are only concerned with matching to type 'Tooth 1'.
		    ;; thus, we would use "toothi !'asserted type'@ohd !'Tooth 1'@ohd"

		    ;; get info about persons
		    (?persontype !rdfs:subClassOf !'dental patient'@ohd)
		    ;;(?personi !rdf:type !'homo sapiens'@ohd) 
		    (?personi !'asserted type'@ohd ?persontype) 
		    (?personi !rdfs:label ?person) ; their label 
		    (?personi !'birth_date'@ohd ?bdate) ; their birth date
		    
		    ;; and the tooth that was worked on
		    (?toothtype !rdfs:subClassOf !'tooth'@ohd)
		    ;;(?toothi !rdf:type ?toothtype)
		    (?toothi !'asserted type'@ohd ?toothtype)
		    ;;(?toothi !rdf:type !'tooth'@ohd)
		    (?toothi !'is part of'@ohd ?personi) ; that is part of the person
		    ;;(?toothi !rdfs:label ?tooth) ; the label of the tooth
		    (?toothtype !'ADA universal tooth number'@ohd ?toothn) ; ADA tooth number of tooth
		    
		    ;; and the the surfaces of the tooth
		    ;; (?surfacetype !rdfs:subClassOf !'Surface enamel of tooth'@ohd)
		    ;; ;;(?surfacei !rdf:type ?surfacetype)
		    ;; (?surfacei !'asserted type'@ohd ?surfacetype)
		    ;; ;;(?surfacei !rdf:type !'Surface enamel of tooth'@ohd)
		    ;; (?surfacei !'is part of'@ohd ?toothi) ; surface instance is part of tooth instance
		    ;; (?surfacetype !rdfs:label ?surface)
		    
		    ;; not all procedures include surfaces -- I'm not sure if this works
		    ;; (:optional (?surfacei !rdf:type !'Surface enamel of tooth'@ohd))
		    ;; (:optional (?surfacei !'is part of'@ohd ?toothi)) ; surface instance is part of tooth instance
		    ;; (:optional (?surfacei !rdfs:label ?surface_instance_label)) ; get label of surface instance
		    (:optional (?surfacei !rdf:type !'Surface enamel of tooth'@ohd)
		     (?surfacei !'is part of'@ohd ?toothi) ; surface instance is part of tooth instance
		     (?surfacei !rdfs:label ?surface_instance_label)) ; get label of surface instance
		    
		    ;; and procedure performed on that tooth
		    (?proceduretype !rdfs:subClassOf !'dental procedure'@ohd)
		    ;;(?procedurei !rdf:type ?proceduretype) ; that are dental procedures
		    (?procedurei !'asserted type'@ohd ?proceduretype) 
		    ;;(?procedurei !rdf:type !'dental procedure'@ohd) ; that are dental procedures
		    (?procedurei !'has participant'@ohd ?personi) ; involving that person
		    (?procedurei !'has participant'@ohd ?toothi) ; and involving that tooth
		    (?procedurei !'has participant'@ohd ?surfacei) ; involving that surface
		    ;;(:optional (?procedurei !'has participant'@ohd ?surfacei)) ; involving that surface
		    (?procedurei !'occurrence date'@ohd ?date) ; of which occurs on ?date
		    (?proceduretype !rdfs:label ?procedure) ; label for the procedure instance

		    ;; and cdt code info for the procedure
		    (?codetype !rdfs:subClassOf !'current dental terminology code'@ohd)
		    ;;(?codei !rdf:type ?codetype)
		    (?codei !'asserted type'@ohd ?codetype)
		    ;;(?codei !rdf:type !'current dental terminology code'@ohd)
		    (?codei !'is about'@ohd ?procedurei) ; get CDT code
		    (?codetype !rdfs:label ?code) ; then get the label of that code type
		    
		    ;;(:filter (equal (str ?person) "patient 1096"))
		    (:filter (and (equal (str ?person) "patient 1096") (equal (str ?toothn) "Tooth 5")))
		    )
	    :expressivity "RL" :reasoner reasoner :trace "story of some teeth" :values nil)))
  (if explain res nil)
  )) ;; RL fastest for this?


(defun test-stardog-query ()
 (r21query '(:select (?s ?l)
	     (:limit 10)
	     (?s !rdf:type !obo:FMA_12516)
	     (?s !rdfs:label ?l))))
(defun test-owlim-query ()
  (r21query 
   '(:select (?s ?l)
     (:limit 10)
     (?s !rdf:type !obo:FMA_12516)
     (?s !rdfs:label ?l)
     ;; (?surfacetype !rdfs:subClassOf !'Surface enamel of tooth'@ohd)
     ;; (?surfacetype !rdfs:label ?l)
     ;; (?surfacei !'asserted type'@ohd ?surfacetype) 
     ;; (?s !rdf:type ?surfacetype)
     )
   :reasoner 'owlim-lite-r21
   :trace "test owlim query")
  nil)