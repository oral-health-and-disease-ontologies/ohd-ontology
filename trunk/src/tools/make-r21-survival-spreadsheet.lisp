(defvar stardog-r21 "http://127.0.0.1:5822/r21db/query")
(defvar owlim-lite-r21 "http://localhost:8080/openrdf-workbench/repositories/ohd/query")

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
			     ?tooth 
			     ?procedure 
			     ;;?procedurei 
			     ?procedure_type 
			     ?procedure_label 
			     ?date 
			     ;;?toothn 
			     ?code_label 
			     ?surface_instance_label
			     ;;?surface_type_label
			     ) 
		    (:limit 10 :order-by (?person ?toothn ?date) )

		    ;; (?procedurei !'asserted type'@ohd ?procedure_type) ; procedure instances 
		    ;; (?procedurei !rdfs:label ?procedure) ; label for the procedure instance
		    ;; (?procedure_type !rdfs:subClassOf !'restorative procedure'@ohd) ; narrowed to restorative procedure
		    ;; (?procedure_type !rdfs:label ?procedure_label) ; and the label of the procedure type
		    
		    ;; (?codetype !rdfs:subClassOf !'current dental terminology code'@ohd)
		    ;; (?code !'is about'@ohd ?procedurei) ; get CDT code
		    ;; (?code !'asserted type'@ohd ?codetype) ; get its asserted ype
		    ;; (?codetype !rdfs:label ?code_label) ; then get the label of that
		    
		    ;; (?procedurei !'occurrence date'@ohd ?date) ; of which occurs on ?date

		    ;; (?procedurei !'has participant'@ohd ?toothi) ; that involve an instance
		    (?toothi !'asserted type'@ohd ?toothtype) ; of some type
		    (?toothtype !rdfs:subClassOf !'tooth'@ohd) ; that is a tooth

		    (?toothtype !'ADA universal tooth number'@ohd ?toothn) ; and we want the tooth number 
		    (?toothi !rdfs:label ?tooth) ; and the label of the tooth

		    (?personi !'asserted type'@ohd ?ptype) 
		    (?ptype !rdfs:subClassOf !'homo sapiens'@ohd) ; Now there is a person involved
		    
		    (?toothi !'is part of'@ohd ?personi) ; that that tooth is part of

		    (?personi !'birth_date'@ohd ?bdate) ; we want their birth date
		    (?personi !rdfs:label ?person)	; their label 
		    
		    ;; (?surfacetype !rdfs:subClassOf !'Surface enamel of tooth'@ohd) ; narrow asserted types to subclass of suface enamel
		    ;; (?surfacetype !rdfs:label ?surface_type_label) ; get label of surface type
		    ;; (?surfacei !'asserted type'@ohd ?surfacetype) ; get surface intsances that are asserted types
		    ;; (?surfacei !'is part of'@ohd ?toothi) ; surface instance is part of tooth instance
		    ;; (?surfacei !rdfs:label ?surface_instance_label) ; get label of surface instance
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
     ;;(?s !rdf:type !obo:FMA_12516)
     ;;(?s !rdfs:label ?l)
     (?surfacetype !rdfs:subClassOf !'Surface enamel of tooth'@ohd)
     (?surfacetype !rdfs:label ?l)
     (?surfacei !'asserted type'@ohd ?surfacetype) 
     (?s !rdf:type ?surfacetype)
     )
   :reasoner 'owlim-lite-r21
   :trace "test owlim query")
  nil)