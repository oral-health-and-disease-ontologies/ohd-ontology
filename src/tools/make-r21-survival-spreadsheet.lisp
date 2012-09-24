(defvar stardog-r21 "http://127.0.0.1:5822/r21db/query")
;;(defvar owlim-lite-r21 "http://localhost:8080/openrdf-workbench/repositories/ohd/query)
;;(defparameter
;;    owlim-lite-r21 "http://localhost:8080/openrdf-workbench/repositories/ohd-top-10-patients/query")
(defparameter
    owlim-lite-r21 "http://localhost:8080/openrdf-workbench/repositories/owlim-5-se-september/query")
(defparameter 
    owlim-se-r21 "http://localhost:8080/openrdf-workbench/repositories/owlim-5-se-september/query")


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
			     ?sex
			     ?bdate 
			     ?code
			     ;;?codecategory
			     ?procedure 
			     ?proceduredate
			     ?toothn 
			     ?surface
			     ?material
			     ) 
		    (:limit 10 :order-by (?person ?toothn))

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
		    (?personi !rdfs:label ?person)	; their label 
		    (?persontype !rdfs:label ?sex)	; their sex
		    (?personi !'birth_date'@ohd ?bdate) ; their birth date
		    
		    ;; and the tooth that was worked on
		    (?toothtype !rdfs:subClassOf !'tooth'@ohd)
		    ;;(?toothi !rdf:type ?toothtype)
		    (?toothi !'asserted type'@ohd ?toothtype)
		    ;;(?toothi !rdf:type !'tooth'@ohd)
		    (?toothi !'is part of'@ohd ?personi) ; that is part of the person
		    ;;(?toothi !rdfs:label ?tooth) ; the label of the tooth
		    (?toothtype !'ADA universal tooth number'@ohd ?toothn) ; ADA tooth number of tooth
		    		    
		    ;; not all procedures include surfaces -- I'm not sure if this works
		    ;; (:optional (?surfacei !rdf:type !'Surface enamel of tooth'@ohd))
		    ;; (:optional (?surfacei !'is part of'@ohd ?toothi)) ; surface instance is part of tooth instance
		    ;; (:optional (?surfacei !rdfs:label ?surface)) ; get label of surface instance
		    (:optional 
 		     (?surfacei !rdf:type !'Surface enamel of tooth'@ohd)
		     (?surfacei !'is part of'@ohd ?toothi) ; surface instance is part of tooth instance
		     (?surfacei !rdfs:label ?surface)) ; get label of surface instance
		    
		    ;; and procedure performed on that tooth
		    (?proceduretype !rdfs:subClassOf !'dental procedure'@ohd)
		    ;;(?procedurei !rdf:type ?proceduretype) ; that are dental procedures
		    (?procedurei !'asserted type'@ohd ?proceduretype) 
		    ;;(?procedurei !rdf:type !'dental procedure'@ohd) ; that are dental procedures
		    (?procedurei !'has participant'@ohd ?personi) ; involving that person
		    (?procedurei !'has participant'@ohd ?toothi) ; and involving that tooth
		    (?procedurei !'has participant'@ohd ?surfacei) ; involving that surface
		    ;;(:optional (?procedurei !'has participant'@ohd ?surfacei)) ; involving that surface
		    (?procedurei !'occurrence date'@ohd ?proceduredate) ; of which occurs on ?date
		    (?proceduretype !rdfs:label ?procedure) ; label for the procedure instance

		    ;; and cdt code info for the procedure
		    (?codetype !rdfs:subClassOf !'current dental terminology code'@ohd)
		    ;;(?codei !rdf:type ?codetype)
		    (?codei !'asserted type'@ohd ?codetype)
		    ;;(?codei !rdf:type !'current dental terminology code'@ohd)
		    (?codei !'is about'@ohd ?procedurei) ; get CDT code
		    (?codetype !rdfs:label ?code) ; then get the label of that code type
		    
		    ;; get the super class (or cateogory) that the ada code belongs to
		    ;; according to the ada classification scheme
		    ;; or (in different terms) the direct super class
		    ;; (:optional 
		    ;;  (?codetype !rdfs:subClassOf ?adacategory)
		    ;;  (?adacategory !rdfs:label ?codecategory)
		    ;;  (?adacategory !rdfs:subClassOf ?x))
		    ;; (:filter (not (bound ?x)))
		    
		    ;; and the material used in the procedure
		    (:optional
		     (?materialtype !rdfs:subClassOf !'dental restoration material'@ohd)
		     (?materiali !rdf:type ?materialtype)
		     (?procedurei !'has participant'@ohd ?materiali)
		     (?materialtype !rdfs:label ?material))

		    ;; this was used for testing
		    ;;(:filter (equal (str ?person) "patient 1096"))
		    ;;(:filter (and (equal (str ?person) "patient 1096") (equal (str ?toothn) "Tooth 5")))
		    
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
   '(:select 
     (?super)
     (:limit 10)
     ;;(:bind !obo:FMA_55696 'as ?concept) DOESN'T WORK!
     ;;(?concept !rdfs:subClassOf ?super)
     (?concept !rdfs:subClassOf !'Tooth'@ohd)
     (:optional 
      (?concept !rdfs:subClassOf ?inbetweener)
      (:filter (and (not (equal ?inbetweener ?concept)) (not (equal ?inbetweener ?super)))))
     (:filter (and (not (bound ?inbetweener)) (not (equal ?super ?concept))))
     )
     
   :reasoner 'owlim-se-r21
   :trace "test owlim query")
  
#|
;; try to work with query like this to get direct super class:
;; code adapted from: http://www.mail-archive.com/owlim-discussion@ontotext.com/msg01728.html

SELECT ?super {
    BIND( obo:FMA_55696 as ?concept )
    ?concept rdfs:subClassOf ?super .
    OPTIONAL {
    ?concept rdfs:subClassOf ?inbetweener .
    ?inbetweener rdfs:subClassOf ?super .
    FILTER( ?inbetweener != ?concept && ?inbetweener != ?super )
  }
  FILTER( ! BOUND(?inbetweener) && ?super != ?concept)
}

|#
  
  ;; return nil
  nil)

(defun test2 ()
  (r21query 
   '(:select (?s ?l)
     (:limit 10)
     (?s !rdf:type !obo:FMA_12516)
     (?s !rdfs:label ?l))
   :reasoner 'owlim-se-r21
   :trace "test2")

   nil)

(defun dental-materials-query ()
  (sparql
"
PREFIX :<http://purl.obolibrary.org/obo/ohd/dev/ohd.owl#>
PREFIX geo-pos:<http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX umbel-ac:<http://umbel.org/umbel/ac/>
PREFIX sw-vocab:<http://www.w3.org/2003/06/sw-vocab-status/ns#>
PREFIX ff:<http://factforge.net/>
PREFIX music-ont:<http://purl.org/ontology/mo/>
PREFIX dc-term:<http://purl.org/dc/terms/>
PREFIX om:<http://www.ontotext.com/owlim/>
PREFIX opencyc-en:<http://sw.opencyc.org/2008/06/10/concept/en/>
PREFIX factbook:<http://www.daml.org/2001/12/factbook/factbook-ont#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX pext:<http://proton.semanticweb.org/protonext#>
PREFIX dc:<http://purl.org/dc/elements/1.1/>
PREFIX ot:<http://www.ontotext.com/>
PREFIX onto:<http://www.ontotext.com/>
PREFIX protege:<http://protege.stanford.edu/plugins/owl/protege#>
PREFIX foaf:<http://xmlns.com/foaf/0.1/>
PREFIX yago:<http://mpii.de/yago/resource/>
PREFIX daml:<http://www.daml.org/2001/03/daml+oil#>
PREFIX umbel:<http://umbel.org/umbel#>
PREFIX pkm:<http://proton.semanticweb.org/protonkm#>
PREFIX wordnet16:<http://xmlns.com/wordnet/1.6/>
PREFIX swrlb:<http://www.w3.org/2003/11/swrlb#>
PREFIX owl:<http://www.w3.org/2002/07/owl#>
PREFIX example:<http://example.com/>
PREFIX gr:<http://purl.org/goodrelations/v1#>
PREFIX wordnet:<http://www.w3.org/2006/03/wn/wn20/instances/>
PREFIX opencyc:<http://sw.opencyc.org/concept/>
PREFIX ro:<http://www.obofoundry.org/ro/ro.owl#>
PREFIX wordn-sc:<http://www.w3.org/2006/03/wn/wn20/schema/>
PREFIX nytimes:<http://data.nytimes.com/>
PREFIX dbp-prop:<http://dbpedia.org/property/>
PREFIX owl2:<http://www.w3.org/2006/12/owl2#>
PREFIX geonames:<http://sws.geonames.org/>
PREFIX dcterms:<http://purl.org/dc/terms/>
PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
PREFIX swrl:<http://www.w3.org/2003/11/swrl#>
PREFIX owl2xml:<http://www.w3.org/2006/12/owl2-xml#>
PREFIX dbpedia:<http://dbpedia.org/resource/>
PREFIX oasis:<http://psi.oasis-open.org/iso/639/#>
PREFIX individuals:<http://purl.obolibrary.org/obo/ohd/individuals/>
PREFIX geo-ont:<http://www.geonames.org/ontology#>
PREFIX umbel-en:<http://umbel.org/umbel/ne/wikipedia/>
PREFIX xsp:<http://www.owl-ontologies.com/2005/08/07/xsp.owl#>
PREFIX bbc-pont:<http://purl.org/ontology/po/>
PREFIX ptop:<http://proton.semanticweb.org/protontop#>
PREFIX lingvoj:<http://www.lingvoj.org/ontology#>
PREFIX fb:<http://rdf.freebase.com/ns/>
PREFIX dbtune:<http://dbtune.org/bbc/peel/work/>
PREFIX obo:<http://purl.obolibrary.org/obo/>
PREFIX psys:<http://proton.semanticweb.org/protonsys#>
PREFIX umbel-sc:<http://umbel.org/umbel/sc/>
PREFIX dbp-ont:<http://dbpedia.org/ontology/>
PREFIX ub:<http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl#>
PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>
PREFIX oboInOwl:<http://www.geneontology.org/formats/oboInOwl#>
PREFIX skos:<http://www.w3.org/2004/02/skos/core#>

SELECT DISTINCT ?material ?year (COUNT(?material) as ?m)
WHERE { 
  ?materialtype rdfs:subClassOf obo:OHD_0000000 . # obo:OHD_0000000 -> 'dental restoration material'
  ?materiali rdf:type ?materialtype .
  #?materiali obo:OHD_0000092 ?materialtype . # obo:OHD_0000092 -> 'asserted type'
  ?procedurei rdf:type obo:OHD_0000006 . # obo:OHD_0000006 -> 'filling restoration'
  ?procedurei obo:BFO_0000057 ?materiali . # obo:BFO_0000057 -> 'has participant'
  ?procedurei obo:OHD_0000015 ?proceduredate . # obo:0000015 -> 'occurrence date'
  BIND (YEAR(?proceduredate) as ?year)
  ?materialtype rdfs:label ?material . 
FILTER (?materialtype != obo:OHD_0000000) # obo:OHD_0000000 -> 'dental restoration material'
FILTER (?materialtype != obo:OHD_0000048) # obo:OHD_0000048 -> 'metal'
}
GROUP BY ?material ?year
ORDER BY ?year ?material
"

:use-reasoner (eval 'owlim-se-r21)
:trace "dental materials")

  nil)

(defun procedures-on-same-tooth-query ()

  (sparql 
"
PREFIX :<http://purl.obolibrary.org/obo/ohd/dev/ohd.owl#>
PREFIX geo-pos:<http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX umbel-ac:<http://umbel.org/umbel/ac/>
PREFIX sw-vocab:<http://www.w3.org/2003/06/sw-vocab-status/ns#>
PREFIX ff:<http://factforge.net/>
PREFIX music-ont:<http://purl.org/ontology/mo/>
PREFIX dc-term:<http://purl.org/dc/terms/>
PREFIX om:<http://www.ontotext.com/owlim/>
PREFIX opencyc-en:<http://sw.opencyc.org/2008/06/10/concept/en/>
PREFIX factbook:<http://www.daml.org/2001/12/factbook/factbook-ont#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX pext:<http://proton.semanticweb.org/protonext#>
PREFIX dc:<http://purl.org/dc/elements/1.1/>
PREFIX ot:<http://www.ontotext.com/>
PREFIX onto:<http://www.ontotext.com/>
PREFIX protege:<http://protege.stanford.edu/plugins/owl/protege#>
PREFIX foaf:<http://xmlns.com/foaf/0.1/>
PREFIX yago:<http://mpii.de/yago/resource/>
PREFIX daml:<http://www.daml.org/2001/03/daml+oil#>
PREFIX umbel:<http://umbel.org/umbel#>
PREFIX pkm:<http://proton.semanticweb.org/protonkm#>
PREFIX wordnet16:<http://xmlns.com/wordnet/1.6/>
PREFIX swrlb:<http://www.w3.org/2003/11/swrlb#>
PREFIX owl:<http://www.w3.org/2002/07/owl#>
PREFIX example:<http://example.com/>
PREFIX gr:<http://purl.org/goodrelations/v1#>
PREFIX wordnet:<http://www.w3.org/2006/03/wn/wn20/instances/>
PREFIX opencyc:<http://sw.opencyc.org/concept/>
PREFIX ro:<http://www.obofoundry.org/ro/ro.owl#>
PREFIX wordn-sc:<http://www.w3.org/2006/03/wn/wn20/schema/>
PREFIX nytimes:<http://data.nytimes.com/>
PREFIX dbp-prop:<http://dbpedia.org/property/>
PREFIX owl2:<http://www.w3.org/2006/12/owl2#>
PREFIX geonames:<http://sws.geonames.org/>
PREFIX dcterms:<http://purl.org/dc/terms/>
PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
PREFIX swrl:<http://www.w3.org/2003/11/swrl#>
PREFIX owl2xml:<http://www.w3.org/2006/12/owl2-xml#>
PREFIX dbpedia:<http://dbpedia.org/resource/>
PREFIX oasis:<http://psi.oasis-open.org/iso/639/#>
PREFIX individuals:<http://purl.obolibrary.org/obo/ohd/individuals/>
PREFIX geo-ont:<http://www.geonames.org/ontology#>
PREFIX umbel-en:<http://umbel.org/umbel/ne/wikipedia/>
PREFIX xsp:<http://www.owl-ontologies.com/2005/08/07/xsp.owl#>
PREFIX bbc-pont:<http://purl.org/ontology/po/>
PREFIX ptop:<http://proton.semanticweb.org/protontop#>
PREFIX lingvoj:<http://www.lingvoj.org/ontology#>
PREFIX fb:<http://rdf.freebase.com/ns/>
PREFIX dbtune:<http://dbtune.org/bbc/peel/work/>
PREFIX obo:<http://purl.obolibrary.org/obo/>
PREFIX psys:<http://proton.semanticweb.org/protonsys#>
PREFIX umbel-sc:<http://umbel.org/umbel/sc/>
PREFIX dbp-ont:<http://dbpedia.org/ontology/>
PREFIX ub:<http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl#>
PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>
PREFIX oboInOwl:<http://www.geneontology.org/formats/oboInOwl#>
PREFIX skos:<http://www.w3.org/2004/02/skos/core#>

select ?patient ?tooth ?surface ?procedure (count(*) as ?total)
where {
  ?patienti rdf:type obo:OHD_0000012 .
  ?toothi rdf:type obo:FMA_12516 .
  ?surfacei rdf:type obo:FMA_no_fmaid_Surface_enamel_of_tooth .
  ?procedurei rdf:type obo:OHD_0000006 .
  
  ?toothi obo:BFO_0000050 ?patienti . 
  ?surfacei obo:BFO_0000050 ?toothi . 
  ?procedurei obo:BFO_0000057 ?toothi .
   ?procedurei obo:BFO_0000057 ?surfacei .
  
  ?patienti rdfs:label ?patient .
  ?toothi rdfs:label ?tooth .
  ?surfacei rdfs:label ?surface .
  ?procedurei rdfs:label ?procedure .
}
group by ?patient ?tooth ?surface ?procedure
order by desc(?total) ?patient ?tooth ?surface ?procedure
limit 5
"

	  :use-reasoner (eval 'owlim-se-r21)
	  :trace "count of restorative procedures on same tooth and surface")

  nil)

(defun filling-procedures-on-patient-3256-query ()
  (sparql 
"
PREFIX :<http://purl.obolibrary.org/obo/ohd/dev/ohd.owl#>
PREFIX geo-pos:<http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX umbel-ac:<http://umbel.org/umbel/ac/>
PREFIX sw-vocab:<http://www.w3.org/2003/06/sw-vocab-status/ns#>
PREFIX ff:<http://factforge.net/>
PREFIX music-ont:<http://purl.org/ontology/mo/>
PREFIX dc-term:<http://purl.org/dc/terms/>
PREFIX om:<http://www.ontotext.com/owlim/>
PREFIX opencyc-en:<http://sw.opencyc.org/2008/06/10/concept/en/>
PREFIX factbook:<http://www.daml.org/2001/12/factbook/factbook-ont#>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX pext:<http://proton.semanticweb.org/protonext#>
PREFIX dc:<http://purl.org/dc/elements/1.1/>
PREFIX ot:<http://www.ontotext.com/>
PREFIX onto:<http://www.ontotext.com/>
PREFIX protege:<http://protege.stanford.edu/plugins/owl/protege#>
PREFIX foaf:<http://xmlns.com/foaf/0.1/>
PREFIX yago:<http://mpii.de/yago/resource/>
PREFIX daml:<http://www.daml.org/2001/03/daml+oil#>
PREFIX umbel:<http://umbel.org/umbel#>
PREFIX pkm:<http://proton.semanticweb.org/protonkm#>
PREFIX wordnet16:<http://xmlns.com/wordnet/1.6/>
PREFIX swrlb:<http://www.w3.org/2003/11/swrlb#>
PREFIX owl:<http://www.w3.org/2002/07/owl#>
PREFIX example:<http://example.com/>
PREFIX gr:<http://purl.org/goodrelations/v1#>
PREFIX wordnet:<http://www.w3.org/2006/03/wn/wn20/instances/>
PREFIX opencyc:<http://sw.opencyc.org/concept/>
PREFIX ro:<http://www.obofoundry.org/ro/ro.owl#>
PREFIX wordn-sc:<http://www.w3.org/2006/03/wn/wn20/schema/>
PREFIX nytimes:<http://data.nytimes.com/>
PREFIX dbp-prop:<http://dbpedia.org/property/>
PREFIX owl2:<http://www.w3.org/2006/12/owl2#>
PREFIX geonames:<http://sws.geonames.org/>
PREFIX dcterms:<http://purl.org/dc/terms/>
PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
PREFIX swrl:<http://www.w3.org/2003/11/swrl#>
PREFIX owl2xml:<http://www.w3.org/2006/12/owl2-xml#>
PREFIX dbpedia:<http://dbpedia.org/resource/>
PREFIX oasis:<http://psi.oasis-open.org/iso/639/#>
PREFIX individuals:<http://purl.obolibrary.org/obo/ohd/individuals/>
PREFIX geo-ont:<http://www.geonames.org/ontology#>
PREFIX umbel-en:<http://umbel.org/umbel/ne/wikipedia/>
PREFIX xsp:<http://www.owl-ontologies.com/2005/08/07/xsp.owl#>
PREFIX bbc-pont:<http://purl.org/ontology/po/>
PREFIX ptop:<http://proton.semanticweb.org/protontop#>
PREFIX lingvoj:<http://www.lingvoj.org/ontology#>
PREFIX fb:<http://rdf.freebase.com/ns/>
PREFIX dbtune:<http://dbtune.org/bbc/peel/work/>
PREFIX obo:<http://purl.obolibrary.org/obo/>
PREFIX psys:<http://proton.semanticweb.org/protonsys#>
PREFIX umbel-sc:<http://umbel.org/umbel/sc/>
PREFIX dbp-ont:<http://dbpedia.org/ontology/>
PREFIX ub:<http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl#>
PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>
PREFIX oboInOwl:<http://www.geneontology.org/formats/oboInOwl#>
PREFIX skos:<http://www.w3.org/2004/02/skos/core#>

select ?patient ?tooth ?surface ?procedure ?date
where {
  ?patienti rdf:type obo:OHD_0000012 .
  ?toothi rdf:type obo:FMA_12516 .
  ?surfacei rdf:type obo:FMA_no_fmaid_Surface_enamel_of_tooth .
  ?procedurei rdf:type obo:OHD_0000006 .
  
  ?toothi obo:BFO_0000050 ?patienti . 
  ?surfacei obo:BFO_0000050 ?toothi . 
  ?procedurei obo:BFO_0000057 ?toothi .
  ?procedurei obo:BFO_0000057 ?surfacei .
  ?procedurei obo:OHD_0000015 ?date .
  
  ?patienti rdfs:label ?patient .
  ?toothi rdfs:label ?tooth .
  ?surfacei rdfs:label ?surface .
  ?procedurei rdfs:label ?procedure .
filter (str(?patient) = \"patient 3256\")
filter (str(?tooth) = \"tooth 30 of patient 3256\")
filter (str(?surface) = \"mesial surface of tooth 30 in patient 3256\")
}
order by ?patient ?tooth ?surface ?procedure ?date
"

	  :use-reasoner (eval 'owlim-se-r21)
	  :trace "restorative procedures on mesial surface of tooth 30 in patient 3256")
  nil)