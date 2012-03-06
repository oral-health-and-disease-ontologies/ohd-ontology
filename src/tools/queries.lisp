(defun demo()
  (let ((teeth (load-ontology (to-iri "ohd:imports;fma-jaws-teeth.owl")  :reasoner :pellet))) 
    (sparql '(:select (?label ?tooth) () 
	      (?tooth !rdfs:subClassOf !'Tooth'@ohd)
	      (?tooth !rdfs:subClassOf (!'part'@ohd some !'Buccal surface enamel of tooth'@ohd))
	      (?tooth !rdfs:label ?label)
	      ) :kb teeth
		:syntax :terp
		:use-reasoner :pellet
		:trace "teeth with Buccal surfaces"
		:values nil)))
