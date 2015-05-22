(in-package :cl-user)

(defun write-inferred-subclasses (source dest iri)
  (let ((ont (load-ontology source)))
    (instantiate-reasoner ont :pellet)
    (with-ontology inferred (:collecting t :about iri :base !ex:)
	((as (loop for child in (descendants !owl:Thing ont)
		collect `(declaration (class ,child))
		append (loop for parent in (parents child ont)
			  collect 
			  `(sub-class-of ,child ,parent)))))
      (write-rdfxml inferred dest))))

;; see https://groups.google.com/d/msg/dentontology-coord/7Fllk7qNtXY/Afor4ahLzyEJ
;; (write-inferred-subclasses "ohd:ontology;ohd.owl" "ohd:ontology;ohd-subclasses.owl" !obo:ohd/subclasses.owl)
