(defmacro push-items (place &rest items)
  "macro used for pushing muliple items onto a list. 
For example, instead of doing multiple push operations like so:
  (push `(declaration (class !A)) axioms)
  (push `(declaration (class !B)) axioms)
  (push `(declaration (class !C)) axioms)

push-items can be called like so:
  (push-items axioms
              `(declaration (class !A))
              `(declaration (class !B))
              `(declaration (class !C))

created by Alan Ruttenberg 11/26/2013"
  `(setf ,place (append (list ,@items) ,place)))

(defmacro setf+ (string1 &rest string2)
  "A shortcut for str+:
 (setf x (str+ y z)) is the same as (setf+ x y z)"
  `(setf ,string1 (str+ ,string1 ,@string2)))

(defmacro with-axioms (axiom-list &body body)
  `(progn
     (labels
	 ((instance-of (instance class)
	    (push `(declaration (named-individual ,instance)) ,axiom-list)
	    (push `(class-assertion ,class ,instance) ,axiom-list)
	    (push `(annotation-assertion !'asserted type'@ohd ,instance ,class) ,axiom-list))

	  (has-label (uri label)
	    (push `(annotation-assertion !rdfs:label ,uri ,label) ,axiom-list))

	  (has-patient-id (patient-uri patient-id)
	    (push `(data-property-assertion
		    !'patient ID'@ohd ,patient-uri ,patient-id) ,axiom-list))

	  (has-code-value (uri code)
	    (push `(data-property-assertion
		    !'has code value'@ohd ,uri ,code) ,axiom-list))
	  
	  (participates-in (continuant process)
	    (push `(object-property-assertion
		    !'participates in'@ohd ,continuant ,process) ,axiom-list))
	  
	  (has-participant (process continuant)
	    (participates-in continuant process))
	  
	  (realizes (process realizable)
	    (push `(object-property-assertion
		    !'realizes'@ohd ,process ,realizable) ,axiom-list))
	  
	  (located-in (entity location)
	    (push `(object-property-assertion
		    !'is located in'@ohd ,entity ,location) ,axiom-list))
	  
	  (part-of (entity1 entity2)
	    (push `(object-property-assertion
		    !'is part of'@ohd ,entity1 ,entity2) ,axiom-list))
	  
	  (has-part (entity1 entity2)
	    (part-of entity2 entity1))

	  (inheres-in (dependent-entity independent-entity)
	    (push `(object-property-assertion
		    !'inheres in'@ohd ,dependent-entity ,independent-entity) ,axiom-list))

	  (has-role (entity role)
	    (push `(object-property-assertion
		    !'has role'@ohd ,entity ,role) ,axiom-list))

	  (is-about (ice entity)
	    (push `(object-property-assertion
		    !'is about'@ohd ,ice ,entity) ,axiom-list))

	  (is-referent-of (entity icd)
	    (push `(object-property-assertion
		    !'is referent of'@ohd ,entity ,ice) ,axiom-list))
		   
	  (is-dental-restoration-of (material surface)
	    (push `(object-property-assertion
		    !'is dental restoration of'@ohd ,material ,surface) ,axiom-list))

	  (has-occurrence-date (entity date)
	    (push `(data-property-assertion
		    !'occurrence date'@ohd ,entity (:literal ,date !xsd:date)) ,axiom-list))

	  (has-birth-date (uri date)
	    (push `(data-property-assertion
		    !'birth_date'@ohd ,uri (:literal ,date !xsd:date)) ,axiom-list))

	  (has-birth-year (uri year)
	    (push `(data-property-assertion
		    !'birth year'@ohd ,uri (:literal ,year !xsd:gYear)) ,axiom-list))

	  (has-graduation-year (uri year)
	    (push `(data-property-assertion
		    !'graduation year'@ohd ,uri (:literal ,year !xsd:gYear)) ,axiom-list))

	  (has-specified-output (process entity)
	    (push `(object-property-assertion
		    !'has_specified_output'@ohd ,process ,entity) ,axiom-list))

	  (has-specified-input (process entity)
	    (push `(object-property-assertion
		    !'has_specified_input'@ohd ,process ,entity) ,axiom-list)))
       
       ,@body)))
