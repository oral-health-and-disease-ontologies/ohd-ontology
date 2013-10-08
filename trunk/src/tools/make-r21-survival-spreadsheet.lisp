(defvar stardog-r21 "http://127.0.0.1:5822/r21db/query")
(defparameter 
    owlim-se-r21 "http://localhost:8080/openrdf-workbench/repositories/owlim-se-2013.01.21/query")
;;(defparameter 
;;    owlim-se-r21 "http://localhost:8080/openrdf-workbench/repositories/owlim-se-2013.01.21")
(defparameter 
    owlim-se-r21-remote "http://den287.sdm.buffalo.edu:8080/openrdf-workbench/repositories/ohd-r21-nightly/query")


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
(defun build-spreadsheet (&key explain translate (reasoner 'owlim-se-r21))
  "Start of the query. Next need to figure out how ?code is to be bound - it isn't in this version. It also appears to return incorrect answers."
  (let ((res 
	 (funcall (if explain 'explain-r21query (if translate 'sparql-stringify 'r21query) )
		  '(:select (?patientid ;A. the patient id  
			     ?sex ;B. the patient's sex
			     ?bdate ;C. the patient's birth date 
			     ?toothn ;D. the tooth number
			     ?date ;E. date of procedure or finding
			     ?fx_or_pr ;F. FX for finding PR for procedure 
			     ?fx_or_pr_type ; the type of finding or procedure
			     ?adacode ;G. the ada code of the procedure
			     ;;## ?codecategory ;H. we need to pull out category info codes THIS CAN BE DELETED
			     ;;## ?toothpresent ;I. indicates tooth was present at the exam
					;values are Y (yes), N (no), U (unerupted)
			     ?material ;J - N. material used on tooth / surfaces
			     ?surface ;O - S. diagnosis on each of the five coronal surfaces
			     ;;## ?toothfinding ; diagnosis about whole tooth; WE DON'T HAVE WHOLE TOOTH FINDING YET
			     ?provider ; T. provider who performed procedure
			     )
		    (:limit 10)  ;; experimenting with order by clause
		    ;;(:limit 10 :order-by (?patientid ?date ?toothn))

		    ;; billd: the letters in comments below (e.g. "A - C") refer
		    ;; to the columns in Dan's spread sheet.  I did to make it easier to
		    ;; cross reference the data variables with Dan's variables

		    ;; billd: general not about 'asserted type'
		    ;; the 'assert type' relation is used to restrict matching so that
		    ;; instances are matched only with their most direct parent.
		    ;; for example, an instance of tooth 1 is matched not only with the
		    ;; type 'Tooth 1', but also with the types 'Secondary upper molar' and
		    ;; the most general type 'Tooth'.
		    ;; we, however, are only concerned with matching to type 'Tooth 1'.
		    ;; thus, we would use "toothi !'asserted type'@ohd !'Tooth 1'@ohd"

		    ;; A - C: get info about patient
		    (?patienttype !rdfs:subClassOf !'dental patient'@ohd)
		    (?patienti !'asserted type'@ohd ?patienttype) 
		    (?patienti !rdfs:label ?patientid)	; their label 
		    (?patienttype !rdfs:label ?sex)	; their sex
		    (?patienti !'birth_date'@ohd ?bdate) ; their birth date
		    
		    ;; D. tooth number - the tooth that is part of the patient
		    (?toothtype !rdfs:subClassOf !'Tooth'@ohd)
		    (?toothi !'asserted type'@ohd ?toothtype)
		    (?toothi !'is part of'@ohd ?patienti) ; that is part of the patient
		    ;;(?toothi !rdfs:label ?tooth) ; the label of the tooth
		    (?toothtype !'ADA universal tooth number'@ohd ?toothn) ; ADA tooth number of tooth

		    
		    
		    
		    ;; E. the date of the procedure or finding on the patient
		    ;; F. action class FX for finding PR for procedure
		    ;; this done using a union query to cover the possible matches
		    (:union
		     
		     ;; procedures performed on the tooth
		     ((!'dental procedure'@ohd !rdfs:label ?fx_or_pr) ; print message for fx or pr
		      (?proceduretype !rdfs:subClassOf !'dental procedure'@ohd)
		      (?procedurei !'asserted type'@ohd ?proceduretype) 
		      (?procedurei !'has participant'@ohd ?patienti) ; involving that patient
		      (?procedurei !'has participant'@ohd ?toothi) ; and involving that tooth
		      (?procedurei !'occurrence date'@ohd ?date) ; and occurs on ?date
		      (?proceduretype !rdfs:label ?fx_or_pr_type)) ; label for the procedure type
		     
		     ;; unerupted tooth finding
		     (;;(!'dental finding'@ohd !rdfs:label ?proctype) ; print message for fx or pr
		      (?untoothfxi !'asserted type'@ohd !'unerupted tooth finding'@ohd)
		      (?untoothfxi !'is about'@ohd ?toothi) ;about the tooth
		      (?untoothfxi !'occurrence date'@ohd ?date) ;date of finding
		      (!'unerupted tooth finding'@ohd !rdfs:label ?fx_or_pr_type)) ;label for finding type
		     
		     ;; missing tooth finding
		     ( ;; get the instance of 'Universal tooth number' that is about the tooth type
		      ;; this will be needed in order to link missing tooth finding to a
		      ;; particular tooth number (via the 'has part' universal tooth number)
		      (!'dental finding'@ohd !rdfs:label ?fx_or_pr) ; print message for fx or pr
		      (?utoothnumi !rdf:type !'Universal tooth number'@ohd)
		      (?utoothnumi !'is about'@ohd ?toothtype)
		       
		      ;; get instance of missing tooth finding
		      (?mstoothfxtype !rdfs:subClassOf !'missing tooth finding'@ohd)
		      (?mstoothfxi !'asserted type'@ohd ?mstoothfxtype)

		      ;; get instance of dentition of patient that the finding is about
		      (?dentition !'asserted type'@ohd !'Secondary dentition'@ohd)
		      (?dentition !'is part of'@ohd ?patienti)
		      (?mstoothfxi !'is about'@ohd ?dentition)

		      ;; link the finding to the universal tooth number
		      (?mstoothfxi !'has part'@ohd ?utoothnumi) ;that has part universal tooth number of tooth
		      (?mstoothfxtype !rdfs:label ?fx_or_pr_type))) ; label for finding
		    

		    ;; G. ada code of procedure 
		    ;; note: finding won't have ada codes
		    (:optional
		     (?codetype !rdfs:subClassOf !'current dental terminology code'@ohd)
		     (?codei !'asserted type'@ohd ?codetype)
		     (?codei !'is about'@ohd ?procedurei) ; get cdt code for the procedure
		     (?codetype !rdfs:label ?adacode)) ; then get the label of that code type

		    ;; H. category of ada code THIS IS DELETED

		    ;; J - N. material used in the procedure
		    ;; note: not al procedures have materials
		    (:optional
		     (?materialtype !rdfs:subClassOf !'dental restoration material'@ohd)
		     (?materiali !'asserted type'@ohd ?materialtype)
		     (?procedurei !'has participant'@ohd ?materiali)
		     (?materialtype !rdfs:label ?material))
		    
		    ;; O - S. findings or procedures on surfaces 
		    ;; note: not all procedures include surfaces
		    (:optional 
 		     (?surfacei !rdf:type !'Surface enamel of tooth'@ohd)
		     (?surfacei !'is part of'@ohd ?toothi) ; surface instance is part of tooth instance
		     (?surfacei !rdfs:label ?surface)) ; get label of surface instance
		    
		    ;; ADDED tooth finding about the tooth as a whole (e.g., low density test)
		    
		    
		    ;; T. provider who performed procedure / findingings
		    ;; note: not all procedures / findings list providers
		    (:optional 
		     (?providertype !rdfs:subClassOf !'dental health care provider'@ohd)
		     (?provideri !'asserted type'@ohd ?providertype)
		     (?procedurei !'has participant'@ohd ?provideri)
		     (?provideri !rdfs:label ?provider))
		    
		    ;;(:filter (equal (str ?fx_or_pr_type) "dental procedure"))
		    ;;(:filter (regex (str ?fx_or_pr_type) "dental procedure" "i")) 
		    ;;(:filter (equal ?fx_or_pr_type !'unerupted tooth finding'@ohd))
		    ;;(:filter (equal ?patientid "patient 3000"))
		    )
	    :expressivity "RL" :reasoner reasoner :trace "story of some teeth" :values t)))
  (if explain res nil)
  )) ;; RL fastest for this?


(defun get-caplan-spreadsheet (&key explain translate (reasoner 'owlim-se-r21) single-patient)
  "Start of the query. Next need to figure out how ?code is to be bound - it isn't in this version. It also appears to return incorrect answers."
  (let ((res (multiple-value-list 
  	      (funcall (if explain 'explain-r21query (if translate 'sparql-stringify 'r21query) )

		       ;; variable names in select clause are intended to match with Caplan's spreadsheet: 
		       ;; 2011-10-27 Output suitable for statistical analysis - Caplan (revised after ftf meeting).xls
		       ;; associated column letters are also provided (if possble)
		       ;; see spreadsheet for further details
		       `(:select
			 (?patientid  ;A. the patient id  
			  ?sex	      ;B. the patient's sex
			  ?birthdate  ;C. the patient's birth date 
			  ?tthnum     ;D. the tooth number
			  ?procdate   ;E. date of procedure or finding
			  ?procclass ;F. FX for finding PR for procedure 
			  ?proccode  ;G. the ada code of the procedure
			  ;;?proctype ; H. the type of finding or procedure ; NOT NEEDED
			  ;;## ?tthpres ;I. indicates tooth was present at the exam
					;values are Y (yes), N (no), U (unerupted) ; not implmented yet
			  ?matm	   ; J. material for mesial surface
			  ?mato	   ; K. material for occlusal surface
			  ?matd	   ; L. material for distal surface
			  ?matf	   ; M. material for facial surface
			  ?matl	   ; N. material for lingual surface
			  ?dxm	   ; O. diagnosis on mesial surface
			  ?dxo	   ; P. diagnosis on occlusial surface
			  ?dxd	   ; Q. diagnosis on distal surface
			  ?dxf ; R. diagnosis on facial surface (buccal surface)
			  ?dxl ; S. diagnosis on lingual surface
			  ?provider ; T. provider who performed procedure finding
		    
			  ;; the following variables are used for testing
			  ;; ?surfacemi
			  ;; ?surfacem
			  ;; ?surfaceoi
			  ;; ?surfaceo
			  ;; ?surfacedi
			  ;; ?surfaced
			  ;; ?surfacefi
			  ;; ?surfacef
			  ;; ?surfaceli
			  ;; ?surfacel
			  ;; ?test
			  
			  )
			 (:limit 30)
			 ;;(:limit 10 :order-by (?patientid ?tthnum ?procdate)) ; experimenting with order by clause
					
			 ;; billd: general not about 'asserted type'
			 ;; the 'assert type' relation is used to restrict matching so that
			 ;; instances are matched only with their most direct parent.
			 ;; for example, an instance of tooth 1 is matched not only with the
			 ;; type 'Tooth 1', but also with the types 'Secondary upper molar' and
			 ;; the most general type 'Tooth'.
			 ;; we, however, are only concerned with matching to type 'Tooth 1'.
			 ;; thus, we would use "toothi !'asserted type'@ohd !'Tooth 1'@ohd"
			 ;; A - C: get info about patient
			 (?patienttype !rdfs:subClassOf !'dental patient'@ohd)
			 (?patienti !'asserted type'@ohd ?patienttype) 
			 (?patienti !rdfs:label ?patientid) ; their label 
			 (?patienttype !rdfs:label ?sex) ; their sex
			 (?patienti !'birth_date'@ohd ?birthdate) ; their birth date
		    
			 ;; D. tooth number - the tooth that is part of the patient
			 (?toothtype !rdfs:subClassOf !'Tooth'@ohd)
			 (?toothtype !'ADA universal tooth number'@ohd ?tthnum) ; ADA tooth number of tooth
			 
			 ;; E. the date of the procedure or finding on the patient
			 ;; F. procclass: procedure of finding
			 ;; H. procctype
			 ;; this done using a union query to cover the possible matches
			 (:union
			  ;; find procedures performed on the tooth, ada code, and provider
			  (,@(get-caplan-spreadsheet-procedures-info)
			     ;; find materials used on surfaces (?matm, ... , ?matl)
			    ,@(get-caplan-spreadsheet-materials-info))
			  
			  ;; unerupted tooth findings
			  (,@(get-caplan-spreadsheet-unerupted-tooth-findings-info))
			  
			  ;; caries findings
			  (,@(get-caplan-spreadsheet-caries-findings-info)
			     ;; find diagnosis info about caries lesions
			     ,@(get-caplan-spreadsheet-diagnosis-info))
			  
			  ;; missing tooth findings
			  (,@(get-caplan-spreadsheet-missing-tooth-findings-info))
			  )
			 ,@(when single-patient 
				 `((:filter (equal ?patientid ,(format nil "patient ~a" single-patient)))))
			 )
		       :expressivity "RL" :reasoner reasoner :trace "story of some teeth" :values t))))
    (if explain res nil)
    ;;(pprint res)
  ))


(defun get-caplan-spreadsheet-procedures-info ()
  `(;(!'dental procedure'@ohd !rdfs:label ?procclass) ; label for procclass
  
    ;; find tooth and procedure that involves that tooth
    (?toothi !'asserted type'@ohd ?toothtype)
    (?toothi !'is part of'@ohd ?patienti) ; that is part of the patient

    (?proceduretype !rdfs:subClassOf !'dental procedure'@ohd) 
    (?procedurei !'asserted type'@ohd ?proceduretype) ; instance of procedure
    (?procedurei !'has participant'@ohd ?patienti) ; involving that patient
    (?procedurei !'has participant'@ohd ?toothi) ; and involving that tooth
    (?proceduretype !rdfs:label ?procclass) ; label for procclass
    
    (?procedurei !'occurrence date'@ohd ?procdate) ; and occurs on ?procdate

    
    

    ;; find which role the procedure realizes
    (:union
     ((?toothroletype !rdfs:subClassOf !'tooth to be restored role'@ohd))
     ((?toothroletype !rdfs:subClassOf !'tooth to undergo endodontic procedure role'@ohd))
     ((?toothroletype !rdfs:subClassOf !'tooth to be extracted role'@ohd)))
    (?toothrolei !'asserted type'@ohd ?toothroletype)
    (?toothrolei !'inheres in'@ohd ?toothi)
    (?procedurei !'realizes'@ohd ?toothrolei)
    
    ;; G. ada code of procedure 
    (:optional
     (?codetype !rdfs:subClassOf !'current dental terminology code'@ohd)
     (?codei !'asserted type'@ohd ?codetype)
     (?codei !'is about'@ohd ?procedurei) ; get cdt code for the procedure
     (?codetype !rdfs:label ?proccode)) ; then get the label of that code type

    ;; T. provider who performed procedure
    (:optional 
     (?providertype !rdfs:subClassOf !'dental health care provider'@ohd)
     (?provideri !'asserted type'@ohd ?providertype)
     (?procedurei !'has participant'@ohd ?provideri)
     (?provideri !rdfs:label ?provider))))

(defun get-caplan-spreadsheet-materials-info ()
  ;; precondition:
  ;; the following have bee defined
  ;; tooth (?toothni) part of patient (?patienti))
  ;; procedure has participant tooth
  ;; procedure realizes some tooth role

  ;; find instances of materials that restore / are part of surfaces
  '(;; J - N. material used in the procedure
    ;; J. material on mesial surface
    (:optional
     (?surfacemi !'asserted type'@ohd !'Mesial surface enamel of tooth'@ohd) 
     (?surfacemi !'is part of'@ohd ?toothi)
     (?surfacemi !rdfs:label ?surfacem)
     (?materialtypem !rdfs:subClassOf !'dental restoration material'@ohd)
     (?matmi !'asserted type'@ohd ?materialtypem)
     (?matmi !'is located in'@ohd ?toothi)
     (?matmi !'is dental restoration of'@ohd ?surfacemi)
     (?procedurei !'has participant'@ohd ?surfacemi)
     (?procedurei !'has participant'@ohd ?matmi)
     (?matmi !rdfs:label ?matm))
    
    ;; K. material on occlusial surface
    (:optional
     ;(:union ;; OHD doesn't have Incicsal surface yet
     ; # note: occlusial is mispelled (
       (?surfaceoi !'asserted type'@ohd !'Occlusial surface enamel of tooth'@ohd)
     ;) ((?surfaceoi !'asserted type'@ohd !'Inscisal surface enamel of tooth'@ohd)))
     (?surfaceoi !'is part of'@ohd ?toothi)
     (?surfaceoi !rdfs:label ?surfaceo)
     (?materialtypeo !rdfs:subClassOf !'dental restoration material'@ohd)
     (?matoi !'asserted type'@ohd ?materialtypeo)
     (?matoi !'is located in'@ohd ?toothi)
     (?matoi !'is dental restoration of'@ohd ?surfaceoi)
     (?procedurei !'has participant'@ohd ?surfaceoi)
     (?procedurei !'has participant'@ohd ?matoi)
     (?matoi !rdfs:label ?mato))
   
    ;; L.  material on distal surface
    (:optional
     (?surfacedi !'asserted type'@ohd !'Distal surface enamel of tooth'@ohd) 
     (?surfacedi !'is part of'@ohd ?toothi)
     (?surfacedi !rdfs:label ?surfaced)
     (?materialtyped !rdfs:subClassOf !'dental restoration material'@ohd)
     (?matdi !'asserted type'@ohd ?materialtyped)
     (?matdi !'is located in'@ohd ?toothi)
     (?matdi !'is dental restoration of'@ohd ?surfacedi)
     (?procedurei !'has participant'@ohd ?surfacedi)
     (?procedurei !'has participant'@ohd ?matdi)
     (?matdi !rdfs:label ?matd))
   
    ;; M.  material on facial surface
    (:optional
     (:union
      ((?surfacefi !'asserted type'@ohd !'Buccal surface enamel of tooth'@ohd))
      ((?surfacefi !'asserted type'@ohd !'Labial surface enamel of tooth'@ohd)))
     (?surfacefi !'is part of'@ohd ?toothi)
     (?surfacefi !rdfs:label ?surfacef)
     (?materialtypef !rdfs:subClassOf !'dental restoration material'@ohd)
     (?matfi !'asserted type'@ohd ?materialtypef)
     (?matfi !'is located in'@ohd ?toothi)
     (?matfi !'is dental restoration of'@ohd ?surfacefi)
     (?procedurei !'has participant'@ohd ?surfacefi)
     (?procedurei !'has participant'@ohd ?matfi)
     (?matfi !rdfs:label ?matf))
   
  
    ;; N.  material on lingual surface
    (:optional
     (?surfaceli !'asserted type'@ohd !'Lingual surface enamel of tooth'@ohd) 
     (?surfaceli !'is part of'@ohd ?toothi)
     (?surfaceli !rdfs:label ?surfacel)
     (?materialtypel !rdfs:subClassOf !'dental restoration material'@ohd)
     (?matli !'asserted type'@ohd ?materialtypel)
     (?matli !'is located in'@ohd ?toothi)
     (?matli !'is dental restoration of'@ohd ?surfaceli)
     (?procedurei !'has participant'@ohd ?surfaceli)
     (?procedurei !'has participant'@ohd ?matli)
     (?matli !rdfs:label ?matl))))

(defun get-caplan-spreadsheet-unerupted-tooth-findings-info ()
  '(;(!'dental finding'@ohd !rdfs:label ?procclass) ; label for procclass
    ;(!'unerupted tooth finding'@ohd !rdfs:label ?proctype) ;label for proctype

    ;; find unerupted tooth that finding is about
    (?utoothi !'asserted type'@ohd ?toothtype)
    (?utoothi !'is part of'@ohd ?patienti) ; that is part of the patient

    (?untoothfxi !rdfs:label ?procclass) ; label for procclass
    
    (?untoothfxi !'asserted type'@ohd !'unerupted tooth finding'@ohd)
    (?untoothfxi !'is about'@ohd ?utoothi)	 ;about the tooth
    (?untoothfxi !'occurrence date'@ohd ?procdate) ;date of finding

    ;; T. provider who reported the finding
    (:optional 
     ;; find exam that has specified output the finding 
     (?exami !'asserted type'@ohd !'dental exam'@ohd)
     (?exami !'has_specified_output'@ohd ?untoothfxi)
		       
     ;; find provider role that is realized by exam
     (?providerroletype !rdfs:subClassOf !'dental health care provider role'@ohd)
     (?providerrolei !'asserted type'@ohd ?providerroletype)
     (?exami !'realizes'@ohd ?providerrolei)

     ;; find the provider that role inhers in
     (?providertype !rdfs:subClassOf !'dental health care provider'@ohd)
     (?provideri !'asserted type'@ohd ?providertype)
     (?providerrolei !'inheres in'@ohd ?provideri)
     (?provideri !rdfs:label ?provider))))

(defun get-caplan-spreadsheet-caries-findings-info ()
  `(;(!'dental finding'@ohd !rdfs:label ?procclass) ; label for procclass
    ;(!'caries finding'@ohd !rdfs:label ?proctype)  ;label for proctype
    (?cariesfxi !rdfs:label ?procclass) ; label for procclass

    ;; find tooth that has caries
    (?cariestoothi !'asserted type'@ohd ?toothtype)
    (?cariestoothi !'is part of'@ohd ?patienti) ; that is part of the patient

    ;; get caries finding that is about the some lesion that is part of tooh
    (?lesionontoothi !'asserted type'@ohd !'carious lesion of tooth'@ohd) ; instance of lesion
    (?lesionontoothi !'is part of'@ohd ?cariestoothi) ; part of patient's tooth
        
    ;; find caries finding that is about the lesion
    (?cariesfxi !'asserted type'@ohd !'caries finding'@ohd) ; instance of finding
    (?cariesfxi !'is about'@ohd ?lesionontoothi) ; about lesion
    (?cariesfxi !'occurrence date'@ohd ?procdate) ;date of finding

    ;; T. provider who reported the finding
    (:optional 
     ;; find exam that has specified output the finding 
     (?exami !'asserted type'@ohd !'dental exam'@ohd)
     (?exami !'has_specified_output'@ohd ?cariesfxi)
		       
     ;; find provider role that is realized by exam
     (?providerroletype !rdfs:subClassOf !'dental health care provider role'@ohd)
     (?providerrolei !'asserted type'@ohd ?providerroletype)
     (?exami !'realizes'@ohd ?providerrolei)

     ;; find the provider that role inhers in
     (?providertype !rdfs:subClassOf !'dental health care provider'@ohd)
     (?provideri !'asserted type'@ohd ?providertype)
     (?providerrolei !'inheres in'@ohd ?provideri)
     (?provideri !rdfs:label ?provider))))

(defun get-caplan-spreadsheet-diagnosis-info ()
  `(;; first get info about lesions on surfaces of tooth
    ;,@(get-caplan-spreadsheet-lesions-info)

    ;; precondition:
    ;; ?lesionontoothi is a lesion that is part of ?cariestoothi
    ;; ?carisfxi is a caries finding that is about ?lesionontoothi
    
    ;; method:
    ;; every lesion is part of some surface, so
    ;; find the those surfaces that ?lesioni is a part of
    
    ;; O - S. findings on surfaces 
    ;; O. finding on mesial surface

    (:optional
     (?surfacemi !'asserted type'@ohd !'Mesial surface enamel of tooth'@ohd) 
     (?surfacemi !'is part of'@ohd ?cariestoothi)
     (?surfacemi !rdfs:label ?surfacem)
     (?lesionontoothi !'is part of'@ohd ?surfacemi)
     (?lesionontoothi !rdfs:label ?dxm))

    ;; P. finding on occlusial surface
    ;;    both incisal and occlusial surfaces count as occlusial
    (:optional
     ;(:union ;; OHD doesn't have Incicsal surface yet
     ;; # note: occlusial is mispelled (
     (?surfaceoi !'asserted type'@ohd !'Occlusial surface enamel of tooth'@ohd)
     ;) ((?surfaceoi !'asserted type'@ohd !'Inscisal surface enamel of tooth'@ohd)))
     (?surfaceoi !'is part of'@ohd ?cariestoothi)
     (?surfaceoi !rdfs:label ?surfaceo)
     (?lesionontoothi !'is part of'@ohd ?surfaceoi)
     (?lesionontoothi !rdfs:label ?dxo))

    ;; Q. finding on distal suface
    (:optional
     (?surfacedi !'asserted type'@ohd !'Distal surface enamel of tooth'@ohd) 
     (?surfacedi !'is part of'@ohd ?cariestoothi)
     (?surfacedi !rdfs:label ?surfaced)
     (?lesionontoothi !'is part of'@ohd ?surfacedi)
     (?lesionontoothi !rdfs:label ?dxd))

      
    ;; R. finding on facial surface
    ;;    both buccal and labial surfaces count as a facial surface
    (:optional
     (:union
      ((?surfacefi !'asserted type'@ohd !'Buccal surface enamel of tooth'@ohd))
      ((?surfacefi !'asserted type'@ohd !'Labial surface enamel of tooth'@ohd)))
     (?surfacefi !'is part of'@ohd ?cariestoothi)
     (?surfacefi !rdfs:label ?surfacef)
     (?lesionontoothi !'is part of'@ohd ?surfacefi)
     (?lesionontoothi !rdfs:label ?dxf))

  
    ;; S. finding on lingual suface
    (:optional
     (?surfaceli !'asserted type'@ohd !'Lingual surface enamel of tooth'@ohd) 
     (?surfaceli !'is part of'@ohd ?cariestoothi)
     (?surfaceli !rdfs:label ?surfacel)
     (?lesionontoothi !'is part of'@ohd ?surfaceli)
     (?lesionontoothi !rdfs:label ?dxl))))


(defun get-caplan-spreadsheet-missing-tooth-findings-info ()
  '(;; get the instance of 'Universal tooth number' that is about the tooth type
    ;; this will be needed in order to link missing tooth finding to a
    ;; particular tooth number (via the 'has part' universal tooth number)
    ;(!'dental finding'@ohd !rdfs:label ?procclass) ; label for procclass
    ;(!'missing tooth finding'@ohd !rdfs:label ?proctype) ; label for proctype
    (?mstoothfxi !rdfs:label ?procclass) ; label for procclass

    ;; get instance of missing tooth finding
    (?mstoothfxtype !rdfs:subClassOf !'missing tooth finding'@ohd)
    (?mstoothfxi !'asserted type'@ohd ?mstoothfxtype)
    (?mstoothfxi !'occurrence date'@ohd ?procdate) ;date of finding
		      

    ;; get instance of dentition of patient that the finding is about
    (?dentitioni !'asserted type'@ohd !'Secondary dentition'@ohd)
    (?dentitioni !'is part of'@ohd ?patienti)
    (?mstoothfxi !'is about'@ohd ?dentitioni)

    

    ;; link the finding to the universal tooth number
    (?utoothnumi !rdf:type !'Universal tooth number'@ohd)
    (?utoothnumi !'is about'@ohd ?toothtype)

    ;(?missingtoothtype !rdfs:subClassOf !'Tooth'@ohd)
    ;(?utoothnumi !'is about'@ohd ?missingtoothtype)

    (?mstoothfxi !'has part'@ohd ?utoothnumi) ;that has part universal tooth number of tooth
    
    ;; T. provider who reported the finding
    (:optional 
     ;; find exam that has specified output the finding 
     (?exami !'asserted type'@ohd !'dental exam'@ohd)
     (?exami !'has_specified_output'@ohd ?mstoothfxi)
		       
     ;; find provider role that is realized by exam
     (?providerroletype !rdfs:subClassOf !'dental health care provider role'@ohd)
     (?providerrolei !'asserted type'@ohd ?providerroletype)
     (?exami !'realizes'@ohd ?providerrolei)

     ;; find the provider that role inhers in
     (?providertype !rdfs:subClassOf !'dental health care provider'@ohd)
     (?provideri !'asserted type'@ohd ?providertype)
     (?providerrolei !'inheres in'@ohd ?provideri)
     (?provideri !rdfs:label ?provider)))
)

(defun test-tooth-17-patient-7291-found ()
  (sparql
   '(:select (?patientid ?findingdate ?missingtooth ?finding)
     (:limit 10)
     (?patienttype !rdfs:subClassOf !'dental patient'@ohd)
     (?patienti !'asserted type'@ohd ?patienttype) 
     (?patienti !rdfs:label ?patientid)

     (?missingtoothtype !rdfs:subClassOf !'Tooth'@ohd)
     ;;(?missingtoothtype !rdfs:label ?missingtooth)
     (?missingtoothtype !'ADA universal tooth number'@ohd ?missingtooth)

     ;; link finding to universal tooth number
     (?utoothnumi !rdf:type !'Universal tooth number'@ohd)
     (?findingi !'has part'@ohd ?utoothnumi)
     (?utoothnumi !'is about'@ohd ?missingtoothtype)
      

     (?findingtype !rdfs:subClassOf !'missing tooth finding'@ohd)
     (?findingi !'asserted type'@ohd ?findingtype)
     (?findingi !'occurrence date'@ohd ?findingdate)
	    
     (?dentitioni !'asserted type'@ohd !'Secondary dentition'@ohd)
     (?dentitioni !'is part of'@ohd ?patienti)
     (?findingi !'is about'@ohd ?dentitioni)
	   
     (?findingi !rdfs:label ?finding)
     (:filter (equal ?patientid "patient 7291")))
   :use-reasoner (eval 'owlim-se-r21)
   :trace "test"
   :values nil))

(defun test-tooth-17-patient-7291-not-found ()
  (sparql
   '(:select (?patientid ?findingdate ?finding)
     (:limit 10)
     (?patienttype !rdfs:subClassOf !'dental patient'@ohd)
     (?patienti !'asserted type'@ohd ?patienttype) 
     (?patienti !rdfs:label ?patientid)

     ;; when ?toothtype, ?toothni, and ?utoothnumi are included
     ;; tooth 17 is not found
     (?toothtype !rdfs:subClassOf !'Tooth'@ohd)
     (?toothi !'asserted type'@ohd ?toothtype)
     (?toothi !'is part of'@ohd ?patienti)

     ;; link finding to universal tooth number
     (?utoothnumi !rdf:type !'Universal tooth number'@ohd)
     (?utoothnumi !'is about'@ohd ?toothtype)
     (?findingi !'has part'@ohd ?utoothnumi)

     (?findingtype !rdfs:subClassOf !'missing tooth finding'@ohd)
     (?findingi !'asserted type'@ohd ?findingtype)
     (?findingi !'occurrence date'@ohd ?findingdate)
	    
     (?dentitioni !'asserted type'@ohd !'Secondary dentition'@ohd)
     (?dentitioni !'is part of'@ohd ?patienti)
     (?findingi !'is about'@ohd ?dentitioni)
	    
     (?findingi !rdfs:label ?finding)
     (:filter (equal ?patientid "patient 7291")))
   :use-reasoner (eval 'owlim-se-r21)
   :trace "test"
   :values nil))

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
;; try to work with query like this to get direct super class: ;
;; code adapted from: http://www.mail-archive.com/owlim-discussion@ontotext.com/msg01728.html ;

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

(defun test-owlim2 ()
  (r21query 
   '(:select (?s ?l)
     (:limit 10)
     (?s !rdf:type !obo:FMA_12516)
     (?s !rdfs:label ?l))
   ;;:reasoner 'owlim-se-r21-remote
   :reasoner 'owlim-se-r21
   :trace "test2")

   nil)

(defun dental-materials-query ()
  (sparql
   (str+ (owlim-prefixes)
"SELECT DISTINCT ?material ?year (COUNT(?material) as ?m)
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
ORDER BY ?year ?material")

:use-reasoner (eval 'owlim-se-r21)
:trace "dental materials")

  nil)

(defun procedures-on-same-tooth-query ()
  (sparql 
   (str+ (owlim-prefixes)
"select ?patient ?tooth ?surface ?procedure (count(*) as ?total)
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
limit 5")
   
   :use-reasoner (eval 'owlim-se-r21)
   :trace "count of restorative procedures on same tooth and surface")

  nil)

(defun filling-procedures-on-patient-3256-query ()
  (sparql 
   (str+ (owlim-prefixes)
"select ?patient ?tooth ?surface ?procedure ?date
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
order by ?patient ?tooth ?surface ?procedure ?date")

	  :use-reasoner (eval 'owlim-se-r21)
	  :trace "restorative procedures on mesial surface of tooth 30 in patient 3256")
  nil)

(defun ada-code-counts-query ()

  (sparql 
   (str+ (owlim-prefixes)
"select ?adacode (count(?adacodei) as ?total)
where {
  ?adacodetype rdfs:subClassOf obo:CDT_1000001 . # obo:CDT_1000001 -> 'current dental terminology code'
  ?adacodei rdf:type ?adacodetype .
  ?adacodei obo:OHD_0000092 ?adacodetype . # obo:OHD_0000092 -> 'asserted type' 
  ?adacodetype rdfs:label ?adacode .

}
group by ?adacode
order by desc(?total)")
:use-reasoner (eval 'owlim-se-r21)
:trace "count of restorative procedures on same tooth and surface")
   
  nil)

(defun owlim-prefixes ()
  "PREFIX :<http://purl.obolibrary.org/obo/ohd/dev/ohd.owl#>
PREFIX geo-pos:<http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX umbel-ac:<http://umbel.org/umbel/ac/>
PREFIX sw-vocab:<http://www.w3.org/2003/06/sw-vocab-status/ns#>
PREFIX ff:<http://factforge.net/>
PREFIX music-ont:<http://purl.org/ontology/mo/>
PREFIX opencyc-en:<http://sw.opencyc.org/2008/06/10/concept/en/>
PREFIX om:<http://www.ontotext.com/owlim/>
PREFIX dc-term:<http://purl.org/dc/terms/>
PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX factbook:<http://www.daml.org/2001/12/factbook/factbook-ont#>
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
PREFIX wordnet:<http://www.w3.org/2006/03/wn/wn20/instances/>
PREFIX gr:<http://purl.org/goodrelations/v1#>
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
PREFIX ptop:<http://proton.semanticweb.org/protontop#>
PREFIX bbc-pont:<http://purl.org/ontology/po/>
PREFIX xsp:<http://www.owl-ontologies.com/2005/08/07/xsp.owl#>
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
")