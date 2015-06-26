(defun old-ohd-uri-list ()
  '(("http://purl.obolibrary.org/obo/OHD_0000008" "tooth to be filled role")
    ("http://purl.obolibrary.org/obo/OHD_0000042" "resin filling restoration")
    ("http://purl.obolibrary.org/obo/OHD_0000055" "tooth to be crowned role")
    ("http://purl.obolibrary.org/obo/OHD_0000041" "amalgam filling restoration")
    ("http://purl.obolibrary.org/obo/OHD_0000144" "porcelain fused to high noble metal dental restoration material")
    ("http://purl.obolibrary.org/obo/OHD_0000049" "female dental patient")
    ("http://purl.obolibrary.org/obo/OHD_0000054" "male dental patient")
    ("http://purl.obolibrary.org/obo/OHD_0000099" "tooth to undergo onlay procedure role")
    ("http://purl.obolibrary.org/obo/OHD_0000047" "tooth to undergo veneer procedure role")
    ("http://purl.obolibrary.org/obo/OHD_0000046" "porcelain laminate veneer restoration")
    ("http://purl.obolibrary.org/obo/OHD_0000095" "ceramic onlay restoration")
    ("http://purl.obolibrary.org/obo/OHD_0000031" "dental caries")
    ("http://purl.obolibrary.org/obo/OHD_0000098" "tooth to undergo inlay procedure role")
    ("http://purl.obolibrary.org/obo/OHD_0000094" "ceramic inlay restoration")
    ("http://purl.obolibrary.org/obo/OHD_0000143" "porcelain fused to noble metal dental restoration material")
    ("http://purl.obolibrary.org/obo/OHD_0000145" "porcelain fused to predominantly base metal dental restoration material")
    ("http://purl.obolibrary.org/obo/OHD_0000045" "resin laminate veneer restoration")
    ("http://purl.obolibrary.org/obo/OHD_0000022" "metallic inlay restoration")
    ("http://purl.obolibrary.org/obo/OHD_0000096" "resin inlay restoration")
    ("http://purl.obolibrary.org/obo/OHD_0000156" "predominantly base metal dental restoration material")
    ("http://purl.obolibrary.org/obo/OHD_0000097" "resin onlay restoration")
    ("http://purl.obolibrary.org/obo/OHD_0000153" "noble metal onlay restoration")
    ("http://purl.obolibrary.org/obo/OHD_0000141" "resin with predominantly base metal dental restoration")
    ("http://purl.obolibrary.org/obo/OHD_0000188" "stainless steel with resin window dental restoration material")
    ("http://purl.obolibrary.org/obo/OHD_0000039" "gold filling restoration"))
  )

(defun find-conflicted-labels ()
  (let
      ((conflicted-label nil)
       (conflicted-list (old-ohd-uri-list)))

    (loop
       for e in conflicted-list
       for uri = (make-uri (car e))
       for label = (second e)
       do
	 ;;(pprint e)
       	 (setf conflicted-label (find-ohd-label-for-uri uri))
	 (format t "uri:~a	original:~a	current:~a~%" uri label conflicted-label)
	 )

    ))
  
(defun find-ohd-label-for-uri (uri)
  (let
      ((uri-label nil)
       (table (label2uri *ohd-label-source*)))

    (loop
       for my-label being the hash-keys
       in table using (hash-value my-uri)
       do
	 (when (equalp my-uri uri)
	   (setf uri-label my-label)
	   (return)))
    
    ;; return label of uri
    uri-label))

(defun check-update-ohd-ontology ()
  (let
      ((conflicted-label nil)
       (conflicted-list (old-ohd-uri-list)))

    (loop
       for e in conflicted-list
       for uri = (make-uri (car e))
       for label = (second e)
       do
	 ;;(pprint e)
       	 (setf conflicted-label (find-ohd-label-for-uri uri))
	 ;; print out labels that don't match
	 (unless (equalp label conflicted-label)
	   (format t "uri:~a	original:~a	current:~a~%" uri label conflicted-label))
	 )

    ))

(defun check-for-temp-uris()
  (let
      ((ohd (load-ontology "/Users/williamduncan/repos/svn/ohd-ontology/trunk/src/ontology/tooth-restoration-research.owl")))
    
    (sparql
     `(:select
       (?uri ?label)
       ()
       (?uri !rdfs:label ?label)
       (:filter (regex (str ?uri) "XXX")))
     :kb ohd)
    ))
