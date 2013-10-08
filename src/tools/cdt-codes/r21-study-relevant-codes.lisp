;; translated from "ADA Codes to be included in study_011312_final.docx"

;; ADA Codes to include in “The Story of a Tooth” datamining study

;; Version 01-13-12

;; Description of this document: This is the list of ADA codes that we
;; are including in our “The Story of a Tooth” datamining study. In our
;; database queries, we will only retrieve the listed codes for
;; analysis. Almost all codes are current (as of 1/2012) ADA CDT
;; codes. However, there is the possibility that some historical codes
;; are in the database. Jim Garrett explained that historical codes in
;; the database are listed in the service_archive table. This means we
;; can also access data that use codes that were phased out.

;; From: CDT_2011_Chapter1.pdf  

;; at (Dropbox/DIM grant projects/Eaglesoft,Picasso ontology based query (Pitt, UB, OHSU)/CDT Distribution and OWL conversion work - UB/CDT 2011-2012 PDFs)

;; structure is:

;; (("major grouping"
;;    :includes
;;    (("minor grouping"
;;      :includes (("dxxxx description"))
;;      :rationale <optional rational for minor grouping)
;;     ...
;;     )
;;    :rationale <optional rationale for major grouping>)
;;  ...
;; )
   
(defparameter *study-codes*
  '(
    ("Diagnostic Codes: D0100-D0999"
     :include
     (("" 
       :include
       (("D0120 Periodic Oral Evaluation")
	("D0140 Limited Oral Evaluation")
	("D0150 Comprehensive Oral Evaluation")
	("D0160 Detailed & Extensive Oral Evaluation")
	("D0180 Comprehensive Periodontal Evaluation – New or Established Patient"))))
     :rationale "The diagnostic codes per se don't give us info that we can analyze, but they indicate that the dentist did an exam on a specific date. So, we would then expect to see findings/existing restorations in the database (if the patient had any) on that date. Examination dates would also give us the end date of follow-up for teeth that never incurred another event (i.e., under the assumption that the absence of codes that indicate an event means “no event occurred”.) Note that the way the ADA uses the term “diagnostic codes” here is different from the general meaning of the word.")

    ("Preventive: D1000-D1999"
     :include
     (("" 
       :include
       (("D1351 Sealant")
	("D1352 Preventive Resin Restoration"))))
     :rationale "A sealant is a way to prevent caries or treat early caries.")

    ("Restorative: D2000-D2999"
     :include
     (("Amalgams"
       :include
       (("D2140 amalgam – one surface, primary or permanent")
	("D2150 amalgam – two surfaces, primary or permanent")
	("D2160 amalgam – three surfaces, primary or permanent")
	("D2161 amalgam – four or more surfaces, primary or permanent")))
      ("Resin"
       :include
       (("D2330 resin-based composite – one surface, anterior")
	("D2331 resin-based composite – two surfaces, anterior")
	("D2332 resin-based composite – three surfaces, anterior")
	("D2335 resin-based composite – four or more surfaces or involving incisal angle (anterior)")
	("D2390 resin-based composite crown, anterior")
	("D2391 resin-based composite – one surface, posterior")
	("D2392 resin-based composite – two surfaces, posterior")
	("D2393 resin-based composite – three surfaces, posterior")
	("D2394 resin-based composite – four or more surfaces, posterior")))
      ("Gold Foil"
       :include
       (("D2410 gold foil – one surface")
	("D2420 gold foil – two surfaces")
	("D2430 gold foil – three surfaces")))
      ("Inlay/Onlay (metallic, porcelain or resin)"
       :include
       (("D2510 Inlay-metallic-one surface")
	("D2520 Inlay-metallic-two surfaces")
	("D2530 Inlay-metallic-three or more surfaces")
	("D2542 Onlay-metallic-two surfaces")
	("D2543 Onlay-metallic-three surfaces")
	("D2544 Onlay-metallic-four or more surfaces")
	("D2610 Inlay-porcelain/ceramic-one surface")
	("D2620 Inlay-porcelain/ceramic-two surfaces")
	("D2630 Inlay-porcelain/ceramic-three or more surfaces")
	("D2642 Onlay-porcelain/ceramic-two surfaces")
	("D2643 Onlay-porcelain/ceramic-three surfaces")
	("D2644 Onlay-porcelain/ceramic-four or more surfaces")
	("D2650 Inlay-resin-based composite - one surface")
	("D2651 Inlay-resin-based composite - two surfaces")
	("D2652 Inlay-resin-based composite - three or more surfaces")
	("D2662 Onlay-resin-based composite - two surfaces")
	("D2663 Onlay-resin-based composite - three surfaces")
	("D2664 Onlay-resin-based composite - four or more surfaces")))
      ("Crowns (various materials including resin, porcelain/ceramic or metal (and combinations of metals)"
       :include
       (("D2710 Crown-resin-based composite (indirect)")
	("D2712 Crown-3/4 resin-based composite (indirect)")
	("D2721 Crown-resin with predominantly base metal")
	("D2722 Crown-resin with noble metal")
	("D2740 Crown-porcelain/ceramic substrate")
	("D2750 Crown-porcelain fused to high noble metal")
	("D2751 Crown-porcelain fused to predominantly base metal")
	("D2752 Crown-porcelain fused to noble metal")
	("D2780 Crown-3/4 cast high noble metal")
	("D2781 Crown- 3/4 cast predominantly base metal")
	("D2782 Crown-3/4 cast noble metal")
	("D2783 Crown-3/4 porcelain/ceramic")
	("D2790 Crown-full cast high noble metal")
	("D2791 Crown-full cast predominantly base metal")
	("D2792 Crown-full cast noble metal")
	("D2794 Crown-titanium")
	("D2799 Provisional crown")
	("D2931 Prefabricated stainless steel crown")
	("D2932 Prefabricated resin crown")
	("D2933 Prefabricated stainless steel crown with resin window")
	("D2970 temporary crown (fractured tooth)")))
      (""
       :include
       (("D2950 core buildup, including any pins")
	("D2940 protective restoration")
	("D2952 post and core in addition to crown, indirectly fabricated ")
	("D2954 prefabricated post and core in addition to crown ")
	("D2960 labial veneer (resin laminate) – chairside")
	("D2961 labial veneer (resin laminate) – laboratory")
	("D2962 labial veneer (porcelain laminate) – laboratory"))
       ))
     :rationale "Restorative codes all restorations and materials used in dentistry: amalgams, resins, inlays, onlays, crowns, materials (gold, porcelain), provisional treatments.")

    ("Endodontics: D3000-D3999"
     :include
     (("Pulpotomies"
       :include
       (("D3220 Pulpotomy therapeutic")
	("D3221 Pulpal debridement")
	("D3222 Pulpotomy partial"))
       :rationale "Pulpotomies indicate a formal entry into the pulp chamber, on purpose.")
      (""
       :include
       (("D3310 Endo anterior (Teeth 6 to 11, 22 to 27)")
	("D3320 Endo Premolar (Teeth 4, 5, 12, 13, 20, 21, 28, 29)")
	("D3330 Endo Molar (Teeth 1, 2, 3, 14, 15, 16, 17, 18, 19, 30, 31, 32)")))
      ("endodontic retreatments"
       :include
       (("D3346 Retreatment anterior")
	("D3347 Retreatment premolar")
	("D3348 Retreatment molar"))
       :rationale "endodontic retreatments because those are potential sequelae from leakage of a lousy coronal restoration.")
      ("Apicoectomy"
       :include
       (("D3410 anterior")
	("D3421 premolar")
	("D3425 molar")
	("D3450 Root amputation")))
      (""
       :include
       (("D3920 Hemisection")))
      )
     ;;Apexifications : Excluded
     :Rationale "Endodontic treatment is, in the view of our study, an adverse event, especially as a sequela to a restoration.")

    ;;(Premolar and bicuspid are interchangeable terms)

    ;; Periodontics: (D4000-D4999)
    ;; Exclude all

    ;; Removable Prosthodontics: (D5000-D5899)
    ;; Exclude all

    ;; Maxillofacial Prosthetics:
    ;; Exclude all

    ;; Implant Service:
    ;; Exclude all

    ("Fixed Prosthodontics: (D6200-D6999)"
     :include
     (("All pontics codes: D6200-D6254 interim pontic" 
       ;; NOTE: Alan filled in the specific codes here
       :include
       (("D6205 pontic - indirect resin based composite")
	("D6210 pontic - cast high noble metal")
	("D6211 pontic - cast predominantly base metal")
	("D6212 pontic - cast noble metal")
	("D6214 pontic - titanium")
	("D6240 pontic - porcelain fused to high noble metal")
	("D6241 pontic - porcelain fused to predominantly base metal")
	("D6242 pontic - porcelain fused to noble metal")
	("D6245 pontic - porcelain/ceramic")
	("D6250 pontic - resin with high noble metal")
	("D6251 pontic - resin with predominantly base metal")
	("D6252 pontic - resin with noble metal")
	("D6253 provisional pontic")
	("D6254 interim pontic"))
       :rationale "The 6200 set of codes is about pontics so those might not need to be kept because they represent teeth that are not present. However, it might be that patient had the tooth extracted outside of that dentist’s office on an emergency basis, and this dentist is doing the bridge. If there is documentation that the tooth is not present but previous codes indicated it was present, either it got extracted outside the dentist’s office or it’s a coding error. This might need to be a discussion point for us. Normally I would suggest not including it because if there’s indication it was extracted in that office the extraction code would be the end of follow-up. If there’s no extraction code but you know the tooth was there previously, the extraction might have occurred elsewhere but you don’t know when, so you’d need to be making assumptions about when the extraction occurred. ")

      ;;Keep all codes in the 6500-6795 range (include retainers, inlays, onlays and crowns), plus 6972 and 6973.
      ("codes in the 6500-6795 range (include retainers, inlays, onlays and crowns), plus 6972 and 6973"
       ;; NOTE: Alan filled in these values
       :include
       (("D6545 retainer - cast metal for resin bonded fixed prosthesis")
	("D6548 retainer - porcelain/ceramic for resin bonded fixed prosthesis")
	("D6600 inlay - porcelain/ceramic, two surfaces")
	("D6601 inlay - porcelain/ceramic, three or more surfaces")
	("D6602 inlay - cast high noble metal, two surfaces")
	("D6603 inlay - cast high noble metal, three or more surfaces")
	("D6604 inlay - cast predominantly base metal, two surfaces")
	("D6605 inlay - cast predominantly base metal, three or more surfaces")
	("D6606 inlay - cast noble metal, two surfaces")
	("D6607 inlay - cast noble metal, three or more surfaces")
	("D6608 onlay -porcelain/ceramic, two surfaces")
	("D6609 onlay - porcelain/ceramic, three or more surfaces")
	("D6610 onlay - cast high noble metal, two surfaces")
	("D6611 onlay - cast high noble metal, three or more surfaces")
	("D6612 onlay - cast predominantly base metal, two surfaces")
	("D6613 onlay - cast predominantly base metal, three or more surfaces")
	("D6614 onlay - cast noble metal, two surfaces")
	("D6615 onlay - cast noble metal, three or more surfaces")
	("D6624 inlay - titanium")
	("D6634 onlay - titanium")
	("D6710 crown - indirect resin based composite")
	("D6720 crown - resin with high noble metal")
	("D6721 crown - resin with predominantly base metal")
	("D6722 crown - resin with noble metal")
	("D6740 crown - porcelain/ceramic")
	("D6750 crown - porcelain fused to high noble metal")
	("D6751 crown - porcelain fused to predominantly base metal")
	("D6752 crown - porcelain fused to noble metal")
	("D6780 crown - 3/4  cast high noble metal")
	("D6781 crown - 3/4  cast predominantly base metal")
	("D6782 crown - 3/4  cast noble metal")
	("D6783 crown - 3/4  porcelain/ceramic")
	("D6790 crown - full cast high noble metal")
	("D6791 crown - full cast predominantly base metal")
	("D6792 crown - full cast noble metal")
	("D6793 provisional retainer crown")
	("D6794 crown - titanium")
	("D6795 interim retainer crown")
	;; Titus: if using code D6972 (prefabricated post and core) and D6973 (build-up), then D6970 (post and core) must be included.
	("D6970 post and core in addition to fixed partial denture retainer, indirectly fabricated")
	("D6972 prefabricated post and core in addition to fixed partial denture retainer")
	("D6973 core build up for retainer, including any pins"))
       :rationale "Crowns, for example, are restorations that can treat carious teeth. But, bridges where an abutment includes a virgin (or good) tooth are a special case that I am not sure about.")))

    ("Oral Surgery: (D7000-D7999)"
     :include
     ((""
       :include
       (("D7110 extraction (simple, historical code)")
	("D7120 extraction (simple, additional tooth, historical code)")
	("D7140 extraction (simple)")
	("D7210 extraction (complicated) surgical"))
       :rationale "Extraction of a tooth is an adverse event after a restoration")))
    
    ;; Orthodontics: (D8000-D8999)
    ;; Exclude all
    ("Adjunctive General Services: (D9000-D9999)"
     :include
     ((""
       :include
       (("D9110 Palliative treatment of dental pain (if related to a specific tooth)"))
       :rationale "Pain on a tooth is an adverse event after a restoration.")))
    ))

(defparameter *proctype-by-first-d-number*
  '((0 "diagnostic")
    (1 "preventive")
    (2 "restoration")
    (3 "endodontic")
    (4 "periodontic")
    (5 "removable")
    (6 "fixed or implant")
    (7 "oral surgery")
    (8 "orthodontic")
    (9 "misc")))

;; map over every minor group calling f with

;; major group label
;; major group rationale
;; minor group label
;; minor group rationale
;; minor group codes

(defun foreach-study-code-group (f)
  (loop for major-entry in *study-codes* do
       (destructuring-bind (major-label &key include rationale) major-entry
	 (loop with major-include = include and major-rationale = rationale 
	    for minor-entry in major-include do
	      (destructuring-bind (minor-label &key include rationale) minor-entry
		(funcall f major-label major-rationale minor-label rationale
			 (mapcar (lambda(el) (subseq (car el) 0 5)) include)))))))

(defun study-codes-for-major-group-label (regex &aux all)
  (foreach-study-code-group (lambda(major-label major-rationale minor-label minor-rationale codes)
			      (if (all-matches major-label regex)
				  (setq all (append codes all)))))
  all)

(defun study-codes-for-group-label (major-regex &optional minor-regex)
  (let ((major-codes nil)
	(minor-codes nil)
	(all-codes nil))
    ;; first get matches major codes
    ;; (foreach-study-code-group (lambda(major-label major-rationale minor-label minor-rationale codes)
    ;; 			      (if (all-matches major-label major-regex)
    ;; 				  (setq major-codes (append codes major-codes)))))

    ;; ;; get matches for minor codes
    ;; (when minor-regex
    ;;   (foreach-study-code-group (lambda(major-label major-rationale minor-label minor-rationale codes)
    ;; 				  (if (all-matches minor-label minor-regex)
    ;; 				      (setq minor-codes (append codes minor-codes))))))
    
    ;; ;; if getting minor codes, then get matches of intersection of major and minor codes
    ;; ;; I do this in case two minor labels happen to be the same
    ;; ;; otherwise all-codes are the major codes
    ;; (if minor-regex
    ;; 	(setf all-codes (intersection minor-codes major-codes :test #'equalp))
    ;; 	(setf all-codes major-codes))
    
    (foreach-study-code-group 
     (lambda(major-label major-rationale minor-label minor-rationale codes)
       (if
	(and 
	 (all-matches major-label major-regex)
	 (or (not minor-regex) (all-matches minor-label minor-regex))
	 )
	(setq all-codes (append codes all-codes)))))

    ;; return all matching codes
    all-codes))


(defun all-study-codes (&aux all)
  (foreach-study-code-group
   (lambda (major-label major-rationale minor-label minor-rationale codes)
     (setq all (append codes all))))
  all)

(defun describe-study-codes ()
  (foreach-study-code-group (lambda (major-label major-rationale minor-label minor-rationale codes)
			      (print-db major-label major-rationale minor-label minor-rationale codes))))


(defun trace-all-study-codes () ;;(&aux all)
    (let ((all nil))
  ;; (trace-foreach-study-code-group
  ;;  (lambda (major-label major-rationale minor-label minor-rationale codes)
  ;;   (setq all (append codes all))))
  (trace-foreach-study-code-group (function trace-f1))
   ;;(trace-f1 major-label major-rationale minor-label minor-rationale codes))
  all))

(defun trace-describe-study-codes ()
  (trace-foreach-study-code-group (lambda (major-label major-rationale minor-label minor-rationale codes)
			      (print-db major-label major-rationale minor-label minor-rationale codes))))


(defun trace-foreach-study-code-group (f)
  (loop for major-entry in *study-codes* do
       (destructuring-bind (major-label &key include rationale) major-entry
	 (loop with major-include = include and major-rationale = rationale 
	    for minor-entry in major-include do
	      (destructuring-bind (minor-label &key include rationale) minor-entry
		(funcall f major-label major-rationale minor-label rationale
			 (trace-f2 include)))))))
;;(mapcar (lambda(el) (subseq (car el) 0 5)) include)))))))

(defun trace-f1 (major-label major-rationale minor-label minor-rationale codes)
  (setq all (append codes all))) 

(defun trace-f2 (el)
  (subseq (car el) 0 5))








