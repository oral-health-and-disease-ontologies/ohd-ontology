(defun number-to-fma-tooth (number &key return-tooth-uri return-tooth-name return-tooth-with-number)
  "Translates a tooth number into corresponding fma class.  By default, a cons is returned consisting of the (fma-class . name).  The :return-tooth-uri key specifies that only the uri is returned.  The :return-tooth-name key specifies that only the name is returned."
  (let ((teeth nil)
	(tooth nil))
    (setf teeth
	  #((!obo:FMA_55696 "Right upper third secondary molar tooth" "tooth 1") 
	    (!obo:FMA_55697 "Right upper second secondary molar tooth" "tooth 2")
	    (!obo:FMA_55698 "Right upper first secondary molar tooth" "tooth 3")
	    (!obo:FMA_55688 "Right upper second secondary premolar tooth" "tooth 4")
	    (!obo:FMA_55689 "Right upper first secondary premolar tooth" "tooth 5")
	    (!obo:FMA_55798 "Right upper secondary canine tooth" "tooth 6")
	    (!obo:FMA_55680 "Right upper lateral secondary incisor tooth" "tooth 7")
	    (!obo:FMA_55681 "Right upper central secondary incisor tooth" "tooth 8")
	    (!obo:FMA_55682 "Left upper central secondary incisor tooth" "tooth 9")
	    (!obo:FMA_55683 "Left upper lateral secondary incisor tooth" "tooth 10")
	    (!obo:FMA_55799 "Left upper secondary canine tooth" "tooth 11")
	    (!obo:FMA_55690 "Left upper first secondary premolar tooth" "tooth 12")
	    (!obo:FMA_55691 "Left upper second secondary premolar tooth" "tooth 13")
	    (!obo:FMA_55699 "Left upper first secondary molar tooth" "tooth 14")
	    (!obo:FMA_55700 "Left upper second secondary molar tooth" "tooth 15")
	    (!obo:FMA_55701 "Left upper third secondary molar tooth" "tooth 16")
	    (!obo:FMA_55702 "Left lower third secondary molar tooth" "tooth 17")
	    (!obo:FMA_55703 "Left lower second secondary molar tooth" "tooth 18")
	    (!obo:FMA_55704 "Left lower first secondary molar tooth" "tooth 19")
	    (!obo:FMA_55692 "Left lower second secondary premolar tooth" "tooth 20")
	    (!obo:FMA_55693 "Left lower first secondary premolar tooth" "tooth 21")
	    (!obo:FMA_55687 "Left lower secondary canine tooth" "tooth 22")
	    (!obo:FMA_57141 "Left lower lateral secondary incisor tooth" "tooth 23")
	    (!obo:FMA_57143 "Left lower central secondary incisor tooth" "tooth 24")
	    (!obo:FMA_57142 "Right lower central secondary incisor tooth" "tooth 25")
	    (!obo:FMA_57140 "Right lower lateral secondary incisor tooth" "tooth 26")
	    (!obo:FMA_55686 "Right lower secondary canine tooth" "tooth 27")
	    (!obo:FMA_55694 "Right lower first secondary premolar tooth" "tooth 28")
	    (!obo:FMA_55695 "Right lower second secondary premolar tooth" "tooth 29")
	    (!obo:FMA_55705 "Right lower first secondary molar tooth" "tooth 30")
	    (!obo:FMA_55706 "Right lower second secondary molar tooth" "tooth 31")
	    (!obo:FMA_55707 "Right lower third secondary molar tooth" "tooth 32")))

    (cond
      (return-tooth-uri (setf tooth (first (aref teeth (1- number)))))
      (return-tooth-name (setf tooth (second (aref teeth (1- number)))))
      (return-tooth-with-number (setf tooth (third (aref teeth (1- number)))))
      (t (setf tooth (aref teeth (1- number)))))
    ;; return tooth uri/name
    tooth))

(defun tooth-number-to-fma-name (tooth-number)
  (number-to-fma-tooth tooth-number :return-tooth-name t))

(defun tooth-number-to-tooth-name (tooth-number)
  (number-to-fma-tooth tooth-number :return-tooth-with-number t))

(defun tooth-number-to-uri (tooth-number)
  (number-to-fma-tooth tooth-number :return-tooth-uri t))

(defun fma-surface-uri (surface-name)
  "Return the uri for a tooth surface, based on the FMA ontology, for a given surface name."
  (let ((uri nil))
    (cond
      ((equalp surface-name "buccal") (setf uri !obo:FMA_no_fmaid_Buccal_surface_enamel_of_tooth))
      ((equalp surface-name "distal") (setf uri !obo:FMA_no_fmaid_Distal_surface_enamel_of_tooth))
      ((equalp surface-name "incisal") (setf uri !obo:FMA_no_fmaid_Incisal_surface_enamel_of_tooth))
      ((equalp surface-name "labial") (setf uri !obo:FMA_no_fmaid_Labial_surface_enamel_of_tooth))
      ((equalp surface-name "lingual") (setf uri !obo:FMA_no_fmaid_Lingual_surface_enamel_of_tooth))
      ((equalp surface-name "mesial") (setf uri !obo:FMA_no_fmaid_Mesial_surface_enamel_of_tooth))
      ((equalp surface-name "occlusial") (setf uri !obo:FMA_no_fmaid_Occlusial_surface_enamel_of_tooth)))

    ;; return suface uri
    uri))


(defun surface-name-from-surface-letter (surface-letter)
  "Returns the name of the tooth surface associated with a letter (e.g 'b' -> 'buccal')."
  (let ((surface-name nil))
    ;; remove any leading and trailing spaces from letter
    (setf surface-letter (string-trim " " surface-letter))

    (cond
      ((equalp surface-letter "b") (setf surface-name "buccal"))
      ((equalp surface-letter "d") (setf surface-name "distal"))
      ((equalp surface-letter "i") (setf surface-name "incisal"))
      ((equalp surface-letter "f") (setf surface-name "labial"))
      ((equalp surface-letter "l") (setf surface-name "lingual"))
      ((equalp surface-letter "m") (setf surface-name "mesial"))
      ((equalp surface-letter "o") (setf surface-name "occlusial")))

    ;; return surface name
    surface-name))
