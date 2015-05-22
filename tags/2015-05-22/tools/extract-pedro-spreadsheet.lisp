#|
Before running this the UNIX newline format must be enforced.
In Emacs do the following to save the file in UNIX format:
C-x RET f UNIX RET
|#
(defun extract-pedro-definitions()
  (let ((axioms nil)
	(ohd nil)
	(uri nil)
	(defn nil)
	(note nil)
	(editor-note-uri nil)
	(match nil)
	(source nil))
    ;; define uri to import ohd ontology
    ;;(setf ohd (make-uri "file:///Users/williamduncan/repos/ohd-ontology/src/ontology/pitt-ub-ohsu-r21/ohd.owl"))
    (setf ohd (make-uri "http://purl.obolibrary.org/obo/ohd/dev/ohd.owl"))
    
    ;; create uri for editor note
    (setf editor-note-uri (make-uri "http://purl.obolibrary.org/obo/IAO_0000116"))

    (with-ontology ont (:collecting t
			:ontology-iri "http://purl.obolibrary.org/ohd/dev/ohd-pedro-definitions.owl")
	((as `(imports ,ohd))
	 (as `(declaration (annotation-property , editor-note-uri)))
	 (with-open-file (f "pedro-definitions.txt" :direction :input)
	   (loop for line = (read-line f nil :eof) until (eq line :eof) do
	        ;; get uri, definition, and source from spreadsheet
		(setf match (split-at-regex line "\\t"))
		(setf uri (make-uri (first match)))
		(setf defn (third match))
		(setf source (fourth match))
		
		;; create editor note annotation
		(when (> (length defn) 1)
		  (setf note (concatenate 'string "Bill Duncan 2/13/2013: Pedro suggests the following definition: " defn))
		  
		  ;; append any source info
		  (when (> (length source) 1)
		    (setf note (concatenate 'string note " (defintion source: " source ")")))
		  
		  ;; add annaaotation axiom to list
		  ;;(print-db note)
		  (push `(annotation-assertion ,editor-note-uri ,uri ,note) axioms))))
	         ;;(as `(annotation-assertion ,editor-note-uri ,uri ,note)))))
	 ;;(print-db axioms)
	 (as axioms))
	 
      ;; return the ontology
      ont)))