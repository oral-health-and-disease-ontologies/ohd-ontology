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
              `(declaration (class !C)))

created by Alan Ruttenberg 11/26/2013"
`(setf ,place (append (list ,@items) ,place)))


(defmacro push-instance (axioms instance class)
  "macro used for creating instances of OHD data.
example usage: (push-instance x !a !A)"
  `(progn 
     (push (list 'declaration (list 'named-individual ,instance)) ,axioms)
     (push (list 'class-assertion ,class ,instance) ,axioms)
     (push (list 'annotation-assertion !'asserted type'@ohd ,instance ,class) ,axioms)))

;; THIS ALSO WORKS
;; (defmacro push-instance (axioms instance class)
;;   "macro used for creating instances of OHD data.
;; example usage: (push-instance x !a !A)
;; created by: Bill Duncan 1/8/2014"
;;   `(push-items ,axioms
;; 	       (list 'declaration (list 'named-individual ,instance))
;; 	       (list 'class-assertion ,class ,instance)
;; 	       (list 'annotation-assertion !'asserted type'@ohd ,instance ,class)))

;; THIS ALSO WORKS
;; (defmacro push-instance (axioms instance class)
;;   "macro used for creating instances of OHD data.
;; example usage: (push-instance x !a !A)
;; created by: Bill Duncan 1/8/2014"
;;   `(setf ,axioms
;; 	 (append 
;; 	  (list (list 'declaration (list 'named-individual ,instance)))
;; 	  (list (list 'class-assertion ,class ,instance))
;; 	  (list (list 'annotation-assertion !'asserted type'@ohd ,instance ,class)) ,axioms)))
