#|
Creator: Bill Duncan
Creation Date: Aug. 29, 2013
This file contains queries (and perhaps other checks) against both the triple-store and eaglesoft database
to check the "sanity" of the triple store.

For preliminary checks, I ran a number of queries to verify that the three tables from which the patient_history
is built (transactions, existing_conditions, patient_conditions) have their respective date info:

select * from transactions where tran_date is null -- no rows found
select * from existing_services where date_completed is null -- no rows found
select * from existing_services where date_entered is null -- no rows found
select * from patient_conditions where date_entered is null -- no rows found

select * from transactions where length(tran_date) = 0 -- no rows found
select * from existing_services where length(date_completed) = 0 -- no rows found
select * from existing_services where length(date_entered) = 0 -- no rows found
select * from patient_conditions where length(date_entered) = 0 -- no rows found

|#

(defparameter 
    owlim-se-sanity-check "http://localhost:8080/openrdf-workbench/repositories/ohd-r21-nightly/query")

(defun check-patient-visits-for-date (&key patient-id visit-date)
  "Check that the number of visits in the ontology matches the database"
(let ((query-string nil)
      (sql-result nil)
      (sparql-result))

  ;; build sql string
  (setf query-string "
select * ")


;; develop sparql string
	
(setf query-string "
select (count (*) as ?X) where {
?x rdf:type dental_visit: .
?x rdfs:label ?s .
} limit 10 ")	


)

(defun sparql-prefixes ()
  (let ((results nil))
    (setf results (sparql 
		   '(:select (?label)
		     ()
		     (?uri !rdf:type !owl:Class)
		     (?uri !rdfs:label ?label))
		   :use-reasoner owlim-string
		   :trace "class names"
		   :values nil))

    (loop for label in results do
	 

))

(defun print-ohd-sparql-prefixes ()
  ;; do a sparql query to pull out ohd iris with lables
  ;; and parse together prefix list
)