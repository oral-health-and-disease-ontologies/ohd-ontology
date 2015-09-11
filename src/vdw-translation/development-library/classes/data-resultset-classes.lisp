(defclass data-resultset (data-resource)
  ((data-source          ;data source for the resultset
    :accessor data-source 
    :initarg :data-source
    :initform nil)
   (field-names     ; holds the field/column names of the resultset
    :accessor field-names
    :initarg :field-names
    :initform nil)
   (with-resultset-as-list            ; flag to have the whole resultset as a list in 
    :accessor with-resultset-as-list  ; results-list slot
    :initarg :with-resultset-as-list
    :initform nil)
   (with-records-as-list            ; flag to have the records put in list form
    :accessor with-records-as-list  ; the will be true (by defult) when the resultset
    :initarg :with-records-as-list  ; is held in list (i.e. :with-resultset-as-list is true)
    :initform nil)
   (current-record	 ; holds the current record in resultset
    :accessor current-record
    :initform nil)
   (next-record          ; when applicable, holds the next record after the current record
    :accessor next-record
    :initform nil)
   (resultset-list           ; holds the list of results when the with-resultset-as-list it true
    :accessor resultset-list
    :initarg :resultset-list
    :initform nil)
   (current-place-number       ; holds the number of the current place in results list that 
    :accessor current-place-number   
    :initarg :current-place-number
    :initform nil) 
   (next-place-number       ; holds the number of the next place in results list that 
    :accessor next-place-number   
    :initarg :next-place-number
    :initform nil)
   (iterator             ; returns an iterator for the resultset
    :accessor iterator
    :initarg :iterator
    :initform nil)))

(defclass file-resultset (data-resultset) 
  ((field-names
    :accessor headers
    :initarg :headers
    :initform nil)
   (current-record
    :accessor line
    :initarg :line
    :initform nil)
   (next-record
    :accessor next-line
    :initarg :next-line
    :initform nil)))

(defclass jdbc-resultset (data-resultset)
  ((sql-string                   ; sql string used build he resultset
    :accessor sql-string
    :initarg :sql-string
    :initform nil)
   (current-record
    :accessor current-results
    :initarg :results
    :initform nil)
   (next-record
    :accessor next-results
    :initarg :next-results
    :initform nil)
   (with-records-as-list            ; flag to have the records put in list form
    :accessor with-results-as-list  ; this will be true (by defult) when the resultset
    :initarg :with-results-as-list  ; is held in list (i.e. :with-resultset-as-list is true)
    :initform nil)
   (with-cached-results           ; builds the resultset using CachedRowSetImpl class
    :accessor with-cached-results 
    :initarg :with-cached-results
    :initform nil)
   (cached-results           ; when returning results as list but still connected to data source
    :accessor cached-results ; this holds the link to data source results that converted to lists
    :initarg :cached-results
    :initform nil)))

