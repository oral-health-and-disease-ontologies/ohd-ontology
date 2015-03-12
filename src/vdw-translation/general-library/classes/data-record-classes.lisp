(defclass data-record (data-resource)
  ((resultset           ; the resultset that record is part of
    :accessor resultset
    :initarg :resultset
    :initform nil)
   (contents            ; record's contents/data
   :accessor contents
   :initarg :contents
   :initform nil)))

(defclass file-record (data-record) 
  ())

(defclass jdbc-results (data-record) 
  ())

(defclass record-list (data-record) 
  ())
