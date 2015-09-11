(defclass iterator (data-resource) 
  ((resultset	         ; resultset class that references the iterator
    :accessor resultset
    :initarg :resultset
    :initform nil)
   (next-fn		  ; returns the next result in the result set
    :accessor next-fn
    :initform nil)
   (has-next-fn		   ; returns true if there is a a next result, false otherwise
    :accessor has-next-fn
    :initform nil)
   (has-current-fn          ; returns true if the current result is not empty, false otherwise
    :accessor has-current-fn
    :initform nil)
   (called-next?              ; boolean that is nil unti the iterator's
    :accessor called-next?    ; call next function is called
    :initarg called?
    :initform nil)
   ))

(defclass file-iterator (iterator) 
    ((current-results  ; holds contents of the current line in file
      :accessor line
      :initarg :line
      :initform nil)
     (next-results          ; holds contents of the next line in file
      :accessor next-line
      :initarg next-line
      :initform nil)))


(defclass jdbc-iterator (iterator) 
  ())
    


   
