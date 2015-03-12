
(defun str+ (&rest values)
  "A shortcut for concatenting a list of strings.
If you want the values separated by a space use :space for the first argument.

Parameters:
  values:  The string to be concatenated.
Usage:
  (str+ \"string1\"   \"string1\" \"string 3\" ...)
  (str+ s1 s2 s3 ...)

When the keywords :space :tab :comma or :pipe are used as the first argument, the values in the
returned string are separated by the corresponding delimiter.
For example:
  (str+ :space \"s1\" \"s2\" \"s3\") 

returns
  \"s1 s2 s3\"

Code found at:
http://stackoverflow.com/questions/5457346/lisp-function-to-concatenate-a-list-of-strings"
    
    
    ;; see http://www.gigamonkeys.com/book/a-few-format-recipes.html
    ;; for info on format funciton iteration
    (cond 
      ((eq :space (car values))
    	(format nil "~{~a~^ ~}" (cdr values))) ; concatenate with spaces
      ((eq :tab (car values))
       (format nil "~{~a~^	~}" (cdr values))) ; concatenate with tabs
      ((eq :comma (car values))
       (format nil "~{~a~^,~}" (cdr values))) ; concatenate with commas
      ((eq :pipe (car values))
       (format nil "~{~a~^|~}" (cdr values))) ; concatenate with pipe chararcters
      (t
       (format nil "~{~a~}" values))))

(defun str-right (string num)
  "Returns num characters from the right of string. E.g. (str-right \"test\" 2) => \"st\""
  (let ((end nil)
	(start nil))
    (setf end (length string))
    (setf start (- end num))
    (when (>= start 0)
      (setf string (subseq string start end)))))

(defun str-left (string num)
  "Returns num characters from the left of string. E.g. (str-right \"test\" 2) => \"te\""
  (when (<= num (length string))
    (setf string (subseq string 0 num))))

(defmacro str-setf+ (string1 &rest string2)
  "A shortcut for str+:
 (setf+ x (str+ y z)) is the same as (str-setf+ x y z)"
  `(setf ,string1 (str+ ,string1 ,@string2)))


;;;; I was tying to create a way to have multiple delimiting charcters in the str+ arg list
;;;; however I couldn't get it going

    ;; ;; collect delimiters stipped of beggining ":" and prepended with "#\"
    ;; ;; e.g. :space => #\space
    ;; (setf delimit-list
    ;; 	  (loop 
    ;; 	     for s in values
    ;; 	     while (eq (type-of s) 'keyword) ; check for keyword
    ;; 	     collect (format nil "#\\~a" s)))
    ;;          ;;collect (format nil "#\\~a" (subseq s 1)))) ; stip ":" & add "#\

    ;; ;; collect values w/o ":"
    ;; (setf values
    ;; 	  (loop 
    ;; 	     for s in values 
    ;; 	     unless (eq (type-of s) 'keyword) collect s ))

    ;; output concatenated string with optional delimited list
    ;; I COULDN'T GET THIS WORK
          ;; (loop 
	  ;;   for i in delimit-list
	  ;;     for j from 1 to (length z)
	  ;;     do
	  ;;     (when (and delimit-list (< j (length z)))
	  ;; 	(loop for r in q do
	  ;; 	     (setf i (format nil "~a~a" i r))
	  ;; 	     ))
	  ;;     collect i into format-string
	  ;;     finally (return (format nil "~{~a~}" format-string)))
