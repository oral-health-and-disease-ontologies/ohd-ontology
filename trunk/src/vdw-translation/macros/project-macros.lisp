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
              `(declaration (class !C))

created by Alan Ruttenberg 11/26/2013"
  `(setf ,place (append (list ,@items) ,place)))

(defmacro setf+ (string1 &rest string2)
  "A shortcut for str+:
 (setf x (str+ y z)) is the same as (setf+ x y z)"
  `(setf ,string1 (str+ ,string1 ,@string2)))
