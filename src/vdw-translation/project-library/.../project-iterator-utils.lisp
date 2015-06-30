;; this contains procedurs for creating iterators for use in a given project
;; the puprose is to make using the with-iterator macro easier
;; instead of specifiying key word parameters to created the iterator, you can use
;; the functions defined in this file create the iterator

(defun vdw-file-iterator (filespec)
  (let (ds iterator) ; data-source and iterator variables
    ;; create connection to data-source
    (setf ds (open-data-source (make-instance 'file-data-source
					      :filespec filespec
					      :headers-line 1)))
    ;; build iterator
    ;; note: get-file-iterator creates resultset for iterator
    (setf iterator (get-file-iterator :data-source ds))

    ;; return the iterator
    iterator))

(defun demographics-iterator ()
  ;; return iterator to demographics file
  (vdw-file-iterator "~/vdw-data/demographics.txt"))

(defun dental-encounter-iterator ()
  ;; return iterator to dental encounter file
  (vdw-file-iterator "~/vdw-data/dental-encounter.txt"))

(defun dental-enrollment-iterator ()
  ;; return iterator to dental enrollment file
  (vdw-file-iterator "~/vdw-data/dental-enrollment.txt"))

(defun dental-procedure-diagnosis-iterator ()
  ;; return iterator to dental procedure diagnosis file
  (vdw-file-iterator "~/vdw-data/dental-procedure-diagnosis.txt"))

(defun provider-iterator ()
  ;; return iterator to provider file
  (vdw-file-iterator "~/vdw-data/provider.txt"))





