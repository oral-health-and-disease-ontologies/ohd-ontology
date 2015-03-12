(defun project-path-name (logical-path-to-file file-name)
  ;; this funciton is needed so files with underscores don't cause an error
  (pathname (str+ (truename logical-path-to-file) file-name)))

(defparameter *vdw-ohd-label-source*
  (make-instance 'label-source :key :ohd :sources (list (truename "vdw:ontology;ohd-merged.owl"))))










