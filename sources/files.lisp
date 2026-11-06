(in-package :om)

;==================================================

(defun clear-directory (directory)
  "Deletes all files matching the pathname DIRECTORY and prints a message in the Listener."

    (mapc #'om-delete-file (directory directory))
    (om-print (format nil "directory \"~A\" is clear" (directory-namestring directory)))

)