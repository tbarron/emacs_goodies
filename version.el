;;
;; Define a version for the collection of files in this repository
;;
(defun emacs-goodies-version ()
  (interactive)
  (message "emacs goodies 1.0.0")
  )
(global-set-key "\M-v" 'emacs-goodies-version)
