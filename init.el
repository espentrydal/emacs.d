;; init.el
(add-to-list 'load-path "~/33-programmer/emacs.d")

(cond ((and (= emacs-major-version 22)
            (> emacs-minor-version 2))
       ;; Emacs version 22.3
       (let ((default-directory "h:/33-programmer/emacs.d/elisp-22"))
	 (if (file-directory-p default-directory)
             ((add-to-list 'load-path "h:/33-programmer/emacs.d/elisp-22")
	      (normal-top-level-add-subdirs-to-load-path))))
       (require 'config-22))
      (t
       ;; Emacs version 23 or later
       (let ((default-directory "~/33-programmer/emacs.d/elisp"))
	 (if (file-directory-p default-directory)
             ((add-to-list 'load-path "~/33-programmer/emacs.d/elisp")
	      (normal-top-level-add-subdirs-to-load-path))))
       (require 'config)))

