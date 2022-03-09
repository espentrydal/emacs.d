;; init.el
(add-to-list 'load-path "h:/33-programmer/emacs.d")

(cond ((and (= emacs-major-version 22)
            (> emacs-minor-version 2))
       ;; Emacs version 22.3
       (add-to-list 'load-path "h:/33-programmer/emacs.d/elisp-22")
       (let ((default-directory "h:/33-programmer/emacs.d/elisp-22"))
         (normal-top-level-add-subdirs-to-load-path))
       (require 'config-22))
      (t
       ;; Emacs version 23 or later
       (add-to-list 'load-path "h:/33-programmer/emacs.d/elisp")
       (let ((default-directory "h:/33-programmer/emacs.d/elisp"))
         (normal-top-level-add-subdirs-to-load-path))
       (require 'config)))

