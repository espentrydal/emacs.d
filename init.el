(add-to-list 'load-path "h:/33-programmer/emacs.d")
(let ((default-directory "h:/33-programmer/emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(cond ((and (= emacs-major-version 22)
            (> emacs-minor-version 2))
       ;; Emacs version 22.3.
       (require 'config-22))
      (t
       ;; Emacs version 23 or later.
       (require 'config)))

