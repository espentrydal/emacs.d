;; init.el
(cond ((string-match-p "\\`PC" (system-name))
       ;; Emacs SUS
       (let ((default-directory "h:/33-programmer/emacs.d"))
         (add-to-list 'load-path default-directory))
       (require 'config))
      (t
       ;; Emacs on linux
       (let ((default-directory "~/33-programmer/emacs.d"))
         (add-to-list 'load-path default-directory))
       (require 'config)))


