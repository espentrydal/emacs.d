;; init.el
(cond ((string-match-p "\\`PC" (system-name))
       ;; SUS
       (let ((default-directory "h:/33-programmer/emacs.d"))
         (add-to-list 'load-path default-directory)))
      ((eq 'windows-nt system-type)
       ;; Windows
       (let ((default-directory "c:/33-programmer/emacs.d"))
         (add-to-list 'load-path default-directory)))
      (t
       ;; Linux og annet
       (let ((default-directory "~/34_01-linux-home/emacs.d"))
         (add-to-list 'load-path default-directory))))

(require 'config)
