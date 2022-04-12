;; init.el
(add-to-list 'load-path "h:/33-programmer/emacs.d")

(cond ((eq system-type 'windows-nt)
       ;; Emacs windows
       (require 'config-win))
      (t
       ;; Emacs on linux
       (require 'config)))
