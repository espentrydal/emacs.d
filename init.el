(add-to-list 'load-path "~/33-programmer/emacs.d-22")
(let ((default-directory "~/33-programmer/emacs.d-22/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'config)

(setq custom-file "~/33-programmer/emacs.d-22/custom.el")
(load custom-file)
