;; init-2.el
(provide 'init-2)

;; UI
(require 'color-theme)
(require 'zenburn)
(color-theme-initialize)
(color-theme-zenburn)

;; Icicles last because of key bindings
(require 'icicles)
(icy-mode 1)