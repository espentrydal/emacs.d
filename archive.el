(provide 'archive)

;; EXWM
(when (eq system-type 'gnu/linux)
  (use-package xelb)
  (use-package exwm
    :init
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (fringe-mode 1)
    (require 'exwm-config)
    (exwm-config-ido)
    (setq exwm-input-global-keys
          `(([?\s-\S-r] . exwm-reset)
            ([?\s-s] . exwm-workspace-switch)
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))
            ,(kbd "s-|") .
            (lambda (command)
              (interactive (list (read-shell-command "$ ")))
              (start-process-shell-command command nil command)))))
  :config (exwm-enable))
