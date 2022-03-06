;; config.el
(provide 'config)

;; UI
(column-number-mode)
(menu-bar-mode 1)
(tool-bar-mode -1)

(require 'color-theme)
(require 'zenburn)
(color-theme-initialize)
(color-theme-zenburn)
(set-face-foreground 'font-lock-comment-face "#fc9")

;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

;; Indentation setting for various languages.
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

;; Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
(when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Custom key sequences.
;; (global-set-key (kbd "C-c t") 'show-current-time)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)

(add-hook 'Info-mode-hook
          (lambda ()
            (local-set-key (kbd "æ") 'Info-backward-node)
            (local-set-key (kbd "'") 'Info-forward-node)
))

;; ;; Icicles last because of key bindings
;; (require 'icicles)
;; (icy-mode 1)

;; Completion
(require 'ido)
(ido-mode t)
