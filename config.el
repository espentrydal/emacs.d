;; config.el
(provide 'config)

;; UI
(column-number-mode)
(menu-bar-mode 1)
(tool-bar-mode -1)
;; Theme
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

;; Completion
(require 'ido)
(ido-mode t)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
;; Do not move the current file while creating backup.
(setq backup-by-copying t)
;; Disable lockfiles.
(setq create-lockfiles nil)
;; Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
(unless system-type 'windows
        (when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
          (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))

;; custom-file
(cond ((and (= emacs-major-version 22)
            (> emacs-minor-version 2))
       ;; Emacs version 22.3.
       (setq custom-file "h:/33-programmer/emacs.d-22/custom-22.el"))
      (t
       ;; Emacs version 23 or later.
       (setq custom-file "~/33-programmer/emacs.d-22/custom.el")))
(load custom-file)

;; Autoload mode
(global-font-lock-mode 1)

;; Custom key sequences.
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(add-hook 'Info-mode-hook
          (lambda ()
            (local-set-key (kbd "æ") 'Info-backward-node)
            (local-set-key (kbd "'") 'Info-forward-node)
))

;; Org
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
;;(org-export-html-style )
