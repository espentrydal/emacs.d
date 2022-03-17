;; config-22.el
;; Config for Emacs version 22.3 (Windows)
(provide 'config-22)


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
(setq custom-file "h:/33-programmer/emacs.d/custom-22.el")
(load custom-file)


;; UI
(column-number-mode)
(menu-bar-mode 1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
;; Theme
(require 'color-theme)
(require 'zenburn)
(color-theme-initialize)
(color-theme-zenburn)
;;(set-face-foreground 'font-lock-comment-face "#fc9")
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
;; Autoload mode
(global-font-lock-mode 1)
;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Custom key sequences.
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(global-set-key (kbd "M-;") 'comment-region)
(add-hook 'Info-mode-hook
          (lambda ()
            (local-set-key (kbd "æ") 'Info-backward-node)
            (local-set-key (kbd "'") 'Info-forward-node)))


;; Interactively do things
(require 'ido)
(ido-mode t)
;; Winner mode
(winner-mode 1)
(define-key winner-mode-map (kbd "<M-left>") #'winner-undo)
(define-key winner-mode-map (kbd "<M-right>") #'winner-redo)
;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs "h:/33-programmer/emacs.d/snippets")
(yas-global-mode 1)
;;(require 'remember)


;; Org
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

(defun org-open-current-frame ()
  "Opens file in current frame."
  (interactive)
  (let ((org-link-frame-setup (cons (cons 'file 'find-file) org-link-frame-setup)))
    (org-open-at-point)))
(define-key global-map (kbd "C-c <C-return>") #'org-open-current-frame)
;; Zotxt
(require 'zotxt)
(require 'org-zotxt)

;; Magit
;; (require 'magit)
