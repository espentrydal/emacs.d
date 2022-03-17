;; config.el
;; For Emacs version 23 and later
(provide 'config)


;; Enable installation of packages via straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; Use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-demand t)

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
(setq custom-file "~/33-programmer/emacs.d/custom.el")
(load custom-file)
;; Start server.
(require 'server)
(unless (server-running-p)
  (server-start))


;; UI
(column-number-mode)
(menu-bar-mode 1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
;; Theme
(use-package zenburn-theme
    :init (load-theme 'zenburn t))
;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
;; Consider a period followed by a single space to be end of sentence.
(setq-default sentence-end-double-space nil)
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
;; Enable Paredit.
(use-package paredit
             :init
             (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
             (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
             (add-hook 'ielm-mode-hook 'enable-paredit-mode)
             (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
             (add-hook 'lisp-mode-hook 'enable-paredit-mode))
;; Enable Rainbow Delimiters.
(use-package rainbow-delimiters
             :init
             (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
             (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
             (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
             (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

             :config
             (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
             (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
             (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
             (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
             (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
             (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
             (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
             (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
             (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray
             )
;; Markup modes
(use-package markdown-mode)

;; Custom key sequences.
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(add-hook 'Info-mode-hook
          (lambda ()
            (local-set-key (kbd "æ") 'Info-backward-node)
            (local-set-key (kbd "'") 'Info-forward-node)
))


;; Interactively do things
(ido-mode t)
(ido-everywhere)
(setq ido-enable-flex-matching t)
(fido-mode)
;; Helm
(use-package helm)
(use-package helm-xref)


;; yasnippet
(use-package s)
(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/33-programmer/emacs.d/snippets"))
  :custom
  (require warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  :config
  (yas-global-mode 1))


;; lsp-mode
(use-package lsp-treemacs)
(use-package helm-lsp)
(use-package projectile)
(use-package hydra)
(use-package flycheck)
(use-package company)
(use-package avy)
(use-package which-key)
(use-package helm-xref)
(use-package dap-mode)
(use-package lsp-mode
  :after (yasnippet
          lsp-treemacs
          helm-lsp
          projectile
          hydra
          flycheck
          company
          avy
          which-key
          helm-xref
          dap-mode)
  :init
  (which-key-mode)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 0.1)  ;; clangd is fast
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
)

;; Org
(use-package org
  :straight (:host github :repo "emacs-straight/org-mode"
                   :build (autoloads compile info))
  :init
  (setq org-directory "~/22-org")
  (setq org-special-ctrl-a/e t)
  (defun org-open-current-frame ()
    "Opens file in current frame."
    (interactive)
    (let ((org-link-frame-setup (cons (cons 'file 'find-file) org-link-frame-setup)))
      (org-open-at-point)))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c <C-return>" . org-open-current-frame)))

;;(use-package helm-bibtex
;;  :after helm)
;;(use-package org-ref
;;  :after org)
;;(use-package citar)
;;(use-package org-roam-bibtex
;;  :straight (:host github :repo "org-roam/org-roam-bibtex")
;;  :after (org-roam helm-bibtex org-ref citar))

(use-package org-attach-screenshot
  :after org
  :bind ("C-c s" . org-attach-screenshot)
  :config (setq org-attach-screenshot-dirfunction
        (lambda ()
          (progn (cl-assert (buffer-file-name))
             (concat (file-name-sans-extension (buffer-file-name))
                 "-att")))
        org-attach-screenshot-command-line "gnome-screenshot -a -f %f"))

(use-package org-download
  :straight (:host github :repo "abo-abo/org-download")
  :after org
  :custom (setq org-download-image-dir "~/org/assets/images")
  :bind ("C-c w . org-download-clipboard"))

;; Org exporter
(require 'ox)
(use-package ox-hugo
  :after ox
  :custom
  (setq org-hugo-base-dir "~/03-hugo")
  (setq org-hugo-default-section-directory "posts"))

;; Winner mode
(winner-mode 1)
(define-key winner-mode-map (kbd "<M-left>") #'winner-undo)
(define-key winner-mode-map (kbd "<M-right>") #'winner-redo)

;; PDF-tools
(use-package pdf-tools
  :straight (:host github :repo "vedang/pdf-tools"))
