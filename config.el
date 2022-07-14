;; config.el
(provide 'config)

(cond ((string-match-p "\\`PC" (system-name))
       ;; SUS
       (setq-default url-proxy-services '(("no_proxy" . "sus\\.no")
                                          ("http" . "proxy-ihn.ihelse.net:3128")
			                              ("https" . "proxy-ihn.ihelse.net:3128")))
       (setq default-directory "h:/33-programmer/emacs.d")
       (setq home-dir "h:/")
       (setq custom-file (file-name-concat default-directory "custom-win.el")))
      ((eq 'windows-nt system-type)
       ;; Windows
       (setq default-directory "c:/33-programmer/emacs.d")
       (setq home-dir "c:/")
       (setq custom-file (file-name-concat default-directory "custom-win.el")))
      (t
       ;; Annet
       (setq default-directory (file-truename "~/34_01-linux-home/emacs.d"))
       (setq home-dir (file-truename "~/"))
       (setq custom-file (file-name-concat default-directory "custom.el"))))

(setq user-config-file (file-name-concat default-directory "config.el"))
(load custom-file)

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

;; Start server.
(require 'server)
(unless (server-running-p)
  (server-start))

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . ".tmp/emacs/backup/")))
;; Do not move the current file while creating backup.
(setq backup-by-copying t)
;; Disable lockfiles.
(setq create-lockfiles nil)
;; Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
(unless system-type 'windows
        (when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
          (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))

;; Language and coding system
(set-language-environment "UTF-8")
(setq-default buffer-file-coding-system 'utf-8-unix)

;; UI
(column-number-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
;; Theme
(use-package circadian
  :config
  (setq calendar-latitude 58.969975)
  (setq calendar-longitude 5.733107)
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

(use-package smart-mode-line
  :init
  (sml/setup))
;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(add-hook 'term-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (setq indicate-empty-lines nil)))
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

;; ;; Enable Paredit.
;; (use-package paredit
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
;;   (add-hook 'ielm-mode-hook 'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;;   (add-hook 'scheme-mode-hook 'enable-paredit-mode))

;; Smartparens
(use-package hydra)
(use-package smartparens
  :config
  (require 'smartparens-config))

; Nix-mode
(use-package nix-mode
  :mode "\\.nix\\'")
;; Markup modes
(use-package markdown-mode)

(use-package fontaine
  :straight (:host github :repo "protesilaos/fontaine")
  :init
  (cond ((string-match-p "pop-os" (system-name))
         (setq fontaine-presets
               '((small
                  :default-family "Hack"
                  :default-weight normal
                  :default-height 100
                  :fixed-pitch-family nil ; falls back to :default-family
                  :fixed-pitch-weight nil ; falls back to :default-weight
                  :fixed-pitch-height 1.0
                  :variable-pitch-family "Noto Sans"
                  :variable-pitch-weight normal
                  :variable-pitch-height 1.0
                  :bold-family nil ; use whatever the underlying face has
                  :bold-weight bold
                  :italic-family nil
                  :italic-slant italic
                  :line-spacing nil)
                 (regular
                  :default-family "Iosevka Comfy"
                  :default-weight normal
                  :default-height 145
                  :fixed-pitch-family nil ; falls back to :default-family
                  :fixed-pitch-weight nil ; falls back to :default-weight
                  :fixed-pitch-height 1.0
                  :variable-pitch-family "FiraGO"
                  :variable-pitch-weight normal
                  :variable-pitch-height 1.05
                  :bold-family nil ; use whatever the underlying face has
                  :bold-weight bold
                  :italic-family nil
                  :italic-slant italic
                  :line-spacing nil)
                 (medium
                  :default-family "Source Code Pro"
                  :default-weight normal
                  :default-height 145
                  :fixed-pitch-family nil ; falls back to :default-family
                  :fixed-pitch-weight nil ; falls back to :default-weight
                  :fixed-pitch-height 1.0
                  :variable-pitch-family "Source Sans Pro"
                  :variable-pitch-weight normal
                  :variable-pitch-height 1.05
                  :bold-family nil ; use whatever the underlying face has
                  :bold-weight semibold
                  :italic-family nil
                  :italic-slant italic
                  :line-spacing nil)
                 (large
                  :default-family "Iosevka Comfy"
                  :default-weight semilight
                  :default-height 160
                  :fixed-pitch-family nil ; falls back to :default-family
                  :fixed-pitch-weight nil ; falls back to :default-weight
                  :fixed-pitch-height 1.0
                  :variable-pitch-family "FiraGO"
                  :variable-pitch-weight normal
                  :variable-pitch-height 1.05
                  :bold-family nil ; use whatever the underlying face has
                  :bold-weight bold
                  :italic-family nil
                  :italic-slant italic
                  :line-spacing nil)
                 (presentation
                  :default-family "Iosevka Comfy"
                  :default-weight semilight
                  :default-height 180
                  :fixed-pitch-family nil ; falls back to :default-family
                  :fixed-pitch-weight nil ; falls back to :default-weight
                  :fixed-pitch-height 1.0
                  :variable-pitch-family "FiraGO"
                  :variable-pitch-weight normal
                  :variable-pitch-height 1.05
                  :bold-family nil ; use whatever the underlying face has
                  :bold-weight bold
                  :italic-family nil
                  :italic-slant italic
                  :line-spacing nil))))
        ((eq 'windows-nt system-type)
         (setq fontaine-presets
               '((regular
                  :default-family "Consolas"
                  :default-weight normal
                  :default-height 135
                  :fixed-pitch-family nil ; falls back to :default-family
                  :fixed-pitch-weight nil ; falls back to :default-weight
                  :fixed-pitch-height 1.0
                  :variable-pitch-family "Georgia"
                  :variable-pitch-weight normal
                  :variable-pitch-height 1.05
                  :bold-family nil ; use whatever the underlying face has
                  :bold-weight bold
                  :italic-family nil
                  :italic-slant italic
                  :line-spacing nil))))
        ((eq 'gnu/linux system-type)
         (setq fontaine-presets
               '((regular
                  :default-family "Source Code Pro"
                  :default-weight normal
                  :default-height 135
                  :fixed-pitch-family nil ; falls back to :default-family
                  :fixed-pitch-weight nil ; falls back to :default-weight
                  :fixed-pitch-height 1.0
                  :variable-pitch-family "Source Sans Pro"
                  :variable-pitch-weight normal
                  :variable-pitch-height 1.05
                  :bold-family nil ; use whatever the underlying face has
                  :bold-weight bold
                  :italic-family nil
                  :italic-slant italic
                  :line-spacing nil)))))
        :config (fontaine-set-preset 'regular))

;; Custom key sequences.
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c z") 'suspend-frame)
(add-hook 'Info-mode-hook
          (lambda ()
            (local-set-key (kbd "æ") 'Info-backward-node)
            (local-set-key (kbd "'") 'Info-forward-node)))
(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key (kbd "å") 'dired-up-directory)))
(global-set-key (kbd "<f7>") (lambda () (interactive) (find-file user-config-file)))
(global-set-key (kbd "C-c C-i") 'ace-swap-window)
;; other hooks
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'Info-mode-hook (progn (lambda () (variable-pitch-mode t))))

;; sr-speedbar
(use-package sr-speedbar
  :config (global-set-key (kbd "<f8>") 'sr-speedbar-toggle))


;; Interactively do things
(ido-mode t)
(ido-everywhere)
(setq ido-enable-flex-matching 1)
(fido-mode)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)
;; Helm
(use-package helm)
(use-package helm-xref)
;; Find things
(setq apropos-sort-by-scores t)

;; create directories if missing dired
(defadvice dired-mark-read-file-name (after rv:dired-create-dir-when-needed (prompt dir op-symbol arg files &optional default) activate)
  (when (member op-symbol '(copy move))
    (let ((directory-name (if (< 1 (length files))
                              ad-return-value
                              (file-name-directory ad-return-value))))
      (when (and (not (file-directory-p directory-name))
                 (y-or-n-p (format "directory %s doesn't exist, create it?" directory-name)))
        (make-directory directory-name t)))))

;; Winner mode
(winner-mode 1)
;; (define-key winner-mode-map (kbd "<M-left>") #'winner-undo)
;; (define-key winner-mode-map (kbd "<M-right>") #'winner-redo)
;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Fix emacs windows behavior
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))
(customize-set-variable 'even-window-sizes nil)     ; avoid resizing

;; Projectile
;; (use-package projectile
;;   :bind ("C-c p" . projectile-command-map)
;;   :init
;;   (projectile-mode +1))
;; Perspectives
(use-package perspective
  :bind ("C-x C-b" . persp-ibuffer)
  :custom
  (persp-mode-prefix-key (kbd "C-t"))
  (persp-state-default-file (file-name-concat user-emacs-directory "persp-exit"))
  :init (persp-mode)
  :config (add-hook 'kill-emacs-hook #'persp-state-save))

;; Consult
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)
  ;; Show more candidates
  ;; (setq vertico-count 20)
  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; yasnippet
(use-package s)
(use-package yasnippet
  :after s
  :init
  (setq yas-snippet-dirs (list (file-name-concat default-directory "snippets")))
  :custom
  (require warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  :config
  (yas-global-mode 1))

;; Magit
(use-package magit
  :straight (magit :type git :host github :repo "magit/magit"))

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
(use-package deadgrep
  :config (global-set-key (kbd "<f5>") #'deadgrep))
(use-package lsp-mode
  :init
  (which-key-mode)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (add-hook 'python-mode-hook 'lsp)
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 0.1) ;; clangd is fast
  :config
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (setq lsp-pylsp-plugins-flake8-ignore ["D100"]))

;; Org
(use-package org
  :straight (:host github :repo "emacs-straight/org-mode"
                   :build (autoloads compile info))
  :init
  (setq org-directory (file-name-concat home-dir "02-org"))
  (setq org-special-ctrl-a/e 'reversed)
  (defun org-open-current-frame ()
    "Opens file in current frame."
    (interactive)
    (let ((org-link-frame-setup (cons (cons 'file 'find-file) org-link-frame-setup)))
      (org-open-at-point)))
  (setq org-structure-template-alist
        '(("a" . "export ascii\n")
          ("c" . "center\n")
          ("C" . "comment\n")
          ("e" . "example\n")
          ("E" . "export")
          ("h" . "export html\n")
          ("l" . "export latex\n")
          ("p" . "src jupyter-python :session py")
          ("q" . "quote\n")
          ("s" . "src")
          ("v" . "verse\n")
          ("n" . "notes\n")))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c <C-return>" . org-open-current-frame)))
;; Time-stamp
(require 'time-stamp)
(add-hook 'write-file-functions 'time-stamp)

;; Denote
(use-package denote
  ;; Remember to check the doc strings of those variables.
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-known-keywords '("emacs" "philosophy" "politics" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))

  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more rigid workflow.
  (setq denote-allow-multi-word-keywords t)

  (setq denote-date-format nil) ; read doc string

  ;; You will not need to `require' all those individually once the
  ;; package is available.
  (require 'denote-retrieve)
  (require 'denote-link)

  ;; By default, we fontify backlinks in their bespoke buffer.
  (setq denote-link-fontify-backlinks t)

  ;; Also see `denote-link-backlinks-display-buffer-action' which is a bit
  ;; advanced.

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer)

  (require 'denote-dired)
  (setq denote-dired-rename-expert nil)

  ;; We use different ways to specify a path for demo purposes.
  (setq denote-dired-directories
        (list denote-directory
              (thread-last denote-directory (expand-file-name "attachments"))
              (expand-file-name "~/Documents/books")))

  ;; Generic (great if you rename files Denote-style in lots of places):
  ;; (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;;
  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)

  ;; Here is a custom, user-level command from one of the examples we
  ;; showed in this manual.  We define it here and add it to a key binding
  ;; below.
  (defun my-denote-journal ()
    "Create an entry tagged 'journal', while prompting for a title."
    (interactive)
    (denote
     (denote--title-prompt)
     '("journal")))

  ;; Denote does not define any key bindings.  This is for the user to
  ;; decide.  For example:
  (let ((map global-map))
    (define-key map (kbd "C-c n j") #'my-denote-journal) ; our custom command
    (define-key map (kbd "C-c n n") #'denote)
    (define-key map (kbd "C-c n N") #'denote-type)
    (define-key map (kbd "C-c n d") #'denote-date)
    (define-key map (kbd "C-c n s") #'denote-subdirectory)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    (define-key map (kbd "C-c n i") #'denote-link) ; "insert" mnemonic
    (define-key map (kbd "C-c n I") #'denote-link-add-links)
    (define-key map (kbd "C-c n l") #'denote-link-find-file) ; "list" links
    (define-key map (kbd "C-c n b") #'denote-link-backlinks)
    ;; Note that `denote-dired-rename-file' can work from any context, not
    ;; just Dired bufffers.  That is why we bind it here to the
    ;; `global-map'.
    (define-key map (kbd "C-c n r") #'denote-dired-rename-file))

  (with-eval-after-load 'org-capture
    (require 'denote-org-capture)
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))

;; bibtex
(setq bibtex-dialect 'biblatex)
(setq bibtex-completion-bibliography '((file-name-concat home-dir "02-org/org-jobb/ref/my-library.bib"))
      bibtex-completion-pdf-field "File"
      bibtex-completion-notes-path (file-name-concat home-dir "02-org/org-jobb/ref/")
      bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
	  bibtex-completion-additional-search-fields '(keywords journal)
      bibtex-completion-pdf-symbol "⌘"
      bibtex-completion-notes-symbol "✎"
	  bibtex-completion-display-formats
	  '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	    (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
      bibtex-completion-pdf-open-function (lambda (fpath)
	                                        (call-process "open" nil 0 nil fpath)))
(use-package helm-bibtex
  :after helm
  :bind (:map org-mode-map ("C-c n B" . helm-bibtex)))
(use-package org-ref
  :after org)
(use-package citar)

(unless (eq system-type 'windows-nt)
  (use-package org-attach-screenshot
    :after org
    :bind ("C-c s" . org-attach-screenshot)
    :config
    (setq org-attach-screenshot-dirfunction
          (lambda ()
            (progn (cl-assert (buffer-file-name))
                   "images"))
          org-attach-screenshot-command-line "gnome-screenshot -a -f %f")
    (setq org-attach-screenshot-relative-links t)))

;; Org reveal
(use-package ox-reveal
  :straight (:host github :repo "yjwen/org-reveal")
  :config (setq org-reveal-root (file-name-concat "file://" default-directory "reveal.js")))

;; epub
(use-package nov
  :init
  (setq nov-variable-pitch t))

;; Not Windows PC at SUS
(unless (and (eq system-type 'windows-nt) (string-match-p "\\`PC" (system-name)))
  (use-package org-noter)
  ;; PDF-tools
  (use-package pdf-tools
    :straight (:host github :repo "vedang/pdf-tools")
    :config
    ;; initialise
    (pdf-tools-install)
    ;; open pdfs scaled to fit page
    (setq-default pdf-view-display-size 'fit-page)
    ;; automatically annotate highlights
    (setq pdf-annot-activate-created-annotations t)
    ;; use normal isearch
    (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
    ;; turn off cua so copy works
    (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
    ;; more fine-grained zooming
    (setq pdf-view-resize-factor 1.1)
    ;; keyboard shortcuts
    (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
    (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
    (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))

  ;; Slime
  (use-package slime
    :init (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
    :config
    (cond ((progn (setq sbcl "/usr/bin/sbcl")
                  (file-exists-p sbcl))
           (setq inferior-lisp-program sbcl))
          ((progn
             (setq sbcl
                   "C:/Program\ Files/Steel\ Bank\ Common\ Lisp/sbcl.exe")
             (file-exists-p sbcl))
           (setq inferior-lisp-program sbcl))
          (t))
    (if (progn (setq slime-helper (expand-file-name "~/quicklisp/slime-helper.el"))
               (file-exists-p slime-helper))
        (load slime-helper))


    (defun override-slime-repl-bindings-with-paredit ()
      (define-key slime-repl-mode-map
        (read-kbd-macro paredit-backward-delete-key) nil))
    (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit))

  ;; Clojure
  (use-package clojure-mode
    :init
    (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'clojure-mode-hook #'subword-mode))
  (use-package cider
    :init
    (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'cider-repl-mode-hook #'subword-mode)))

  ;; Python
  ;; (use-package conda
  ;;   :config
  ;;   (conda-env-initialize-interactive-shells)
  ;;   (conda-env-initialize-eshell)
  ;;   (conda-env-autoactivate-mode nil)
  ;;   (setq conda-env-home-directory (expand-file-name "~/miniconda3/"))
  ;;   (custom-set-variables '(conda-anaconda-home (expand-file-name "~/miniconda3/"))))

  ;; (use-package jupyter
  ;;   :commands
  ;;   (jupyter-run-server-repl
  ;;    jupyter-run-repl
  ;;    jupyter-server-list-kernels)
  ;;   :init
  ;;   (eval-after-load 'jupyter-org-extensions ; conflicts with my helm config, I use <f2 #>
  ;;     '(unbind-key "C-c h" jupyter-org-interaction-mode-map))
  ;;   :config
  ;;   (defun my/jupyter-refresh-kernelspecs ()
  ;;     "Refresh Jupyter kernelspecs"
  ;;     (interactive)
  ;;     (jupyter-available-kernelspecs t)))

  ;; (setq org-confirm-babel-evaluate nil)
  ;; (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
  ;;                                                      (:session . "py")
  ;;                                                      (:kernel . "python3")))
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((emacs-lisp . t)
  ;;    (shell . t)
  ;;    (latex . t)
  ;;    (C . t)
  ;;    (makefile . t)
  ;;    (python . t)
  ;;    (jupyter . t))))
