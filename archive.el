(provide 'archive)

;; Org-roam
  (use-package org-roam
    :after (org)
    :custom
    (org-roam-directory (file-truename (file-name-concat home-dir "02-org/org-roam/")))
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ;; Dailies
           ("C-c n j" . org-roam-dailies-capture-today))
    :config
    ;; If you're using a vertical completion framework, you might want a more informative completion interface
    ;;(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-db-autosync-mode)
    ;; If using org-roam-protocol
    (require 'org-roam-protocol)

    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target (file+head "${slug}.org"
                                "#+title: ${title}\n#+date: %U\nTime-stamp: \" \"\n")
             :immediate-finish t
             :unnarrowed t)
            ("r" "bibliography reference" plain "%?"
             :target
             (file+head "ref/${citekey}.org"
                        "#+title: ${title}\n#+date: %U\nTime-stamp: \" \"\n")
             :unnarrowed t)))
    (setq org-roam-dailies-capture-templates
          '(("d" "default" entry
             "* %?"
             :if-new (file+head "%<%Y-%m-%d-%H%M%S>.org"
                                "#+title: %<%Y-%m-%d-%H%M>\nTime-stamp: \" \"\n")))))
  (use-package org-roam-bibtex
    :after (org-roam helm-bibtex citar)
    :bind (:map org-mode-map ("C-c n b" . orb-note-actions))
    :config
    (org-roam-bibtex-mode))


;; EXWM
(if (eq system-type 'gnu/linux)
     (use-package exwm
       :init
       ;; Fix problems with Ido (if you use it).
       (require 'exwm-config)
       (exwm-config-ido)
       (setq-default exwm-replace nil)

       ;; Set the initial number of workspaces (they can also be created later).
       (setq exwm-workspace-number 4)

       ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
       ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
       ;; are run when a new X window class name or title is available.  Here's
       ;; some advice on this topic:
       ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
       ;; + For applications with multiple windows (e.g. GIMP), the class names of
       ;;  windows are probably the same.  Using window titles for them makes
       ;;   more sense.
       ;; In the following example, we use class names for all windows except for
       ;; Java applications and GIMP.
       (add-hook 'exwm-update-class-hook
                 (lambda ()
                   (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                               (string= "gimp" exwm-instance-name))
                     (exwm-workspace-rename-buffer exwm-class-name))))
       (add-hook 'exwm-update-title-hook
                 (lambda ()
                   (when (or (not exwm-instance-name)
                             (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                             (string= "gimp" exwm-instance-name))
                     (exwm-workspace-rename-buffer exwm-title))))

       ;; Global keybindings can be defined with `exwm-input-global-keys'.
       ;; Here are a few examples:
       (setq exwm-input-global-keys
             `(
               ;; Bind "s-r" to exit char-mode and fullscreen mode.
               ([?\s-r] . exwm-reset)
               ;; Bind "s-w" to switch workspace interactively.
               ([?\s-w] . exwm-workspace-switch)
               ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
               ,@(mapcar (lambda (i)
                           `(,(kbd (format "s-%d" i)) .
                             (lambda ()
                               (interactive)
                               (exwm-workspace-switch-create ,i))))
                         (number-sequence 0 9))
               ;; Bind "s-&" to launch applications ('M-&' also works if the output
               ;; Buffer does not bother you).
               ([?\s-&] . (lambda (command)
		                    (interactive (list (read-shell-command "$ ")))
		                    (start-process-shell-command command nil command)))
               ;; Bind "s-<f2>" to "slock", a simple X display locker.
               ([s-f2] . (lambda ()
		                   (interactive)
		                   (start-process "" nil "/usr/bin/slock")))))

       ;; To add a key binding only available in line-mode, simply define it in
       ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
       (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

       ;; The following example demonstrates how to use simulation keys to mimic
       ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
       ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
       ;; and DEST is what EXWM actually sends to application.  Note that both SRC
       ;; and DEST should be key sequences (vector or string).
       (setq exwm-input-simulation-keys
             '(
               ;; movement
               ([?\C-b] . [left])
               ([?\M-b] . [C-left])
               ([?\C-f] . [right])
               ([?\M-f] . [C-right])
               ([?\C-p] . [up])
               ([?\C-n] . [down])
               ([?\C-a] . [home])
               ([?\C-e] . [end])
               ([?\M-v] . [prior])
               ([?\C-v] . [next])
               ([?\C-d] . [delete])
               ([?\C-k] . [S-end delete])
               ;; cut/paste.
               ([?\C-w] . [?\C-x])
               ([?\M-w] . [?\C-c])
               ([?\C-y] . [?\C-v])
               ;; search
               ([?\C-s] . [?\C-f])))

       ;; You can hide the minibuffer and echo area when they're not used, by
       ;; uncommenting the following line.
       ;;(setq exwm-workspace-minibuffer-position 'bottom)
       :config
       (menu-bar-mode -1)
       (add-hook 'exwm-init-hook #'exwm-show-mode-line)
       ;; (require 'exwm-systemtray)
       ;; (exwm-systemtray-enable)
       ;; using xim input
       (require 'exwm-xim)
       (exwm-xim-enable)
       (push ?\C-\\ exwm-input-prefix-keys)
       (exwm-enable)))


;; (use-package org-download
;;   :straight (:host github :repo "abo-abo/org-download")
;;   :after org
;;   :custom (setq org-download-image-dir "./images")
;;   :bind ("C-c w" . org-download-clipboard))

;;        (defun my-org-screenshot ()
;;          "Take a screenshot into a time stamped unique-named file in the
;; same directory as the org-buffer and insert a link to this file."
;;          (interactive)
;;          (setq filename
;;                (concat
;;                 (make-temp-name
;;                  (concat (buffer-file-name)
;;                          "_"
;;                          (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
;;          (shell-command "snippingtool /clip")
;;          (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
;;          (insert (concat "[[file:" filename "]]"))
;;          (org-display-inline-images))

;; Change font
(defun my/variable-font-setup () (interactive)
    (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                             :height 1.0))

;; Lispy
(use-package lispy
  :config (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

(use-package smartparens
  :config
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'smartparens-strict-mode)
  (add-hook 'ielm-mode-hook 'smartparens-strict-mode)
  (add-hook 'lisp-interaction-mode-hook 'smartparens-strict-mode)
  (add-hook 'lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'scheme-mode-hook 'smartparens-strict-mode)
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; keybinding management
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

  (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

  (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
  (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
  (define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
  (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

  (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

  (define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
  (define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)

  (bind-key "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2)) smartparens-mode-map)
  (bind-key "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)) smartparens-mode-map)

  (bind-key "C-M-s"
            (defhydra smartparens-hydra ()
              "Smartparens"
              ("d" sp-down-sexp "Down")
              ("e" sp-up-sexp "Up")
              ("u" sp-backward-up-sexp "Up")
              ("a" sp-backward-down-sexp "Down")
              ("f" sp-forward-sexp "Forward")
              ("b" sp-backward-sexp "Backward")
              ("k" sp-kill-sexp "Kill" :color blue)
              ("q" nil "Quit" :color blue))
            smartparens-mode-map)

  (bind-key "H-t" 'sp-prefix-tag-object smartparens-mode-map)
  (bind-key "H-p" 'sp-prefix-pair-object smartparens-mode-map)
  (bind-key "H-y" 'sp-prefix-symbol-object smartparens-mode-map)
  (bind-key "H-h" 'sp-highlight-current-sexp smartparens-mode-map)
  (bind-key "H-e" 'sp-prefix-save-excursion smartparens-mode-map)
  (bind-key "H-s c" 'sp-convolute-sexp smartparens-mode-map)
  (bind-key "H-s a" 'sp-absorb-sexp smartparens-mode-map)
  (bind-key "H-s e" 'sp-emit-sexp smartparens-mode-map)
  (bind-key "H-s p" 'sp-add-to-previous-sexp smartparens-mode-map)
  (bind-key "H-s n" 'sp-add-to-next-sexp smartparens-mode-map)
  (bind-key "H-s j" 'sp-join-sexp smartparens-mode-map)
  (bind-key "H-s s" 'sp-split-sexp smartparens-mode-map)
  (bind-key "H-s r" 'sp-rewrap-sexp smartparens-mode-map)
  (defvar hyp-s-x-map)
  (define-prefix-command 'hyp-s-x-map)
  (bind-key "H-s x" hyp-s-x-map smartparens-mode-map)
  (bind-key "H-s x x" 'sp-extract-before-sexp smartparens-mode-map)
  (bind-key "H-s x a" 'sp-extract-after-sexp smartparens-mode-map)
  (bind-key "H-s x s" 'sp-swap-enclosing-sexp smartparens-mode-map)

  (bind-key "C-x C-t" 'sp-transpose-hybrid-sexp smartparens-mode-map)

  (bind-key ";" 'sp-comment emacs-lisp-mode-map)

  (bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)

  ;;;;;;;;;;;;;;;;;;
  ;; pair management

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (bind-key "C-(" 'sp---wrap-with-40 minibuffer-local-map)

  (sp-with-modes 'org-mode
    (sp-local-pair "=" "=" :wrap "C-="))

  (sp-with-modes 'textile-mode
    (sp-local-pair "*" "*")
    (sp-local-pair "_" "_")
    (sp-local-pair "@" "@"))

  (sp-with-modes 'web-mode
    (sp-local-pair "{{#if" "{{/if")
    (sp-local-pair "{{#unless" "{{/unless"))

  ;;; tex-mode latex-mode
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-tag "i" "\"<" "\">"))

  ;;; lisp modes
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil
                   :wrap "C-("
                   :pre-handlers '(my-add-space-before-sexp-insertion)
                   :post-handlers '(my-add-space-after-sexp-insertion)))

  (defun my-add-space-after-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (forward-char (sp-get-pair id :cl-l))
        (when (or (eq (char-syntax (following-char)) ?w)
                  (looking-at (sp--get-opening-regexp)))
          (insert " ")))))

  (defun my-add-space-before-sexp-insertion (id action _context)
    (when (eq action 'insert)
      (save-excursion
        (backward-char (length id))
        (when (or (eq (char-syntax (preceding-char)) ?w)
                  (and (looking-back (sp--get-closing-regexp))
                       (not (eq (char-syntax (preceding-char)) ?'))))
          (insert " ")))))

  ;;; C++
  (sp-with-modes '(malabar-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                      ("* ||\n[i]" "RET")))

  (sp-with-modes '(js2-mode typescript-mode)
    (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                               ("* ||\n[i]" "RET")))))
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
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#667")  ; dark gray
  )
