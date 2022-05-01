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
