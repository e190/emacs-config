;;; config-base.el --- Important packages.

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package autorevert
  :diminish auto-revert-mode
  :ensure nil ; built-in package
  :config
  ;; Reverts buffers automatically when underlying files are changed externally.
  (global-auto-revert-mode t))

(use-package deferred
  :defer t)

(use-package recentf
  :ensure nil ; built-in package
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 200)
  (setq recentf-save-file (concat shadow-cache-dir "/recentf")))

;; Save point position between sessions.
(use-package saveplace
  :config
  (save-place-mode)
  (setq save-place-file (concat shadow-cache-dir "/places")))

(use-package simple
  :ensure nil
  :hook ((window-setup . size-indication-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        ;; kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(provide 'init-base)
;;; config-base.el ends here
