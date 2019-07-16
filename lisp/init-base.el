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


(provide 'init-base)
;;; config-base.el ends here
