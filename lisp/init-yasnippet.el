;;; config-yasnippet.el --- Config for yasnippet

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :commands (yas-hippie-try-expand)
  :hook (after-init . yas-global-mode)
  :init
  (setq yas-verbosity 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  ;; ;; Disable default yas minor mode map and use hippie integration.
  ;; (setq yas-minor-mode-map (make-sparse-keymap))
  (setq yas-wrap-around-region t)
  :config (use-package yasnippet-snippets))

(provide 'init-yasnippet)
;;; config-yasnippet.el ends here
