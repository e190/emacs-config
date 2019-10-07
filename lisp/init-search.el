;; init-search.el --- Initialize search.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Jump to things in Emacs tree-style
(use-package avy
  :defer t
  :ensure t
  :hook (after-init . avy-setup-default)
  :bind
  (:map shadow-leader-map
   ("jc" . avy-goto-char-2)
   ("jw" . avy-goto-word-or-subword-1)
   ("jl" . avy-goto-line)
   ("jp" . #'shadow/goto-match-parent))
  :config (setq avy-background t))

;; Search tools: `wgrep', `ag' and `rg'
(use-package wgrep
  :defer t
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package ag
  :defer t
  :defines projectile-command-map
  :init
  (with-eval-after-load 'projectile
    (bind-key "s S" #'ag-project projectile-command-map))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t)
  (use-package wgrep-ag))

(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :config
  (setq rg-group-result t)
  (setq rg-show-columns t)

  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep 'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map))

  (when (fboundp 'ag)
    (bind-key "a" #'ag rg-global-map))

  (with-eval-after-load 'counsel
    (bind-keys :map rg-global-map
               ("c r" . counsel-rg)
               ("c s" . counsel-ag)
               ("c p" . counsel-pt)
               ("c f" . counsel-fzf))))

(use-package color-rg
  :demand t
  :ensure nil; local package
  :after counsel
  :load-path "site-lisp/color-rg"
  :bind
  ("M-s p" . color-rg-search-input-in-projcet)
  ;; ("M-s p" . color-rg-search-project)
  (:map shadow-leader-map
  ("sc" . color-rg-search-input)
  ("sp" . color-rg-search-project)
  ("ss" . color-rg-search-symbol))
  :config
    ;; `color-rg' do not kill any buffer
  (setq color-rg-kill-temp-buffer-p nil)
  ;; (define-key isearch-mode-map (kbd "M-s e") 'isearch-toggle-color-rg)
  (with-eval-after-load 'evil
    (evil-define-key 'normal color-rg-mode-map (kbd "RET") 'color-rg-open-file)
    (evil-define-key 'normal color-rg-mode-map (kbd "q") 'quit-window)))

;; SnailsPac
(use-package snails
  :ensure nil; local package
  :load-path "site-elisp/snails"
  :bind
  (:map shadow-leader-map
   ("sa" . snails))
  :custom-face
  (snails-content-buffer-face ((t (:background "#111" :height 110))))
  (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
  (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
  :config
  (add-hook 'snails-mode-hook (lambda ()
                              (snails-init-face-with-theme)
                              (evil-emacs-state)))
  ;; Functions for specific backends
  (defun snails-current-project ()
    (interactive)
    (snails '(snails-backend-projectile snails-backend-rg snails-backend-fd)))
  (defun snails-active-recent-buffers ()
    (interactive)
    (snails '(snails-backend-buffer snails-backend-recentf)))
  (defun snails-everywhere ()
    (interactive)
    (snails '(snails-backend-everything snails-backend-mdfind)))
  :bind
  (("M-s s" . snails)
   ("M-s g" . snails-current-project)
   ("M-s b" . snails-active-recent-buffers)
   ("M-s e" . snails-everywhere)))
;; -SnailsPac



(provide 'init-search)
;;; config-search.el ends here
