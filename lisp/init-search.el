;;; config-search.el --- Config for yasnippet

;;; Commentary:
;;

;;; Code:

(defun kevin/goto-match-parent ()
  "Go to the matching  if on (){}[], similar to vi style of %."
  (interactive)
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

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
   ("jp" . #'kevin/goto-match-parent))
  :config (setq avy-background t))

;; Search tools: `wgrep', `ag' and `rg'
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package ag
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
  :ensure nil; local package
  :load-path "site-lisp/color-rg"
  :bind
  (:map shadow-leader-map
   ("sc" . color-rg-search-input)
   ("sp" . color-rg-search-input-in-projcet)
   ("ss" . color-rg-search-symbol)))
  ;; :config
  ;; (with-eval-after-load 'evil
  ;;   (evil-define-key 'normal color-rg-mode-map
  ;;   ("RET" . color-rg-open-file))))
  ;; :map color-rg-mode-map
  ;;   ("j" . color-rg-jump-next-keyword)
  ;;   ("k" . color-rg-jump-prev-keyword)
  ;;   ("h" . color-rg-jump-next-file)
  ;;   ("l" . color-rg-jump-prev-file)
  ;;   ("RET" . color-rg-open-file)
  ;;   ("C-m" . color-rg-open-file)))
  ;; :config)
  ;; (define-key isearch-mode-map (kbd "M-s M-s") 'isearch-toggle-color-rg))

;; SnailsPac
(use-package snails
  :load-path "site-elisp/snails"
  ;; :init
  :bind
  (:map shadow-leader-map
   ("sa" . snails))
  :custom-face
  (snails-content-buffer-face ((t (:background "#111" :height 110))))
  (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
  (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1)))))
;; -SnailsPac

(provide 'init-search)
;;; config-search.el ends here
