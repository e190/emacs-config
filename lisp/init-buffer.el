;; config-buffer.el -- initialize buffer configurations. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; Buffers
(use-package funcs-buffer
  :ensure nil ; local package
  :bind
  (:map shadow-leader-map
        ("TAB" . shadow/alternate-buffer)
        ;; ("bd" . shadow/kill-this-buffer)
        ("bd" . kill-this-buffer)
        ("be" . shadow/safe-erase-buffer)
        ("bn" . next-buffer)
        ("bm" . shadow/kill-other-buffers)
        ("bN" . shadow/new-empty-buffer)
        ("bP" . shadow/copy-clipboard-to-whole-buffer)
        ("bp" . previous-buffer)
        ("bR" . shadow/safe-revert-buffer)
        ("bs" . shadow/switch-to-scratch-buffer)
        ("bY" . shadow/copy-whole-buffer-to-clipboard)
        ("bw" . read-only-mode)
        ("b1" . buffer-to-window-1)
        ("b2" . buffer-to-window-2)
        ("b3" . buffer-to-window-3)
        ("b4" . buffer-to-window-4)
        ("b5" . buffer-to-window-5)
        ("b6" . buffer-to-window-6)
        ("b7" . buffer-to-window-7)
        ("b8" . buffer-to-window-8)
        ("b9" . buffer-to-window-9)))

(use-package awesome-tab
  :ensure nil
  :load-path "site-lisp/awesome-tab"
  :config
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd ",tt") 'awesome-tab-switch-group)
    (define-key evil-normal-state-map (kbd ",ta") 'awesome-tab-select-beg-tab)
    (define-key evil-normal-state-map (kbd ",te") 'awesome-tab-select-end-tab)
    (define-key evil-normal-state-map (kbd ",t<") 'awesome-tab-move-current-tab-to-left)
    (define-key evil-normal-state-map (kbd ",t>") 'awesome-tab-move-current-tab-to-right)
    (define-key evil-normal-state-map (kbd ",th") 'awesome-tab-forward)
    (define-key evil-normal-state-map (kbd ",tl") 'awesome-tab-backward))
  (setq awesome-tab-cycle-scope 'tabs) ; Navigate through visible tabs only.
  ;; (awesome-tab-mode t))
)
(provide 'init-buffer)
