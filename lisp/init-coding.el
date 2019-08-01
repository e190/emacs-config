;;; config-coding.el --- Config for general coding.

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

;; Highlights matching parens.  Included in Emacs.
(use-package paren
  :defer 1
  :ensure nil ; built-in package
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-delay 0)
  (set-face-foreground 'show-paren-match "red")
  (set-face-background 'show-paren-match "SkyBlue2"))

;; Elec pair
(use-package elec-pair
  :defer t
  :ensure nil
  :init (add-hook 'after-init-hook #'electric-pair-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit
;; 在 a-string 两边加上 " 或者 (), 只要将光标放置于 a-string 开头按下 M-( 或者 M-" 即可
;; (hello world) 光标放置于 hello world 中间按下 M-S 即可将其分割成 (hello) (world)
;; 按下 M-J 可以将其重新连接起来， 字符串也一样
;; C-(, C-) 吃掉左边或者右边的 s-exp, C-{, C-} 吐出来
;; M-r 跳出外围块(去掉外层代码)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package paredit
  :hook ((scheme-mode lisp-mode emacs-lisp-mode inferior-lisp-mode geiser-repl-mode sly-mrepl-mode) . enable-paredit-mode)
  :config
  (setq paredit-lighter nil)
  (define-key paredit-mode-map (kbd "M-s") nil)
  (define-key paredit-mode-map (kbd "M-r") nil)
  (define-key paredit-mode-map (kbd "M-?") nil)
  (define-key paredit-mode-map (kbd "M-<up>") 'paredit-splice-sexp)
  (define-key paredit-mode-map (kbd "M-<down>") '(lambda ()
                                                   (interactive)
                                                   (lzl-look-forward-char 1 ?\))
                                                   (paredit-newline)
                                                   (ove-mode 0)))
  (define-key paredit-mode-map (kbd "(") nil)
  (define-key paredit-mode-map (kbd ")") nil)
  (define-key paredit-mode-map (kbd "[") nil)
  (define-key paredit-mode-map (kbd "]") nil)
  (define-key paredit-mode-map (kbd ";") nil)
  (advice-add 'paredit-comment-dwim :after
              #'(lambda (&optional arg) (unless mark-active
                                          (ove-mode 0)))))

;; AwesomePairPac
(use-package awesome-pair
  :ensure nil
  :load-path "site-lisp/awesome-pair"
  :bind
  (("C-c C-k" . awesome-pair-kill)
   ("SPC" . awesome-pair-space)
   ("=" . awesome-pair-equal)
   ("M-F" . awesome-pair-jump-right)
   ("M-B" . awesome-pair-jump-left))
  :config
  (add-hook 'prog-mode-hook '(lambda () (awesome-pair-mode 1))))

;; Hungry deletion
(use-package hungry-delete
  :defer t
  :ensure t
  :diminish hungry-delete-mode
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode)
  (setq hungry-delete-chars-to-skip " \t\n\r\f\v"))

;; Treat undo history as a tree
(use-package undo-tree
  :defer t
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(concat shadow-cache-dir "undo-tree-history"))))
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (shadow/define-leader-keys "tu" 'undo-tree-visualize)
  (global-undo-tree-mode))

(use-package thing-edit
  :ensure nil; local package
  :load-path "site-lisp/thing-edit"
  :bind
  (:map shadow-leader-map
   ("cw" . thing-copy-word)
   ("cs" . thing-copy-symbol)
   ("cx" . thing-copy-sexp)
   ("mp" . thing-copy-parentheses)
   ("ca" . thing-copy-to-line-beginning)
   ("ce" . thing-copy-to-line-end)
   ("mW" . thing-replace-word)
   ("mS" . thing-replace-symbol)
   ("mX" . thing-replace-sexp)
   ("mP" . thing-replace-parentheses)))

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region)
  (:map shadow-leader-map
   ("mw" . er/mark-word)
   ("ms" . er/mark-symbol)
   ("mx" . er/mark-inside-pairs)))

(provide 'init-coding)
;;; config-coding.el ends here