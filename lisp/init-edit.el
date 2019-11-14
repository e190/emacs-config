;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t -*-

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

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

(use-package maple-iedit
  :ensure nil
  :after evil
  :load-path "site-lisp/maple-iedit"
  :commands (maple-iedit-match-all maple-iedit-match-next maple-iedit-match-previous)
  :config
  (setq maple-iedit-evil-keybind t
        maple-iedit-ignore-case t)

  (defhydra maple/iedit ()
    ("a" maple-iedit-match-all "all match")
    ("n" maple-iedit-match-next "next")
    ("t" maple-iedit-skip-and-match-next "skip and next")
    ("T" maple-iedit-skip-and-match-previous "skip and previous")
    ("p" maple-iedit-match-previous "prev"))
  :bind (:map evil-visual-state-map
              ("n" . maple/iedit/body)
              ("C-n" . maple-iedit-match-next)
              ("C-p" . maple-iedit-match-previous)
              ("C-t" . maple-iedit-skip-and-match-next))
)

(use-package thing-edit
  :ensure nil; local package
  :load-path "site-lisp/thing-edit"
  :bind
  (:map shadow-leader-map
   ("cw" . thing-copy-word)
   ("cs" . thing-copy-symbol)
   ("cx" . thing-copy-sexp)
   ("cp" . thing-copy-parentheses)
   ("ca" . thing-copy-to-line-beginning)
   ("ce" . thing-copy-to-line-end)
   ("rw" . thing-replace-word)
   ("rs" . thing-replace-symbol)
   ("rx" . thing-replace-sexp)
   ("rp" . thing-replace-parentheses)))

;; Increase selected region by semantic units
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region)
  (:map shadow-leader-map
   ("mw" . er/mark-word)
   ("ms" . er/mark-symbol)
   ("mp" . er/mark-inside-pairs)))

(use-package anzu
  :hook (after-init . global-anzu-mode)
  :config
  (setq anzu-cons-mode-line-p nil
        anzu-kode-lighter ""
        anzu-search-threshold 1000
        anzu-replace-to-string-separator " → ")
  ;; (use-package evil-anzu :after evil :demand)
  :custom-face
  (anzu-replace-to ((t (:inherit query-replace))))
  :bind (:map query-replace-map
              ([return] . 'automatic)))

(provide 'init-edit)
;;; init-edit.el ends here
