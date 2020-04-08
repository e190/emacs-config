;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

;; Highlights matching parens.  Included in Emacs.
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
  (defun display-line-overlay (pos str &optional face)
    "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
    (let ((ol (save-excursion
                (goto-char pos)
                (make-overlay (line-beginning-position)
                              (line-end-position)))))
      (overlay-put ol 'display str)
      (overlay-put ol 'face
                   (or face '(:inherit highlight)))
      ol))

  (defvar-local show-paren--off-screen-overlay nil)
  (defun show-paren-off-screen (&rest _args)
    "Display matching line for off-screen paren."
    (when (overlayp show-paren--off-screen-overlay)
      (delete-overlay show-paren--off-screen-overlay))
    ;; check if it's appropriate to show match info,
    (when (and (overlay-buffer show-paren--overlay)
               (not (or cursor-in-echo-area
                        executing-kbd-macro
                        noninteractive
                        (minibufferp)
                        this-command))
               (and (not (bobp))
                    (memq (char-syntax (char-before)) '(?\) ?\$)))
               (= 1 (logand 1 (- (point)
                                 (save-excursion
                                   (forward-char -1)
                                   (skip-syntax-backward "/\\")
                                   (point))))))
      ;; rebind `minibuffer-message' called by
      ;; `blink-matching-open' to handle the overlay display
      (cl-letf (((symbol-function #'minibuffer-message)
                 (lambda (msg &rest args)
                   (let ((msg (apply #'format-message msg args)))
                     (setq show-paren--off-screen-overlay
                           (display-line-overlay
                            (window-start) msg ))))))
        (blink-matching-open))))
  (advice-add #'show-paren-function :after #'show-paren-off-screen))

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
  :bind (("C-c DEL" . hungry-delete-backward))
  :bind (("C-c d" . hungry-delete-forward))
  :diminish hungry-delete-mode
  :init (add-hook 'after-init-hook #'global-hungry-delete-mode)
  (setq hungry-delete-chars-to-skip " \t\n\r\f\v"))

;; Treat undo history as a tree
(use-package undo-tree
  :ensure t
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-history-directory-alist `(("." . ,(concat shadow-cache-dir "undo-tree-history")))
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil)

  (shadow/define-leader-keys "tu" 'undo-tree-visualize)
  ;; HACK: keep the diff window
  (make-variable-buffer-local 'undo-tree-visualizer-diff)
  (setq-default undo-tree-visualizer-diff t))

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

;; Redefine M-< and M-> for some modes
(when emacs/>=25.3p
  (use-package beginend
    :diminish (beginend-mode beginend-global-mode)
    :hook (after-init . beginend-global-mode)
    :config
    (mapc (lambda (pair)
            (add-hook (car pair) (lambda () (diminish (cdr pair)))))
          beginend-modes)))

(provide 'init-edit)
;;; init-edit.el ends here
