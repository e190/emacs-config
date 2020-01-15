;; init-evil.el --- Initialize evil-mode configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

(use-package evil-leader
  :disabled
  :after evil
  :init
  (setq evil-want-keybinding nil)
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-magic t
        evil-echo-state t
        evil-default-state 'normal
        ;; 使能C-u 往上翻
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-visual-char-semi-exclusive t
        evil-indent-convert-tabs t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-insert-skip-empty-lines t
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; don't activate mark on shift-click
        shift-select-mode nil
        evil-cross-lines t
        evil-move-cursor-back t ;; move back the cursor one position when exiting insert mode
        ;; Prevents esc-key from translating to meta-key in terminal mode.
        evil-esc-delay 0.01
        ;; It's better that the default value is too small than too big.
        evil-shift-width 2
        ;; Controls position of the mode line tag for the current mode,
        ;; e.g. <N>, <I>, etc.  Before places it before the major-mode.
        evil-mode-line-format 'after)
  ;; evil cursor color
  (setq evil-default-cursor '("red" box)
        evil-normal-state-cursor '("DarkGoldenrod2" box)
        evil-insert-state-cursor '("chartreuse3" (bar . 2))
        evil-emacs-state-cursor '("SkyBlue2" box)
        evil-hybrid-state-cursor '("SkyBlue2" (bar . 2))
        evil-replace-state-cursor '("chocolate" (hbar . 2))
        evil-evilified-state-cursor '("LightGoldenrod3" box)
        evil-visual-state-cursor '("gray" (hbar . 2))
        evil-motion-state-cursor '("plum3" box)
        evil-lisp-state-cursor '("HotPink1" box)
        evil-iedit-state-cursor '("firebrick1" box)
        evil-iedit-state-cursor-insert '("firebrick1" (bar . 2)))
  ;; Must be set before evil is loaded.
  ;; (setq evil-respect-visual-line-mode t)
  :config
  ;; Set SPACE to invoke `abn-leader-map' in modes except emacs and insert.
  (evil-define-key '(normal visual motion) 'global
    (kbd shadow-leader-key) shadow-leader-map)

  ;; Set the M-m keybinding for `abn-leader-map' in all modes.
  (evil-define-key '(normal insert visual motion emacs) 'global
    (kbd shadow-emacs-leader-key) shadow-leader-map)


  ;; evil insert state keybinds
  (define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-p") 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "C-n") 'evil-next-visual-line)
  ;; 系统剪贴板快捷键（C-c复制，C-v粘贴）
  (define-key evil-insert-state-map (kbd "C-v") 'clipboard-yank)
  ;; evil normal state keybinds
  (define-key evil-normal-state-map "Y" (kbd "y$"))
  (define-key evil-normal-state-map (kbd ",w") 'evil-write)
  (define-key evil-normal-state-map (kbd ",q") 'evil-quit)
  ;; evil visual state keybinds
  (define-key evil-visual-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-visual-state-map (kbd "C-c") 'clipboard-kill-ring-save)

  ;; http://emacs.stackexchange.com/questions/14940
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Major modes that should default to an insert state.
  (add-to-list 'evil-insert-state-modes 'git-commit-mode)

  ;; Sets more useful movement commands.
  (shadow/define-leader-keys
   "jt" 'evil-window-top
   "jb" 'evil-window-bottom))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-visualstar
  :after evil
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode))

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :init
  (shadow/define-leader-keys "ci" 'evilnc-comment-or-uncomment-lines
                             "cl" 'evilnc-comment-or-uncomment-paragraphs))

(use-package evil-mc
  :after evil
  :diminish evil-mc-mode "ⓜ"
  :init
  (defun kevin/toggle-evil-mc ()
    (interactive)
    (if evil-mc-mode
        (progn
          (evil-mc-undo-all-cursors)
          (evil-mc-mode -1)
          (message "evil mc mode disabled"))
      (progn
        (evil-mc-mode 1)
        (message "evil mc mode enabled"))))
  (shadow/define-leader-keys "tm" #'kevin/toggle-evil-mc)
  (defun kevin/reset-evil-mc-key-map ()
    (let ((keys '(("ma" . evil-mc-make-all-cursors)
                  ("mu" . evil-mc-undo-all-cursors)
                  ("ms" . evil-mc-pause-cursors)
                  ("mr" . evil-mc-resume-cursors)
                  ("mf" . evil-mc-make-and-goto-first-cursor)
                  ("mb" . evil-mc-make-and-goto-last-cursor)
                  ("mh" . evil-mc-make-cursor-here)
                  ("mn" . evil-mc-skip-and-goto-next-match)
                  ("mp" . evil-mc-skip-and-goto-prev-match)
                  ("C-n" . evil-mc-make-and-goto-next-match)
                  ("C-p" . evil-mc-make-and-goto-prev-match)
                  )))
      (dolist (key-data keys)
        ;; (evil-define-key 'normal 'evil-mc-key-map (kbd (car key-data)) (cdr key-data))
        (evil-define-key 'visual 'evil-mc-key-map (kbd (car key-data)) (cdr key-data)))))
  :config
  (kevin/reset-evil-mc-key-map))

(use-package evil-escape
  :ensure t
  :after evil
  :diminish evil-escape-mode
  :hook (evil-mode . evil-escape-mode)
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.3)
  (global-set-key (kbd "C-c C-g") 'evil-escape))

(use-package ace-mc
  :after evil
  :init
  (define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
  (define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor))

;; https://github.com/hlissner/evil-snipe
;; It provides 2-character motions for quickly (and more accurately) jumping around
;; text, compared to evil's built-in f/F/t/T motions, incrementally highlighting
;; candidate targets as you type.
(use-package evil-snipe
  :ensure t
  :after evil
  :diminish evil-snipe-local-mode
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
   ;; fix problems with magit buffer
  (add-hook 'rg-mode-hook #'turn-off-evil-snipe-mode)
  (add-hook 'rg-mode-hook #'turn-off-evil-snipe-override-mode)
  (add-hook 'magit-mode-hook #'turn-off-evil-snipe-mode)
  (add-hook 'magit-mode-hook #'turn-off-evil-snipe-override-mode))

(provide 'init-evil)
;;; init-evil.el ends here
