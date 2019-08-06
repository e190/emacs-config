;;; init-evil.el --- Basic evil-mode configuration.

;;; Code:
(eval-when-compile
  (require 'init-constants))

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
  :demand t
  :hook (after-init . evil-mode)
  :init
  ;; Must be set before evil is loaded.
  (setq evil-respect-visual-line-mode t)
  ;; 使能C-u 往上翻
  (setq evil-want-C-u-scroll t)
  ;; (evil-mode 1)
  :config
  ;; Set SPACE to invoke `abn-leader-map' in modes except emacs and insert.
  (evil-define-key '(normal visual motion) 'global
    (kbd shadow-leader-key) shadow-leader-map)

  ;; Set the M-m keybinding for `abn-leader-map' in all modes.
  (evil-define-key '(normal insert visual motion emacs) 'global
    (kbd shadow-emacs-leader-key) shadow-leader-map)

  ;; Prevents esc-key from translating to meta-key in terminal mode.
  (setq evil-esc-delay 0)

  ;; It's better that the default value is too small than too big.
  (setq-default evil-shift-width 2)

  ;; * and # search using symbols.
  (setq-default evil-symbol-word-search t)

  ;; evil-want-Y-yank-to-eol must be set via customize to have an effect.
  (customize-set-variable 'evil-want-Y-yank-to-eol t)

  ;; Controls position of the mode line tag for the current mode,
  ;; e.g. <N>, <I>, etc.  Before places it before the major-mode.
  (setq evil-mode-line-format 'before)
  ;;; modify evil-state-tag
  (setq evil-normal-state-tag   (propertize "[Normal]")
        evil-emacs-state-tag    (propertize "[Emacs]")
        evil-insert-state-tag   (propertize "[Insert]")
        evil-motion-state-tag   (propertize "[Motion]")
        evil-visual-state-tag   (propertize "[Visual]")
        evil-operator-state-tag (propertize "[Operator]"))

  ;; Cursor colors.
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
  (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))
  (setq evil-emacs-state-cursor '("SkyBlue2" box))
  (setq evil-hybrid-state-cursor '("SkyBlue2" (bar . 2)))
  (setq evil-replace-state-cursor '("chocolate" (hbar . 2)))
  (setq evil-evilified-state-cursor '("LightGoldenrod3" box))
  (setq evil-visual-state-cursor '("gray" (hbar . 2)))
  (setq evil-motion-state-cursor '("plum3" box))
  (setq evil-lisp-state-cursor '("HotPink1" box))
  (setq evil-iedit-state-cursor '("firebrick1" box))
  (setq evil-iedit-state-cursor-insert '("firebrick1" (bar . 2)))

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
  :demand t
  :after evil
  :config
  (global-evil-surround-mode t))

(use-package evil-visualstar
  :demand t
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

(use-package evil-escape
  :demand t
  :after evil
  :diminish evil-escape-mode
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.3))

(use-package ace-mc
 :init
 (define-key evil-visual-state-map (kbd "mm") 'ace-mc-add-multiple-cursors)
 (define-key evil-visual-state-map (kbd "ms") 'ace-mc-add-single-cursor))

(provide 'init-evil)
;;; init-evil.el ends here
