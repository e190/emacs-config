;; init-keybindings.el --- Initialize key bindings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

(use-package bind-key)

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer shadow/set-leader-keys-for-major-mode
    :states '(normal visual)
    :keymaps 'override ; keybindings that should not be overriden
    :prefix shadow-major-mode-leader-key)
  (general-create-definer shadow/define-leader-keys
    :states '(normal insert emacs visual)
    :prefix shadow-leader-key
    :keymaps 'override ; keybindings that should not be overriden
    :non-normal-prefix shadow-emacs-leader-key))

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init
  ;; Minibuffer feels much faster than using windows.
  (setq which-key-popup-type 'minibuffer)
  ;; :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  ;; Shows available keybindings after you start typing.
  (defun shadow-declare-prefix (prefix name)
    "Declare a which-key PREFIX.
  PREFIX is a string describing a key sequence.  NAME is a string
  used as the prefix command."
      (let* ((full-prefix (concat shadow-leader-key " " prefix))
            (full-prefix-emacs (concat shadow-emacs-leader-key " " prefix)))
        (which-key-add-key-based-replacements
          full-prefix name
          full-prefix-emacs name)))

  (defun shadow/declare-prefix-for-mode (mode prefix name)
    "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
  be added. PREFIX is a string describing a key sequence. NAME is a symbol name
  used as the prefix command."
      (let* ((full-prefix (concat shadow-major-mode-leader-key " " prefix)))
        (which-key-add-major-mode-key-based-replacements mode full-prefix name)))
  :config
  (setq which-key-idle-delay 0.3
        which-key-compute-remaps t
        which-key-min-display-lines 1
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-sort-uppercase-first nil
        which-key-side-window-max-width 0.33
        which-key-side-window-max-height 0.25
        which-key-sort-order #'which-key-prefix-then-key-order)
  (which-key-setup-side-window-bottom)
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
  ;; rename winum-select-window-1 entry to 1..9
  (add-to-list 'which-key-replacement-alist '(("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9")))
  ;; hide winum-select-window-[2-9] entries
  (add-to-list 'which-key-replacement-alist '((nil . "winum-select-window-[2-9]") . t))
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold))

;; I always hit this by mistake to get to `describe-char' and I'm tired of
;; seeing the GNU license.
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)

;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; Auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Makes <escape> quit as much as possible.
(define-key minibuffer-local-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map
  (kbd "<escape>") 'keyboard-escape-quit)

(setq shadow-key-binding-prefixes
  '((","   "leader")
	("a"   "applications")
	("b"   "buffers")
	("c"   "copy/comments")
	("e"   "flycheck")
	("f"   "files")
	("h"   "help/Highlight")
	("j"   "jump")
	("g"   "magit")
	("m"   "mark")
	("p"   "project")
	("q"   "quit")
	("r"   "replace")
	("s"   "search/symbol")
	("t"   "toggles")
	("w"   "windows")
	("z"   "zoom")))
(mapc (lambda (x) (apply #'shadow-declare-prefix x))
      shadow-key-binding-prefixes)

;; (bind-key "C-c C-l" #'reload-init-file)
(shadow/define-leader-keys "rl" 'reload-init-file)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
