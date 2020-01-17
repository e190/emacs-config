;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

;; Start a clean slate.
(blink-cursor-mode -1)
(menu-bar-mode -1)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Title
;; Sets a more useful frame title, showing either a file or a buffer
;; name (if the buffer isn't visiting a file).
(setq frame-title-format
      '("" invocation-name " - "
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

;; (setq frame-title-format '("Shadow Emacs - %b")
;;       icon-title-format frame-title-format)

(when sys/mac-x-p
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;; Control over modes displayed in the modeline.
(use-package diminish)

;; Icons
;; NOTE: Must run `M-x all-the-icons-install-fonts' manually on Windows
(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (unless (or sys/win32p (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t)))

;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
  ;; config built-in "display-line-numbers-mode" (require Emacs >= 26)
  (use-package display-line-numbers
    :ensure nil
    :hook ((prog-mode text-mode) . display-line-numbers-mode)
    :init
    (setq-default display-line-numbers-width 2)
    ;; (setq-default display-line-numbers-type 'relative)
    (setq display-line-numbers-current-absolute t)
    (shadow/define-leader-keys "tn" 'display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :config
    (setq linum-format "%4d ")

    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :hook (global-linum-mode . hlinum-activate)
      :custom-face (linum-highlight-face
                    ((t `(
                          :inherit default
                          :background nil
                          :foreground nil
                          ))))
      :init
      (setq linum-highlight-in-all-buffersp t))))

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)

(use-package imenu-list
  :defer t
  :ensure t
  :init
  (shadow/define-leader-keys "tl" 'imenu-list-smart-toggle)
  :config
  (setq imenu-list-size     0.2)
  (setq imenu-list-position 'right)
  (setq imenu-list-focus-after-activation t))

(use-package imenu-anywhere
  :defer t
  :config
  (setq imenu-anywhere-delimiter ": "))

;; https://emacs-china.org/t/imenu-list-tagbar/7341
(use-package maple-imenu
  :ensure nil
  :load-path "site-lisp/emacs-maple-imenu"
  :commands (maple-imenu)
  :init
  (shadow/define-leader-keys "ti" 'maple-imenu)
  :config
  (setq maple-imenu-autoresize t)
  ;; (setq maple-imenu-display-alist '((side . left) (slot . -1)))
  (setq maple-imenu-display-alist '((side . right) (slot . -1)))
  (defun maple-sidebar()
    (interactive)
    (maple-imenu)
    (neotree-toggle))
  (with-eval-after-load 'evil
    (evil-define-key 'normal maple-imenu-mode-map (kbd "q") 'quit-window))
  (add-hook 'maple-imenu-mode-hook
            (lambda() (setq-local maple-modeline-style 'sidebar))))

;; Display Time
(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init
  (setq display-time-format "%Y-%m-%d %H:%M")
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t))

(use-package posframe
  :defer t)

(use-package awesome-tab
  :ensure nil
  :load-path "site-lisp/awesome-tab"
  :hook (after-init . awesome-tab-mode)
  :bind
  (:map shadow-leader-map
    ("tt" . awesome-tab-switch-group)
    ("ta" . awesome-tab-select-beg-tab)
    ("te" . awesome-tab-select-end-tab)
    ("t<" . awesome-tab-move-current-tab-to-left)
    ("t>" . awesome-tab-move-current-tab-to-right)
    ("tf" . awesome-tab-forward)
    ("tb" . awesome-tab-backward))
  :config
  (setq awesome-tab-cycle-scope 'tabs) ; Navigate through visible tabs only.
  (setq awesome-tab-style 'alternate)
  (setq awesome-tab-face-height 100))

(provide 'init-ui)
;;; init-ui.el ends here
