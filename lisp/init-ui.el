;;; config-ui.el --- UI tweaks

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

(when (member "Consolas" (font-family-list))
  (set-frame-font shadow-font 'keep-size)
  (add-to-list 'default-frame-alist (cons 'font shadow-font)))

;; Start a clean slate.
(blink-cursor-mode -1)
(menu-bar-mode -1)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Sets a more useful frame title, showing either a file or a buffer
;; name (if the buffer isn't visiting a file).
(setq frame-title-format
      '("" invocation-name " - "
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

;; 定义窗口位置
(set-frame-position (selected-frame) 0 0)

;; 自定义窗口大小
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if sys/win32p
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 180))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 140)
                          (frame-char-height))))
    )))

(set-frame-size-according-to-resolution)
;; Control over modes displayed in the modeline.
(use-package diminish
  :defer nil ; load immediately
  :demand)

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
  (shadow/define-leader-keys "ti" 'imenu-list-smart-toggle)
  :config
  (setq imenu-list-size     0.2)
  (setq imenu-list-position 'right)
  (setq imenu-list-focus-after-activation t))

(use-package imenu-anywhere
  :config
  (setq imenu-anywhere-delimiter ": "))

;; Display Time
(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

(provide 'init-ui)
;;; config-ui.el ends here
