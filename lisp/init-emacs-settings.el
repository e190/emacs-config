;;; core-emacs-settings.el --- Editor defaults

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

(defun shadow//make-cache-dir (dir)
  "Create DIR in `shadow-cache-dir', making parents and returning DIR."
  (let ((new-dir (concat shadow-cache-dir "/" dir)))
    (make-directory new-dir 'parents)
    (file-truename new-dir)))

(setq auto-save-file-name-transforms
      `((".*" ,(shadow//make-cache-dir "auto-save-list") t)))

;; 缩放字体
(if sys/win32p
    (progn
    (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
    (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease))
    (progn
    (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
    (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease))
)
;; Directory to store backup files.
(setq-default backup-directory-alist `(("." . ,(shadow//make-cache-dir "backups"))))
(setq-default tramp-auto-save-directory (shadow//make-cache-dir "tramp-auto-save"))
(setq-default tramp-backup-directory-alist backup-directory-alist)
(setq-default url-cache-directory (shadow//make-cache-dir "url"))
;;(setq-default url-configuration-directory url-cache-directory)
(setq-default savehist-file (concat shadow-cache-dir "/" "history"))

;; Silence ad-handle-definition about advised functions getting redefined.
(setq ad-redefinition-action 'accept)

;; Deletes excess backup versions silently.
(setq delete-old-versions t)

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from Emacs (especially on Microsoft Windows).
(prefer-coding-system 'utf-8)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; (set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(set-file-name-coding-system 'utf-8)

;; (setq locale-coding-system 'utf-8
;;       default-process-coding-system '(utf-8 . utf-8))

;; 复制粘贴
(setq select-enable-primary t)
(setq select-enable-clipboard t)

(setq-default indent-tabs-mode nil ;; do not insert tab indentation
              tab-width 4 ;; 将TAB显示为4个空格.
              fill-column 80 ;; 设置列宽度
              buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              grep-highlight-matches t
              grep-scroll-output t
              line-spacing 0
              ;; 让光标无法离开视线
              mouse-yank-at-point nil
              tab-always-indent 'complete
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              truncate-partial-width-windows nil
              truncate-lines nil           ; Do not display continuation lines
              split-height-threshold nil   ; Disable vertical window splitting
              split-width-threshold nil    ; Disable horizontal window splitting
              majar-mode 'text-mode)

;; We don't share the file-system with anyone else.
(setq create-lockfiles nil)

;; Skip startup screen.
(setq inhibit-startup-screen t)

;; Start with a blank canvas.
;; (setq initial-scratch-message "")

;; Warns when opening files bigger than 10MB.
(setq large-file-warning-threshold (* 10 1024 1024))

;; Load the newer .elc or .el file, rather than stopping at .elc.
(setq load-prefer-newer t)

;; Potentially speed up cursor operations
;; https://emacs.stackexchange.com/questions/28736
(setq auto-window-vscroll nil)

;; Too useful to disable
(put 'narrow-to-region 'disabled nil)

;; Newline at end of file.
(setq require-final-newline t)

;; Disables the annoying bell ring.
(setq ring-bell-function 'ignore)

;; Store pastes from other programs in the kill-ring before
;; overwriting with Emacs' yanks.
(setq save-interprogram-paste-before-kill t)

;; Double space for sentences.
(setq-default sentence-end-double-space t)

;; Enables nice scrolling.
(setq scroll-preserve-screen-position 1)

;; Number backup files.
(setq version-control t)

;; Don't ask for confirmation when opening symlinked file.
(setq vc-follow-symlinks t)

;; Make backup files even when in version controlled directory.
(setq vc-make-backup-files t)

;; y is shorter than yes.
(fset 'yes-or-no-p 'y-or-n-p)

(setq user-mail-address shadow-mail-address)

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
;; (setq scroll-step 1
;;       scroll-margin 0
;;       scroll-conservatively 100000
;;       scroll-up-aggressively 0.01
;;       scroll-down-aggressively 0.01
;;       auto-window-vscroll nil
;;       fast-but-imprecise-scrolling nil
;;       mouse-wheel-scroll-amount '(1 ((shift) . 1))
;;       mouse-wheel-progressive-speed nil
;;       ;; Horizontal Scroll
;;       hscroll-step 1
;;       hscroll-margin 1)
;; -SmoothScroll
(use-package emacs
  :config
  (setq scroll-preserve-screen-position t)
  (setq scroll-conservatively 1)        ; affects `scroll-step'
  (setq scroll-margin 0)

  (setq-default cursor-type 'box)
  ;; (setq-default cursor-in-non-selected-windows '(bar . 2))
  (setq-default blink-cursor-blinks 50)
  (setq-default blink-cursor-interval 0.75)
  (setq-default blink-cursor-delay 0.2)

  ;; In Emacs 27, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
        '(1
          ((shift) . 5)
          ((meta) . 0.5)
          ((control) . text-scale)))
  ;; (setq mouse-drag-copy-region t)
  (setq make-pointer-invisible t)
  (setq mouse-wheel-progressive-speed t)
  (setq mouse-wheel-follow-mouse t)
  :hook
  (after-init-hook . blink-cursor-mode)
  (after-init-hook . mouse-wheel-mode))

(provide 'init-emacs-settings)
;;; init-emacs-settings.el ends here
