;;; core-emacs-settings.el --- Editor defaults

;;; Commentary:
;;

;;; Code:

;; Alphabetical order by identifier.

(defun shadow//make-cache-dir (dir)
  "Create DIR in `shadow-cache-dir', making parents and returning DIR."
  (let ((new-dir (concat shadow-cache-dir "/" dir)))
    (make-directory new-dir 'parents)
    (file-truename new-dir)))

;;;###autoload
(defun shadow/open-init-file ()
  "Open emacs init file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(setq auto-save-file-name-transforms
      `((".*" ,(shadow//make-cache-dir "auto-save-list") t)))

;; Directory to store backup files.
(setq-default backup-directory-alist `(("." . ,(shadow//make-cache-dir "backups"))))
(setq-default tramp-auto-save-directory (shadow//make-cache-dir "tramp-auto-save"))
(setq-default tramp-backup-directory-alist backup-directory-alist)
(setq-default url-cache-directory (shadow//make-cache-dir "url"))
;;(setq-default url-configuration-directory url-cache-directory)
(setq-default savehist-file (concat shadow-cache-dir "/" "history"))

;; Silence ad-handle-definition about advised functions getting redefined.
(setq ad-redefinition-action 'accept)

;; Displays column number in the mode line.
(setq column-number-mode t)

;; Deletes excess backup versions silently.
(setq delete-old-versions t)

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from Emacs (especially on Microsoft Windows).
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

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
              mouse-yank-at-point t
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
(setq initial-scratch-message "")

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
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
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

(provide 'init-emacs-settings)
;;; core-emacs-settings.el ends here
