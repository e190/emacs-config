;;; init.el --- Emacs Initialization File
;;
;; Copyright (c) 2018 Joe Schafer
;;
;; Author: Shadow <e190@163.com>
;; URL: https://github.com/e190
;;
;; This file is not part of GNU Emacs.

;; Prevent Emacs from calling package-initialize.  We'll do it ourselves.
;; (package-initialize)

;;; Code:

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires %s or higher" minver)))

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 0.8MB.  Measured in bytes.
;; (setq gc-cons-threshold (* 50 1000 1000))
;; Portion of heap used for allocation.  Defaults to 0.1.
;; (setq gc-cons-percentage 0.6)

;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 40000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold 40000000))

            (defun my-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold 800000))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))
;; Core
;;----------------------------------------------------------------------------
;; Core files required.
;;----------------------------------------------------------------------------
;; (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Load custom file first.
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)
;; Must come first
(require 'init-constants)
(require 'init-packages) 
(require 'init-base)
(require 'init-funcs)
(require 'init-keybindings)
(require 'init-ui)
(require 'init-theme)
(require 'init-mode-line)
(require 'init-coding)
(require 'init-emacs-settings)

;; Modules
(require 'init-ivy)
(require 'init-complete)
(require 'init-yasnippet)

(require 'init-dashboard)
(require 'init-buffer)
(require 'init-chinese)
(require 'init-dired)
(require 'init-evil)
(require 'init-eshell)
(require 'init-filetree)
(require 'init-treemacs)
(require 'init-highlight)
(require 'init-helper)
(require 'init-git)
(require 'init-org)
(require 'init-search)
(require 'init-window)
(require 'init-projectile)
(require 'init-lsp)
(require 'init-c)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
