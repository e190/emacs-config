;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

;; Always load the newer .el or .elc file.
;; (setq load-prefer-newer t)
;;
;; ELPA: refer to https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
;;
(defun set-package-archives (archives)
  "Set specific package ARCHIVES repository."
  (interactive
   (list (intern (completing-read "Choose package archives: "
                                  '(melpa melpa-mirror emacs-china netease tuna)))))

  (setq package-archives
        (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                            (not (gnutls-available-p))))
               (proto (if no-ssl "http" "https")))
          (pcase archives
            ('melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
            ('melpa-mirror
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
            ('emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
            ('netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
            ('tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
            (archives
             (error "Unknown archives: '%s'" archives)))))

  (message "Set package archives to '%s'." archives))

(set-package-archives shadow-package-archives)

(setq package-user-dir "~/.emacs.d/elpa")

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(defun shadow-install-use-package ()
  "Install `use-package'."
  (unless (package-installed-p 'use-package)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "done.")
    (package-install 'use-package)))

(shadow-install-use-package)

(eval-when-compile
  (require 'use-package)

  ;; Should set before loading `use-package'
  (setq use-package-always-ensure t)
  ;; (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(defun shadow-package-menu-find-marks ()
  "Find packages marked for action in *Packages*."
  (interactive)
  (occur "^[A-Z]"))

(defun shadow-package-menu-filter-by-status (status)
  "Filter the *Packages* buffer by status."
  (interactive
   (list (completing-read
          "Status: " '("new" "installed" "dependency" "obsolete"))))
  (package-menu-filter (concat "status:" status)))

(define-key package-menu-mode-map "s" #'shadow-package-menu-filter-by-status)
(define-key package-menu-mode-map "a" #'shadow-package-menu-find-marks)

;; A modern Packages Menu
(use-package paradox
  :defer t
  :init
  (setq paradox-execute-asynchronously t)
  (setq paradox-github-token t)
  (setq paradox-display-star-count nil)

  (defalias 'upgrade-packages #'paradox-upgrade-packages)

  ;; Replace default `list-packages'
  (defadvice list-packages (before my-list-packages activate)
    (paradox-enable)))

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update
  :defer t)

;; Auto update packages
(use-package auto-package-update
  :defer t
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-last-update-day-filename (concat shadow-cache-dir "/.last-package-update-day")
        auto-package-update-hide-results t)
  (defalias 'upgrade-packages #'auto-package-update-now))

(use-package esup
  :defer t
  :ensure t)

(provide 'init-packages)
;;; init-packages.el ends here
