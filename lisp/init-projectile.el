;; init-projectile.el --- Initialize projectile configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

;; Manage and navigate projects
(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
         ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
         ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-sort-order 'recentf
        projectile-cache-file (concat shadow-cache-dir "projectile.cache")
        projectile-known-projects-file (concat shadow-cache-dir "projectile-bookmarks.eld")
        projectile-use-git-grep t)
  :config
  ;; (projectile-update-mode-line)         ; Update mode-line at the first time

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Faster searching on Windows
  (when sys/win32p
    (when (or (executable-find "fd") (executable-find "rg"))
      (setq projectile-indexing-method 'alien
            projectile-enable-caching nil))

    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil)))

(with-eval-after-load 'counsel
  (use-package counsel-projectile
    :init
    (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
    (counsel-projectile-mode 1)
    :bind
    (:map shadow-leader-map
    ("pD" . counsel-projectile-dired)
    ("pF" . counsel-projectile-find-file-dwim)
    ("pb" . counsel-projectile-switch-to-buffer)
    ("pd" . counsel-projectile-find-dir)
    ("pf" . counsel-projectile-find-file)
    ("pp" . counsel-projectile-switch-project)
    ("pr" . counsel-projectile-rg)
    ("pi" . counsel-projectile-git-grep))))

(provide 'init-projectile)
;;; init-projectile ends here
