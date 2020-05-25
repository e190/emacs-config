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
        projectile-sort-order 'recentf
        projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-cache-file (concat shadow-cache-dir "/projectile.cache")
        projectile-known-projects-file (concat shadow-cache-dir "/projectile-bookmarks.eld")
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

(use-package find-file-in-project
  :defer t
  :ensure t
  :config
  (setq ffip-use-rust-fd t)

  ;; ffip-diff-mode (read only) evil setup
  (defun ffip-diff-mode-hook-setup ()
    (evil-local-set-key 'normal "q" (lambda () (interactive) (quit-window t)))
    (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
    ;; "C-c C-a" is binding to `diff-apply-hunk' in `diff-mode'
    (evil-local-set-key 'normal "a" 'ffip-diff-apply-hunk)
    (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
  (add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)
)


(provide 'init-projectile)
;;; init-projectile ends here
