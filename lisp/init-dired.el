;;; config-dired.el --- Config for dired

;;; Commentary:
;;

;;; Code:

(use-package dired
  :ensure nil ; built-in package
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map "gr" 'revert-buffer))
  :bind
  (:map shadow-leader-map
   ("ad" . dired))
  :config
  (define-key dired-mode-map (kbd "SPC") shadow-leader-map)
  ;; Show directory first
  ;;  (setq dired-listing-switches "-alh --group-directories-first")
  (setq dired-dwim-target t)
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
  ;; automatically refresh dired buffer on changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  ;; Extra Dired functionality
  (use-package dired-aux
    :ensure nil)

  (use-package dired-x
    :ensure nil ; built-in package
    :bind
    (:map shadow-leader-map
    ("fj" . dired-jump)
    ("jd" . dired-jump)
    ("jD" . dired-jump-other-window))))

(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode
  :after (dired all-the-icons)
  :hook ((dired-mode . all-the-icons-dired-mode)
	 (ranger-mode . all-the-icons-dired-mode)))

(provide 'init-dired)
;;; config-dired.el ends here
