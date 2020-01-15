;; init-dired.el --- Initialize dired configurations.	-*- lexical-binding: t -*-

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
  :general
  (general-nmap dired-mode-map
    ;; Lower keys for commands not operating on all the marked files
    "RET" 'dired-find-alternate-file
    "q" 'quit-window)
  :config
  (define-key dired-mode-map (kbd "SPC") shadow-leader-map)
  ;; Show directory first
  ;;  (setq dired-listing-switches "-alh --group-directories-first")
  (setq dired-dwim-target t)
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)
  (put 'dired-find-alternate-file 'disabled nil)

  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
  ;; automatically refresh dired buffer on changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  ;; Extra Dired functionality
  (use-package dired-aux
    :ensure nil)

  ;; Show git info in dired
  (use-package dired-git-info
    :general
    (general-nmap dired-mode-map
      ;; Lower keys for commands not operating on all the marked files
      ")" 'dired-git-info-mode))

  (use-package dired-x
    :ensure nil ; built-in package
    :bind
    (:map shadow-leader-map
    ("fj" . dired-jump)
    ("jd" . dired-jump)
    ("jD" . dired-jump-other-window)))

  ;; Colourful dired
  (use-package diredfl
    :init (diredfl-global-mode 1))

  ;; Shows icons
  (use-package all-the-icons-dired
    :diminish
    :after (dired all-the-icons)
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    (with-no-warnings
      (defun my-all-the-icons-dired--display ()
        "Display the icons of files without colors in a dired buffer."
        (when dired-subdir-alist
          (let ((inhibit-read-only t))
            (save-excursion
              ;; TRICK: Use TAB to align icons
              (setq-local tab-width 1)
              (goto-char (point-min))
              (while (not (eobp))
                (when (dired-move-to-filename nil)
                  (insert " ")
                  (let ((file (dired-get-filename 'verbatim t)))
                    (unless (member file '("." ".."))
                      (let ((filename (dired-get-filename nil t)))
                        (if (file-directory-p filename)
                            (insert (all-the-icons-icon-for-dir filename nil ""))
                          (insert (all-the-icons-icon-for-file file :v-adjust -0.05))))
                      ;; Align and keep one space for refeshing after some operations
                      (insert "\t "))))
                (forward-line 1))))))
      (advice-add #'all-the-icons-dired--display
                  :override #'my-all-the-icons-dired--display))))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired
      :defer t))

(provide 'init-dired)
;;; init-dired.el ends here
