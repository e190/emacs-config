;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'init-constants))

(unless emacs/>=25.3p (setq shadow-dashboard nil))
(when shadow-dashboard
  (use-package dashboard
    :diminish (dashboard-mode page-break-lines-mode)
    :defines persp-special-last-buffer
    :functions (all-the-icons-faicon
                all-the-icons-material
                open-custom-file
                persp-get-buffer-or-null
                persp-load-state-from-file
                persp-switch-to-buffer
                winner-undo
                widget-forward)
    :custom-face (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
    :pretty-hydra
    ((:title (pretty-hydra-title "Dashboard" 'material "dashboard" :height 1.1 :v-adjust -0.225)
      :color pink :quit-key "q")
      ("Navigator"
      (("U" update-config-and-packages "update" :exit t)
        ("H" browse-homepage "homepage" :exit t)
        ("R" restore-session "recover session" :exit t)
        ("L" persp-load-state-from-file "list sessions" :exit t)
        ("S" open-custom-file "settings" :exit t)
        ("O" shadow/open-init-file "init file" :exit t))
      "Section"
      (("}" dashboard-next-section "next")
        ("{" dashboard-previous-section "previous")
        ("r" dashboard-goto-recent-files "recent files")
        ("m" dashboard-goto-bookmarks "bookmarks")
        ("p" dashboard-goto-projects "projects"))
      "Item"
      (("RET" widget-button-press "open" :exit t)
        ("<tab>" widget-forward "next")
        ("C-i" widget-forward "next")
        ("<backtab>" widget-backward "previous")
        ("C-n" next-line "next line")
        ("C-p" previous-line "previous  line"))
      "Misc"
      (("<f2>" open-dashboard "open" :exit t)
        ("g" dashboard-refresh-buffer "refresh" :exit t)
        ("Q" quit-dashboard "quit" :exit t))))
    :bind ("<f2>" . open-dashboard)
    :hook (dashboard-mode . (lambda () (setq-local frame-title-format "Shadow's Emacs")))
    :init (dashboard-setup-startup-hook)
    :general
    (general-nmap dashboard-mode-map
      ;; Lower keys for commands not operating on all the marked files
      "j" 'evil-next-line-first-non-blank
      "k" 'evil-previous-line-first-non-blank
      "<down>" 'evil-next-line-first-non-blank
      "<up>" 'evil-previous-line-first-non-blank
      "O" 'shadow/open-init-file
      "r" 'dashboard-goto-recent-files
      "p" 'dashboard-goto-projects
      "q" 'quit-dashboard
      "h" 'dashboard-hydra/body
      "?" 'dashboard-hydra/body)
    :config
    (setq dashboard-banner-logo-title "Emacs ♥ You - Enjoy Programming & Writing"
          dashboard-startup-banner (or Shadow-logo 'official)
          ;; dashboard-center-content t
          dashboard-show-shortcuts nil
          dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            (projects . 5))

          dashboard-set-init-info t
          dashboard-set-file-icons t
          dashboard-set-heading-icons t
          dashboard-heading-icons '((recents   . "file-text")
                                    (bookmarks . "bookmark")
                                    (agenda    . "calendar")
                                    (projects  . "briefcase")
                                    (registers . "database"))

          dashboard-set-footer t
          dashboard-footer (format "Powered by Shadow, %s" (format-time-string "%Y"))
          dashboard-footer-icon (cond ((display-graphic-p)
                                        (all-the-icons-faicon "heart"
                                                              :height 1.1
                                                              :v-adjust -0.05
                                                              :face 'error))
                                      ((char-displayable-p ?🧡) "🧡 ")
                                      (t (propertize ">" 'face 'font-lock-doc-face)))

          dashboard-set-navigator t
          dashboard-navigator-buttons
          `(((,(when (display-graphic-p)
                  (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0))
              "Homepage" "Browse homepage"
              (lambda (&rest _) (browse-url Shadow-homepage)))
              (,(when (display-graphic-p)
                  (all-the-icons-material "restore" :height 1.35 :v-adjust -0.24))
              "Restore" "Restore previous session"
              (lambda (&rest _) (restore-session)))
              (,(when (display-graphic-p)
                  (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
              "Settings" "Open custom file"
              (lambda (&rest _) (find-file custom-file)))
              (,(when (display-graphic-p)
                  (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
              "Update" "Update Shadow Emacs"
              (lambda (&rest _) (shadow-update)))
              (,(if (display-graphic-p)
                    (all-the-icons-faicon "question" :height 1.2 :v-adjust -0.1)
                  "?")
              "" "Help (?/h)"
              (lambda (&rest _) (dashboard-hydra/body))
              font-lock-string-face))))

    (defun my-banner-path (&rest _)
      "Return the full path to banner."
      (expand-file-name "banner.txt" user-emacs-directory))
    (advice-add #'dashboard-get-banner-path :override #'my-banner-path)

    ;; WORKAROUND: fix differnct background color of the banner image.
    ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/203
    (defun my-dashboard-insert-image-banner (banner)
      "Display an image BANNER."
      (when (file-exists-p banner)
        (let* ((title dashboard-banner-logo-title)
                (spec (create-image banner))
                (size (image-size spec))
                (width (car size))
                (left-margin (max 0 (floor (- dashboard-banner-length width) 2))))
          (goto-char (point-min))
          (insert "\n")
          (insert (make-string left-margin ?\ ))
          (insert-image spec)
          (insert "\n\n")
          (when title
            (dashboard-center-line title)
            (insert (format "%s\n\n" (propertize title 'face 'dashboard-banner-logo-title)))))))
    (advice-add #'dashboard-insert-image-banner :override #'my-dashboard-insert-image-banner)

    (defvar dashboard-recover-layout-p nil
      "Wether recovers the layout.")

    (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      ;; Check if need to recover layout
      (if (> (length (window-list-1))
              ;; exclude `treemacs' window
              (if (and (fboundp 'treemacs-current-visibility)
                      (eq (treemacs-current-visibility) 'visible))
                  2
                1))
          (setq dashboard-recover-layout-p t))

      (delete-other-windows)

      ;; Refresh dashboard buffer
      (if (get-buffer dashboard-buffer-name)
          (kill-buffer dashboard-buffer-name))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name)

      ;; Jump to the first section
      (goto-char (point-min))
      (dashboard-goto-recent-files))

    (defun restore-session ()
      "Restore last session."
      (interactive)
      (when (bound-and-true-p persp-mode)
        (message "Restoring session...")
        (condition-case-unless-debug err
            (persp-load-state-from-file)
          (error
            (message "Error: Unable to restore last session -- %s" err)))
        (quit-window t)
        (when (persp-get-buffer-or-null persp-special-last-buffer)
          (persp-switch-to-buffer persp-special-last-buffer))
        (message "Done")))

    (defun quit-dashboard ()
      "Quit dashboard window."
      (interactive)
      (quit-window t)
      (when (and dashboard-recover-layout-p
                  (bound-and-true-p winner-mode))
        (winner-undo)
        (setq dashboard-recover-layout-p nil)))

    (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (let (func (local-key-binding "r"))
        (and func (funcall func))))

    (defun dashboard-goto-projects ()
      "Go to projects."
      (interactive)
      (let ((func (local-key-binding "p")))
        (and func (funcall func))))

    (defun dashboard-goto-bookmarks ()
      "Go to bookmarks."
      (interactive)
      (let ((func (local-key-binding "m")))
        (and func (funcall func))))))

(provide 'init-dashboard)

;;; init-dashboard.el ends here
