;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;;; Code:

(use-package dashboard
  :ensure t
  :diminish page-break-lines-mode
  :preface
  (defvar homepage-url "https://github.com/e190/emacs-config")

  :functions (all-the-icons-faicon
              all-the-icons-material
              open-custom-file
              persp-get-buffer-or-null
              persp-load-state-from-file
              persp-switch-to-buffer
              winner-undo
              widget-forward)
  :pretty-hydra
  ((:title (pretty-hydra-title "Dashboard" 'material "dashboard" :height 1.1 :v-adjust -0.225)
    :color pink :quit-key "q")
    ("Navigator"
    (("U" update-config-and-packages "update" :exit t)
      ("H" browse-homepage "homepage" :exit t)
      ("R" restore-session "recover session" :exit t)
      ("L" persp-load-state-from-file "list sessions" :exit t)
      ("S" open-custom-file "settings" :exit t))
    "Section"
    (("}" dashboard-next-section "next")
      ("{" dashboard-previous-section "previous")
      ("r" dashboard-goto-recent-files "recent files")
      ("m" dashboard-goto-bookmarks "projects")
      ("p" dashboard-goto-projects "bookmarks"))
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
  :init (setq initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  :hook (after-init . dashboard-setup-startup-hook)
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("H" . browse-homepage)
         ("O" . dashboard-open-init-file)
         ("R" . restore-session)
         ("q" . quit-dashboard)
         ("h" . dashboard-hydra/body)
         ("?" . dashboard-hydra/body))
  :config
  (setq dashboard-banner-logo-title "Happy Hacking, Emacs ♥ You!"
        dashboard-startup-banner (expand-file-name "img/dashLogo.png" user-emacs-directory)
        dashboard-items '((recents . 10)
                                (bookmarks . 5)
                                (projects . 5))

        dashboard-set-init-info t
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-heading-icons '((recents   . "file-text")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "file-directory")
                                  (registers . "database"))

        dashboard-set-footer t
        dashboard-footer (format "Powered by Shadow, %s" (format-time-string "%Y"))
        dashboard-footer-icon (cond ((display-graphic-p)
                                     (all-the-icons-faicon "heart"
                                                           :height 1.1
                                                           :v-adjust -0.05
                                                           :face 'error))
                                    ((char-displayable-p ?🧡) "🧡 ")
                                    (t (propertize ">" 'face 'font-lock-doc-face))))
  (defun dashboard-insert-buttons (_list-size)
    (insert "\n")
    (insert (make-string (max 0 (floor (/ (- dashboard-banner-length 51) 2))) ?\ ))
    (widget-create 'url-link
                   :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
                   :help-echo "Open Emacs Github page"
                   :mouse-face 'highlight
                   homepage-url)
    (insert " ")
    (widget-create 'push-button
                   :help-echo "Open Personal Configurations"
                   :action (lambda (&rest _) (shadow/open-init-file))
                   :mouse-face 'highlight
                   :button-prefix ""
                   :button-suffix ""
                   (propertize "Open Config" 'face 'font-lock-keyword-face))
    (insert " ")
    (widget-create 'push-button
                   :help-echo "Restore previous session"
                   :action (lambda (&rest _) (restore-session))
                   :mouse-face 'highlight
                   :button-prefix ""
                   :button-suffix ""
                   (propertize "Restore Session" 'face 'font-lock-keyword-face))
    (insert "\n")
    (insert "\n")
    (insert (make-string (max 0 (floor (/ (- dashboard-banner-length 48) 2))) ?\ )))
  (add-to-list 'dashboard-item-generators  '(buttons . dashboard-insert-buttons))
  (add-to-list 'dashboard-items '(buttons))

  (dashboard-insert-startupify-lists)
  (with-eval-after-load 'evil
    (evil-define-key 'normal dashboard-mode-map
       (kbd "h") 'dashboard-hydra/body
       (kbd "?") 'dashboard-hydra/body)
       "r" 'dashboard-goto-recent-files)

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    (goto-char (point-min))
    (dashboard-goto-recent-files)
    (if (> (length (window-list-1))
           ;; exclude `treemacs' window
           (if (and (fboundp 'treemacs-current-visibility)
                    (eq (treemacs-current-visibility) 'visible))
               2
             1))
        (setq dashboard-recover-layout-p t))
    (delete-other-windows))

  (defun restore-session ()
    "Restore last session."
    (interactive)
    (when (bound-and-true-p persp-mode)
      (message "Restoring session...")
      (condition-case-unless-debug err
          (persp-load-state-from-file)
        (error
         (message "Error: Unable to restore last session -- %s" err)))
      (when (persp-get-buffer-or-null persp-special-last-buffer)
        (persp-switch-to-buffer persp-special-last-buffer))))

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
    (funcall (local-key-binding "r")))

  (defun dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (funcall (local-key-binding "p")))

  (defun dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (funcall (local-key-binding "m")))
  )

(provide 'init-dashboard)

;;; init-dashboard.el ends here
