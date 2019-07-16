;;; config-dashboard.el --- Config for dashboard

;;; Code:

(use-package dashboard
  :ensure t
  :diminish page-break-lines-mode
  :preface
  (defvar homepage-url "https://github.com/lkzz/emacs.d")

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
  
  :init (setq initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  :hook (after-init . dashboard-setup-startup-hook)
  :bind (("<f2>" . open-dashboard)
         :map dashboard-mode-map
         ("H" . browse-homepage)
         ("O" . dashboard-open-init-file)
         ("R" . restore-session)
         ("q" . quit-dashboard))
  :config
  (setq dashboard-banner-logo-title "Happy Hacking, Emacs ♥ You!")
  (setq dashboard-startup-banner (expand-file-name "img/dashLogo.png" user-emacs-directory))
  (setq dashboard-items '((recents . 10)
                          (bookmarks . 5)
                          (projects . 5)))

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
    (insert (make-string (max 0 (floor (/ (- dashboard-banner-length 48) 2))) ?\ ))
    (insert (format "[%d packages loaded in %s]" (length package-activated-list) (emacs-init-time))))
  (add-to-list 'dashboard-item-generators  '(buttons . dashboard-insert-buttons))
  (add-to-list 'dashboard-items '(buttons))

  (dashboard-insert-startupify-lists)

  (defhydra hydra-dashboard (:color red :hint none)
      "
^Head^               ^Section^            ^Item^                  ^Dashboard^
^^───────────────────^^───────────────────^^──────────────────────^^───────────────
_U_pdate             _}_: Next            _RET_: Open             _<f2>_: Open
_H_omePage           _{_: Previous        _<tab>_/_C-i_: Next       _Q_: Quit
_R_ecover session    _r_: Recent Files    _<backtab>_: Previous
_L_ist sessions      _m_: Bookmarks       _C-n_: Next line
_S_ettings           _p_: Projects        _C-p_: Previous Line
"
      ("<tab>" widget-forward)
      ("C-i" widget-forward)
      ("<backtab>" widget-backward)
      ("RET" widget-button-press :exit t)
      ("g" dashboard-refresh-buffer :exit t)
      ("}" dashboard-next-section)
      ("{" dashboard-previous-section)
      ("r" dashboard-goto-recent-files)
      ("p" dashboard-goto-projects)
      ("m" dashboard-goto-bookmarks)
      ("H" browse-homepage :exit t)
      ("R" restore-session :exit t)
      ("L" persp-load-state-from-file :exit t)
      ("S" open-custom-file :exit t)
      ("U" update-config-and-packages :exit t)
      ("C-n" next-line)
      ("C-p" previous-line)
      ("<f2>" open-dashboard :exit t)
      ("Q" quit-dashboard :exit t)
      ("q" nil "quit")
      ("C-g" nil "quit"))

  (with-eval-after-load 'evil
    (evil-define-key 'normal dashboard-mode-map
       (kbd "h") 'hydra-dashboard/body
       (kbd "?") 'hydra-dashboard/body)
       "r" 'dashboard-goto-recent-files)
  )

(provide 'init-dashboard)

;;; config-dashboard.el ends here
