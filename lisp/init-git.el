;;; init-git.el --- version contral setup. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package magit
  :commands (magit-status magit-init magit-file-log magit-blame-mode)
  :bind
  (("C-x g i" . magit-init)
   ("C-x g f" . magit-file-log)
   ("C-x g b" . magit-blame-mode)
   ("C-x g m" . magit-branch-manager)
   ("C-x g c" . magit-branch)
   ("C-x g s" . magit-status)
   ("C-x g r" . magit-reflog)
   ("C-x g t" . magit-tag))
  :config
  ;; display buffer fullframe
  (setq magit-display-buffer-function #'shadow/magit-display-buffer-function)
  ;; `git-commit-mode'
  ;; see https://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

(use-package evil-magit
  :ensure t
  :after (evil magit)
  :init
  (setq evil-magit-want-horizontal-movement nil)
  ;; optional: this is the evil state that evil-magit will use
  (setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
  (setq evil-magit-use-y-for-yank nil)
  (add-hook 'git-commit-mode-hook #'evil-insert-state))

;; Gitflow externsion for Magit
(use-package magit-gitflow
  :after magit
  :diminish magit-gitflow-mode
  :bind
  (:map magit-status-mode-map
              ("G" . magit-gitflow-popup))
  :init
  (add-hook 'magit-mode-hook #'turn-on-magit-gitflow)
  :config
  (magit-define-popup-action 'magit-dispatch-popup
    ?G "GitFlow" #'magit-gitflow-popup ?!))

;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine)))

;; Pop up last commit information of current line
(use-package git-messenger
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init
  ;; Use `magit-show-commit' to show status/diff commands
  (setq git-messenger:use-magit-popup t))

;; Git modes
(use-package gitconfig-mode
  :defer t
  :mode (("/\\.?git/?config\\'" . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)
         ("/_gitconfig\\'" . gitconfig-mode))
  :config
  (add-hook 'gitconfig-mode-hook 'flyspell-mode))

(use-package gitignore-mode
  :defer t
  :mode (("/\\.gitignore\\'" . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'" . gitignore-mode)))

(use-package smerge
  :ensure nil
  :commands (smerge-mode)
  :init
  (defhydra hydra-smerge-mode (:hint nil
                                     :pre (smerge-mode 1)
                                     ;; Disable `smerge-mode' when quitting hydra if
                                     ;; no merge conflicts remain.
                                     :post (smerge-auto-leave))
    "
                                                    ╭────────┐
  Movement   Keep           Diff              Other │ smerge │
  ╭─────────────────────────────────────────────────┴────────╯
     ^_g_^       [_b_] base       [_<_] upper/base    [_C_] Combine
     ^_C-k_^     [_u_] upper      [_=_] upper/lower   [_r_] resolve
     ^_k_ ↑^     [_l_] lower      [_>_] base/lower    [_R_] remove
     ^_j_ ↓^     [_a_] all        [_H_] hightlight
     ^_C-j_^     [_RET_] current  [_E_] ediff             ╭──────────
     ^_G_^                                            │ [_q_] quit"
    ("g" (progn (goto-char (point-min)) (smerge-next)))
    ("G" (progn (goto-char (point-max)) (smerge-prev)))
    ("C-j" smerge-next)
    ("C-k" smerge-prev)
    ("j" next-line)
    ("k" previous-line)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("H" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("R" smerge-kill-current)
    ("q" nil :color blue))
  (defun kevin/enable-smerge-mode-maybe ()
    "Auto-enable `smerge-mode' when merge conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil :noerror)
        (smerge-mode 1)
        (when (featurep 'hydra)
          (hydra-smerge-mode/body))
        )))
  (add-hook 'find-file-hook #'kevin/enable-smerge-mode-maybe)
  )

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :custom-face
  (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
  (diff-hl-insert ((t (:background nil))))
  (diff-hl-delete ((t (:background nil))))
  :bind (:map diff-hl-command-map
         ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :init (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (defun my-diff-hl-fringe-bmp-function (_type _pos)
    "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    (define-fringe-bitmap 'my-diff-hl-bmp
      (vector (if sys/macp #b11100000 #b11111100))
      1 8
      '(center t)))
  (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

  (unless (display-graphic-p)
    (setq diff-hl-margin-symbols-alist
          '((insert . " ") (delete . " ") (change . " ")
            (unknown . " ") (ignored . " ")))
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(provide 'init-git)
;;; init-git.el ends here
