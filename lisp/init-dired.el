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
    "?" 'hydra-dired/body
    "q" 'quit-window)
  :config
  (defhydra hydra-dired (:hint nil :color pink)
    "
――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
_+_ mkdir          │ _A_ find regexp  │ _m_ark           │ _(_ details       │ _C-i_nsert-subdir │ C-x C-q : edit
_C_opy             │ _Q_ repl regexp  │ _U_nmark all     │ _)_ omit-mode     │ _C-r_emove-subdir │ C-c C-c : commit
_D_elete           │ _O_ view other   │ _u_nmark         │ _l_ redisplay     │ _$_ hide-subdir   │ C-c ESC : abort
_R_ename           │ _o_pen other     │ _t_oggle         │ _g_ revert buf    │ _w_ kill-subdir   │
_Y_ rel symlink    │ _M_ chmod        │ _E_xtension mark │ _s_ort            │ _e_ ediff         │
_S_ymlink          │ _G_ chgrp        │ _F_ind marked    │ _?_ toggle hydra  │ _=_ pdiff         │
_z_ compress-file  │ _i_nsert-subtree │ ^ ^              │ ^ ^               │                 │
_Z_ compress       │ _r_emove-subtree │                │                 │                 │
_v_iew             │ ^ ^              │                │                 │                 │
――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
"
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("C-i" dired-maybe-insert-subdir)
    ("C-r" dired-kill-subdir)
    ("i" dired-subtree-insert)
    ("r" dired-subtree-remove)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("C-x k" kill-buffer-and-window :exit t)
    ("q" nil)
    ("<escape>" nil)
    ("?" nil :color blue))
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
    (defun my-all-the-icons-dired--display ()
      "Display the icons of files without colors in a dired buffer."
      (when dired-subdir-alist
        (let ((inhibit-read-only t))
          ;; NOTE: don't display icons it too many items
          (if (<= (count-lines (point-min) (point-max)) 1000)
              (save-excursion
                ;; TRICK: Use TAB to align icons
                (setq-local tab-width 1)

                ;; Insert icons before the filenames
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
                  (forward-line 1)))
            (message "Not display icons because of too many items.")))))
    (advice-add #'all-the-icons-dired--display
                :override #'my-all-the-icons-dired--display)))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired
      :defer t))

(provide 'init-dired)
;;; init-dired.el ends here
