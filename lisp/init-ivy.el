;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package counsel
  :diminish counsel-mode ivy-mode
  :functions (my-ivy-fly-time-travel
              my-swiper-toggle-counsel-rg
              my-swiper-toggle-rg-dwim)
  :commands (ivy--format-function-generic
             ivy--add-face)
  :bind
  (;; Current global keymap
   ("C-s" . swiper-isearch)
   ("M-x" . counsel-M-x)
   ("C-c C-r" . ivy-resume)

   :map shadow-leader-map
   ("SPC" . counsel-M-x)

   ;; files
   ("fel" . counsel-find-library)
   ("ff" . counsel-find-file)
   ("fL" . counsel-locate)
   ("fr" . counsel-recentf)

   ;; help
   ("?"  . counsel-descbinds)
   ("hdf" . counsel-describe-function)
   ("hdm" . describe-mode)
   ("hdv" . counsel-describe-variable)
   ;; register/ring
   ("ry" . counsel-yank-pop)

   ;; jumping
   ("ji" . counsel-imenu)

   ;; search
   ("/"  . counsel-rg)
   ("si" . swiper-isearch)
   ("sb" . swiper-all)
   ("sr" . swiper-isearch-backward)

   ("bb" . ivy-switch-buffer)

  :map counsel-mode-map
  ([remap swiper] . counsel-grep-or-swiper)
  ("C-x C-r" . counsel-recentf)
  ("C-x j" . counsel-mark-ring)
  ("C-c c e" . counsel-colors-emacs)
  ("C-c c u" . counsel-unicode-char)
  :map swiper-map
  ("M-s" . swiper-isearch-toggle)
  ("M-%" . swiper-query-replace)

  :map isearch-mode-map
  ("M-s" . swiper-isearch-toggle)
  :map ivy-minibuffer-map
  ("C-n" . ivy-next-line)
  ("C-p" . ivy-previous-line)
  ("C-M-j" . ivy-scroll-up-command)
  ("C-M-k" . ivy-scroll-down-command)
  ("C-<return>" . ivy-alt-done)
  ("M-<return>" . ivy-immediate-done)
  ("C-M-n" . ivy-restrict-to-matches)
  ("C-h" . backward-delete-char-untabify)
  ("C-S-h" . help-map)
  ("C-l" . ivy-alt-done)
  ("<escape>" . minibuffer-keyboard-quit))

  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 10
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil)

  (defun my-ivy-format-function-arrow (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat (if (display-graphic-p)
                   (all-the-icons-octicon "chevron-right" :height 0.8 :v-adjust -0.05)
                 ">")
               (propertize " " 'display `(space :align-to 2))
               (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat (propertize " " 'display `(space :align-to 2)) str))
     cands
     "\n"))
  (setq ivy-format-functions-alist '((t . my-ivy-format-function-arrow)))
  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")
  :config
  (add-to-list 'ivy-format-functions-alist '(counsel-describe-face . counsel--faces-format-function))

  ;; Use faster search tools: ripgrep or the silver search
  (let ((cmd (cond ((executable-find "rg")
                    "rg -S --no-heading --line-number --color never '%s' %s")
                   ((executable-find "ag")
                    "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
                   (t counsel-grep-base-command))))
    (setq counsel-grep-base-command cmd))

  ;; Build abbreviated recent file list.
  (defun my-counsel-recentf ()
    "Find a file on `recentf-list'."
    (interactive)
    (require 'recentf)
    (recentf-mode)
    (ivy-read "Recentf: " (mapcar #'abbreviate-file-name recentf-list)
              :action (lambda (f)
                        (with-ivy-window
                          (find-file f)))
              :require-match t
              :caller 'counsel-recentf))
  (advice-add #'counsel-recentf :override #'my-counsel-recentf)

  ;; Pre-fill search keywords
  ;; @see https://www.reddit.com/r/emacs/comments/b7g1px/withemacs_execute_commands_like_marty_mcfly/
  (defvar my-ivy-fly-commands
    '(query-replace-regexp
      flush-lines
      keep-lines
      ivy-read
      swiper
      swiper-backward
      swiper-all
      swiper-isearch
      swiper-isearch-backward
      counsel-grep-or-swiper
      counsel-grep-or-swiper-backward
      counsel-grep
      counsel-ack
      counsel-ag
      counsel-rg
      counsel-pt))

  (defun my-ivy-fly-back-to-present ()
    (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
           ;; repeat one time to get straight to the first history item
           (setq unread-command-events
                 (append unread-command-events
                         (listify-key-sequence (kbd "M-p")))))
          ((memq this-command '(self-insert-command
                                yank
                                ivy-yank-word
                                counsel-yank-pop))
           (delete-region (point)
                          (point-max)))))

  (defun my-ivy-fly-time-travel ()
    (when (memq this-command my-ivy-fly-commands)
      (let* ((kbd (kbd "M-n"))
             (cmd (key-binding kbd))
             (future (and cmd
                          (with-temp-buffer
                            (when (ignore-errors
                                    (call-interactively cmd) t)
                              (buffer-string))))))
        (when future
          (save-excursion
            (insert (propertize (replace-regexp-in-string
                                 "\\\\_<" ""
                                 (replace-regexp-in-string
                                  "\\\\_>" ""
                                  future))
                                'face 'shadow)))
          (add-hook 'pre-command-hook 'my-ivy-fly-back-to-present nil t)))))

  (add-hook 'minibuffer-setup-hook #'my-ivy-fly-time-travel)

  ;; Improve search experience of `swiper'
  ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
  (defun my-swiper-toggle-counsel-rg ()
    "Toggle `counsel-rg' with current swiper input."
    (interactive)
    (let ((text (replace-regexp-in-string
                 "\n" ""
                   (replace-regexp-in-string "^.*Swiper: " ""
                                             (thing-at-point 'line t)))))
	  (setq my-swiper-to-counsel-rg-search text)
      (ivy-quit-and-run
        (counsel-rg my-swiper-to-counsel-rg-search default-directory))))
  (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)
  (defun swiper-toggle-color-rg ()
    "Toggle `color-rg' with current swiper input."
    (interactive)
    (let ((text (replace-regexp-in-string
                "\n" ""
                 (replace-regexp-in-string
                  "\\\\_<" ""
                  (replace-regexp-in-string
                   "\\\\_>" ""
                   (replace-regexp-in-string "^.*Swiper: " ""
                                             (thing-at-point 'line t)))))))
      (setq my-last-swiper-to-counsel-rg-search text)
      (ivy-quit-and-run
        (color-rg-search-input my-last-swiper-to-counsel-rg-search default-directory))))
  (bind-key "<M-return>" #'swiper-toggle-color-rg swiper-map)

  ;; (with-eval-after-load 'rg
  ;;   (defun my-swiper-toggle-rg-dwim ()
  ;;     "Toggle `rg-dwim' with current swiper input."
  ;;     (interactive)
  ;;     (ivy-quit-and-run (rg-dwim default-directory)))
  ;;   (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
  ;;   (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim ivy-minibuffer-map))
  ;; Enhance fuzzy matching
  (use-package flx
    :config (setq ivy-re-builders-alist
                  '((swiper . ivy--regex-plus)
                    (swiper-all . ivy--regex-plus)
                    (swiper-isearch . ivy--regex-plus)
                    (counsel-ag . ivy--regex-plus)
                    (counsel-rg . ivy--regex-plus)
                    (counsel-pt . ivy--regex-plus)
                    (counsel-ack . ivy--regex-plus)
                    (counsel-grep . ivy--regex-plus)
                    (t . ivy--regex-fuzzy))))

  ;; Add help menu by pressing C-o in minibuffer.
(use-package ivy-hydra
  :bind (:map ivy-minibuffer-map
          ("M-o" . ivy-dispatching-done-hydra)))

(use-package counsel-projectile
  :bind
  (:map shadow-leader-map
   ("p SPC" . counsel-projectile)
   ("pb" . counsel-projectile-switch-to-buffer)
   ("pd" . counsel-projectile-find-dir)
   ("pp" . counsel-projectile-switch-project)
   ("pf" . counsel-projectile-find-file)
   ("pr" . projectile-recentf))
  :init
  (with-eval-after-load 'projectile
    (setq projectile-switch-project-action 'counsel-projectile-find-file))))

;; counsel-M-x will use smex if available.
(use-package smex
  :defer t
  :init
  (setq smex-save-file (concat shadow-cache-dir "/smex-items")))

(use-package ivy-rich
  :ensure t
  :hook ((ivy-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)

  ;; Setting tab size to 1, to insert tabs as delimiters
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1)))
  ;; :config
  ;; replace “/home/username” with “~”
  ;; (setq ivy-rich-path-style 'abbrev)
  ;; (setq ivy-virtual-abbreviate 'full)
  (setq ivy-rich--display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30))  ; return the candidate itself
            (ivy-rich-switch-buffer-size (:width 10 :face warning))  ; return the buffer size
            (ivy-rich-switch-buffer-major-mode (:width 20 :face error))          ; return the major mode info
            (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
            (ivy-rich-switch-buffer-path (:width (lambda (x)
                                                   (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))
                                                 :face font-lock-comment-face)))
           :predicate (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))  ; thr original transfomer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8)) ; return the candidate itself
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))) ; return the last modified time of the file)
          counsel-bookmark
          (:columns
           ((ivy-rich-candidate (:width 20 :face success)) ; return the candidate itself
            (ivy-rich-bookmark-info (:face font-lock-comment-face)))) ; return the last modified time of the file)
          )
        )
  )

(provide 'init-ivy)
;;; init-ivy.el ends here
