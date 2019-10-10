;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

(use-package counsel
  :diminish counsel-mode ivy-mode
  :functions (my-ivy-fly-time-travel
              my-swiper-toggle-counsel-rg
              my-swiper-toggle-rg-dwim
              shadow-counsel-git-fast
              shadow-counsel-rg-current-dir)
  :commands (ivy--format-function-generic
             ivy--add-face)
  :bind(
   ;; Current global keymap
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
   ("sg" . shadow-counsel-git-fast)
   ("se" . shadow-counsel-rg-current-dir)

   ("bb" . ivy-switch-buffer)

  :map counsel-mode-map
  ([remap swiper] . counsel-grep-or-swiper)
  ([remap dired] . counsel-dired)
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
  (setq ivy-format-functions-alist '((counsel-describe-face . counsel--faces-format-function)
                                     (t . my-ivy-format-function-arrow)))
  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")
  :config
  ;; Use faster search tools: ripgrep or the silver search
  (let ((cmd (cond ((executable-find "rg")
                    "rg -S --no-heading --line-number --color never '%s' %s")
                   ((executable-find "ag")
                    "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
                   (t counsel-grep-base-command))))
    (setq counsel-grep-base-command cmd))

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
  (add-hook 'minibuffer-exit-hook
            (lambda ()
              (remove-hook 'pre-command-hook 'my-ivy-fly-back-to-present t)))

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
        (counsel-rg my-swiper-to-counsel-rg-search))))
  (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)

  ;; (defun swiper-toggle-color-rg ()
  ;;   "Toggle `color-rg' with current swiper input."
  ;;   (interactive)
  ;;     (ivy-quit-and-run
  ;;       (color-rg-search-symbol-in-project)))
  ;; (bind-key "<M-return>" #'swiper-toggle-color-rg swiper-map)

  (with-eval-after-load 'rg
    (defun my-swiper-toggle-rg-dwim ()
      "Toggle `rg-dwim' with current swiper input."
      (interactive)
      (ivy-quit-and-run (rg-dwim default-directory)))
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim swiper-map)
    (bind-key "<M-return>" #'my-swiper-toggle-rg-dwim ivy-minibuffer-map))

  (defun shadow-counsel-git-fast ()
    "use ripgrep as the backed for counsel-git"
    (interactive)
    (let ((counsel-git-cmd "rg --files"))
      (counsel-git)))

  (defun shadow-counsel-rg-current-dir ()
    "run `counsel-rg' in current direcotry"
    (interactive)
    (counsel-rg nil default-directory))

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
            ("M-o" . ivy-dispatching-done-hydra))))

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
          )))


;; Use universal ctags to build the tags database for the project.
;; When you first want to build a TAGS database run 'touch TAGS'
;; in the root directory of your project.
(use-package counsel-etags
  :ensure t
  :if (eq shadow-lsp-mode 'ctags)
  :after evil counsel
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function counsel-etags-virtual-update-tags "counsel-etags.el")
    (declare-function counsel-etags-guess-program "counsel-etags.el")
    (declare-function counsel-etags-locate-tags-file "counsel-etags.el"))
  :config
  ;; Ignore files above 800kb
  (setq counsel-etags-max-file-size 800)
  ;; Ignore build directories for tagging
  (add-to-list 'counsel-etags-ignore-directories '"build*")
  (add-to-list 'counsel-etags-ignore-directories '".vscode")
  (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Do case-sensitive tag searches
  (setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 10)

  (define-key evil-normal-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
  (define-key evil-visual-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
  ;; (setq ctags-command "ctags.exe -e -R ")

  ;; Set up auto-update
  ;; (add-hook 'prog-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'after-save-hook
  ;;                       'counsel-etags-virtual-update-tags 'append 'local)))

  ;; The function provided by counsel-etags is broken (at least on Linux)
  ;; and doesn't correctly exclude directories, leading to an excessive
  ;; amount of incorrect tags. The issue seems to be that the trailing '/'
  ;; in e.g. '*dirname/*' causes 'find' to not correctly exclude all files
  ;; in that directory, only files in sub-directories of the dir set to be
  ;; ignore.
  (defun my-scan-dir (src-dir &optional force)
    "Create tags file from SRC-DIR. \
     If FORCE is t, the commmand is executed without \
     checking the timer."
    (let* ((find-pg (or
                     counsel-etags-find-program
                     (counsel-etags-guess-program "find")))
           (ctags-pg (or
                      counsel-etags-tags-program
                      (format "%s -e -L" (counsel-etags-guess-program
                                          "ctags"))))
           (default-directory src-dir)
           ;; run find&ctags to create TAGS
           (cmd (format
                 "%s . \\( %s \\) -prune -o -type f -not -size +%sk %s | %s -"
                 find-pg
                 (mapconcat
                  (lambda (p)
                    (format "-iwholename \"*%s*\"" p))
                  counsel-etags-ignore-directories " -or ")
                 counsel-etags-max-file-size
                 (mapconcat (lambda (n)
                              (format "-not -name \"%s\"" n))
                            counsel-etags-ignore-filenames " ")
                 ctags-pg))
           (tags-file (concat (file-name-as-directory src-dir) "TAGS"))
           (doit (or force (not (file-exists-p tags-file)))))
      ;; always update cli options
      (when doit
        (message "%s at %s" cmd default-directory)
        (async-shell-command cmd)
        (visit-tags-table tags-file t))))

  ;; (setq counsel-etags-update-tags-backend
  ;;       (lambda ()
  ;;         (interactive)
  ;;         (let* ((tags-file (counsel-etags-locate-tags-file)))
  ;;           (when tags-file
  ;;             (my-scan-dir (file-name-directory tags-file) t)
  ;;             (run-hook-with-args
  ;;              'counsel-etags-after-update-tags-hook tags-file)
  ;;             (unless counsel-etags-quiet-when-updating-tags
  ;;               (message "%s is updated!" tags-file))))))
  )


(use-package counsel-gtags
  :demand t
  :if (eq shadow-lsp-mode 'gtags)
  :hook
  ((c-mode-hook . counsel-gtags-mode)
   (c++-mode-hook . counsel-gtags-mode)))
  ;;:config (  (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
  ;;(define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
  ;;(define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
  ;;(define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)))

(provide 'init-ivy)
;;; init-ivy.el ends here
