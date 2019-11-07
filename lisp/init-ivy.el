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
   ("fr" . counsel-buffer-or-recentf)

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

  (when (executable-find "fd")
    (defun cm/counsel-locate-cmd-fd (input)
      (format "fd --color never --hidden  -- \"%s\" /"
              (counsel--elisp-to-pcre
               (ivy--regex input t))))
    (setq counsel-locate-cmd 'cm/counsel-locate-cmd-fd))

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
      (let ((text (replace-regexp-in-string
                  "\n" ""
                    (replace-regexp-in-string "^.*Swiper: " ""
                                              (thing-at-point 'line t)))))
        (setq my-swiper-to-counsel-rg-search text)
        (setq my-directory (ffip-get-project-root-directory))
      (ivy-quit-and-run (rg my-swiper-to-counsel-rg-search "everything" (ffip-get-project-root-directory)))))
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
  ;; https://github.com/lewang/flx
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

  ;; A hydra for better `ivy' experience
  ;; Add help menu by pressing C-o in minibuffer.
  (use-package ivy-hydra
    :bind (:map ivy-minibuffer-map
            ("M-o" . ivy-dispatching-done-hydra))))

;; counsel-M-x will use smex if available.
;; (use-package smex
;;   :defer t
;;   :init
;;   (setq smex-save-file (concat shadow-cache-dir "/smex-items")))

;; Enchanced M-x
;; https://github.com/DarwinAwardWinner/amx
(use-package amx
  ;; :defer t
  :hook (after-init . amx-mode)
  :init
  (setq amx-save-file (concat shadow-cache-dir "/amx-items")))

;; More friendly display transformer for Ivy
(use-package ivy-rich
  :defines (all-the-icons-icon-alist
            all-the-icons-dir-icon-alist
            bookmark-alist)
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-icon-family
              all-the-icons-faicon
              all-the-icons-octicon
              all-the-icons-material
              all-the-icons-match-to-alist
              all-the-icons-auto-mode-match?
              all-the-icons-dir-is-submodule
              my-ivy-rich-bookmark-type)
  :commands (ivy-rich-bookmark-filename
             ivy-rich-bookmark-type)
  :preface
  (defun ivy-rich-bookmark-name (candidate)
    (car (assoc candidate bookmark-alist)))

  (defun ivy-rich-buffer-icon (candidate)
    "Display buffer icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((buffer (get-buffer candidate))
             (buffer-file-name (buffer-file-name buffer))
             (major-mode (buffer-local-value 'major-mode buffer))
             (icon (if (and buffer-file-name
                            (all-the-icons-auto-mode-match?))
                       (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name) :v-adjust -0.05)
                     (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-file-icon (candidate)
    "Display file icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((path (file-local-name (concat ivy--directory candidate)))
             (file (file-name-nondirectory path))
             (icon (cond
                    ((file-directory-p path)
                     (cond
                      ((and (fboundp 'tramp-tramp-file-p)
                            (tramp-tramp-file-p default-directory))
                       (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01))
                      ((file-symlink-p path)
                       (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.01))
                      ((all-the-icons-dir-is-submodule path)
                       (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.01))
                      ((file-exists-p (format "%s/.git" path))
                       (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                      (t (let ((matcher (all-the-icons-match-to-alist path all-the-icons-dir-icon-alist)))
                           (apply (car matcher) (list (cadr matcher) :v-adjust 0.01))))))
                    ((string-match "^/.*:$" path)
                     (all-the-icons-material "settings_remote" :height 1.0 :v-adjust -0.2))
                    ((not (string-empty-p file))
                     (all-the-icons-icon-for-file file :v-adjust -0.05)))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-dir-icon (_candidate)
    "Display directory icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

  (defun ivy-rich-function-icon (_candidate)
    "Display function icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "cube" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-purple)))

  (defun ivy-rich-variable-icon (_candidate)
    "Display the variable icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face 'all-the-icons-lblue)))

  (defun ivy-rich-symbol-icon (_candidate)
    "Display the symbol icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

  (defun ivy-rich-theme-icon (_candidate)
    "Display the theme icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

  (defun ivy-rich-keybinding-icon (_candidate)
    "Display the keybindings icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "keyboard" :height 1.0 :v-adjust -0.2)))

  (defun ivy-rich-library-icon (_candidate)
    "Display the library icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue)))

  (defun ivy-rich-package-icon (_candidate)
    "Display the package icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)))

  (defun ivy-rich-font-icon (_candidate)
    "Display the font icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "font" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue)))

  (when (display-graphic-p)
    (defun my-ivy-rich-bookmark-type (candidate)
      (let ((filename (file-local-name (ivy-rich-bookmark-filename candidate))))
        (cond ((null filename)
               (all-the-icons-material "block" :v-adjust -0.2 :face 'warning))  ; fixed #38
              ((file-remote-p filename)
               (all-the-icons-material "wifi_tethering" :v-adjust -0.2 :face 'mode-line-buffer-id))
              ((not (file-exists-p filename))
               (all-the-icons-material "block" :v-adjust -0.2 :face 'error))
              ((file-directory-p filename)
               (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
              (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
    (advice-add #'ivy-rich-bookmark-type :override #'my-ivy-rich-bookmark-type))
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

  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          persp-switch-to-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-M-x
          (:columns
           ((ivy-rich-function-icon)
            (counsel-M-x-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((ivy-rich-function-icon)
            (counsel-describe-function-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((ivy-rich-variable-icon)
            (counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-apropos
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-info-lookup-symbol
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-descbinds
          (:columns
           ((ivy-rich-keybinding-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-file-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-dired
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-dired-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-fzf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-git
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
           :delimiter "\t")
          counsel-buffer-or-recentf
          (:columns
           ((ivy-rich-file-icon)
            (counsel-buffer-or-recentf-transformer (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
           :delimiter "\t")
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (ivy-rich-bookmark-name (:width 40))
            (ivy-rich-bookmark-info))
           :delimiter "\t")
          counsel-package
          (:columns
           ((ivy-rich-package-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-fonts
          (:columns
           ((ivy-rich-font-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-major
          (:columns
           ((ivy-rich-function-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-find-library
          (:columns
           ((ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-load-library
          (:columns
           ((ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-load-theme
          (:columns
           ((ivy-rich-theme-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-projectile-switch-project
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-file-transformer))
           :delimiter "\t")
          counsel-projectile-find-dir
          (:columns
           ((ivy-rich-dir-icon)
            (counsel-projectile-find-dir-transformer))
           :delimiter "\t")
          treemacs-projectile
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t"))))

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
