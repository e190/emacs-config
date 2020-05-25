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

   ;; register/ring
   ;; ("ry" . counsel-yank-pop)
  :map counsel-mode-map
  ([remap swiper] . counsel-grep-or-swiper)
  ([remap swiper-backward] . counsel-grep-or-swiper-backward)
  ([remap cd] . counsel-cd)
  ([remap dired] . counsel-dired)
  ([remap set-variable] . counsel-set-variable)
  ([remap recentf-open-files] . counsel-recentf)
  ("C-x C-r" . counsel-recentf)
  ("C-x j" . counsel-mark-ring)
  ("C-c c e" . counsel-colors-emacs)
  ("C-c c u" . counsel-unicode-char)
  ("C-c i" . counsel-git)
  ("C-c j" . counsel-git-grep)
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
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil)

  ;; Better performance on Windows
  (when sys/win32p
    (setq ivy-dynamic-exhibit-delay-ms 200))

  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  ;; ensure recentf-list loaded on startup
  ;; (with-eval-after-load 'counsel (recentf-mode))
  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")
  ;; Use faster search tools: ripgrep or the silver search
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")
    (when (and sys/macp (executable-find "gls"))
      (setq counsel-find-file-occur-use-find nil
            counsel-find-file-occur-cmd
            "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first")))

  (when (executable-find "fd")
    (defun cm/counsel-locate-cmd-fd (input)
      (format "fd --color never --hidden  -- \"%s\" /"
              (counsel--elisp-to-pcre
               (ivy--regex input t))))
    (setq counsel-locate-cmd 'cm/counsel-locate-cmd-fd))
  :config
  ;; Display an arrow with the selected item
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
  ;; (setf (alist-get 't ivy-format-functions-alist) #'my-ivy-format-function-arrow)

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
  (defvar-local my-ivy-fly--travel nil)

  (defun my-ivy-fly-back-to-present ()
    (cond ((and (memq last-command my-ivy-fly-commands)
                (equal (this-command-keys-vector) (kbd "M-p")))
            ;; repeat one time to get straight to the first history item
            (setq unread-command-events
                  (append unread-command-events
                          (listify-key-sequence (kbd "M-p")))))
          ((or (memq this-command '(self-insert-command
                                    ivy-forward-char end-of-line mwim-end-of-line
                                    mwim-end-of-code-or-line mwim-end-of-line-or-code
                                    yank ivy-yank-word counsel-yank-pop))
                (equal (this-command-keys-vector) (kbd "M-n")))
            (unless my-ivy-fly--travel
              (delete-region (point) (point-max))
              (when (memq this-command '(ivy-forward-char
                                        end-of-line mwim-end-of-line
                                        mwim-end-of-code-or-line
                                        mwim-end-of-line-or-code ))
                (insert (ivy-cleanup-string ivy-text)))
              (setq my-ivy-fly--travel t)))))

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
  ;;
  ;; Improve search experience of `swiper' and `counsel'
  ;;
  (defun my-ivy-switch-to-swiper (&rest _)
    "Switch to `swiper' with the current input."
    (swiper ivy-text))

  (defun my-ivy-switch-to-swiper-isearch (&rest _)
    "Switch to `swiper-isearch' with the current input."
    (swiper-isearch ivy-text))

  (defun my-ivy-switch-to-swiper-all (&rest _)
    "Switch to `swiper-all' with the current input."
    (swiper-all ivy-text))

  (defun my-ivy-switch-to-rg-dwim (&rest _)
    "Switch to `rg-dwim' with the current input."
    (rg-dwim default-directory))

  (defun my-ivy-switch-to-counsel-rg (&rest _)
    "Switch to `counsel-rg' with the current input."
    (counsel-rg ivy-text default-directory))

  (defun my-ivy-switch-to-counsel-git-grep (&rest _)
    "Switch to `counsel-git-grep' with the current input."
    (counsel-git-grep ivy-text default-directory))

  (defun my-ivy-switch-to-counsel-find-file (&rest _)
    "Switch to `counsel-find-file' with the current input."
    (counsel-find-file ivy-text))

  (defun my-ivy-switch-to-counsel-fzf (&rest _)
    "Switch to `counsel-fzf' with the current input."
    (counsel-fzf ivy-text default-directory))

  (defun my-ivy-switch-to-counsel-git (&rest _)
    "Switch to `counsel-git' with the current input."
    (counsel-git ivy-text))

  ;; Improve search experience of `swiper'
  ;; @see https://emacs-china.org/t/swiper-swiper-isearch/9007/12
  (defun my-swiper-toggle-counsel-rg ()
    "Toggle `counsel-rg' with current swiper input."
    (interactive)
    (ivy-quit-and-run
      (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
          (counsel-rg ivy-text default-directory)
        (swiper-isearch ivy-text))))
  (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg swiper-map)
  (bind-key "<C-return>" #'my-swiper-toggle-counsel-rg counsel-ag-map)

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

  (defun my-swiper-toggle-swiper-isearch ()
    "Toggle `swiper' and `swiper-isearch' with the current input."
    (interactive)
    (ivy-quit-and-run
      (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
          (swiper ivy-text)
        (swiper-isearch ivy-text))))
  (bind-key "<s-return>" #'my-swiper-toggle-swiper-isearch swiper-map)

  (defun my-counsel-find-file-toggle-fzf ()
    "Toggle `counsel-fzf' with the current `counsel-find-file' input."
    (interactive)
    (ivy-quit-and-run
      (counsel-fzf (or ivy-text "") default-directory)))
  (bind-key "<C-return>" #'my-counsel-find-file-toggle-fzf counsel-find-file-map)

   (defun shadow-counsel-git-fast ()
    "use ripgrep as the backed for counsel-git"
    (interactive)
    (let ((counsel-git-cmd "rg --files"))
      (counsel-git)))

  (defun shadow-counsel-rg-current-dir ()
    "run `counsel-rg' in current direcotry"
    (interactive)
    (counsel-rg nil default-directory))

  ;; More actions
  (ivy-add-actions
    'swiper-isearch
    '(("r" my-ivy-switch-to-counsel-rg "rg")
      ("d" my-ivy-switch-to-rg-dwim "rg dwim")
      ("s" my-ivy-switch-to-swiper "swiper")
      ("a" my-ivy-switch-to-swiper-all "swiper all")))

  (ivy-add-actions
    'swiper
    '(("r" my-ivy-switch-to-counsel-rg "rg")
      ("d" my-ivy-switch-to-rg-dwim "rg dwim")
      ("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
      ("a" my-ivy-switch-to-swiper-all "swiper all")))

  (ivy-add-actions
    'swiper-all
    '(("g" my-ivy-switch-to-counsel-git-grep "git grep")
      ("r" my-ivy-switch-to-counsel-rg "rg")
      ("d" my-ivy-switch-to-rg-dwim "rg dwim")
      ("s" my-swiper-toggle-swiper-isearch "swiper isearch")
      ("S" my-ivy-switch-to-swiper "swiper")))

  (ivy-add-actions
    'counsel-rg
    '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
      ("S" my-ivy-switch-to-swiper "swiper")
      ("a" my-ivy-switch-to-swiper-all "swiper all")
      ("d" my-ivy-switch-to-rg-dwim "rg dwim")))

  (ivy-add-actions
    'counsel-git-grep
    '(("s" my-ivy-switch-to-swiper-isearch "swiper isearch")
      ("S" my-ivy-switch-to-swiper "swiper")
      ("r" my-ivy-switch-to-rg-dwim "rg")
      ("d" my-ivy-switch-to-rg-dwim "rg dwim")
      ("a" my-ivy-switch-to-swiper-all "swiper all")))

    (ivy-add-actions
     'counsel-find-file
     '(("g" my-ivy-switch-to-counsel-git "git")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     'counsel-git
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("z" my-ivy-switch-to-counsel-fzf "fzf")))

    (ivy-add-actions
     'counsel-fzf
     '(("f" my-ivy-switch-to-counsel-find-file "find file")
       ("g" my-ivy-switch-to-counsel-git "git")))

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
    :commands ivy-hydra-read-action
    :init (setq ivy-read-action-function #'ivy-hydra-read-action))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :hook (counsel-mode . counsel-projectile-mode)
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

  ;; Enchanced M-x
  ;; https://github.com/DarwinAwardWinner/amx
  (use-package amx
    :hook (after-init . amx-mode)
    :init
    (setq amx-save-file (concat shadow-cache-dir "/amx-items")))

   ;; Better sorting and filtering
  (use-package prescient
    :defer t
    :commands prescient-persist-mode
    :init
    (setq prescient-filter-method '(literal regexp initialism fuzzy))
    (prescient-persist-mode 1))

  (use-package ivy-prescient
    :commands ivy-prescient-re-builder
    :custom-face
    (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
    :hook (after-init . ivy-prescient-mode)
    :init
    (defun ivy-prescient-non-fuzzy (str)
      "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
      (let ((prescient-filter-method '(literal regexp)))
        (ivy-prescient-re-builder str)))

    (setq ivy-prescient-retain-classic-highlighting t
          ivy-re-builders-alist
          '((counsel-ag . ivy-prescient-non-fuzzy)
            (counsel-rg . ivy-prescient-non-fuzzy)
            (counsel-pt . ivy-prescient-non-fuzzy)
            (counsel-grep . ivy-prescient-non-fuzzy)
            (counsel-imenu . ivy-prescient-non-fuzzy)
            (counsel-yank-pop . ivy-prescient-non-fuzzy)
            (swiper . ivy-prescient-non-fuzzy)
            (swiper-isearch . ivy-prescient-non-fuzzy)
            (swiper-all . ivy-prescient-non-fuzzy)
            (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
            (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
            (insert-char . ivy-prescient-non-fuzzy)
            (counsel-unicode-char . ivy-prescient-non-fuzzy)
            (t . ivy-prescient-re-builder))
          ivy-prescient-sort-commands
          '(:not swiper swiper-isearch ivy-switch-buffer
            counsel-grep counsel-git-grep counsel-ag counsel-imenu
            counsel-yank-pop counsel-recentf counsel-buffer-or-recentf)))
)

;; Better experience with icons
;; Enable it before`ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :if (icons-displayable-p)
  :defer t
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :init (setq all-the-icons-ivy-rich-icon-size 0.85))

;; More friendly display transformer for Ivy
(use-package ivy-rich
  :hook (;; Must load after `counsel-projectile'
         (counsel-projectile-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))

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
  (defun my-counsel-imenu ()
    "Jump to a buffer position indexed by imenu."
    (interactive)
    (let* ((cands (counsel--imenu-candidates))
          (pre-selected (thing-at-point 'symbol))
          (pos (point))
          closest)
      (dolist (c cands)
        (let* ((item (cdr c))
              (m (cdr item)))
          (when (and m (<= (marker-position m) pos))
            (cond
            ((not closest)
              (setq closest item))
            ((< (- pos (marker-position m))
                (- pos (marker-position (cdr closest))))
              (setq closest item))))))
      (if closest (setq pre-selected (car closest)))
      (ivy-read "imenu items: " cands
                :preselect pre-selected
                :require-match t
                :action #'counsel-imenu-action
                :keymap counsel-imenu-map
                :history 'counsel-imenu-history
                :caller 'counsel-imenu)))

  (defun my-use-tags-as-imenu-function-p ()
    "Can use tags file to build imenu function"
    (and (locate-dominating-file default-directory "TAGS")
        ;; ctags needs extra setup to extract typescript tags
        (file-exists-p counsel-etags-ctags-options-file)
        (memq major-mode '(cc-mode
                            js-mode))))

  (defun my-imenu-or-list-tag-in-current-file ()
    "Combine the power of counsel-etags and imenu."
    (interactive)
    (cond
    ((my-use-tags-as-imenu-function-p)
      (let* ((imenu-create-index-function 'counsel-etags-imenu-default-create-index-function))
        (my-counsel-imenu)))
    (t
      (my-counsel-imenu))))
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
