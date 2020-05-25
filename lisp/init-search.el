;; init-search.el --- Initialize search.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Jump to things in Emacs tree-style
(use-package avy
  :defer t
  :ensure t
  :hook (after-init . avy-setup-default)
  :config (setq avy-background t))

;; Search tools: `wgrep', `ag' and `rg'
(use-package wgrep
  :defer t
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(use-package ag
  :defer t
  :defines projectile-command-map
  :init
  (with-eval-after-load 'projectile
    (bind-key "s S" #'ag-project projectile-command-map))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t)
  (use-package wgrep-ag))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired
    :defer t))

;;https://github.com/dajva/rg.el
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :functions (shadow-custumize-rg
              shadow-custumize-rg-dwim
              shadow-rg-dwim-current-dir)
  :config
  (setq rg-group-result t)
  (setq rg-show-columns t)

  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep 'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map))

  (when (fboundp 'ag)
    (bind-key "a" #'ag rg-global-map))

  ;; used in rg result buffer
  (rg-define-toggle "--context 3" (kbd "C-c c t"))

  (defun rg/dwim-at-point ()
    "If there's an active selection, return that.
  Otherwise, get the symbol at point, as a string."
    (cond ((use-region-p)
          (buffer-substring-no-properties (region-beginning) (region-end)))
          ((symbol-at-point)
          (substring-no-properties
            (symbol-name (symbol-at-point))))))

  (defun shadow/read-from-minibuffer ()
    "Read a value from the minibuffer with PROMPT.
  If there's a string at point, offer that as a default."
    (let* ((things (rg/dwim-at-point))
          (final-prompt
            (if things
                (format "Search string (default %s): " things)
              (format "Search string: ")))
          ;; Ask the user for input, but add `string' to the history
          ;; so they can use M-n if they want to modify it.
          (user-input (read-from-minibuffer
                        final-prompt
                        nil nil nil nil things)))
      ;; Return the input provided by the user, or use `string' if
      ;; the input was empty.
      (if (> (length user-input) 0)
          user-input
        things)))

  ;; (defun shadow-custumize-rg ()
  ;;   (interactive)
  ;;   (let* ((string (shadow/read-from-minibuffer))
  ;;          (root-dir-origin (shell-command-to-string "git rev-parse --show-toplevel"))
  ;;          (root-dir (replace-regexp-in-string "\n" "" root-dir-origin)))
  ;;     (rg string "everything" root-dir)))

  (rg-define-search shadow-custumize-rg
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((proj (projectile-project-root)))
           (if proj
               proj
             default-directory))
    :confirm prefix
    :flags ("--hidden -g !.git"))

  (defun shadow-custumize-rg-dwim ()
    (interactive)
    (deactivate-mark)
    (ring-insert find-tag-marker-ring (point-marker))
    (rg-dwim))

  ;; original `rg-dwim-current-dir' only match current kind of file. But
  ;; I need everything.
  (rg-define-search shadow-rg-dwim-current-dir
    "Search for thing at point in every files under the current
  directory."
    :query point
    :format literal
    :files "everything"
    :dir current)

  ;; Add to existing sub group
  (rg-menu-transient-insert "Manage" "K" "kill-rg" 'kill-rg)

  (add-hook 'rg-mode-hook #'(lambda ()
                              (interactive)
                              ;; (switch-to-buffer-other-window "*rg*")
                              (setq compilation-scroll-output nil)
                              (define-key rg-mode-map (kbd "g") 'evil-goto-first-line)
                              (define-key rg-mode-map (kbd "TAB") 'next-error-no-select)
                              (define-key rg-mode-map (kbd "<tab>") 'next-error-no-select)
                              (define-key rg-mode-map (kbd "<backtab>") 'previous-error-no-select))))


(use-package deadgrep
  :ensure t
  :pretty-hydra
  ((:title (pretty-hydra-title "deadgrep Management" 'faicon "search")
    :foreign-keys warn :quit-key "q")
    ("Actions"
    (("e" deadgrep-edit-mode "edit-mode" :exit t)
     ("k" deadgrep-kill-process "kill-process" :exit t)
     ("g" deadgrep-restart "deadgrep-restart" :exit t))))
  :general
  (general-nmap deadgrep-mode-map
    ;; Lower keys for commands not operating on all the marked files
    "RET" 'deadgrep-visit-result-other-window
    "q" 'quit-window
    "TAB" 'deadgrep-forward-match
    "<backtab>" 'deadgrep-backward-match
    "?" 'deadgrep-hydra/body)
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal deadgrep-edit-mode-map (kbd "C-c C-g") 'deadgrep-mode)))

;; https://github.com/manateelazycat/color-r
(use-package color-rg
  :demand t
  :ensure nil; local package
  :after counsel
  :load-path "site-lisp/color-rg"
  :pretty-hydra
  ((:title (pretty-hydra-title "color-rg Management" 'faicon "search")
    :foreign-keys warn :quit-key "q")
   ("Replace"
     (("r" color-rg-replace-all-matches "replace-all-matches" :exit t))
    "Filter"
     (("f" color-rg-filter-match-results "filter-match-results" :exit t)
     ("F" color-rg-filter-mismatch-results "filter-mismatch-results" :exit t)
     ("x" color-rg-filter-match-files "filter-match-files" :exit t)
     ("X" color-rg-filter-mismatch-files "filter-mismatch-files" :exit t)
     ("u" color-rg-unfilter "unfilter" :exit t))
    "Actions"
     (("c" color-rg-rerun-toggle-case "rerun-toggle-case" :exit t)
     ("s" color-rg-rerun-regexp "rerun-regexp" :exit t)
     ("d" color-rg-rerun-change-dir "change-dir" :exit t)
     ("z" color-rg-rerun-change-globs "change-globs" :exit t)
     ("Z" color-rg-rerun-change-exclude-files "change-exclude-files" :exit t))))
  :general
  (general-nmap color-rg-mode-map
    "j" 'evil-next-line
    "k" 'evil-previous-line
    "?" 'color-rg-hydra/body)
  :config
    ;; `color-rg' do not kill any buffer
  (setq color-rg-kill-temp-buffer-p nil)
  ;; https://emacs.stackexchange.com/a/10588/22102
  (eval-after-load 'evil
    '(progn
      (evil-make-overriding-map color-rg-mode-map 'normal)
      ;; force update evil keymaps after color-rg loaded
      (add-hook 'color-rg-mode-hook #'evil-normalize-keymaps))))

;; SnailsPac
(use-package snails
  :ensure nil; local package
  ;; :demand t
  :load-path "site-elisp/snails"
  ;; :init
  ;; (autoload 'snails "snails" nil t)
  :bind
  (("M-s s" . snails)
   ("M-s g" . snails-current-project)
   ("M-s b" . snails-fd)
   ("M-s e" . snails-everywhere))
  :custom-face
  (snails-content-buffer-face ((t (:background "#111" :height 110))))
  (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
  (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
  :config
  (add-hook 'snails-mode-hook (lambda ()
                              (snails-init-face-with-theme)
                              (evil-emacs-state)))
  ;; Functions for specific backends
  (defun snails-current-project ()
    (interactive)
    (snails '(snails-backend-projects snails-backend-projectile)))
  (defun snails-rg ()
    (interactive)
    (snails '(snails-backend-rg)))
  (defun snails-imenu ()
    (interactive)
    (snails '(snails-backend-imenu)))
  (defun snails-fd ()
    (interactive)
    (snails '(snails-backend-fd)))
  (defun snails-everywhere ()
    (interactive)
    (snails '(snails-backend-everything)))
    ;; (snails '(snails-backend-everything snails-backend-mdfind)))
  (eval-after-load 'evil
    '(progn
      (evil-make-overriding-map snails-mode-map 'normal)
      ;; force update evil keymaps after color-rg loaded
      (add-hook 'snails-mode-hook #'evil-normalize-keymaps))))
;; -SnailsPac

(provide 'init-search)
;;; init-search.el ends here
