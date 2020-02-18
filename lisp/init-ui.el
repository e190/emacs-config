;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

;; Start a clean slate.
(blink-cursor-mode -1)
(menu-bar-mode -1)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Title
;; Sets a more useful frame title, showing either a file or a buffer
;; name (if the buffer isn't visiting a file).
(setq frame-title-format
      '("" invocation-name " - "
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

;; (setq frame-title-format '("Shadow Emacs - %b")
;;       icon-title-format frame-title-format)

(when sys/mac-x-p
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;; Control over modes displayed in the modeline.
(use-package diminish)

;; Icons
;; NOTE: Must run `M-x all-the-icons-install-fonts' manually on Windows
(use-package all-the-icons
  :if (display-graphic-p)
  :defer t
  :init
  (unless (or sys/win32p (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t))
  :config
  ;; FIXME: Align the directory icons
  ;; @see https://github.com/domtronn/all-the-icons.el/pull/173
  (defun all-the-icons-icon-for-dir (dir &optional chevron padding)
    "Format an icon for DIR with CHEVRON similar to tree based directories."
    (let* ((matcher (all-the-icons-match-to-alist (file-name-base (directory-file-name dir)) all-the-icons-dir-icon-alist))
           (path (expand-file-name dir))
           (chevron (if chevron (all-the-icons-octicon (format "chevron-%s" chevron) :height 0.8 :v-adjust -0.1) ""))
           (padding (or padding "\t"))
           (icon (cond
                  ((file-symlink-p path)
                   (all-the-icons-octicon "file-symlink-directory" :height 1.0 :v-adjust 0.0))
                  ((all-the-icons-dir-is-submodule path)
                   (all-the-icons-octicon "file-submodule" :height 1.0 :v-adjust 0.0))
                  ((file-exists-p (format "%s/.git" path))
                   (format "%s" (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.0)))
                  (t (apply (car matcher) (list (cadr matcher) :v-adjust 0.0))))))
      (format "%s%s%s%s%s" padding chevron padding icon padding)))

  (defun all-the-icons-reset ()
    "Reset (unmemoize/memoize) the icons."
    (interactive)
    (dolist (f '(all-the-icons-icon-for-file
                 all-the-icons-icon-for-mode
                 all-the-icons-icon-for-url
                 all-the-icons-icon-family-for-file
                 all-the-icons-icon-family-for-mode
                 all-the-icons-icon-family))
      (ignore-errors
        (memoize-restore f)
        (memoize f)))
    (message "Reset all-the-icons"))

  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-playlist-mode all-the-icons-material "playlist_play" :height 1.2 :v-adjust -0.2 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-library-mode all-the-icons-material "library_music" :height 1.1 :v-adjust -0.2 :face 'all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-group-mode all-the-icons-fileicon "gnu" :face 'all-the-icons-silver))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-summary-mode all-the-icons-octicon "inbox" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-article-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(message-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.rss$" all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-search-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-show-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-list-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-item-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue)))

;; Show native line numbers if possible, otherwise use linum
(if (fboundp 'display-line-numbers-mode)
  ;; config built-in "display-line-numbers-mode" (require Emacs >= 26)
  (use-package display-line-numbers
    :ensure nil
    :hook ((prog-mode text-mode) . display-line-numbers-mode)
    :init
    (setq-default display-line-numbers-width 2)
    ;; (setq-default display-line-numbers-type 'relative)
    (setq display-line-numbers-current-absolute t)
    (shadow/define-leader-keys "tn" 'display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :config
    (setq linum-format "%4d ")

    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :hook (global-linum-mode . hlinum-activate)
      :custom-face (linum-highlight-face
                    ((t `(
                          :inherit default
                          :background nil
                          :foreground nil
                          ))))
      :init
      (setq linum-highlight-in-all-buffersp t))))

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)

(use-package imenu-list
  :defer t
  :ensure t
  :init
  (shadow/define-leader-keys "tl" 'imenu-list-smart-toggle)
  :config
  (setq imenu-list-size     0.2)
  (setq imenu-list-position 'right)
  (setq imenu-list-focus-after-activation t))

(use-package imenu-anywhere
  :defer t
  :config
  (setq imenu-anywhere-delimiter ": "))

;; https://emacs-china.org/t/imenu-list-tagbar/7341
(use-package maple-imenu
  :ensure nil
  :load-path "site-lisp/emacs-maple-imenu"
  :commands (maple-imenu)
  :init
  (shadow/define-leader-keys "ti" 'maple-imenu)
  :config
  (setq maple-imenu-autoresize t)
  ;; (setq maple-imenu-display-alist '((side . left) (slot . -1)))
  (setq maple-imenu-display-alist '((side . right) (slot . -1)))
  (defun maple-sidebar()
    (interactive)
    (maple-imenu)
    (neotree-toggle))
  (with-eval-after-load 'evil
    (evil-define-key 'normal maple-imenu-mode-map (kbd "q") 'quit-window))
  (add-hook 'maple-imenu-mode-hook
            (lambda() (setq-local maple-modeline-style 'sidebar))))

;; Display Time
(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :hook (after-init . display-time-mode)
  :init
  (setq display-time-format "%Y-%m-%d %H:%M")
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t))

(use-package posframe
  :defer t)

(use-package awesome-tab
  :ensure nil
  :load-path "site-lisp/awesome-tab"
  :hook (after-init . awesome-tab-mode)
  :bind
  (:map shadow-leader-map
    ("tt" . awesome-tab-switch-group)
    ("ta" . awesome-tab-select-beg-tab)
    ("te" . awesome-tab-select-end-tab)
    ("t<" . awesome-tab-move-current-tab-to-left)
    ("t>" . awesome-tab-move-current-tab-to-right)
    ("tf" . awesome-tab-forward)
    ("tb" . awesome-tab-backward))
  :config
  (setq awesome-tab-cycle-scope 'tabs) ; Navigate through visible tabs only.
  (setq awesome-tab-style 'alternate)
  (setq awesome-tab-face-height 100))

;; (use-package sublimity
;;   ;; :hook (after-init . sublimity-mode)
;;   :config
;;   (require 'sublimity-scroll)
;;   (require 'sublimity-map)
;;   (require 'sublimity-attractive)
;;   (setq sublimity-scroll-weight       4
;;         sublimity-scroll-drift-length 3)
;;   (setq sublimity-attractive-centering-width 100)
;;   (sublimity-attractive-hide-bars)
;;   (sublimity-mode 1)

  ;; (setq sublimity-map-size 20)
  ;; (setq sublimity-map-fraction 0.3)
  ;; (setq sublimity-map-text-scale -7)
  ;; (add-hook 'sublimity-map-setup-hook
  ;;         (lambda ()
  ;;           (setq buffer-face-mode-face '(:family "Monospace"))
  ;;           (buffer-face-mode)))
;; )

(provide 'init-ui)
;;; init-ui.el ends here
