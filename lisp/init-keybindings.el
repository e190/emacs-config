;; init-keybindings.el --- Initialize key bindings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

(use-package bind-key)

;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init
  ;; Minibuffer feels much faster than using windows.
  (setq which-key-popup-type 'minibuffer)
  :config
  (setq which-key-idle-delay 0.3
        which-key-compute-remaps t
        which-key-min-display-lines 1
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-sort-uppercase-first nil
        which-key-side-window-max-width 0.33
        which-key-side-window-max-height 0.25
        which-key-sort-order #'which-key-prefix-then-key-order)
  (which-key-setup-side-window-bottom)
  (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
  (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
  (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
  (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("␣" . nil)))
  ;; rename winum-select-window-1 entry to 1..9
  (add-to-list 'which-key-replacement-alist '(("\\(.*\\)1" . "winum-select-window-1") . ("\\11..9" . "window 1..9")))
  ;; hide winum-select-window-[2-9] entries
  (add-to-list 'which-key-replacement-alist '((nil . "winum-select-window-[2-9]") . t))
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold))

;; I always hit this by mistake to get to `describe-char' and I'm tired of
;; seeing the GNU license.
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)

;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; Auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Makes <escape> quit as much as possible.
(define-key minibuffer-local-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map
  (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer shadow/space-key-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer shadow/comma-key-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix ",")

  (general-create-definer shadow/colon-key-define
    :states '(normal visual motion evilified)
    :keymaps 'override
    :prefix ";"))

(defun shadow/init-default-keybinds ()
  (shadow/space-key-define
    "?"  'counsel-descbinds
    "1"  'winum-select-window-1
    "2"  'winum-select-window-2
    "3"  'winum-select-window-3
    "4"  'winum-select-window-4
    "5"  'winum-select-window-5
    "6"  'winum-select-window-6
    "7"  'winum-select-window-7
    "8"  'winum-select-window-8
    "9"  'winum-select-window-9
    "SPC" 'amx
    "b" '(:ignore t :wk "Buffer")
    "b b" 'switch-to-buffer
    "b c" '(shadow/new-empty-buffer :wk "new-buffer")
    "b e" 'eval-buffer  ;; read-only-mode
    ;; "b e" 'shadow/safe-erase-buffer
    "b d" 'kill-this-buffer
    "b D" '(shadow/kill-other-buffers :wk "kill-other")
    "b i" '(shadow/indent-region-or-buffer :wk "indent-buffer")
    ;; "b i" 'ibuffer
    "b k" 'kill-buffer
    "b l" 'ibuffer-list-buffers
    "b m" '(shadow/kill-all-buffers :wk "kill-all-buffer")
    "b p" '(shadow/switch-to-prev-buffer :wk "prev-buffer")
    "b n" '(shadow/switch-to-next-buffer :wk "next-buffer")
    "b r" 'reveal-in-osx-finder
    "b g" '(shadow/revert-buffer-no-confirm :wk "revert-buffer")
    "b s" 'save-buffer
    "b S" '(shadow/create-scratch-buffer :wk "create-scratch-buffer")
    "TAB" 'shadow/alternate-buffer
    "b P" 'shadow/copy-clipboard-to-whole-buffer
    "b R" 'shadow/safe-revert-buffer
    "b Y" 'shadow/copy-whole-buffer-to-clipboard
    "c" '(nil :which-key "Comment")
    "c i" '(evilnc-comment-or-uncomment-lines :wk "comment-lines")
    "c l" '(evilnc-quick-comment-or-uncomment-to-the-line :wk "comment-line")
    "c p" '(evilnc-comment-or-uncomment-paragraphs :wk "comment paragraphs")
    "c y" '(evilnc-copy-and-comment-operator :wk "comment-and-copy")
    "c w" 'thing-copy-word
    "c s" 'thing-copy-symbol
    "c x" 'thing-copy-sexp
    "c a" 'thing-copy-to-line-beginning
    "c e" 'thing-copy-to-line-end
    "r w" 'thing-replace-word
    "r s" 'thing-replace-symbol
    "r x" 'thing-replace-sexp
    "d" '(nil :which-key "Delete")
    "d d" '(kevin/delete-delimiter-enclosed-text :wk "delete-enclosed-text")
    "d f" 'delete-frame
    "d w" '(kevin/delete-word :wk "delete-word")
    "e" '(nil :which-key "Errors")
    "e b" 'flycheck-hydra/body
    "e l" 'flycheck-list-errors
    "e n" 'flycheck-next-error
    "e p" 'flycheck-previous-error
    "f" '(nil :which-key "File")
    "f r" 'recentf-open-files
    "f f" 'find-file
    "f e" 'rename-this-file
    "f l" 'counsel-find-library
    "f L" 'counsel-locate
    "g" '(:ignore t :which-key "Git")
    "g a" '(shadow/git-add-current-file :wk "add-current-file")
    "g b" 'magit-blame
    "g c" '(shadow/git-checkout-current-file :wk "checkout-current-file")
    "g d" 'magit-diff-buffer-file
    "g i" 'magit-init
    "g l" 'magit-log-buffer-file
    "g L" 'magit-list-repositories
    "g m" '(git-messenger:popup-message :wk "popup-message")
    "g r" '(hydra-smerge-mode/body :wk "hydra-smerge-mode")
    "g s" 'magit-status
    "g S" 'magit-stage-file
    "g t" '(hydra-git-timemachine/body :wk "git-timemachine")
    "g u" 'magit-unstage-file
    "g v" 'vc-annotate
    "h" '(:ignore t :wk "Highlight & Help")
    "h a" 'shadow/toggle-invisibles
    "h f" 'helpful-callable
    "h h" 'helpful-at-point-dwim
    "h v" 'helpful-variable
    "h k" 'helpful-key
    "h w" 'shadow/toggle-whitespace
    "h p" 'symbol-overlay-put
    "h c" 'symbol-overlay-remove-all
    "j" '(nil :which-key "Jump")
    "j c" 'avy-goto-char-2
    "j d" 'dired-jump
    "j D" 'dired-jump-other-window
    "j f" 'beginning-of-defun
    "j i" 'counsel-imenu
    "j j" 'awesome-tab-ace-jump
    "j l" 'avy-goto-line
    "j m" '(shadow/jump-match-delimiter :wk "goto-match-delimiter")
    "j w" 'avy-goto-word-or-subword-1
    "m" '(nil :which-key "Book/mark")
    "m s" 'bookmark-set
    "m r" 'bookmark-rename
    "m d" 'bookmark-delete
    "m j" 'counsel-bookmark
    "m l" 'bookmark-bmenu-list
    "m w" 'er/mark-word
    "m s" 'er/mark-symbol
    "m p" 'er/mark-inside-pairs
    "o" '(nil :which-key "Open")
    "o a" 'counsel-osx-app
    "o i" '(shadow/open-init-file :wk "open-init")
    "o c" '(open-custom-file :wk "custom-file")
    "o s" 'my-org-screenshot
    "p" '(nil :which-key "Project")
    "p SPC" 'find-file-in-project-by-selected
    "p c" 'find-file-in-current-directory
    "p d" 'ffip-show-diff
    "p f" 'find-file-in-project
    "p s" 'ffip-save-ivy-last
    "p r" 'ffip-ivy-resume
    "p a" 'find-file-in-project-at-point
    "q" '(nil :which-key "Quit")
    "q r" 'restart-emacs
    "s" '(nil :which-key "Search")
    "s /" 'counsel-rg
    "s a" 'swiper-all
    ;; "s a" 'snails
    "s d" 'deadgrep
    "s k" 'deadgrep-kill-all-buffers
    "s r" 'rg-dwim
    "s i" 'shadow-custumize-rg
    "s q" 'shadow-rg-dwim-current-dir
    "s c" 'color-rg-search-input
    "s p" 'color-rg-search-input-in-project
    "s s" 'color-rg-search-symbol
    "t" '(nil :which-key "Toggle")
    "t ;" 'toggle-frame-fullscreen
    "t b" 'toggle-scroll-bar
    "t f" 'neotree-toggle
    "t i" 'maple-imenu
    "t m" 'kevin/toggle-evil-mc
    "t n" 'display-line-numbers-mode
    "t s" 'symbol-overlay-mode
    "t u" 'undo-tree-visualize
    "t w" 'delete-trailing-whitespace
    "t g" 'kevin/toggle-golden-ratio
    "t <" 'centaur-tabs-move-current-tab-to-left
    "t >" 'centaur-tabs-move-current-tab-to-right
    ;; "t c" 'ggtags-create-tags
    ;; "t u" 'ggtags-update-tags
    "w" '(nil :which-key "Window")
    "w c" 'centered-window-mode
    "w d" 'delete-window
    "w h" 'evil-window-left
    "w l" 'evil-window-right
    "w k" 'evil-window-up
    "w j" 'evil-window-down
    "w d" 'delete-window
    "w r" 'resize-window
    "w z" 'zoom-window-zoom
    "w o" 'ace-window
    "w w" 'ace-window-hydra/body
    "w /" '(shadow/split-window-right-and-focus :wk "split-window-right")
    "w -" '(shadow/split-window-below-and-focus :wk "split-window-below")
    "w +"  'shadow/window-layout-toggle
    "w _"  'shadow/maximize-horizontally
    "w D" 'delete-other-windows)
  (general-nmap dired-mode-map
    ;; Lower keys for commands not operating on all the marked files
    "a" 'dired-find-alternate-file
    "d" 'dired-flag-file-deletion
    "h" 'dired-up-directory
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "l" 'dired-find-file
    "q" 'shadow/kill-this-buffer
    "r" 'dired-do-redisplay
    "v" 'dired-git-info-mode
    "x" 'dired-do-flagged-delete
    "+" 'dired-create-directory
    "RET" 'dired-find-alternate-file)
  (general-nmap neotree-mode-map
    "RET" 'neotree-enter
    "TAB" 'neotree-stretch-toggle
    "o" 'neotree-enter
    "q" 'neotree-hide
    "h" 'neotree-select-up-node
    "l" 'neotree-change-root
    "c" 'neotree-create-node
    "C" 'neotree-copy-node
    "d" 'neotree-delete-node
    "g" 'neotree-refresh
    "r" 'neotree-rename-node
    "th" 'neotree-hidden-file-toggle))

(run-at-time "2sec" nil #'shadow/init-default-keybinds)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
