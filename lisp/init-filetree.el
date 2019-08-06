;;; init-filetree --- a tree layout file explorer for Emacs,such as treemacs or neotree. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(use-package neotree
  :ensure t
  :commands (neotree-change-root
             neotree-quick-look
             neotree-toggle
             neotree-hide
             neotree-enter)
  :pretty-hydra
  ((:title (pretty-hydra-title "neotree Management" 'faicon "tree")
    :foreign-keys warn :quit-key "q")
   ("Navigation"
     (("L" neotree-select-next-sibling-node "next sibling")
     ("H" neotree-select-previous-sibling-node "previous sibling")
     ("J" neotree-select-down-node "goto child")
     ("K" neotree-select-up-node "goto parent")
     ("l" neotree-enter "open/expand")
     ("h" spacemacs/neotree-collapse-or-up "up/collapse")
     ("j" neotree-next-line "line down")
     ("k" neotree-previous-line "line up")
     ("'" neotree-quick-look "quick look")
     ("RET" neotree-enter "open"))
    "Actions"
     (("c" neotree-create-node "create")
     ("C" neotree-copy-node "copy")
     ("d" neotree-delete-node "delete")
     ("r" neotree-rename-node "rename")
     ("R" neotree-change-root "change root"))
    "Visual actions/config"
     (("TAB" neotree-stretch-toggle "shrink/enlarge")
     ("|" neotree-enter-vertical-split "vertical split")
     ("-" neotree-enter-horizontal-split "horizontal split")
     ("gr" neotree-refresh "refresh")
     ("s" neotree-hidden-file-toggle "hidden:^^^ %s(if neo-buffer--show-hidden-file-p \"on\" \"off\")"))))
  :init
  (shadow/define-leader-keys "ft" 'neotree-toggle)
  (setq neo-create-file-auto-open t
        neo-auto-indent-point nil
        ;; neo-autorefresh t
        neo-smart-open t
        neo-mode-line-type 'none
        neo-window-width 28
        neo-show-updir-line nil
        ;; neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files nil
        neo-keymap-style 'concise
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(DS_store\\|git\\|gitignore\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
          ;; generated files, caches or local pkgs
          "^\\(node_modules\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(sync\\|export\\|attach\\)$"
          "~$" "\\.emacs*"
          ;; ignore bazel file
          "^bazel-*"
          "^#.*#$"))
  :config
  (when (bound-and-true-p evil-mode)
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-select-up-node)
    (evil-define-key 'normal neotree-mode-map (kbd "l") 'neotree-change-root)
    (evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-create-node)
    (evil-define-key 'normal neotree-mode-map (kbd "C") 'neotree-copy-node)
    (evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-delete-node)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-rename-node)
    (evil-define-key 'normal neotree-mode-map (kbd "s") 'neotree-hidden-file-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "?") 'neotree-hydra/body)))

(provide 'init-filetree)
;;; init-filetree ends here
