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
  :general
  (general-nmap neotree-mode-map
    ;; Lower keys for commands not operating on all the marked files
    "TAB" 'neotree-stretch-toggle
    "q" 'neotree-hide
    "RET" 'neotree-enter
    "h" 'neotree-select-up-node
    "l" 'neotree-change-root
    "c" 'neotree-create-node
    "C" 'neotree-copy-node
    "d" 'neotree-delete-node
    "g" 'neotree-refresh
    "r" 'neotree-rename-node
    "s" 'neotree-hidden-file-toggle
    "?" 'neotree-hydra/body))

(provide 'init-filetree)
;;; init-filetree ends here
