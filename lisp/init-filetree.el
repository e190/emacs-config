;;; config-filetree --- a tree layout file explorer for Emacs,such as treemacs or neotree. -*- lexical-binding: t; -*-
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
  (defhydra hydra-neotree (:color red
                                    :hint nil)
         "
Navigation^^^^             Actions^^         Visual actions/config^^^
───────^^^^─────────────── ───────^^──────── ───────^^^────────────────
[_L_]   next sibling^^     [_c_] create      [_TAB_] shrink/enlarge
[_H_]   previous sibling^^ [_C_] copy        [_|_]   vertical split
[_J_]   goto child^^       [_d_] delete      [_-_]   horizontal split
[_K_]   goto parent^^      [_r_] rename      [_gr_]  refresh^
[_l_]   open/expand^^      [_R_] change root [_s_]   hidden:^^^ %s(if neo-buffer--show-hidden-file-p \"on\" \"off\")
[_h_]   up/collapse^^      ^^                ^^^
[_j_]   line down^^        ^^                ^^^
[_k_]   line up^^          ^^                ^^
[_'_]   quick look         ^^                ^^
[_RET_] open               ^^^^              [_?_]   close hints
"
        ("RET" neotree-enter)
        ("TAB" neotree-stretch-toggle)
        ("|" neotree-enter-vertical-split)
        ("-" neotree-enter-horizontal-split)
        ("?" nil :exit t)
        ("'" neotree-quick-look)
        ("c" neotree-create-node)
        ("C" neotree-copy-node)
        ("d" neotree-delete-node)
        ("gr" neotree-refresh)
        ("h" spacemacs/neotree-collapse-or-up)
        ("H" neotree-select-previous-sibling-node)
        ("j" neotree-next-line)
        ("J" neotree-select-down-node)
        ("k" neotree-previous-line)
        ("K" neotree-select-up-node)
        ("l" neotree-enter)
        ("L" neotree-select-next-sibling-node)
        ("r" neotree-rename-node)
        ("R" neotree-change-root)
        ("s" neotree-hidden-file-toggle))
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
    (evil-define-key 'normal neotree-mode-map (kbd "?") 'hydra-neotree/body)))

(provide 'init-filetree)
;;; config-filetree ends here
