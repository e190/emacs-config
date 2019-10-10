;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Numbered window shortcuts
(use-package winum
  :defer t
  :ensure t
  :init
  (setq window-numbering-scope 'global)
  (setq winum-auto-setup-mode-line nil)
  (setq winum-ignored-buffers '(" *which-key*"))
  (setq winum-auto-assign-0-to-minibuffer t)
  (shadow/define-leader-keys
   "w TAB"  'shadow/alternate-window
   "w2"  'shadow/layout-double-columns
   "w3"  'shadow/layout-triple-columns
   "wb"  'shadow/switch-to-minibuffer-window
   "wd"  'shadow/delete-window
   "wt"  'shadow/toggle-current-window-dedication
   "wf"  'follow-mode
   "wF"  'make-frame
   "wH"  'evil-window-move-far-left
   "wh"  'evil-window-left
   "wJ"  'evil-window-move-very-bottom
   "wj"  'evil-window-down
   "wK"  'evil-window-move-very-top
   "wk"  'evil-window-up
   "wL"  'evil-window-move-far-right
   "wl"  'evil-window-right
   "wm"  'shadow/toggle-maximize-buffer
   "wr"  'shadow/rotate-windows-forward
   "wR"  'shadow/rotate-windows-backward
   "ws"  'split-window-below
   "w-"  'shadow/split-window-below-and-focus
   "wU"  'winner-redo
   "wu"  'winner-undo
   "wv"  'split-window-right
   "w/"  'shadow/split-window-right-and-focus
   "w="  'balance-windows
   "w+"  'shadow/window-layout-toggle
   "w_"  'shadow/maximize-horizontally)
  (winum-mode))

;; Zoom window like tmux
(use-package zoom-window
  :defer t
  :ensure t
  :bind ("C-x C-z" . zoom-window-zoom)
  :init (setq zoom-window-mode-line-color "DarkGreen"))

(use-package centered-window
  :defer t
  :ensure t
  :init
  (setq cwm-use-vertical-padding t)
  (setq cwm-frame-internal-border 15)
  (setq cwm-incremental-padding t)
  (setq cwm-left-fringe-ratio 0)
  (shadow/define-leader-keys "wc" #'centered-window-mode))

;; resize window
(use-package resize-window
  :defer t
  :ensure t
  :init
  (shadow/define-leader-keys "wr" #'resize-window))

;; Restore old window configurations
(use-package winner
  :defer t
  :init
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*"))
  (add-hook 'after-init-hook #'winner-mode))

;; Quickly switch windows
(use-package ace-window
  :preface
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Window Management" 'faicon "windows")
    :foreign-keys warn :quit-key "q")
   ("Actions"
    (("TAB" other-window "switch")
     ("x" ace-delete-window "delete")
     ("m" ace-delete-other-windows "maximize")
     ("s" ace-swap-window "swap")
     ("a" ace-select-window "select")
     ("f" toggle-frame-fullscreen "fullscreen"))
    "Resize"
    (("h" shrink-window-horizontally "←")
     ("j" enlarge-window "↓")
     ("k" shrink-window "↑")
     ("l" enlarge-window-horizontally "→")
     ("n" balance-windows "balance"))
    "Split"
    (("b" split-window-right "horizontally")
     ("B" split-window-horizontally-instead "horizontally instead")
     ("v" split-window-below "vertically")
     ("V" split-window-vertically-instead "vertically instead")
     ("t" toggle-window-split "toggle"))
    "Zoom"
    (("+" text-scale-increase "in")
     ("=" text-scale-increase "in")
     ("-" text-scale-decrease "out")
     ("0" (text-scale-increase 0) "reset"))
    "Appearance"
    (("F" set-frame-font "font")
     ("T" shadow-load-theme "theme"))))
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 3.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :bind (([remap other-window] . ace-window)
        (:map shadow-leader-map
        ("wo" . #'ace-window)
        ("ww" . ace-window-hydra/body)))
  :hook (emacs-startup . ace-window-display-mode)
  :config (add-to-list 'aw-dispatch-alist '(?w ace-window-hydra/body) t))

;; (use-package centered-cursor-mode
;;   :defer t
;;   :ensure t
;;   :commands (centered-cursor-mode global-centered-cursor-mode)
;;   :diminish centered-cursor-mode "⊝"
;;   :init
;;   (shadow/define-leader-keys "t-" 'centered-cursor-mode)
;;   (setq ccm-recenter-at-end-of-file t
;;         ccm-ignored-commands '(mouse-drag-region
;;                                mouse-set-point
;;                                widget-button-click
;;                                scroll-bar-toolkit-scroll
;;                                evil-mouse-drag-region))
;;   (global-centered-cursor-mode +1))

(provide 'init-window)
;;; init-window.el ends here
