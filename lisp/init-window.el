;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Numbered window shortcuts
(use-package winum
  :hook (after-init . winum-mode)
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
   "w_"  'shadow/maximize-horizontally))

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
  ;; :defer t
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
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
                                "*esh command on file*")))
  ;; (add-hook 'after-init-hook #'winner-mode))

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

;; Enforce rules for popups
(defvar shackle--popup-window-list nil) ; all popup windows
(defvar-local shackle--current-popup-window nil) ; current popup window
(put 'shackle--current-popup-window 'permanent-local t)

(use-package shackle
  :functions org-switch-to-buffer-other-window
  :commands shackle-display-buffer
  :hook (after-init . shackle-mode)
  :config
  (eval-and-compile
    (defun shackle-last-popup-buffer ()
      "View last popup buffer."
      (interactive)
      (ignore-errors
        (display-buffer shackle-last-buffer)))
    (bind-key "C-h z" #'shackle-last-popup-buffer)

    ;; Add keyword: `autoclose'
    (defun shackle-display-buffer-hack (fn buffer alist plist)
      (let ((window (funcall fn buffer alist plist)))
        (setq shackle--current-popup-window window)

        (when (plist-get plist :autoclose)
          (push (cons window buffer) shackle--popup-window-list))
        window))

    (defun shackle-close-popup-window-hack (&rest _)
      "Close current popup window via `C-g'."
      (setq shackle--popup-window-list
            (cl-loop for (window . buffer) in shackle--popup-window-list
                     if (and (window-live-p window)
                             (equal (window-buffer window) buffer))
                     collect (cons window buffer)))
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p)))
        (let (window buffer)
          (if (one-window-p)
              (progn
                (setq window (selected-window))
                (when (equal (buffer-local-value 'shackle--current-popup-window
                                                 (window-buffer window))
                             window)
                  (winner-undo)))
            (setq window (caar shackle--popup-window-list))
            (setq buffer (cdar shackle--popup-window-list))
            (when (and (window-live-p window)
                       (equal (window-buffer window) buffer))
              (delete-window window)

              (pop shackle--popup-window-list))))))

    (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
    (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

  ;; HACK: compatibility issuw with `org-switch-to-buffer-other-window'
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

  ;; rules
  (setq shackle-default-size 0.4
        shackle-default-alignment 'below
        shackle-default-rule nil
        shackle-rules
        '(("*Help*" :select t :size 0.3 :align 'below :autoclose t)
          ("*Apropos*" :select t :size 0.3 :align 'below :autoclose t)
          ("*compilation*" :select t :size 0.3 :align 'below :autoclose t)
          ("*Compile-Log*" :select t :size 0.3 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
          ("*ert*" :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align 'below)
          ("*Warnings*" :size 0.3 :align 'below :autoclose t)
          ("*Messages*" :size 0.3 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          ("\\*ivy-occur .*\\*" :regexp t :size 0.4 :select t :align 'below)
          (" *undo-tree*" :select t)
          ("*quickrun*" :select t :size 15 :align 'below)
          ("*tldr*" :align 'below :autoclose t)
          ("*Youdao Dictionary*" :size 0.3 :align 'below :autoclose t)
          ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)
          ("*lsp-help*" :size 0.3 :align 'below :autoclose t)
          ("*lsp session*" :size 0.4 :align 'below :autoclose t)
          (" *Org todo*" :select t :size 4 :align 'below :autoclose t)
          ("*Org Dashboard*" :select t :size 0.4 :align 'below :autoclose t)
          ("*gud-debug*" :select t :size 0.4 :align 'below :autoclose t)
          ("^\\*macro expansion\\**" :regexp t :size 0.4 :align 'below)
          (" *Install vterm" :size 0.3 :align 'below)
          ("*Paradox Report*" :size 0.2 :align 'below :autoclose t)
          ("*package update results*" :size 0.2 :align 'below :autoclose t)

          ("*Gofmt Errors*" :select t :size 0.3 :align 'below :autoclose t)
          ("*Go Test*" :select t :size 0.3 :align 'below :autoclose t)

          (ag-mode :select t :align 'below)
          (grep-mode :select t :align 'below)
          (pt-mode :select t :align 'below)
          (rg-mode :select t :align 'below)

          (flycheck-error-list-mode :select t :size 0.3 :align 'below :autoclose t)
          (flymake-diagnostics-buffer-mode :select t :size 0.3 :align 'below :autoclose t)

          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (comint-mode :align 'below)
          (helpful-mode :select t :size 0.3 :align 'below :autoclose t)
          (process-menu-mode :select t :size 0.3 :align 'below :autoclose t)
          (cargo-process-mode :select t :size 0.3 :align 'below :autoclose t)
          (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
          (profiler-report-mode :select t :size 0.5 :align 'below)
          (tabulated-list-mode :align 'below))))

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
