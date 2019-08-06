;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(defun shadow/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun shadow/toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(defun shadow/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(defun shadow/layout-double-columns ()
  " Set the layout to double columns. "
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun shadow/layout-triple-columns ()
  " Set the layout to triple columns. "
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

;; https://tsdh.wordpress.com/2007/03/28/deleting-windows-vertically-or-horizontally/
(defun shadow/maximize-horizontally ()
  "Delete all windows left or right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun shadow/rotate-windows-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (abn/rotate-windows-forward (* -1 count)))

;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun shadow/rotate-windows-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))

(defun shadow/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun shadow/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

;; from @bmag
(defun shadow/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

;;;###autoload
(defun shadow/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

;;;###autoload
(defun shadow/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))
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
     ("T" centaur-load-theme "theme"))))
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
