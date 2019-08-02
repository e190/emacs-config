;;; config-highlight.el --- Config for highlight

;;; Commentary:
;;

;;; Code:

;; Beacon flashes the cursor whenever you adjust position.
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init
  (setq beacon-color "red")
  (setq beacon-size 80)
  (beacon-mode t))
;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Visualize TAB, (HARD) SPACE, NEWLINE
(setq-default show-trailing-whitespace nil)
(dolist (hook '(prog-mode-hook outline-mode-hook conf-mode-hook))
  (add-hook hook (lambda ()
                   (setq show-trailing-whitespace t)
                   (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))))
;; Highlight symbols
(use-package symbol-overlay
  :diminish
  :custom-face
  (symbol-overlay-default-face ((t (:inherit 'region))))
  (symbol-overlay-face-1 ((t (:inherit 'highlight))))
  (symbol-overlay-face-2 ((t (:inherit 'font-lock-builtin-face :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit 'warning :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit 'font-lock-constant-face :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit 'error :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit 'dired-mark :inverse-video t :bold nil))))
  (symbol-overlay-face-7 ((t (:inherit 'success :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit 'dired-symlink :inverse-video t :bold nil))))
  :bind
  (("M-n" . symbol-overlay-jump-next)
  ("M-p" . symbol-overlay-jump-prev)
  ([M-f3] . symbol-overlay-remove-all)
  (:map shadow-leader-map
   ("hp" . symbol-overlay-put)
   ("hc" . symbol-overlay-remove-all)))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . (lambda () (symbol-overlay-mode -1)))
         (iedit-mode-end . symbol-overlay-mode))
  :init (setq symbol-overlay-idle-time 0.01))

;; Colorize color names in buffers
(use-package rainbow-mode
  :defer t
  :ensure t
  :diminish rainbow-mode
  :hook ((text-mode . rainbow-mode)
         (prog-mode . rainbow-mode)))
;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :bind (:map hl-todo-mode-map
         ([C-f3] . hl-todo-occur)
         ("C-c t p" . hl-todo-previous)
         ("C-c t n" . hl-todo-next)
         ("C-c t o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

(provide 'init-highlight)
;;; config-highlight.el ends here
