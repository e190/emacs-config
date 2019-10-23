;; init-highlight.el --- Initialize highlighting configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode)
  :config
  ;; (set-face-attribute hl-line-face nil :underline "red")
  (custom-set-faces '(hl-line ((t (:background "grey13"))))))

;; Beacon flashes the cursor whenever you adjust position.
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :hook (after-init . beacon-mode)
  :init
  (setq beacon-color "red")
  (setq beacon-size 80))

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
;; "i" -> symbol-overlay-put
;; "n" -> symbol-overlay-jump-next
;; "p" -> symbol-overlay-jump-prev
;; "w" -> symbol-overlay-save-symbol
;; "t" -> symbol-overlay-toggle-in-scope
;; "e" -> symbol-overlay-echo-mark
;; "d" -> symbol-overlay-jump-to-definition
;; "s" -> symbol-overlay-isearch-literally
;; "q" -> symbol-overlay-query-replace
;; "r" -> symbol-overlay-rename
(use-package symbol-overlay
  :diminish
  :functions (turn-off-symbol-overlay
              turn-on-symbol-overlay)

  :custom-face
  (symbol-overlay-default-face ((t (:inherit (region bold)))))
  (symbol-overlay-face-1 ((t (:inherit (highlight bold)))))
  (symbol-overlay-face-2 ((t (:inherit (font-lock-builtin-face bold) :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit (warning bold) :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit (font-lock-constant-face bold) :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit (error bold) :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit (dired-mark bold) :inverse-video t))))
  (symbol-overlay-face-7 ((t (:inherit (success bold) :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit (dired-symlink bold) :inverse-video t))))
  :bind
  (("M-n" . symbol-overlay-jump-next)
  ("M-p" . symbol-overlay-jump-prev)
  ([M-f3] . symbol-overlay-remove-all)
  (:map shadow-leader-map
   ("hp" . symbol-overlay-put)
   ("hc" . symbol-overlay-remove-all)))
  :hook ((prog-mode . symbol-overlay-mode)
        (evil-visual-beginning . turn-off-symbol-overlay)
        (evil-normal-state-entry . turn-on-symbol-overlay))

  :init (setq symbol-overlay-idle-time 0.01)
  :config
    ;; Disable symbol highlighting while selecting
    (defun turn-off-symbol-overlay (&rest _)
      "Turn off symbol highlighting."
      (interactive)
      (symbol-overlay-mode -1))
    (advice-add #'set-mark :after #'turn-off-symbol-overlay)

    (defun turn-on-symbol-overlay (&rest _)
      "Turn on symbol highlighting."
      (interactive)
      (when (derived-mode-p 'prog-mode)
        (symbol-overlay-mode 1)))
    (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))

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
;;; init-highlight.el ends here
