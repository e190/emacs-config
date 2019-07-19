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

;; Highlight show trailing whitespace
(use-package whitespace
  :ensure nil
  :defer t
  :diminish whitespace-mode
  :hook (after-init . whitespace-mode)
  :init
  (add-hook 'minibuffer-setup-hook (lambda () (setq show-trailing-whitespace nil)))
  (add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (setq-default show-trailing-whitespace t)
  (setq whitespace-style '(face trailing))
  :config
  (with-eval-after-load 'popup
    ;; advice for whitespace-mode conflict with popup
    (defvar my-prev-whitespace-mode nil)
    (make-local-variable 'my-prev-whitespace-mode)
    (defadvice popup-draw (before my-turn-off-whitespace activate compile)
	  "Turn off whitespace mode before showing autocomplete box."
	  (if whitespace-mode
		  (progn
            (setq my-prev-whitespace-mode t)
            (whitespace-mode -1))
        (setq my-prev-whitespace-mode nil)))
    (defadvice popup-delete (after my-restore-whitespace activate compile)
	  "Restore previous whitespace mode when deleting autocomplete box."
	  (if my-prev-whitespace-mode
		  (whitespace-mode 1)))))
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
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all)
         ([M-f3] . symbol-overlay-remove-all))
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

(provide 'init-highlight)
;;; config-highlight.el ends here
