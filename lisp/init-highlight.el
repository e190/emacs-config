;;; config-highlight.el --- Config for highlight

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package funcs-highlight
  :ensure nil ; local package
  :commands (shadow/highlight-symbol))

(use-package auto-highlight-symbol
  :defer t
  :bind
  (:map shadow-leader-map
   ("sh" . shadow/highlight-symbol)))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

;; Beacon flashes the cursor whenever you adjust position.
(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode t)
  (setq beacon-color "red")
  (setq beacon-size 80)
  (add-to-list 'beacon-dont-blink-major-modes 'eshell-mode))


(provide 'init-highlight)
;;; config-highlight.el ends here
