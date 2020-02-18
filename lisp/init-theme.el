;; init-theme.el --- Initialize theme configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

(defvar shadow-theme-alist)

;; Theme
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(defun shadow--real-theme (theme)
  "Return real THEME name."
  (alist-get theme shadow-theme-alist 'doom-one))

(defun is-doom-theme-p (theme)
  "Check whether the THEME is a doom theme. THEME is a symbol."
  (string-prefix-p "doom" (symbol-name (shadow--real-theme theme))))

(defun shadow-load-theme (theme)
  "Set color THEME."
  (interactive
   (list
    (intern (completing-read "Load theme: "
                             (mapcar #'car shadow-theme-alist)))))
  (setq shadow-theme theme)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (shadow--real-theme theme) t))
(global-set-key (kbd "C-c T") #'shadow-load-theme)

(if (is-doom-theme-p shadow-theme)
    (progn
      ;; Make certain buffers grossly incandescent
      (use-package solaire-mode
        :functions persp-load-state-from-file
        :hook (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
               (minibuffer-setup . solaire-mode-in-minibuffer)
               (after-load-theme . solaire-mode-swap-bg))
        :config
        (setq solaire-mode-remap-fringe nil)
        (solaire-global-mode 1)
        (solaire-mode-swap-bg)
        (advice-add #'persp-load-state-from-file
                    :after #'solaire-mode-restore-persp-mode-buffers))
      (use-package doom-themes
        :hook (after-load-theme . (lambda ()
                                    (set-face-foreground
                                     'mode-line
                                     (face-foreground 'default))))
        :init (shadow-load-theme shadow-theme)
        :config
        ;; FIXME: @see https://github.com/hlissner/emacs-doom-themes/issues/317.
        (set-face-foreground 'mode-line (face-foreground 'default))

        ;; Make swiper match clearer
	    (with-eval-after-load 'swiper
	      (set-face-background 'swiper-background-match-face-1 "SlateGray1"))

        ;; Enable flashing mode-line on errors
        (doom-themes-visual-bell-config)
        (set-face-attribute 'doom-visual-bell nil
                            :background (face-foreground 'error)
                            :foreground (face-background 'default)
                            :inverse-video nil)
        ;; Corrects (and improves) org-mode's native fontification.
        (doom-themes-org-config)
        ;; Enable custom treemacs theme (all-the-icons must be installed!)
        (doom-themes-treemacs-config)))
  (progn
    (warn "The current theme may not be compatible with Centaur!")
    (shadow-load-theme shadow-theme)))

(provide 'init-theme)
;;; init-theme.el ends here
