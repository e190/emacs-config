;; init-hydra.el --- Initialize hydra configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Nice looking hydras.
;;

;;; Code:

(eval-when-compile
  (require 'init-constants))

(use-package hydra
  :commands (hydra-default-pre
             hydra-keyboard-quit
             hydra--call-interactively-remap-maybe
             hydra-show-hint
             hydra-set-transient-map))

(use-package pretty-hydra
  :defer t
  :functions set-package-archives shadow-load-theme
  :bind ("<f6>" . toggles-hydra/body)
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
        (when (and (display-graphic-p) icon-type icon-name)
          (let ((f (intern (format "all-the-icons-%s" icon-type))))
            (when (fboundp f)
              (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
        (propertize title 'face face))))

  ;; Global toggles
  (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "toggle-on")
                                      :color amaranth :quit-key "q")
    ("Basic"
    (("n" (if (fboundp 'display-line-numbers-mode)
              (display-line-numbers-mode (if display-line-numbers-mode -1 1))
            (global-linum-mode (if global-linum-mode -1 1)))
      "line number" :toggle (if (fboundp 'display-line-numbers-mode)
                                display-line-numbers-mode
                              global-linum-mode))
    ;; (("n" display-line-numbers-mode "line number" :toggle t)
      ("N" linum-mode "legacy line number" :toggle t)
      ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
      ("h" global-hungry-delete-mode "hungry delete" :toggle t)
      ("e" electric-pair-mode "electric pair" :toggle t)
      ("c" flyspell-mode "spell check" :toggle t)
      ("S" prettify-symbols-mode "pretty symbol" :toggle t)
      ("P" awesome-tab-mode "awesome-tab" :toggle t)
      ("L" global-page-break-lines-mode "page break lines" :toggle t)
      ("M" doom-modeline-mode "modern mode-line" :toggle t))
    "Highlight"
    (("l" global-hl-line-mode "line" :toggle t)
      ("p" show-paren-mode "paren" :toggle t)
      ("s" symbol-overlay-mode "symbol" :toggle t)
      ("r" rainbow-mode "rainbow" :toggle t)
      ("w" (setq show-trailing-whitespace (not show-trailing-whitespace))
      "whitespace" :toggle show-trailing-whitespace)
      ("R" rainbow-delimiters-mode "delimiter" :toggle t)
      ("i" highlight-indent-guides-mode "indent" :toggle t)
      ("T" hl-todo-mode "todo" :toggle t))
    "Coding"
    (("f" flycheck-mode "flycheck" :toggle t)
      ("F" flymake-mode "flymake" :toggle t)
      ("o" origami-mode "folding" :toggle t)
      ("O" hs-minor-mode "hideshow" :toggle t)
      ("u" subword-mode "subword" :toggle t)
      ("W" which-function-mode "which function" :toggle t)
      ("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
      ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))
    "Version Control"
    (("v" diff-hl-mode "gutter" :toggle t)
      ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
      ("m" diff-hl-margin-mode "margin gutter" :toggle t)
      ("E" diff-hl-dired-mode "dired gutter" :toggle t))
      "Theme"
      (("t d" (shadow-load-theme 'default) "default"
        :toggle (eq shadow-theme 'default))
      ("t c" (shadow-load-theme 'classic) "classic"
        :toggle (eq shadow-theme 'classic))
      ("t r" (shadow-load-theme 'colorful) "colorful"
        :toggle (eq shadow-theme 'colorful))
      ("t k" (shadow-load-theme 'dark) "dark"
        :toggle (eq shadow-theme 'dark))
      ("t l" (shadow-load-theme 'light) "light"
        :toggle (eq shadow-theme 'light))
      ("t y" (shadow-load-theme 'day) "day"
        :toggle (eq shadow-theme 'day))
      ("t n" (shadow-load-theme 'night) "night"
        :toggle (eq shadow-theme 'night))
      ("t o" (ivy-read "Load custom theme: "
                        (mapcar #'symbol-name
                                (custom-available-themes))
                        :predicate (lambda (candidate)
                                    (string-prefix-p "doom-" candidate))
                        :action (lambda (theme)
                                  (setq shadow-theme
                                        (let ((x (intern theme)))
                                          (or (car (rassoc x shadow-theme-alist))
                                              x)))
                                  (counsel-load-theme-action theme))
                        :caller 'counsel-load-theme)
        "others" :toggle (not (assoc shadow-theme shadow-theme-alist))))
    "Package Archive"
    (("k m" (progn (setq shadow-package-archives 'melpa)
                    (set-package-archives shadow-package-archives))
      "melpa" :toggle (eq shadow-package-archives 'melpa))
      ("k i" (progn (setq shadow-package-archives 'melpa-mirror)
                    (set-package-archives shadow-package-archives))
      "melpa mirror" :toggle (eq shadow-package-archives 'melpa-mirror))
      ("k c" (progn (setq shadow-package-archives 'emacs-china)
                    (set-package-archives shadow-package-archives))
      "emacs china" :toggle (eq shadow-package-archives 'emacs-china))
      ("k n" (progn (setq shadow-package-archives 'netease)
                    (set-package-archives shadow-package-archives))
      "netease" :toggle (eq shadow-package-archives 'netease))
      ("k t" (progn (setq shadow-package-archives 'tencent)
                    (set-package-archives shadow-package-archives))
      "tencent" :toggle (eq shadow-package-archives 'tencent))
      ("k u" (progn (setq shadow-package-archives 'tuna)
                    (set-package-archives shadow-package-archives))
      "tuna" :toggle (eq shadow-package-archives 'tuna))))))

(provide 'init-hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hydra.el ends here
