;; init-prog.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; General programming configurations.
;;

;;; Code:

;; Jump to definition via `ag'/`rg'/`grep'
(use-package dumb-jump
  :pretty-hydra
  ((:title (pretty-hydra-title "Dump Jump" 'faicon "anchor")
    :color blue :quit-key "q")
   ("Jump"
    (("j" dumb-jump-go "Go")
     ("o" dumb-jump-go-other-window "Go other window")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
    "Other"
    (("i" dumb-jump-go-prompt "Prompt")
     ("l" dumb-jump-quick-look "Quick look")
     ("b" dumb-jump-back "Back"))))
  :bind (:map dumb-jump-mode-map
         ("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("C-M-j" . dumb-jump-hydra/body))
  :hook (after-init . dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-selector 'ivy))

;; https://github.com/ankurdave/color-identifiers-mode
(use-package color-identifiers-mode
  :hook (after-init . global-color-identifiers-mode)
  :config
  (defun myfunc-color-identifiers-mode-hook ()
  (face-remap-add-relative 'font-lock-keyword-face '((:weight bold)))
  (face-remap-add-relative 'font-lock-comment-face '((:slant italic)))
  (face-remap-add-relative 'font-lock-builtin-face '((:weight bold)))
  (face-remap-add-relative 'font-lock-preprocessor-face '((:weight bold)))
  (face-remap-add-relative 'font-lock-function-name-face '((:slant italic)))
  (face-remap-add-relative 'font-lock-string-face '((:slant italic)))
  (face-remap-add-relative 'font-lock-constant-face '((:weight bold))))

  (add-hook 'color-identifiers-mode-hook 'myfunc-color-identifiers-mode-hook))

(provide 'init-prog)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
