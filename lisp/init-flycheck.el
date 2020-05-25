;; init-flycheck.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Flycheck configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-constants))

(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode)
  :pretty-hydra
  ((:color red :quit-key "q")
   ("Flycheck"
    (("q" nil "quit" :exit t)
     ("v" flycheck-verify-setup "verify setup" :exit t)
     ("m" flycheck-manual "manual" :exit t))
    "Errors"
    (("l" flycheck-list-errors "list" :exit t)
     ("c" flycheck-buffer "check" :exit t)
     ("n" flycheck-next-error "next")
     ("p" flycheck-previous-error "previous"))
    "Checker"
    (("d" flycheck-disable-checker "disable" :exit t)
     ("s" flycheck-select-checker "select" :exit t)
     ("?" flycheck-describe-checker "describe" :exit t))))
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; c/c++ mode
  (setq flycheck-gcc-language-standard "c++11")
  (setq flycheck-clang-language-standard "c++11")
  ;; (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))

  ;; Set fringe style
  (setq flycheck-indication-mode 'right-fringe)
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (if emacs/>=26p
          (use-package flycheck-posframe
            :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
            :hook (flycheck-mode . flycheck-posframe-mode)
            :init (setq flycheck-posframe-border-width 1
                        flycheck-posframe-inhibit-functions
                        '((lambda (&rest _) (bound-and-true-p company-backend)))))
        (use-package flycheck-pos-tip
          :defines flycheck-pos-tip-timeout
          :hook (global-flycheck-mode . flycheck-pos-tip-mode)
          :config (setq flycheck-pos-tip-timeout 30)))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode))))

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
