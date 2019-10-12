;; init-c.el --- Initialize c configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; C/C++ configuration.
;;

;;; Code:

(eval-when-compile
  (require 'init-constants))

;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("C-c c" . compile))
  :hook (c-mode-common . (lambda ()
                           (c-set-style "bsd")
                           (setq tab-width 4)
                           (setq evil-shift-width 4)
                           (setq c-basic-offset 4)))
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t))
  ;; Company mode backend for C/C++ header files
  (use-package company-c-headers
    :after company
    :defines company-backends
    :init (cl-pushnew 'company-c-headers company-backends))
  )

(use-package cmake-mode
  :ensure t
  :mode (
	 ("CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Origami - Does code folding, ie hide the body of an
;; if/else/for/function so that you can fit more code on your screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package origami
  :ensure t
  :commands (origami-mode)
  :bind (:map origami-mode-map
              ("C-c o :" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o o" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t)
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (add-hook 'prog-mode-hook 'origami-mode)

  ;; Support LSP
  (when (eq shadow-lsp-mode 'lsp-mode)
    (use-package lsp-origami
      :hook (origami-mode . (lambda ()
                              (if (bound-and-true-p lsp-mode)
                                  (lsp-origami-mode)))))))

(use-package ggtags
  :if (eq shadow-lsp-mode 'gtags)
  :init
  (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))
  :bind (:map ggtags-mode-map
        ([remap xref-find-definitions] . ggtags-find-definition)
        ([remap xref-find-references] . ggtags-find-references)
        ("C-c d" . ggtags-find-definition)
        ("C-c r" . ggtags-find-references)
        ("C-c i" . ggtags-find-tag-dwim)
        ("M-," . pop-tag-mark)
        ("C-c <" . ggtags-prev-mark)
        ("C-c >" . ggtags-next-mark)
        :map shadow-leader-map
        ("tc" . ggtags-create-tags)
        ("tu" . ggtags-update-tags)))

(provide 'init-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-c.el ends here
