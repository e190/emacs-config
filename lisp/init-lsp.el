;; init-lsp.el --- Initialize lsp (Language Server Protocol) configurations. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'init-constants))

(pcase shadow-lsp-mode
  ('eglot
   (use-package eglot
     :hook (prog-mode . eglot-ensure)))

  ('lsp-mode
   ;; Emacs client for the Language Server Protocol
   ;; https://github.com/emacs-lsp/lsp-mode#supported-languages
   (use-package lsp-mode
     :diminish lsp-mode
     :hook (prog-mode . lsp-deferred)
     :bind (:map lsp-mode-map
            ("C-c C-d" . lsp-describe-thing-at-point))
     :init (setq lsp-auto-guess-root t       ; Detect project root
                 lsp-prefer-flymake nil      ; Use lsp-ui and flycheck
                 flymake-fringe-indicator-position 'right-fringe)
     :config
     ;; Configure LSP clients
     (use-package lsp-clients
       :ensure nil
       :init (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))))
  (use-package lsp-ui
    :commands lsp-ui-doc-hide
    ;; :custom-face (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
     :custom-face
     (lsp-ui-doc-background ((t (:background nil))))
     (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
     ;; :hook (after-load-theme . (lambda ()
     ;;                             (set-face-attribute 'lsp-ui-doc-background nil
     ;;                                                 :background (face-background 'tooltip))))
    :bind (:map lsp-ui-mode-map
                ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                ([remap xref-find-references] . lsp-ui-peek-find-references)
	            ("C-c d" . lsp-ui-peek-find-definitions)
	            ("C-c r" . lsp-ui-peek-find-references)
	            ("C-c i" . lsp-ui-imenu)
	            ("C-c F" . lsp-ui-sideline-apply-code-actions)
	            ("C-c R" . lsp-rename))
    :init (setq lsp-ui-doc-enable t
                lsp-ui-doc-use-webkit nil
                lsp-ui-doc-delay 1.0
                lsp-ui-doc-include-signature t
                lsp-ui-doc-position 'at-point
                lsp-ui-doc-border (face-foreground 'default)

                lsp-ui-sideline-enable nil
                lsp-ui-sideline-ignore-duplicate t)
    :config
    ;; (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))
     (add-to-list 'lsp-ui-doc-frame-parameters '(left-fringe . 0))

    ;; `C-g'to close doc
    (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

    ;; Reset `lsp-ui-doc-background' after loading theme
    (add-hook 'after-load-theme-hook
              (lambda ()
                (setq lsp-ui-doc-border (face-foreground 'default))
                (set-face-background 'lsp-ui-doc-background
                                     (face-background 'tooltip))))
    ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
    ;; https://github.com/emacs-lsp/lsp-ui/issues/243
    (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
      (setq mode-line-format nil)))

    (use-package company-lsp
      :init (setq company-lsp-cache-candidates 'auto))
      ;; :ensure t
      ;; :after (company lsp-mode)
      ;; :config
      ;; (cl-pushnew 'company-lsp company-backends)
      ;; (setq company-lsp-async t)
      ;; (setq company-lsp-enable-snippet t)
      ;; (setq company-lsp-cache-candidates t))

   ;; C/C++/Objective-C support
   (use-package ccls
     :defines projectile-project-root-files-top-down-recurring
     :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
                                                      (require 'ccls)
                                                      (lsp-deferred)))
     :config
     (with-eval-after-load 'projectile
       (setq projectile-project-root-files-top-down-recurring
             (append '("compile_commands.json"
                       ".ccls")
                     projectile-project-root-files-top-down-recurring))))
    ;; Python support for lsp-mode using pyls.
    ;; Install: pip install python-language-server
    (use-package lsp-python
      :ensure t
      :after python-mode
      :commands lsp-python-enable
      :hook (python-mode . lsp-python-enable)
      ;; :config
      ;; (setq-default flycheck-flake8-maximum-line-length 100)
      )
    ;; Bash support for lsp-mode using Mads Hartmann's bash-language-server
    ;; Install: npm i -g bash-language-server@1.4.0
    ;; Require Python2.5+, use --python to specify.
    (use-package lsp-sh
      :commands lsp-sh-enable
      :hook (sh-mode . lsp-sh-enable))))

(when shadow-lsp-mode
  ;; Enable LSP in org babel
  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (cl-defmacro lsp-org-babel-enbale (lang)
    "Support LANG in org source code block."
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((filename (or (->> info caddr (alist-get :file))
                               buffer-file-name)))
             (setq buffer-file-name filename)
             (pcase shadow-lsp-mode
               ('eglot
                (and (fboundp 'eglot) (eglot)))
               ('lsp-mode
                (and (fboundp 'lsp)
                     ;; `lsp-auto-guess-root' MUST be non-nil.
                     (setq lsp-buffer-uri (lsp--path-to-uri filename))
                     (lsp-deferred))))))
         (put ',intern-pre 'function-documentation
              (format "Enable `%s' in the buffer of org source block (%s)."
                      shadow-lsp-mode (upcase ,lang)))

         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))

  (defvar org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))
  (add-to-list 'org-babel-lang-list (if emacs/>=26p "shell" "sh"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enbale ,lang))))
(provide 'init-lsp)
;;; init-lsp.el ends here
