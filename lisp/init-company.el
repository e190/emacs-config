;;; config-complete.el --- Config for yasnippet

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

(defconst shadow/company-global-backends '(
                                          ;; 当前文件所属编程语言的语法关键词
                                          company-keywords
                                          ;; 使用 completion-at-point-functions 的后端
                                          company-capf
                                          ;; 主要用来补全当前 buffer 中出现的 word
                                          company-dabbrev
                                          ;; 使用 yasnippet 补全的后端
                                          company-yasnippet
                                          ;; 补全文件系统的路径后端
                                          company-files
                                          ))
(use-package company
  :defer 1
  :ensure t
  :diminish company-mode "ⓒ"
  :bind
  (:map company-mode-map
  ("M-/" . company-complete)
  ("<backtab>" . company-yasnippet)
  :map company-active-map
  ("C-s" . company-filter-candidates)
  ("C-n" . company-select-next)
  ("C-p" . company-select-previous)
  ("<tab>" . company-complete-common-or-cycle)
  ("<backtab>" . my-company-yasnippet)
  ;; ("C-c C-y" . my-company-yasnippet)
   ("C-g" . company-abort)
   ("C-/" . yas-expand-from-trigger-key)
   ;; ("<tab>" . company-complete-common)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :init
  (defun my-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  (add-hook 'after-init-hook #'global-company-mode)
  (add-hook 'company-completion-started-hook
            (lambda (&rest ignore)
              (when (and (bound-and-true-p evil-mode) (evil-insert-state-p))
                (define-key evil-insert-state-map (kbd "C-n") nil)
                (define-key evil-insert-state-map (kbd "C-p") nil))))
  :config
  (setq tab-always-indent 'complete)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 10)
  (setq company-require-match nil)
  (setq company-show-numbers t)
  ;; make previous/next selection in the popup cycles
  (setq company-selection-wrap-around t)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-global-modes '(not
                               comint-mode
                               erc-mode
                               message-mode
                               help-mode
                               gud-mode))
  (setq company-backends shadow/company-global-backends))

  ;; Icons and quickhelp
   (when emacs/>=26p
    (use-package company-box
      :diminish
      :functions (my-company-box--make-line my-company-box-icons--elisp)
      :hook (company-mode . company-box-mode)
      :config
      (setq company-box-backends-colors nil
            company-box-show-single-candidate t
            company-box-max-candidates 50
            company-box-doc-delay 0.5
            company-box-icons-alist 'company-box-icons-all-the-icons)

      ;; Support `company-common'
      (defun my-company-box--make-line (candidate)
        (-let* (((candidate annotation len-c len-a backend) candidate)
                (color (company-box--get-color backend))
                ((c-color a-color i-color s-color) (company-box--resolve-colors color))
                (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
                (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                          (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
                (align-string (when annotation
                                (concat " " (and company-tooltip-align-annotations
                                                 (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
                (space company-box--space)
                (icon-p company-box-enable-icon)
                (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
                (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                                (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                              (company-box--apply-color icon-string i-color)
                              (company-box--apply-color candidate-string c-color)
                              align-string
                              (company-box--apply-color annotation-string a-color)))
                (len (length line)))
          (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                           'company-box--color s-color)
                               line)
          line))
      (advice-add #'company-box--make-line :override #'my-company-box--make-line)

      ;; Prettify icons
      (defun my-company-box-icons--elisp (candidate)
        (when (derived-mode-p 'emacs-lisp-mode)
          (let ((sym (intern candidate)))
            (cond ((fboundp sym) 'Function)
                  ((featurep sym) 'Module)
                  ((facep sym) 'Color)
                  ((boundp sym) 'Variable)
                  ((symbolp sym) 'Text)
                  (t . nil)))))
      (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

      (with-eval-after-load 'all-the-icons
        (declare-function all-the-icons-faicon 'all-the-icons)
        (declare-function all-the-icons-material 'all-the-icons)
        (setq company-box-icons-all-the-icons
              `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
                (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
                (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
                (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
                (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
                (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
                (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
                (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
                (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
                (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
                (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
                (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
                (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
                (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
                (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
                (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
                (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
                (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
                (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
                (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))))))

  ;; Popup documentation for completion candidates
  (when (and (not emacs/>=26p) (display-graphic-p))
    (use-package company-quickhelp
      :defines company-quickhelp-delay
      :bind (:map company-active-map
             ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :init (setq company-quickhelp-delay 0.5))))

(use-package company-ycmd
  :defer 2
  :ensure t
  :init
  :config
  (add-to-list 'company-backends 'company-ycmd))

(use-package flycheck
  :defer 3
  :ensure t
  :diminish flycheck-mode
  :commands (hydra-flycheck/body)
  :hook (after-init . global-flycheck-mode)
  :bind
  (:map shadow-leader-map
        ("ec" . #'flycheck-buffer)
        ("el" . #'flycheck-list-errors)
        ("ep" . #'flycheck-previous-error)
        ("en" . #'flycheck-next-error))

  ;; (defhydra hydra-flycheck (:color red
  ;;                                   :hint nil)
  ;;   "
  ;;   ^
  ;;   ^Flycheck^        ^Errors^          ^Checker^
  ;;   ^────────^────────^──────^──────────^───────^───────────
  ;;   _q_ quit          _c_ check         _s_ select
  ;;   _v_ verify setup  _n_ next          _d_ disable
  ;;   _m_ manual        _p_ previous      _?_ describe
  ;;                   _l_ list
  ;;   ^^                  ^^                  ^^
  ;;   "
  ;;   ("q" nil exit: t)
  ;;   ("c" flycheck-buffer exit: t)
  ;;   ("d" flycheck-disable-checker exit: t)
  ;;   ("l" flycheck-list-errors exit: t)
  ;;   ("m" flycheck-manual exit: t)
  ;;   ("n" flycheck-next-error exit: t)
  ;;   ("p" flycheck-previous-error exit: t)
  ;;   ("s" flycheck-select-checker exit: t)
  ;;   ("v" flycheck-verify-setup exit: t)
  ;;   ("?" flycheck-describe-checker exit: t))

  :config
  (setq flycheck-emacs-lisp-check-declare t)
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-highlighting-mode 'symbols)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

;;; C/C++ headers completion
;; (use-package company-c-headers
;;   :ensure t
;;   :init (progn
;; 	  (cond ((sys/linuxp)
;; 		 (setq company-c-headers-path-system '("/usr/include/c++/7" "/usr/include" "/usr/local/include")))
;; 		((sys/macp)
;; 		 (setq company-c-headers-path-system '("/usr/local/include/c++/8.3.0"
;; 						       "/usr/include"
;; 						       "/usr/local/include"
;; 						       "/usr/local/opt"
;; 						       "/Library/Developer/CommandLineTools/usr/include/c++/v1")))
;; 		((eq system-type 'windows-nt)
;; 		 ))
;; 	  (run-with-idle-timer samray-idle-time nil (lambda () (add-to-list 'company-backends 'company-c-headers)))
;; 	  )
;;   )
(use-package popup
  :defer t)

(use-package ycmd
  :defer 2
  :ensure t
  :init
  :config)

(provide 'init-company)
;;; config-complete.el ends here
