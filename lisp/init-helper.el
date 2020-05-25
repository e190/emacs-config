;;; init-helper.el --- helper setup

;;; Commentary:
;;

;;; Code:

;;;; Help
;; https://emacs-china.org/t/helpful-el/8153/3
(use-package helpful
  :ensure t
  :preface
  (defun helpful-at-point-dwim ()
    (interactive)
    (let ((symbol (symbol-at-point)))
      (if symbol (helpful-symbol symbol)
        (call-interactively #'helpful-symbol))))

  :bind (([remap describe-variable] . helpful-variable)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         :map emacs-lisp-mode-map
         ("C-c C-d" . helpful-at-point-dwim))

  :init
  (setq helpful-max-buffers 10)

  :config
  (defun helpful-reuse-window-function (buf)
    (if-let ((window (display-buffer-reuse-mode-window buf '((mode . helpful-mode)))))
        (select-window window)
      (pop-to-buffer buf)))
  (setq helpful-switch-buffer-function #'helpful-reuse-window-function)

  (defun helpful-previous-helpful-buffer ()
    (interactive)
    (let ((bufname (buffer-name)))
      (previous-buffer)
      (while (and (not (eq major-mode 'helpful-mode))
                  (not (string= (buffer-name) bufname)))
        (previous-buffer))))

  (defun helpful-next-helpful-buffer ()
    (interactive)
    (let ((bufname (buffer-name)))
      (next-buffer)
      (while (and (not (eq major-mode 'helpful-mode))
                  (not (string= (buffer-name) bufname)))
        (next-buffer))))

  (bind-keys :map helpful-mode-map
             ("," . beginning-of-buffer)
             ("." . end-of-buffer)
             ("b" . helpful-previous-helpful-buffer)
             ("f" . helpful-next-helpful-buffer)
             ("q" . delete-window))
  (with-eval-after-load 'evil
    (evil-define-key 'normal helpful-mode-map (kbd "q") 'quit-window)))

(provide 'init-helper)
;;; init-helper.el ends here
