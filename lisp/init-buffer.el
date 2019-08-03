;; config-buffer.el -- initialize buffer configurations. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defun shadow/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(defun shadow/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer."
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun shadow/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

;; Found at http://emacswiki.org/emacs/KillingBuffers.
(defun shadow/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix ARG is non-nil then kill the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

;; our own implementation of kill-this-buffer from menu-bar.el
(defun shadow/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix ARG is non-nil then also kill the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun shadow/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)."
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)
    (prog-mode)))

(defun shadow/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun shadow/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defun shadow/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer.
Create the *scratch* buffer first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(shadow/define-leader-keys
        "TAB" 'shadow/alternate-buffer
        "bd" 'kill-this-buffer
        "be" 'shadow/safe-erase-buffer
        "bn" 'next-buffer
        "bm" 'shadow/kill-other-buffers
        "bN" 'shadow/new-empty-buffer
        "bP" 'shadow/copy-clipboard-to-whole-buffer
        "bp" 'previous-buffer
        "bR" 'shadow/safe-revert-buffer
        "bs" 'shadow/switch-to-scratch-buffer
        "bY" 'shadow/copy-whole-buffer-to-clipboard
        "bw" 'read-only-mode
        "b1" 'buffer-to-window-1
        "b2" 'buffer-to-window-2
        "b3" 'buffer-to-window-3
        "b4" 'buffer-to-window-4
        "b5" 'buffer-to-window-5
        "b6" 'buffer-to-window-6
        "b7" 'buffer-to-window-7
        "b8" 'buffer-to-window-8
        "b9" 'buffer-to-window-9)

(use-package ibuffer
  :ensure nil
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-auto-mode-match?
              all-the-icons-faicon)
  :commands ibuffer-find-file
  :bind ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))

  ;; Display buffer icons on GUI
  (when (display-graphic-p)
    ;; For alignment, the size of the name field should be the width of an icon
    (define-ibuffer-column icon (:name "  ")
      (let ((icon (if (and (buffer-file-name)
                           (all-the-icons-auto-mode-match?))
                      (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                    (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
          icon)))

    (setq ibuffer-formats `((mark modified read-only ,(if emacs/>=26p 'locked "")
                                  ;; Here you may adjust by replacing :right with :center or :left
                                  ;; According to taste, if you want the icon further from the name
                                  " " (icon 2 2 :left :elide)
                                  ,(propertize " " 'display `(space :align-to 8))
                                  (name 18 18 :left :elide)
                                  " " (size 9 -1 :right)
                                  " " (mode 16 16 :left :elide) " " filename-and-process)
                            (mark " " (name 16 -1) " " filename))))

  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                 (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                   default-directory))))
        (counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))

  ;; Group ibuffer's list by project root
  (use-package ibuffer-projectile
    :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
    :hook ((ibuffer . (lambda ()
                        (ibuffer-projectile-set-filter-groups)
                        (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic)))))
    :config
    (setq ibuffer-projectile-prefix
          (if (display-graphic-p)
              (concat
               (all-the-icons-octicon "file-directory"
                                      :face ibuffer-filter-group-name-face
                                      :v-adjust -0.05
                                      :height 1.25)
               " ")
            "Project: "))))

(provide 'init-buffer)
