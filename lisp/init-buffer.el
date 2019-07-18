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

(provide 'init-buffer)
