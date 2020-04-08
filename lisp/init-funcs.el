;;; init-funcs.el --- Functions for base. -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; Suppress warnings
(declare-function async-inject-variables 'async)
(declare-function chart-bar-quickie 'chart)
(declare-function flycheck-buffer 'flycheck)
(declare-function flymake-start 'flymake)
(declare-function upgrade-packages 'init-package)

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer.")))
(global-set-key (kbd "s-r") #'revert-this-buffer)

(defun shadow/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun shadow/toggle-current-window-dedication ()
  "Toggle dedication state of a window."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

(defun shadow/delete-window (&optional arg)
  "Delete the current window.
If the universal prefix argument is used then kill the buffer too."
  (interactive "P")
  (if (equal '(4) arg)
      (kill-buffer-and-window)
    (delete-window)))

(defun shadow/layout-double-columns ()
  " Set the layout to double columns. "
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun shadow/layout-triple-columns ()
  " Set the layout to triple columns. "
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

;; https://tsdh.wordpress.com/2007/03/28/deleting-windows-vertically-or-horizontally/
(defun shadow/maximize-horizontally ()
  "Delete all windows left or right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun shadow/rotate-windows-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (abn/rotate-windows-forward (* -1 count)))

;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun shadow/rotate-windows-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))

(defun shadow/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun shadow/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

;; from @bmag
(defun shadow/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

;;;###autoload
(defun shadow/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

;;;###autoload
(defun shadow/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-carrage-returns ()
  "Delete `^M' characters in the buffer.
Same as `replace-string C-q C-m RET RET'."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

;; Open custom file
(defun open-custom-file()
  "Open custom.el if exists, otherwise create it."
  (interactive)
  (let ((custom-example
         (expand-file-name "custom-example.el" user-emacs-directory)))
    (unless (file-exists-p custom-file)
      (if (file-exists-p custom-example)
          (copy-file custom-file)
        (error "Unable to find \"%s\"" custom-example)))
    (find-file custom-file)))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))

;;;###autoload
(defun shadow/open-init-file ()
  "Open emacs init file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Update
(defun update-config ()
  "Update shadow Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (if (file-exists-p dir)
        (progn
          (message "Updating Emacs configurations...")
          (cd dir)
          (shell-command "git pull")
          (message "Update finished. Restart Emacs to complete the process."))
      (message "\"%s\" doesn't exist." dir))))
(defalias 'shadow-update-config #'update-config)

(defvar centaur--updating-packages nil)
(defun update-packages (&optional sync)
  "Refresh package contents and update all packages.

If SYNC is non-nil, the updating process is synchronous."
  (interactive)
  (when centaur--updating-packages
    (user-error "Still updating packages..."))

  (message "Updating packages...")
  (if (and (not sync)
           (require 'async nil t))
      (progn
        (setq centaur--updating-packages t)
        (async-start
        `(lambda ()
            ,(async-inject-variables "\\`\\(load-path\\)\\'")
            (require 'init-funcs)
            (require 'init-packages)
            (upgrade-packages)
            (with-current-buffer auto-package-update-buffer-name
              (buffer-string)))
        (lambda (result)
          (message "%s" result)
          (message "Updating packages...done"))))
    (progn
      (upgrade-packages)
      (message "Updating packages...done"))))
(defalias 'shadow-update-packages #'update-packages)

(defvar centaur--updating nil)
(defun update-config-and-packages(&optional sync)
  "Update confgiurations and packages.

If SYNC is non-nil, the updating process is synchronous."
  (interactive)
  (when centaur--updating
    (user-error "Centaur Emacs is still updating..."))

  (message "This will update Shadow Emacs to the latest")
  (if (and (not sync)
           (require 'async nil t))
      (progn
        (setq centaur--updating t)
        (async-start
        `(lambda ()
            ,(async-inject-variables "\\`\\(load-path\\)\\'")
            (require 'init-funcs)
            (require 'init-packages)
            (update-config)
            (update-packages t)
            (with-current-buffer auto-package-update-buffer-name
              (buffer-string)))
        (lambda (result)
          (message "%s" result)
          (message "Done. Restart to complete process"))))
    (progn
      (update-config)
      (update-packages t)
      (message "Done. Restart to complete process"))))
(defalias 'shadow-update #'update-config-and-packages)

;; Browse the homepage
(defun browse-homepage ()
  "Browse the Github page of Shadow Emacs."
  (interactive)
  (browse-url Shadow-homepage))

;; Create a new scratch buffer
(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;; Save a file as UTF-8
(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

;; Recompile elpa directory
(defun recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

;; Recompile site-lisp directory
(defun recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and shadow-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)))

;;;###autoload
(defun shadow/revert-buffer-no-confirm ()
  "Revert buffer without confirm."
  (interactive)
  (revert-buffer t t))

;;;###autload
(defun git-get-current-file-relative-path ()
  "Get current file relative path."
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

;;;###autload
(defun shadow/git-checkout-current-file ()
  "Git checkout current file."
  (interactive)
  (when (and (buffer-file-name)
             (yes-or-no-p (format "git checkout %s?"
                                  (file-name-nondirectory (buffer-file-name)))))
    (let* ((filename (git-get-current-file-relative-path)))
      (shell-command (concat "git checkout " filename))
      (shadow/revert-buffer-no-confirm)
      (message "DONE! git checkout %s" filename))))

;;;###autload
(defun shadow/git-add-current-file ()
  "Git add file of current buffer."
  (interactive)
  (let ((filename))
    (when buffer-file-name
      (setq filename (git-get-current-file-relative-path))
      (shell-command (concat "git add " filename))
      (message "DONE! git add %s" filename))))

;;;###autload
(defun shadow/magit-display-buffer-function (buffer)
  (if magit-display-buffer-noselect
      ;; the code that called `magit-display-buffer-function'
      ;; expects the original window to stay alive, we can't go
      ;; fullscreen
      (magit-display-buffer-traditional buffer)
    (delete-other-windows)
    ;; make sure the window isn't dedicated, otherwise
    ;; `set-window-buffer' throws an error
    (set-window-dedicated-p nil nil)
    (set-window-buffer nil buffer)
    ;; return buffer's window
    (get-buffer-window buffer)))

(defun shadow/goto-match-parent ()
  "Go to the matching  if on (){}[], similar to vi style of %."
  (interactive)
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

;; FontsList
;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
(defvar font-list '(("Input" . 11)
                    ("Consolas" . 11)
                    ("DejaVu Sans Mono" . 10)
                    ("SF Mono" . 10)
                    ("Source Code Pro" . 10)
                    ("Menlo" . 10))

  "List of fonts and sizes.  The first one available will be used.")
;; -FontsList

;; FontFun
(defun change-font ()
  "Documentation."
  (interactive)
  (let* (available-fonts font-name font-size font-setting)
    (dolist (font font-list (setq available-fonts (nreverse available-fonts)))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))))

;; 自定义窗口大小
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if sys/win32p
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 180))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 140)
                          (frame-char-height))))
    )))

(provide 'init-funcs)
;;; init-funcs.el ends here
