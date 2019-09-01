;;; init-funcs.el --- Functions for base. -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defmacro shadow/ktop-watch (&rest forms)
  (let ((temp-var (make-symbol "start")))
    `(let ((,temp-var (float-time)))
       (progn . ,forms)
       (message "%f" (- (float-time) ,temp-var)))))
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

;;;###autoload
(defun shadow/open-init-file ()
  "Open emacs init file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Update
(defun update-config ()
  "Update Centaur Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (if (file-exists-p dir)
        (progn
          (message "Updating Emacs configurations...")
          (cd dir)
          (shell-command "git pull")
          (message "Update finished. Restart Emacs to complete the process."))
      (message "\"%s\" doesn't exist." dir))))
(defalias 'centaur-update-config 'update-config)

(declare-function upgrade-packages 'init-package)
(defalias 'centaur-update-packages 'upgrade-packages)

(defun update-config-and-packages()
  "Update confgiurations and packages."
  (interactive)
  (update-config)
  (upgrade-packages nil))
(defalias 'centaur-update 'update-config-and-packages)
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

(defun shadow-current-theme ()
  "The current enabled theme."
  (car custom-enabled-themes))

(defun my-project-name-contains-substring (REGEX)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               "")))
    (string-match-p REGEX dir)))

(defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
  "return the full path of tags file"
  (let ((dir (file-name-as-directory (file-truename SRC-DIR)) )
       file)
    (setq file (concat dir "TAGS"))
    (when (or FORCE (not (file-exists-p file)))
      (message "Creating TAGS in %s ..." dir)
      (shell-command
       (format "ctags -f %s -e -R %s" file dir))
      )
    file
    ))

(defvar my-tags-updated-time nil)

(defun my-update-tags ()
  (interactive)
  "check the tags in tags-table-list and re-create it"
  (dolist (tag tags-table-list)
    (my-create-tags-if-needed (file-name-directory tag) t)
    ))

(defun my-auto-update-tags-when-save ()
  (interactive)
  (cond
   ((not my-tags-updated-time)
    (setq my-tags-updated-time (current-time)))
   ((< (- (float-time (current-time)) (float-time my-tags-updated-time)) 300)
    ;; < 300 seconds
    ;; do nothing
    )
   (t
    (setq my-tags-updated-time (current-time))
    (my-update-tags)
    (message "updated tags after %d seconds." (- (float-time (current-time))  (float-time my-tags-updated-time)))
    )
   ))

;; (defun my-setup-develop-environment ()
;;     (when (my-project-name-contains-substring "Loris")
;;       (cond
;;        ((my-project-name-contains-substring "src/desktop")
;;         ;; C++ project don't need html tags
;;         (setq tags-table-list (list
;;                                (my-create-tags-if-needed
;;                                 (concat (file-name-as-directory (getenv "WXWIN")) "include"))
;;                                (my-create-tags-if-needed "~/projs/Loris/loris/src/desktop")))
;;         )
;;        ((my-project-name-contains-substring "src/html")
;;         ;; html project donot need C++ tags
;;         (setq tags-table-list (list (my-create-tags-if-needed "~/projs/Loris/loris/src/html")))
;;         ))))
(defun my-setup-develop-environment ()
  (interactive)
  (when (my-project-name-contains-substring "guanghui")
    (cond
     ((my-project-name-contains-substring "cocos2d-x")
      ;; C++ project don't need html tags
      (setq tags-table-list (list (my-create-tags-if-needed "~/cocos2d-x/cocos"))))
     ((my-project-name-contains-substring "Github/fireball")
      (message "load tags for fireball engine repo...")
      ;; html project donot need C++ tags
      (setq tags-table-list (list (my-create-tags-if-needed "~/Github/fireball/engine/cocos2d")))))))



(provide 'init-funcs)
;;; init-funcs.el ends here
