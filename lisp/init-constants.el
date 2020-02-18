;;; init-constants.el --- Locations of everything

;;; Commentary:
;;

;;; Code:

(defconst system-init-time (current-time)
  "When Emacs starting evaling our code.
Similar to `before-init-time'")

(defvar shadow-site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)
  "The home of my site-lisp functionality.")
(add-to-list 'load-path shadow-site-lisp-dir)

(defvar shadow-cache-dir (expand-file-name ".cache" user-emacs-directory)
  "This directory for cache files.")

;;--------------------------------------------
(defconst Shadow-homepage
  "https://github.com/e190/emacs-config"
  "The Github page of Centuar Emacs.")

(defcustom Shadow-logo (expand-file-name "img/dashLogo.png" user-emacs-directory)
  "Set Centaur logo. nil means official logo."
  :type 'string)

(defconst *sys/gui*
  (display-graphic-p)
  "Are we running on a GUI Emacs?")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

(defconst emacs/>=25.3p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 3)))
  "Emacs is 25.2 or above.")

;;------------------------------------------------------
(defcustom shadow-mail-address "e190@163.com"
  "Default email address."
  :group 'shadow
  :type 'string
  )

(defcustom shadow-icon (display-graphic-p)
  "Display icons or not."
  :group 'shadow
  :type 'boolean)

(defcustom shadow-dashboard t
  "Use dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'shadow
  :type 'boolean)

(defcustom shadow-lsp-mode 'ctags
 "Set language server."
  :group 'shadow
  :type '(choice
         (const :tag "LSP Mode" 'lsp-mode)
         (const :tag "eglot" 'eglot)
         (const :tag "ctags" 'ctags)
         (const :tag "gtags" 'gtags)
         nil))

(defcustom shadow-theme-alist
  '((default  . doom-one)
    (classic  . doom-molokai)
    (colorful . doom-snazzy)
    (dark     . doom-dark+)
    (light    . doom-one-light)
    (day      . doom-acario-light)
    (night    . doom-city-lights))
  "The color theme list."
  :group 'shadow
  :type '(alist :key-type (symbol :tag "Theme name")
                :value-type (symbol :tag "Internal theme name")))

(defcustom shadow-theme 'default
  "Set color theme."
  :group 'shadow
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    shadow-theme-alist)
                 symbol))

(defcustom shadow-package-archives 'emacs-china
  "Set package archives from which to fetch."
  :group 'shadow
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Melpa Mirror" melpa-mirror)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Netease" netease)
          (const :tag "Tuna" tuna)))

(defvar shadow-leader-key "SPC"
  "The leader key in Evil normal, visual and motion states.")

(defconst shadow-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar shadow-emacs-leader-key "M-m"
  "The leader key accessible in the Evil Emacs and insert states.")

(defvar shadow-ex-command-key ":"
  "The key used for Vim Ex commands.")

(defvar shadow-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing on the leader key).")

(defvar shadow-leader-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-example-file
       (expand-file-name "custom-example.el" user-emacs-directory)))
  (if (and (file-exists-p custom-example-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-example-file custom-file)))

(if (file-exists-p custom-file)
    (load custom-file))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
(add-hook 'after-init-hook
          (lambda ()
            (let ((file
                   (expand-file-name "custom-post.el" user-emacs-directory)))
              (if (file-exists-p file)
                  (load file)))))

(provide 'init-constants)
;;; init-constants.el ends here
