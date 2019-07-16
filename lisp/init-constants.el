;;; core-constants.el --- Locations of everything

;;; Commentary:
;;

;;; Code:

(defconst system-init-time (current-time)
  "When Emacs starting evaling our code.
Similar to `before-init-time'")

(defvar shadow-site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)
  "The home of my site-lisp functionality.")
(add-to-list 'load-path shadow-site-lisp-dir)

(defvar shadow-cache-dir (expand-file-name "~/.emacs.d/.cache")
  "This directory for cache files.")

;;--------------------------------------------
(defconst centaur-homepage
  "https://github.com/seagle0128/.emacs.d"
  "The Github page of Centuar Emacs.")

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

;;------------------------------------------------------
(defcustom shadow-mail-address "e190@163.com"
  "Default email address."
  :type 'string
  )

(defcustom shadow-lsp-mode 'lsp-mode
 "Set language server."
 :type '(choice
         (const :tag "LSP Mode" 'lsp-mode)
         (const :tag "eglot" 'eglot)
         nil)) 

(defcustom shadow-theme 'default
  "Set color theme."
  :type '(choice
          (const :tag "Default theme" default)
          (const :tag "Classic theme" classic)
          (const :tag "Doom theme" doom)
          (const :tag "Dark theme" dark)
          (const :tag "Light theme" light)
          (const :tag "Daylight theme" daylight)
          symbol))
(defcustom shadow-package-archives 'emacs-china
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Melpa Mirror" melpa-mirror)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Netease" netease)
          (const :tag "Tuna" tuna)))

(defvar shadow-leader-key "SPC"
  "The leader key in Evil normal, visual and motion states.")

(defvar shadow-emacs-leader-key "M-m"
  "The leader key accessible in the Evil Emacs and insert states.")

(defvar shadow-ex-command-key ":"
  "The key used for Vim Ex commands.")

(defvar shadow-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing on the leader key).")

(provide 'init-constants)
;;; init-constants.el ends here
