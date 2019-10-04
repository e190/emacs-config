;; init-keybindings.el --- Initialize key bindings configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-constants))

(defvar shadow-leader-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.5)
  ;; Minibuffer feels much faster than using windows.
  (setq which-key-popup-type 'minibuffer)
  :config
  ;; Shows available keybindings after you start typing.
  (which-key-mode 1))

;; I always hit this by mistake to get to `describe-char' and I'm tired of
;; seeing the GNU license.
(global-set-key (kbd "C-h C-c") 'describe-key-briefly)

(defun shadow-declare-prefix (prefix name)
  "Declare a which-key PREFIX.
PREFIX is a string describing a key sequence.  NAME is a string
used as the prefix command."
  (let* ((command name)
	 (full-prefix (concat shadow-leader-key " " prefix))
	 (full-prefix-emacs (concat shadow-emacs-leader-key " " prefix))
	 (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
	 (full-prefix-emacs-lst (listify-key-sequence
				 (kbd full-prefix-emacs))))
    (which-key-declare-prefixes
      full-prefix-emacs name
      full-prefix name)))
(put 'shadow-declare-prefix 'lisp-indent-function 'defun)

(defun shadow/declare-prefix-for-mode (mode prefix name)
  "Declare a prefix PREFIX. MODE is the mode in which this prefix command should
be added. PREFIX is a string describing a key sequence. NAME is a symbol name
used as the prefix command."
  (let  ((command (intern (concat (symbol-name mode) name)))
	 (full-prefix (concat shadow-leader-key " " prefix))
	 (full-prefix-emacs (concat shadow-emacs-leader-key " " prefix)))

    (which-key-declare-prefixes-for-mode mode
      full-prefix-emacs name
      full-prefix name)))

(put 'abn/declare-prefix-for-mode 'lisp-indent-function 'defun)

(defun shadow//define-keys (keymap key def &rest bindings)
  "In KEYMAP define KEY to DEF as well as all BINDINGS.
`kbd' is applied to all KEYs.  BINDINGS is additional KEY-DEF pairs.
Always defines C-g as `keyboard-quit'."
  (declare (indent 1))
  (define-key keymap (kbd "C-g") 'keyboard-quit)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings))
    (setq def (pop bindings))))

(defun shadow/define-leader-keys (key def &rest bindings)
  "Set KEY to DEF in `abn-leader-map'.
BINDINGS is additional key-definition pairs.  `kbd' is used for
every KEY."
  (declare (indent 0))
  (apply 'shadow//define-keys shadow-leader-map key def bindings))

;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; Auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Makes <escape> quit as much as possible.
(define-key minibuffer-local-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map
  (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map
  (kbd "<escape>") 'keyboard-escape-quit)

(setq shadow-key-binding-prefixes
      '((","   "leader")
	("a"   "applications")
	("b"   "buffers")
	("c"   "copy/comments")
	("e"   "flycheck")
	("f"   "files")
	("h"   "help/Highlight")
	("j"   "jump")
	("g"   "magit")
	("m"   "mark")
	("p"   "projectile")
	("q"   "quit")
	("r"   "replace")
	("s"   "search/symbol")
	("t"   "toggles")
	("w"   "windows")
	("z"   "zoom")))
(mapc (lambda (x) (apply #'shadow-declare-prefix x))
      shadow-key-binding-prefixes)

(use-package restart-emacs
  :bind
  (:map shadow-leader-map
        ("qr" . restart-emacs)))


(provide 'init-keybindings)
;;; init-keybindings.el ends here
