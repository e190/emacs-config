;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :preface
  (defun hot-expand (str &optional mod)
    "Expand org template."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (org-try-structure-completion)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  ;; https://emacs-china.org/t/org-mode/79
  (defun my-org-screenshot ()
    "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer and insert a link to this file."
    (interactive)
    (org-display-inline-images)
    (setq filename
          (concat
          (make-temp-name
            (concat (file-name-nondirectory (buffer-file-name))
                    "_imgs/"
                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))
                      ; take screenshot
    (if (eq system-type 'darwin)
        (progn
      (call-process-shell-command "screencapture" nil nil nil nil " -s " (concat
                                          "\"" filename "\"" ))
      (call-process-shell-command "convert" nil nil nil nil (concat "\"" filename "\" -resize  \"50%\"" ) (concat "\"" filename "\"" ))
      ))
    (if (eq system-type 'gnu/linux)
        (call-process "import" nil nil nil filename))
                      ; insert into file if correctly taken
    (if (file-exists-p filename)
        (insert (concat "[[file:" filename "]]")))
    (org-display-inline-images))

  :pretty-hydra
  ((:title (pretty-hydra-title "Org Template" 'fileicon "org")
    :color blue :quit-key "q")
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("e" (hot-expand "<e") "example")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("m" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("u" (hot-expand "<s" "plantuml :file CHANGE.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
            (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
            (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         :map org-mode-map
         ("<" . (lambda ()
                  "Insert org template."
                  (interactive)
                  (if (or (region-active-p) (looking-back "^\s*" 1))
                      (org-hydra/body)
                    (self-insert-command 1))))
          :map shadow-leader-map
          ("os" . my-org-screenshot))
  :init
  (add-hook 'org-mode-hook (lambda ()
                             (org-indent-mode 1)
                             (diminish 'org-indent-mode)))
  (add-to-list 'file-coding-system-alist '("\\.org\\'" . utf-8))
  :config
  (setq org-agenda-files '("~/org")
        org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
                            (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
   org-todo-keyword-faces
        '(("TODO" . (:foreground "#ee6363" :weight bold))
          ("DOING" . (:foreground "#3a81c3" :weight bold))
          ("HANGUP" . (:foreground "red" :weight bold))
          ("DONE" . (:foreground "#7ccd7c" :weight bold))
          ("CANCEL"  . (:foreground "yellow" :weight bold)))
        org-log-done 'time
        org-catch-invisible-edits 'smart
        org-startup-indented t
        ;; org-ellipsis (if (char-displayable-p ?) "  " nil)
        org-pretty-entities t
        org-hide-emphasis-markers t)

  ;; ;; Enable markdown backend
  (add-to-list 'org-export-backends 'md)


;;; show org-mode bullets as UTF-8 character
(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :config
  ;; (setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷")))
  (setq org-bullets-bullet-list
          '("✡" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇")))

  ;; Presentation
  (use-package org-tree-slide
    :config
    (add-hook 'org-tree-slide-play-hook
              (lambda ()
                (text-scale-set 4)
                (org-display-inline-images)
                (read-only-mode 1)))
    (add-hook 'org-tree-slide-stop-hook
              (lambda ()
                (text-scale-set 0)
                (org-remove-inline-images)
                (read-only-mode -1))))

  ;; Pomodoro
  (use-package org-pomodoro
    :init (with-eval-after-load 'org-agenda
            (bind-key "P" 'org-pomodoro org-agenda-mode-map)))

  ;; Visually summarize progress
  (use-package org-dashboard
    :ensure t)
  )

(use-package org-archive
  :after org-agenda
  :ensure nil
  :config
  ;; 使用 org-archive-subtree 时，原来的 header 层级容易被打乱，而且容易
  ;; 因为保存不及时而导致 archive 文件内容丢失， 所以这个命令适合每月的
  ;; 大归档, 日常情况下，使用 ARCHIVE TAG 来隐藏已经完成的任务，安全又方便。
  ;; (setq org-archive-default-command 'org-archive-subtree)
  (setq org-archive-default-command 'org-archive-set-tag)
  )
  (use-package org-agenda
  :ensure nil
  :bind (("C-c a" . org-agenda)
         :map org-agenda-mode-map
         ("g" . org-agenda-redo-all)
         ("i" . (lambda () (interactive) (org-capture nil "s")))
         ("A" . org-agenda-archive-default-with-confirmation)
         ("J" . counsel-org-agenda-headlines)
         ("h" . ignore)
         ("y" . ignore)
         ("a" . ignore))
  :config
  (setq org-agenda-files '("~/Workspace/org"))
  ;; Set the agenda view to show the tasks on day/week/month/year
  (setq org-agenda-span 'week)
  ;; only keep agenda window,delete all other window
  (setq org-agenda-window-setup 'only-window)
  ;; (setq org-agenda-todo-ignore-scheduled t)
  ;; (setq org-agenda-todo-ignore-deadlines t)
  ;; (setq org-agenda-skip-deadline-if-done t)
  ;; (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-todo-list-sublevels t)
  ;; format 9:30-->09:30
  (setq org-agenda-time-leading-zero nil)
  (setq org-agenda-format-date "%Y-%m-%d %a----------------------------------------------------------------")
  ;; Custom commands for the agenda -- start with a clean slate.
  (setq org-agenda-custom-commands nil)
  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)
  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-scheduled-leaders
        '("计划任务 " "应在 %02d 天前开始 "))
  (setq org-agenda-deadline-leaders
        '("过期任务 " "将在 %02d 天后到期 " "已过期 %02d 天 "))
  )

  (provide 'init-org)
;;; config-org.el ends here
