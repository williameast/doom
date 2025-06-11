(setq user-full-name "William East"
      user-mail-address "williameast@live.com")

(setq projectile-project-search-path '("~/org/projects"))

(setq org-directory "~/org/")

(setq auto-save-default t)

(setq doom-theme 'doom-xcode
      doom-themes-enable-bold t)

(setq doom-font (font-spec :family "JetBrainsMono" :size 14)
doom-unicode-font (font-spec :family "monospace" :size 12))

(defvar doom-modeline-icon (display-graphic-p))

(beacon-mode 1)

(setq display-line-numbers-type 'absolute)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

(setq window-combination-resize t)

(setq ispell-dictionary "en_GB")

(defun my/switch-to-de-dict ()
  (interactive)
  (ispell-change-dictionary "de_DE")
  (flyspell-buffer))

(defun my/switch-to-en-dict ()
  (interactive)
  (ispell-change-dictionary "en_GB")
  (flyspell-buffer))

(setq c-default-style "linux"
      c-basic-offset 4)

(add-hook! 'c-mode-common-hook
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'statement-case-open 0))

(load "~/.doom.d/42/list.el")
(load "~/.doom.d/42/string.el")
(load "~/.doom.d/42/comments.el")
(load "~/.doom.d/42/header.el")

(defun run-norminette-on-current-buffer ()
  (interactive)
  (defvar foo)
  (setq foo (concat "nm " (buffer-name) "" ))
  (shell-command foo))

(after! exec-path-from-shell
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK"))

(defun set-ssh-auth-sock ()
  "Set SSH_AUTH_SOCK environment variable."
  (interactive)
  (setenv "SSH_AUTH_SOCK" "/run/user/1000/keyring/ssh"))

(add-hook 'emacs-startup-hook 'set-ssh-auth-sock)

(setq +treemacs-git-mode 'deferred)

(defun sa-find-org-file-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
     (case-fold-search t)         ; filesystems are case sensitive
     (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backup files
     (filext (or filext "org$\\\|org_archive"))
     (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
     (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir) ; regular files
    (if (string-match fileregex file-or-dir) ; org files
        (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
    (dolist (org-file (sa-find-org-file-recursively file-or-dir filext)
              org-file-list) ; add files found to result
      (add-to-list 'org-file-list org-file)))))))

(setq org-agenda-files
      (append (sa-find-org-file-recursively org-directory)))

(after! org
  (use-package! org-super-agenda
    :after org-agenda
    :init
    (setq org-habit-show-done-always-green 't
          org-agenda-prefix-format
          '((agenda . " %?-12t% s")
            (todo . " %i %-12:c")
            (tags . " %i %-12:c")
            (search . " %i %-12:c")))
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-start-day "+0d")
    (setq org-agenda-span 'day)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-start-on-weekday nil)
    (setq org-agenda-dim-blocked-tasks nil) ;; makes main tasks visible in agenda-view
    (setq org-super-agenda-groups
          '((:name "Due today"
             :deadline today)
            (:name "Overdue"
             :deadline past)
            (:name "Due soon"
             :deadline future)
            (:name "Habits"
             :habit t)
            (:name "Start today"
             :scheduled today)
            (:name "Start soon"
             :scheduled future)
            (:name "Reschedule or review"
             :scheduled past)
            ))
    :config
    (org-super-agenda-mode)))

(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t))

(setq org-journal-dir "~/org/journal"
      org-journal-date-prefix "#+TITLE: "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-date-format "%A, %d %B %Y")

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq org-export-with-smart-quotes t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)  ;; with AUCTeX LaTeX mode

(map!
 ("M-q" #'kill-current-buffer)
 ("M-w" #'save-buffer)
 ("M-d" #'fill-paragraph)
 ("M-s" #'up-list)
 :leader
 (:desc "switch to treemacs" "-" #'treemacs-select-window)
 (:prefix-map ("z" . "Custom")
  :desc "Change to german" "g" #'my/switch-to-de-dict
  :desc "Change to english" "e" #'my/switch-to-en-dict
  :desc "Add comment box" "b" #'comments-insert-box
  :desc "Add header" "h" #'header-insert
  :desc "Add comment bar" "n" #'comments-insert-bar
  :desc "Tabify" "t" #'tabify
  :desc "Open Treemacs" "a" #'treemacs
  :desc "Run norminette" "n" #'run-norminette-on-current-buffer))

(map!
 :map org-mode-map
 :leader
 (:desc "Tabify" "t" #'tabify
  :desc "Open Treemacs" "a" #'treemacs
  :desc "Run norminette" "n" #'run-norminette-on-current-buffer))
