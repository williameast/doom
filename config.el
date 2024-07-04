(setq user-full-name "William East"
      user-mail-address "williameast@live.com")

(setq projectile-project-search-path '("/home/weast"
                                       "~/org/projects"))

(setq org-directory "~/org/")

;; If you ever need it, this is how you set your reftex bib for bibliography management.
;; (setq reftex-default-bibliography "/home/weast/org/projects/Coding/Latex/testbill/bib.bib") ;; change the path

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
      (append (sa-find-org-file-recursively "~/org")))

(setq auto-save-default t)

(setq doom-font (font-spec :family "JetBrainsMono" :size 14)
doom-unicode-font (font-spec :family "monospace" :size 12))


(defvar doom-modeline-icon (display-graphic-p)
  "Whether show `all-the-icons' or not.

Non-nil to show the icons in mode-line.
The icons may not be showed correctly in terminal and on Windows.")

(beacon-mode 1)

(setq doom-theme 'doom-xcode
      doom-themes-enable-bold t)

(unless (equal "Battery Status not available"
               (battery))
  (display-battery-mode 1))

(display-time-mode 1)

(setq display-time-24hr-format t
      display-time-default-load-average nil)

(defun doom-modeline-conditional-buffer-encoding ()
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq display-line-numbers-type 'relative)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

(setq window-combination-resize t)

(add-hook! 'text-mode-hook 'auto-fill-mode)

(require 'company-tabnine)

(after! company
  (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
  (setq company-show-numbers t
        company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-quick-access t
        company-quick-access-modifier 'super))


(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

(after! company-box
  (setq company-box-max-candidates 10))

(setq ispell-dictionary "en_GB")

(defun my/switch-to-de-dict ()
  (interactive)
  (ispell-change-dictionary "de_DE")
  (flyspell-buffer))

(defun my/switch-to-en-dict ()
  (interactive)
  (ispell-change-dictionary "en_GB")
  (flyspell-buffer))

(after! org
  (defun org-babel-tangle-jump ()
    "Jump to tangle file for the source block at point."
    (interactive)
    (let (file org-babel-pre-tangle-hook org-babel-post-tangle-hook)
      (cl-letf (((symbol-function 'write-region) (lambda (start end filename &rest _ignore)
                                                   (setq file filename)))
                ((symbol-function 'delete-file) #'ignore))
        (org-babel-tangle '(4)))
      (when file
        (setq file (expand-file-name file))
        (if (file-readable-p file)
            (find-file file)
          (error "Cannot open tangle file %S" file))))))

(after! org
  (setq org-src-window-setup 'current-window
        org-babel-python-command "python3"))

(after! geiser-mode
    (setq geiser-active-implementations '(mit)))

(setq org-use-property-inheritance t
      org-list-allow-alphabetical t
      org-export-in-background t
      org-indent-mode t
      org-catch-invisible-edits 'smart)
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

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

(use-package! org-appear)

(add-hook! org-mode :append 'org-appear-mode)

(after! org
  (setq org-hide-emphasis-markers t))

(use-package! transcription-mode)

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang stringp)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
  '("go" "python" "ipython" "bash" "sh"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq org-export-with-smart-quotes t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)  ;; with AUCTeX LaTeX mode

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil))
    (dolist (ext org-view-output-file-extensions)
      (unless output-file
        (when (file-exists-p
               (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) org-view-external-file-extensions)
            (browse-url-xdg-open output-file)
          (pop-to-buffer (or (find-buffer-visiting output-file)
                             (find-file-noselect output-file))))
      (message "No exported file found"))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Search for output files with these extensions, in order, viewing the first that matches")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")

(use-package pdf-view
  :hook (pdf-tools-enabled . pdf-view-themed-minor-mode))

(require 'latex-preview-pane)
(latex-preview-pane-enable)
(setq csv-separator-separators '["," ";"])

(setq +treemacs-git-mode 'deferred)

(after! exec-path-from-shell
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK"))

(defun set-ssh-auth-sock ()
  "Set SSH_AUTH_SOCK environment variable."
  (interactive)
  (setenv "SSH_AUTH_SOCK" "/run/user/1000/keyring/ssh"))

(add-hook 'emacs-startup-hook 'set-ssh-auth-sock)

(map!
 ("M-q" #'kill-current-buffer)
 ("M-w" #'save-buffer)
 ("M-d" #'fill-paragraph)
 ("M-s" #'up-list)
 :leader
 (:desc "switch to treemacs" "-" #'treemacs-select-window)
 (:prefix-map ("d" . "42")
  :desc "Change to german" "g" #'my/switch-to-de-dict
  :desc "Change to english" "e" #'my/switch-to-en-dict
  :desc "Add comment box" "b" #'comments-insert-box
  :desc "Add header" "h" #'header-insert
  :desc "Add comment bar" "n" #'comments-insert-bar)
 ;; (:prefix ("v" . "custom")
 ;;  :desc "Open treemacs" "t" #'treemacs)
 )

(map!
 :map org-mode-map
 (:leader
  (:prefix ("t" . "toggle/tangle")
   :desc "Tangle src blocks" "t" #'org-babel-tangle
   :desc "Jump to src block" "j" #'org-babel-tangle-jump
   :desc "Run C program" "c" #'execute-c-program
   :desc "Run C program without ftlib" "x" #'execute-c-program-without-ftlib
   :desc "Tabify" "t" #'tabify
   :desc "Open Treemacs" "a" #'treemacs
   :desc "Run norminette" "n" #'run-norminette-on-current-buffer
   :desc "Detangle" "d" #'org-babel-detangle )
  (:prefix ("m" . "view")
   :desc "View exported file" "v" #'org-view-output-file )
  (:prefix ("a" . "archive")
   :desc "Archive tree" "a" )))

(load "~/.config/doom/42/list.el")
(load "~/.config/doom/42/string.el")
(load "~/.config/doom/42/comments.el")
(load "~/.config/doom/42/header.el")

;; (require 'flycheck)
;; (flycheck-define-checker norminette
;;   :command ("normised" source)
;;   :error-patterns
;;   ((error line-start "Error (line " line ", col " column "): " (message) line-end))
;;   :modes c-mode
;;   :next-checkers ((error . c/c++-clang)
;; 				  (warning . c/c++-cppcheck)))
;; (add-to-list 'flycheck-checkers 'norminette)
;;Run C programs directly from within emacs
(defun execute-c-program ()
  (interactive)
  (defvar foo)
  (setq foo (concat "cc -Wextra -Werror -Wall " (buffer-name) " libft.a && ./a.out" ))
  (shell-command foo))

(defun execute-c-program-without-ftlib ()
  (interactive)
  (defvar foo)
  (setq foo (concat "cc -Wextra -Werror -Wall " (buffer-name) " && ./a.out" ))
  (shell-command foo))

(defun run-norminette-on-current-buffer ()
  (interactive)
  (defvar foo)
  (setq foo (concat "norminette " (buffer-name) "" ))
  (shell-command foo))

(c-set-offset 'substatement-open 0)
(setq c-default-style "linux"
      c-basic-offset 4)

;; this fixes the problem with the bases
 (map! :after cc-mode
      :map c-mode-base-map
      "{" #'c-electric-brace)
