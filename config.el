(setq user-full-name "William East"
      user-mail-address "williameast@live.com")

(setq projectile-project-search-path '("/home/weast"
                                       "~/org/projects"))

(setq org-directory "~/org/"
      org-agenda-files "home/weast/org/todo.org")

(setq reftex-default-bibliography "/home/weast/org/projects/Coding/Latex/testbill/bib.bib") ;; change the path

(setq auto-save-default t)

(setq doom-font (font-spec :family "monospace" :size 14))

(setq doom-theme 'doom-one
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

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

(setq window-combination-resize t)

(add-hook! 'text-mode-hook 'auto-fill-mode)

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

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(after! mu4e
  (setq mu4e-compose-complete-addresses 't
        mu4e-use-fancy-chars 'nil
        mu4e-sent-messages-behavior 'sent
        mu4e-compose-format-flowed 't
        mu4e-update-interval 300
        mu4e-attachment-dir "~/Downloads/"
        mu4e-view-html-plaintext-ratio-heuristic 10000
        smtpmail-debug-info 't
        mml-secure-openpgp-encrypt-to-self 't)

  (set-email-account! "Live"
                      '((user-mail-address              . "williameast@live.com")
                        (user-full-name                 . "William East")
                        (mu4e-sent-folder               . "/Live/Sent")
                        (mu4e-drafts-folder             . "/Live/Drafts")
                        (mu4e-trash-folder              . "/Live/Deleted")
                        (mu4e-refile-folder             . "/Live/Archive")
                        (smtpmail-smtp-user             . "williameast@live.com")
                        (smtpmail-smtp-server           . "smtp-mail.outlook.com")
                        (smtpmail-stream-type           . ssl)
                        (smtpmail-smtp-service          . 587))
                      t)
  (set-email-account! "McGill"
                      '((user-mail-address              . "william.east@mail.mcgill.ca")
                        (user-full-name                 . "William East")
                        (mu4e-sent-folder               . "/McGill/Sent")
                        (mu4e-drafts-folder             . "/McGill/Drafts")
                        (mu4e-trash-folder              . "/McGill/Trash")
                        (mu4e-refile-folder             . "/McGill/Archive")
                        (smtpmail-smtp-user             . "williameast@live.com")
                        (smtpmail-smtp-server           . "outlook.office365.com")
                        (smtpmail-stream-type           . ssl)
                        (smtpmail-smtp-service          . 587))
                      t))

(add-hook 'mu4e-compose-mode-hook (lambda () (use-hard-newlines -1)))

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

(after! geiser-mode
    (setq geiser-active-implementations '(mit)))

(setq org-use-property-inheritance t
      org-list-allow-alphabetical t
      org-export-in-background t
      org-catch-invisible-edits 'smart)
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

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

(use-package! doct
  :commands (doct))

(after! org-capture
  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (defun set-org-capture-templates ()
    (setq org-capture-templates
          (doct `(("Personal todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a")
                   )
                  ("Personal note" :keys "n"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a"))
                  ;; ("Email" :keys "e"
                  ;;  :icon ("envelope" :set "faicon" :color "blue")
                  ;;  :file +org-capture-todo-file
                  ;;  :prepend t
                  ;;  :headline "Inbox"
                  ;;  :type entry
                  ;;  :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                  ;;             "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                  ;;             "about %^{topic}"
                  ;;             "%U %i %a"))
                  ("Interesting" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :icon ("globe" :set "faicon" :color "green")
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web"
                               )
                              ("Article" :keys "a"
                               :icon ("file-text" :set "octicon" :color "yellow")
                               :desc ""
                               :i-type "read:reaserch"
                               )
                              ("Information" :keys "i"
                               :icon ("info-circle" :set "faicon" :color "blue")
                               :desc ""
                               :i-type "read:info"
                               )
                              ("Idea" :keys "I"
                               :icon ("bubble_chart" :set "material" :color "silver")
                               :desc ""
                               :i-type "idea"
                               )))
                  ("Tasks" :keys "k"
                   :icon ("inbox" :set "octicon" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra ""
                               )
                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t"
                               )
                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t"
                               )
                              ))
                  ("Stuff for Others" :keys "o"
                   :icon ("person" :set "octicon" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Stuff for others"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra ""
                               )
                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t"
                               )
                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t"
                               )
                              ))
                  ("Project" :keys "p"
                   :icon ("repo" :set "octicon" :color "silver")
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children (("Project-local todo" :keys "t"
                               :icon ("checklist" :set "octicon" :color "green")
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              ("Project-local note" :keys "n"
                               :icon ("sticky-note" :set "faicon" :color "yellow")
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              ("Project-local changelog" :keys "c"
                               :icon ("list" :set "faicon" :color "blue")
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file))
                   )
                  ("\tCentralised project templates"
                   :keys "o"
                   :type entry
                   :prepend t
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :children (("Project todo"
                               :keys "t"
                               :prepend nil
                               :time-or-todo "TODO"
                               :heading "Tasks"
                               :file +org-capture-central-project-todo-file)
                              ("Project note"
                               :keys "n"
                               :time-or-todo "%U"
                               :heading "Notes"
                               :file +org-capture-central-project-notes-file)
                              ("Project changelog"
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-central-project-changelog-file))
                   )))))

  (set-org-capture-templates)
  (unless (display-graphic-p)
    (add-hook 'server-after-make-frame-hook
              (defun org-capture-reinitialise-hook ()
                (when (display-graphic-p)
                  (set-org-capture-templates)
                  (remove-hook 'server-after-make-frame-hook
                               #'org-capture-reinitialise-hook))))))

(after! org-roam
  (setq org-roam-directory "~/org/roam/"
        org-roam-db-location "~/org/roam/.roam.db"
        ;; don't match my private org stuff
        org-roam-file-exclude-regexp "/org"))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq org-export-with-smart-quotes t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)  ;; with AUCTeX LaTeX mode

;; CV
(use-package ox-moderncv
  :load-path "/home/weast/org/admin/CV/org-cv"
  :init (require 'ox-moderncv))

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

(setq yas-triggers-in-field t)

(map!
 ("M-q" #'kill-current-buffer)
 ("M-w" #'save-buffer)
 :leader
 (:prefix-map ("d" . "dictionary")
  :desc "Change to german" "g" #'my/switch-to-de-dict
  :desc "Change to english" "e" #'my/switch-to-en-dict)
 ;; (:prefix ("f" . "file")
 ;;  :desc "Open neotree" "t" #'+neotree/open)
 )

(map!
 :map org-mode-map
 (:leader
  (:prefix ("t" . "toggle/tangle")
   :desc "Tangle src blocks" "t" #'org-babel-tangle
   :desc "Jump to src block" "j" #'org-babel-tangle-jump
   :desc "Detangle" "d" #'org-babel-detangle )
  (:prefix ("m" . "view")
   :desc "View exported file" "v" #'org-view-output-file )
  (:prefix ("a" . "archive")
   :desc "Archive tree" "a" )))
