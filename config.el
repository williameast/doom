(setq user-full-name "William East"
      user-mail-address "williameast@live.com")

(setq ispell-dictionary "en_GB")

(global-set-key (kbd "C-c N")
  (lambda()(interactive)
    (ispell-change-dictionary "german")
    (flyspell-buffer)))

(setq-default
 window-combination-resize t            ; take window space from all windows not just selected
 x-stretch-cursor t)                    ; stretch cursor to the glyph width

(setq auto-save-default t
      truncate-string-ellipsis "…")     ; not working currently

(setq line-spacing 0.2)                 ; how great the distance between lines should be

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))

(setq +ivy-buffer-preview t)

(setq doom-font (font-spec :family "monospace" :size 14))

(setq doom-theme 'doom-one)

;; battery indicator
(unless (equal "Battery Status not available"
               (battery))
  (display-battery-mode 1))

;; Time display
(display-time-mode 1)


;; Hide encoding unless it is not UTF-8

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(setq display-line-numbers-type 'relative) ;; either `nil' or `relative'

;; set the org directory. must be done before org loads!
(setq org-directory "~/org/")

;; set line wrap to occur at 80 characters, I think
(add-hook 'text-mode-hook 'auto-fill-mode)

;; set level of subtrees to 9
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; set shortcut for agenda view

(setq TeX-auto-save t) ; enable parse on load
(setq TeX-parse-self t) ; enable parse on save
(setq-default TeX-master nil)

(setq reftex-default-bibliography "/home/weast/org/projects/Coding/Latex/testbill/bib.bib") ;; change the path
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)  ;; with AUCTeX LaTeX mode

(use-package! transcription-mode)

;; Each path is relative to `+mu4e-mu4e-mail-path', which is ~/.mail by default
(set-email-account! "live.com"
  '((mu4e-sent-folder       . "/live/Sent")
    (mu4e-drafts-folder     . "/live/Drafts")
    (mu4e-trash-folder      . "/live/Deleted")
    (mu4e-refile-folder     . "/live/inbox")
    (smtpmail-smtp-user     . "williameast@live.com")
    (mu4e-compose-signature . "---\nWilliam East"))
  t)
