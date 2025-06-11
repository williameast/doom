;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el


;; (package! pdf-tools :recipe
;;   (:host github
;;    :repo "dalanicolai/pdf-tools"
;;    :branch "pdf-roll"
;;    :files ("lisp/*.el"
;;            "README"
;;            ("build" "Makefile")
;;            ("build" "server")
;;            (:exclude "lisp/tablist.el" "lisp/tablist-filter.el"))))

;; agenda, improved
(package! org-super-agenda)
;; debugging. WIP.
(package! dap-mode)
;; show me where I am using a blinking cursor!
(package! beacon)
