(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-native-compile t)
(add-to-list 'exec-path "~/.bin/")
;;(load "~/.emacs.d/custom.el")


(setq native-comp-async-report-warnings-errors nil)

(use-package emacs
   :bind
   ("C-=" . 'text-scale-increase)
   ("C--" . 'text-scale-decrease))
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))
(use-package mixed-pitch
  :ensure t)

;;Auto-update packages
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-interval 2)
  (auto-package-update-maybe))

;; Evil settings
(use-package evil
  :ensure t
  :init ;; I'm changing some things before it loads. They have to be
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-keybinding nil)
  :config ;; Tweak after it loads
  (evil-mode)
  (add-hook 'org-log-buffer-setup-hook 'evil-insert-state)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (evil-set-initial-state 'vterm-mode 'insert)
  (evil-set-initial-state 'dired-mode 'insert)  
  (evil-set-initial-state 'helpful-mode 'insert)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (evil-set-initial-state 'git-commit-mode 'insert))
(use-package evil-collection
  :ensure t
  )
(use-package magit
  :ensure t
  :bind
  ("C-c g" . magit-file-dispatch)
  ("C-x g" . magit-status))
(use-package vterm
  :ensure t)
(use-package cider
  :ensure t)
(use-package geiser-guile
  :ensure t)
(use-package racket-mode
  :ensure t)
(use-package nim-mode
  :ensure t)
(use-package lua-mode
  :ensure t)
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))


(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-clang)
  (global-company-mode t))
(use-package company-box
  :disabled t)
(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-s" . 'swiper))
(use-package avy
  :ensure t
  :bind
  ("C-c C-'" . 'avy-goto-char)
  ("C-c q" . 'avy-goto-char-2))
(use-package counsel
  :ensure t
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-x C-f" . 'counsel-find-file)
  ("C-x l" . 'counsel-locate)
  ("<f1> l" . 'counsel-find-library)
  ("<f2> u" . 'counsel-unicode-char)
  :config
  (counsel-mode 1))
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . 'ace-window))
(use-package kdeconnect
  :ensure t
  :config
  (setq kdeconnect-active-device "29d7fe6628f4b696"))
(use-package speed-type
  :ensure t)
(use-package highlight-defined
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config))
(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")
(use-package pdf-tools
  :ensure t)
(use-package helpful
  :ensure t
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-variable)
  ("C-c C-d" . 'helpful-at-point))
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 4)
  (which-key-mode))
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-set-footer nil))
(use-package org-superstar
  :ensure t
  :config
  ;(setq org-superstar-headline-bullets-list '("✖" "✚" "◉" "○" "▶")
  (setq org-superstar-headline-bullets-list '("✚" "◉" "○" "✸" "✿")     
        ;; org-superstar-special-todo-items t
        org-ellipsis " ↴ ")
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
(use-package org-chef
  :ensure t)
  
(use-package olivetti
  :ensure t)
(use-package writegood-mode
  :ensure t
  :bind
  ("C-c C-g" . 'writegood-mode))
(global-display-line-numbers-mode)

;;latitude and longitude
(setq calendar-latitude 43.6)
(setq calendar-longitude -79.3)
(setq calendar-location-name "Toronto, ON")
;;Dictionary things
(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_CA")

;;Theming
(toggle-scroll-bar -1)

;;Org mode stuff

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))


(use-package org
  :hook
  (org-mode . visual-line-mode)
   (org-mode . toggle-word-wrap)
   (org-mode . flyspell-mode)
   (prettify-symbols-mode)
  :bind
  ("C-c a" . 'org-agenda)
  ("C-c l" . 'org-store-link)
  ("<f6>" . 'org-capture)
  :config
  (setq org-directory "~/org")
  (setq org-mobile-directory "~/Dropbox/org")
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))  
  (setq org-log-into-drawer t
        org-clock-into-drawer "TIMESPENT")
  (setq org-mobile-force-id-on-agenda-items nil)
  (setq org-log-reschedule 'time)
  (setq org-log-done 'time)
  (setq org-export-with-smart-quotes t)
  (setq org-pretty-entities t)
  (setq org-startup-indented t)
  (setq org-hide-emphasis-markers t)
  (setq org-catch-invisible-edits  'smart)
  
  (setq org-refile-targets
        '((nil :maxlevel . 4)
          (org-agenda-files :maxlevel . 4)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")
  (setq org-capture-templates
        '(("n" "Test template")
          ("nt" "TODO entry" entry
           (file+headline "~/org/test.org" "Capture")
           "* TODO %^{Description} :NEW:\nDEADLINE: %^t\n%?\n:LOGBOOK:\n- Added: %U\n:END:")
          ("nb" "Add book to list" entry
           (file+headline "~/org/test.org" "Books to read")
           "* %^{Author} - %^{Title}\n:PROPERTIES:\n:AUTHOR:   %\\1\n:TITLE:    %\\2 \n:END:\n- %?\n:LOGBOOK:\n- Added: %U\n:END:")
          ;;("nb" "Basic entry" entry
           ;;(file+headline "~/org/test.org" "Capture")
           ;;"* %^{Description} :NEW:\n%?\n:LOGBOOK:\n- Added: %U\n:END:")
          ("c" "Cookbook capture")
          ("cb" "Breakfast Recipe" entry
           (file+headline "~/org/recipes/cookbook.org" "Breakfast")
           "%(org-chef-get-recipe-from-url)")
          ("cp" "Pasta Recipe" entry
           (file+headline "~/org/recipes/cookbook.org" "Pasta")
           "%(org-chef-get-recipe-from-url)")
          ("cu" "Uncategorized" entry
           (file+headline "~/org/recipes/cookbook.org" "Uncategorized")
           "%(org-chef-get-recipe-from-url)")

          )))


;; (add-hook 'org-mode-hook '(lambda ()
;;                           (visual-line-mode)
;;                           (toggle-word-wrap)
;;                           (org-indent-mode)
;;                           (flyspell-mode)))
(setq-default c-default-style "linux"
	      c-basic-offset 4
	      indent-tabs-mode nil)
;; Capture templates

;;Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
;;Move variables set by emacs automatically into another file so this one looks clean 
;;(setq custom-file (concat user-emacs-directory "/custom.el"))

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/bin/sbcl")

;;I tried this for something and I don't think it made a difference
(setq posframe-gtk-resize-child-frames 'resize-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/jeffrey/org/recipes/breakfast potatoes.org" "/home/jeffrey/org/recipes/cookbook.org" "/home/jeffrey/org/capture.org" "/home/jeffrey/org/darksouls.org" "/home/jeffrey/org/test.org"))
 '(package-selected-packages
   '(which-key org-chef doom-theme mixed-pitch gcmh smartparens org-superstar org-appear writegood-mode vterm use-package speed-type solarized-theme racket-mode pdf-tools olivetti nim-mode magit lua-mode kdeconnect ivy-avy highlight-defined helpful geiser-guile evil-collection ebdb dashboard counsel company-c-headers cmake-mode cider autothemer auto-package-update ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
