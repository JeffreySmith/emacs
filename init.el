(require 'use-package)
(setq load-prefer-newer t)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-native-compile t)
(add-to-list 'exec-path "~/.bin/")


(global-set-key (kbd "C-c c") "->")
(setq js-indent-level 2)
(setq typescript-indent-level 2)

(setq line-number-mode t)
(setq column-number-mode t)
(setq-default tab-width 4)
;;LSP performance tuning
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)
(setq lsp-log-io nil)

(setq url-debug t)



;;hides some annoying errors
(setq native-comp-async-report-warnings-errors nil)
(require 'ox-beamer)
(require 'ox-md)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (awk . t)
   (shell . t)))


(use-package emacs
  :bind
  ("C-=" . 'text-scale-increase)
  ("C--" . 'text-scale-decrease))
(use-package sqlite-mode
  :ensure nil
  :defer
  :bind (:map sqlite-mode-map
              ("n" . next-line)
              ("p" . previous-line)))
(use-package visual-regexp
  :bind
  ("C-c r" . 'vr/replace)
  ("C-c s" . 'vr/query-replace)
  :ensure t)
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))
(use-package mixed-pitch
  :ensure t)
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :config
  (add-hook 'JavaScript-mode-hook #'lsp)
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'swift-mode-hook #'lsp)
  (add-hook 'web-mode-hook #'lsp)
  (add-hook 'sh-mode-hook #'lsp)
  (lsp-enable-which-key-integration t))


(use-package lsp-sourcekit
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))
(use-package swift-mode
  :ensure t
  :hook (swift-mode . (lambda () (lsp))))
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  :commands lsp-ui-mode)
(use-package typescript-mode
  
  :ensure t
  :hook (typescript-mode . lsp-deferred)
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (setq typescript-indent-level 2))
;;Auto-update packages

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-interval 7)
  (auto-package-update-maybe))
        

(use-package mos-mode
  :ensure t)
(use-package anaconda-mode
  :ensure t)


(use-package eshell
  :ensure nil
  :bind
  ("C-c w" . eshell-isearch-backward))



(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)
(use-package eshell-prompt-extras
  
  :ensure t
  :config
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))
(use-package dired
  :ensure nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/opt/homebrew/bin/gls"))
  :custom
  (dired-listing-switches "-lDBXhgG --group-directories-first"))  

(use-package magit
  :ensure t
  :bind
  ("C-c g" . magit-file-dispatch)
  ("C-x g" . magit-status))
(if (not (eq system-type 'windows-nt))
 (use-package vterm
   :ensure t))
(use-package eat
  :ensure t)
(use-package cider
  :ensure t)
(use-package racket-mode
  :ensure t)
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))
(use-package haskell-mode
  :ensure t)
(use-package nim-mode
  :ensure t)
(use-package lua-mode
  :ensure t)
(use-package rust-mode
  :ensure t)
(use-package go-mode
  :ensure t)

(defalias 'go-err
   (kmacro "i f SPC e r r ! = SPC n i l SPC { <return> <return> } C-p <tab>"))


(global-set-key (kbd "C-c m") 'go-err)

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-quoting t
        web-mode-enable-current-element-highlight t)
  (add-to-list 'auto-mode-alist '("\\.ejs?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(defun disable-company ()
  "This will disable company in a particular mode"
  (company-mode -1))
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-no-png-images t
        treemacs-width 36)
  :bind
  ("C-c t" . treemacs))
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-global-modes '(not org-mode))
  (setq company-global-modes '(not magit-status-mode))
  (setq company-global-modes '(not magit-diff-mode))
  (setq company-global-modes '(not magit-mode))
  (setq company-global-modes '(not text-mode))
  (add-hook 'org-mode-hook #'disable-company)
  (add-hook 'eshell-mode-hook 'disable-company)
  (add-to-list 'company-backends 'company-clang)
  (add-to-list 'company-backends 'company-css)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (global-company-mode t))
(use-package zig-mode
  :ensure t)
  
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
  (setq ivy-use-selectable-prompt t)
  :bind
  ("C-c C-r" . ivy-resume))
(use-package lsp-ivy
  :ensure t)
(use-package avy
  :ensure t
  :bind
  ("C-c C-'" . 'avy-goto-char)
  ("C-c q" . 'avy-goto-char-2))
(use-package counsel
  :ensure t
  :config
  ;; This helps greatly with performance
  (setq swiper-use-visual-line-p #'ignore)
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-x C-f" . 'counsel-find-file)
  ("C-x l" . 'counsel-locate)
  ("<f1> l" . 'counsel-find-library)
  ("<f2> u" . 'counsel-unicode-char)
  ("C-s" . 'counsel-grep-or-swiper)
  :config
  (counsel-mode 1))
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . 'ace-window))

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
  ("C-h k" . 'helpful-key)
  ("C-h x" . 'helpful-command)
  ("C-c C-d" . 'helpful-at-point)
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 2)
  (which-key-mode))
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-week-agenda t)
  (setq dashboard-center-content t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-set-footer nil))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package org-superstar
  :ensure t
  :config
  (setq org-superstar-headline-bullets-list '("✚" "◉" "○" "✸" "✿")     
        org-ellipsis " ↴ ")
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
(use-package org-chef
  :ensure t)
  
(use-package olivetti
  :ensure t)
(use-package writegood-mode
  :ensure t
  :bind
  ("C-c C-w" . 'writegood-mode))
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
  (setq org-mobile-directory "~/Documents/org")
  ;;(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
  (setq org-agenda-files '( "~/org/capture.org" "~/org/acceldata/agenda.org" "~/org/acceldata/training.org" "~/org/courses.org"))
  ;;(setq org-log-into-drawer t
  ;;      org-clock-into-drawer "CLOCKING")
  (setq org-mobile-force-id-on-agenda-items nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-log-reschedule 'time)
  (setq org-log-done 'time)
  (setq org-export-with-smart-quotes t)
  (setq org-pretty-entities t)
  (setq org-startup-indented t)
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (setq org-catch-invisible-edits  'smart)
  (setq org-html-validation-link nil);;removes validate from the bottom of org exported html pages


  (setq org-refile-targets '((nil :maxlevel . 4)
          (org-agenda-files :maxlevel . 4)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-track-ordered-property-with-tag t)
  (setq org-latex-toc-command "\\tableofcontents \\clearpage")
  (setq org-publish-project-alist
        (list
         (list "org-website"
               :base-directory "~/org/publishing/"
               :publishing-directory "~/public_html"
               :publishing-function 'org-html-publish-to-html
               :section-numbers nil
               :with-toc nil)))
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
          ("j" "Journal entry" entry (file+olp+datetree
                                      "~/org/journal.org") "* %U - %^{Activity}\n-%?")
          ("c" "Cookbook capture")
          ("cb" "Breakfast Recipe" entry
           (file+headline "~/org/recipes/cookbook.org" "Breakfast")
           "%(org-chef-get-recipe-from-url)")
          ("cp" "Pasta Recipe" entry
           (file+headline "~/org/recipes/cookbook.org" "Pasta")
           "%(org-chef-get-recipe-from-url)")
          ("cd" "Dinner Recipe" entry
           (file+headline "~/org/recipes/cookbook.org" "Dinner")
           "%(org-chef-get-recipe-from-url)")
          ("cu" "Uncategorized" entry
           (file+headline "~/org/recipes/cookbook.org" "Uncategorized")
           "%(org-chef-get-recipe-from-url)")

)))


(setq-default c-default-style "linux"
	      c-basic-offset 4
	      indent-tabs-mode nil)
;; Capture templates

;;Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)

;;I tried this for something and I don't think it made a difference
(setq posframe-gtk-resize-child-frames 'resize-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(package-selected-packages
   '(slime eat emacs-eat anaconda-mode eshell-prompt-extras esh-autosuggest mos-mode go-mode zig-mode zig lsp-haskell swift-mode lsp-sourcekit lsp-ui lsp-ivy typescript-mode typescript lsp-mode visual-regexp vterm rust-mode emmet-mode all-the-icons which-key org-chef doom-theme mixed-pitch gcmh smartparens org-superstar org-appear writegood-mode solarized-theme pdf-tools olivetti nim-mode lua-mode kdeconnect ivy-avy highlight-defined helpful ebdb counsel company-c-headers autothemer auto-package-update ace-window))
 '(safe-local-variable-values '((org-emphasis-alist))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
