(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
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
(setq read-process-output-max (* 1024 1024 3)) ;; 1mb
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


(defun my/org-present-start ()
  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun my/org-present-end ()
  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0))



(use-package exec-path-from-shell
  :ensure t
  :if (string= system-type "darwin")
  :config
  (exec-path-from-shell-initialize))


(use-package org-present
  :ensure t
  :config
  (add-hook 'org-present-mode-hook 'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook 'my/org-present-end))
(use-package visual-fill-column
  :ensure t)

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
(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1)
  :mode "\\.ledger\\'")
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
(use-package yaml-mode
  :ensure t)
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  (setq lsp-headerline-arrow "=>")
  (setq lsp-idle-delay 0.500)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
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

(use-package flymake-ruff
  :ensure t
  :hook (python-mode . flymake-ruff-load))
(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))
(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook #'lsp))

(use-package lsp-sourcekit
  :ensure t
  :after lsp-mode
  :config
  (when (string= system-type "darwin")
    (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))
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
  (when (not (string= system-type "gnu/linux"))
    (setq ls-lisp-use-insert-directory-program nil)
    (require 'ls-lisp))
  :custom
  (dired-listing-switches "-lDBXhgG --group-directories-first"))

(use-package magit
  :ensure t
  :config
  (setq git-commit-major-mode 'markdown-mode)
  :bind
  ("C-c g" . magit-file-dispatch)
  ("C-x g" . magit-status))

(use-package racket-mode
  :ensure t)
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))
(use-package haskell-mode
  :ensure t)
(use-package lua-mode
  :ensure t)
(use-package rust-mode
  :ensure t)
(use-package go-mode
  :ensure t)



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
  ;;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startupify-list nil))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

  
(use-package olivetti
  :ensure t)
(use-package writegood-mode
  :ensure t
  :bind
  ("C-c C-w" . 'writegood-mode))
(global-display-line-numbers-mode)

;;latitude and longitude
(setq calendar-latitude 43.544)
(setq calendar-longitude -80.2482)
(setq calendar-location-name "Guelph, ON")
;;Dictionary things
(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_CA")

;;Theming
(toggle-scroll-bar -1)

;;Org mode stuff
(setq org-agenda-include-diary t)
(use-package org-superstar
  :ensure t
  :config
  (setq org-superstar-headline-bullets-list '("✚" "◉" "○" "✸" "✿")     
        org-ellipsis " ↴ ")
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-chef
  :ensure t)

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(defun diary-last-day-of-month (date)
"Return `t` if DATE is the last day of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day-of-month
            (calendar-last-day-of-month month year)))
    (= day last-day-of-month)))

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
  (setq org-fold-catch-invisible-edits  'smart)
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
  ;; Capture templates
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

(defun my/org-present-prepare-buffer ()
  "Turn on `visual-fill-column-mode' and configure font sizes."
  (visual-fill-column-mode 1)
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text t)
  (setq-local visual-fill-column-center-text t)
  (display-line-numbers-mode -1)
  (global-display-line-numbers-mode))

(defun my/org-present-exit-cleanup ()
  "Turn off `visual-fill-column-mode' when exiting presentation."
  (setq-local visual-fill-column-center-text nil)
  (display-line-numbers-mode 1)
  (visual-fill-column-mode -1))

(with-eval-after-load 'org-present
  (add-hook 'org-present-mode-hook #'my/org-present-prepare-buffer)
  (add-hook 'org-present-mode-exit-hook #'my/org-present-exit-cleanup))

(defun my/org-present-prepare-buffer ()
  "Enable and configure visual-fill-column for centering."
  (setq-local visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun my/org-present-exit-cleanup ()
  "Disable visual-fill-column when exiting."
  (visual-fill-column-mode -1))

(with-eval-after-load 'org-present
  (add-hook 'org-present-mode-hook #'my/org-present-prepare-buffer)
  (add-hook 'org-present-mode-exit-hook #'my/org-present-exit-cleanup))


(setq-default c-default-style "linux"
	      c-basic-offset 4
	      indent-tabs-mode nil)

;;Various functions
(defun get-line-text ()
  "Get the text of the line you're currently on."
  (let ((p1 (line-beginning-position))
        (p2 (line-end-position)))
    (setq text (buffer-substring-no-properties p1 p2)))
  text)


;;Something to make go errors easier to write
(defun go-error-not-nil nil
  "Inserts a go err != nil block."
  (interactive)

  (let ((start (point))
        (set-point 0)
        (end 0))
    
    (if (not ((lambda (text)
                (string-equal (string-trim (get-line-text)) ""))
              (get-line-text)))
        (progn
          (move-end-of-line nil)
          (insert "\n")
          (setq start (point))))

    (indent-for-tab-command)
    (insert "if err != nil {\n")
    (indent-for-tab-command)
    (setq set-point (point))
    (insert "\n}")
    (setq end (point))
    (indent-region start end)
    (goto-char set-point)))

(define-key go-mode-map (kbd "C-c e") 'go-error-not-nil)


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
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(lsp-pyright flymake-ruff magit-section visual-fill-column org-present magit lsp-ui lsp-sourcekit lsp-java mos-mode lsp-ivy yaml-mode ledger ledger-mode slime eat emacs-eat anaconda-mode eshell-prompt-extras esh-autosuggest go-mode zig-mode zig lsp-haskell swift-mode typescript-mode typescript lsp-mode visual-regexp rust-mode emmet-mode all-the-icons which-key org-chef doom-theme mixed-pitch gcmh smartparens org-superstar org-appear writegood-mode solarized-theme pdf-tools olivetti nim-mode lua-mode kdeconnect ivy-avy highlight-defined helpful ebdb counsel company-c-headers autothemer auto-package-update ace-window))
 '(safe-local-variable-values '((org-emphasis-alist))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
