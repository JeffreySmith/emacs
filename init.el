(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
(require 'use-package)
(setq load-prefer-newer t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-native-compile t)

(dolist (path '("/opt/homebrew/opt/openjdk/bin"
                "/Users/jeffrey/.opam/default/bin"
                "/usr/local/go/bin"
                "/usr/local/go"
                "/opt/homebrew/opt/libpq/bin"
                "/opt/homebrew/opt/ruby/bin"
                "/Users/jeffrey/.local/bin"
                "/opt/homebrew/bin"
                "/opt/homebrew/sbin"
                "/usr/local/bin"
                "/opt/pmk/env/global/bin"
                "/Library/TeX/texbin"
                "/opt/devkitpro/tools/bin"
                "/Users/jeffrey/.cargo/bin"
                "/Users/jeffrey/.go/bin"
                "/Users/jeffrey/.local/bin"
                "/Users/jeffrey/.zig"
                "/Users/jeffrey/go/bin"))
  (add-to-list 'exec-path path)
  (setenv "PATH" (concat path ":" (getenv "PATH"))))

(use-package emacs
  :custom
  (context-menu-mode t)
  (tab-always-indent 'complete)
  (enable-recursive-minibuffers t)
  ;;(read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  :bind
  ("C-=" . 'text-scale-increase)
  ("C--" . 'text-scale-decrease))

(setq js-indent-level 2)
(setq typescript-indent-level 2)

(setq line-number-mode t)
(setq column-number-mode t)
(setq-default tab-width 4)

;;LSP performance tuning
<<<<<<< HEAD
(setq read-process-output-max (* 1024 1024 3)) ;; 1mb
(setq gc-cons-threshold 100000000)
(setq lsp-log-io nil)

(setq url-debug t)

=======
(setq read-process-output-max (* 1024 1024)) ;; 1mb
>>>>>>> 54f2136 (Many config improvements and updates)


;;hides some annoying errors
(setq native-comp-async-report-warnings-errors nil)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (awk . t)
   (shell . t)))


<<<<<<< HEAD
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
=======

>>>>>>> 54f2136 (Many config improvements and updates)
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
  :custom
  (gcmh-high-cons-threshold (* 64 1024 1024))
  (gcmh-idle-delay 5)
  :config
  (gcmh-mode 1))

(use-package mixed-pitch
  :ensure t)
<<<<<<< HEAD
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
=======
>>>>>>> 54f2136 (Many config improvements and updates)

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (elixir-mode . elixir-ts-mode)
        (c++-mode . c++-ts-mode)
        (go-mode . go-ts-mode)
        (rust-mode . rust-ts-mode)
        (js-mode . js-ts-mode)
        (lua-mode . lua-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (css-mode . css-ts-mode)))

(use-package eglot
  :init
  (setq eglot-autoshutdown t)
  :ensure t
  :hook ((
          c-ts-mode
          clojure-ts-mode
          go-ts-mode
          elixir-ts-mode
          haskell-mode
          java-ts-mode
          js-ts-mode
          lua-ts-mode
          python-ts-mode
          rust-ts-mode
          typescript-ts-mode
          zig-mode
          )
         . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(rust-ts-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "language_server.sh")))

;; `cargo install emacs-lsp-booster` is required for this
(use-package eglot-booster
  :vc (eglot-booster
       :url "https://github.com/jdtsmith/eglot-booster"
       :branch "main"
       :rev :newest)
  :after eglot
  :init
  (setq eglot-booster-no-remote-boost t)
  (setq eglot-booster-io-only t)
  :config
  (eglot-booster-mode))

(setq treesit-language-source-alist
      '(
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
        (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
        (heex . ("https://github.com/phoenixframework/tree-sitter-heex"))
        (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
        (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
        (zig . ("https://github.com/tree-sitter-grammars/tree-sitter-zig"))))

(use-package jarchive
  :ensure t
  :after eglot
  :config
  (jarchive-setup))

(use-package clojure-ts-mode
  :ensure t
  :hook
  (clojure-ts-mode . eglot-ensure))

(use-package cider
  :ensure t)

(use-package elixir-ts-mode
  :ensure t
  :hook
  (elixir-ts-mode . eglot-ensure)
  (elixir-ts-mode . (lambda ()
                      (push '("|>" . ?\u25B7) prettify-symbols-alist)
                      (add-hook 'before-save-hook #'eglot-format nil t))))

(use-package typescript-mode
  
  :ensure t
  :hook (typescript-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (setq typescript-indent-level 2))
;;Auto-update packages

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-interval 7)
  (auto-package-update-maybe))
        
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

<<<<<<< HEAD
=======
(use-package eat
  :ensure t)

>>>>>>> 54f2136 (Many config improvements and updates)
(use-package racket-mode
  :ensure t)

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :ensure t)

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package haskell-mode
  :ensure t)

(use-package lua-mode
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

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 20)
  (vertico-scroll-margin 0)
  ;;(keymap-set vertico-map "TAB" #'minibuffer-complete)
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion--category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Font Mono") nil 'append)
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


(use-package consult
  :ensure t
  :bind(
;;  ("M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c m" . consult-man)
  ("C-c i" . consult-info)
  ([remap Info-search] . consult-info)
  ;; M-# bindings for registers
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store)
  ("C-M-#" . consult-register)
  ("C-s"     . consult-line)
  ("C-x b"   . consult-buffer)
  ;; `goto-map`
  ("M-g e" . consult-compile-error)
  ("M-g r" . consult-grep-match)
  ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
  ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; Bindings in `search-map`
  ("M-s d" . consult-find)
  ("M-s c" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)


  ("C-x C-f" . find-file)

  ;; Isearch integration
  ("M-s e" . consult-isearch-history)
  :map isearch-mode-map
  ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
  ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
  ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
  ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch

  :map minibuffer-local-map
  ("M-s" . consult-history)
  ("M-r" . consult-history))
  :init
  (setq consult-preview-key "M-.")
  (advice-add #'register-preview :override #'consult-register-window)

  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file))
   ;; :preview-key "M-."
   ;;:preview-key '(:debounce 0.4 any)))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-auto t)
  (corfu-auto-delay .4)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary t)
  (corfu-on-exact-match 'insert)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))


(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :init
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  :config
  (setq cape-dabbrev-check-other-buffers nil))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  (context-menu-mode 1)
  (add-hook 'context-menu-functions #'embark-context-menu 100)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t)

(use-package zig-mode
  :ensure t)

(use-package avy
  :ensure t
  :bind
  ("C-c C-'" . 'avy-goto-char)
  ("C-c q" . 'avy-goto-char-2))

(use-package ace-window
  :ensure t
  :bind
  ("M-o" . 'ace-window))


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
<<<<<<< HEAD
  ;;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-startupify-list nil))
=======
  (setq dashboard-set-footer nil))

>>>>>>> 54f2136 (Many config improvements and updates)
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
(setq calendar-latitude 43.443978)
(setq calendar-longitude -80.471030)
(setq calendar-location-name "Kitchener, ON")
;;Dictionary things
(when (not (string= system-type "gnu/linux"))
  (setq ispell-program-name "/opt/homebrew/bin/hunspell"))
(when (string= system-type "gnu/linux")
  (setq ispell-program-name "hunspell"))
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

<<<<<<< HEAD
(defun diary-last-day-of-month (date)
"Return `t` if DATE is the last day of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day-of-month
            (calendar-last-day-of-month month year)))
    (= day last-day-of-month)))

=======
>>>>>>> 54f2136 (Many config improvements and updates)
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
  (require 'ox-beamer)
  (require 'ox-md)
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

;;(define-key go-ts-mode-map (kbd "C-c e") 'go-error-not-nil)


;;Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
<<<<<<< HEAD
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(lsp-pyright flymake-ruff magit-section visual-fill-column org-present magit lsp-ui lsp-sourcekit lsp-java mos-mode lsp-ivy yaml-mode ledger ledger-mode slime eat emacs-eat anaconda-mode eshell-prompt-extras esh-autosuggest go-mode zig-mode zig lsp-haskell swift-mode typescript-mode typescript lsp-mode visual-regexp rust-mode emmet-mode all-the-icons which-key org-chef doom-theme mixed-pitch gcmh smartparens org-superstar org-appear writegood-mode solarized-theme pdf-tools olivetti nim-mode lua-mode kdeconnect ivy-avy highlight-defined helpful ebdb counsel company-c-headers autothemer auto-package-update ace-window))
 '(safe-local-variable-values '((org-emphasis-alist))))
=======
 '(package-selected-packages '(eglot-booster)))
>>>>>>> 54f2136 (Many config improvements and updates)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
