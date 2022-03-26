(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-native-compile t)
(add-to-list 'exec-path "~/.bin/")


;;Auto-update packages
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
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
  (evil-set-initial-state 'vterm-mode 'insert)
  (evil-set-initial-state 'dired-mode 'insert)  
  (evil-set-initial-state 'helpful-mode 'insert)
  (evil-set-initial-state 'git-commit-mode 'insert))
(use-package evil-collection
  :ensure t
  )
(use-package magit
  :ensure t
  :bind
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
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-set-footer nil))

(global-display-line-numbers-mode)


;;Dictionary things
(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_CA")

;;Theming
(toggle-scroll-bar -1)

;;Org mode stuff
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(setq org-log-into-drawer t)
(add-hook 'org-mode-hook '(lambda ()
                          (visual-line-mode)
                          (toggle-word-wrap)
                          (org-indent-mode)
                          (flyspell-mode)))
(setq-default c-default-style "linux"
	      c-basic-offset 4
	      indent-tabs-mode nil)

;;Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
;;Move variables set by emacs automatically into another file so this one looks clean 
(setq custom-file (concat user-emacs-directory "/custom.el"))
