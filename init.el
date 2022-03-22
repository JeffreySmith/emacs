(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "b89ae2d35d2e18e4286c8be8aaecb41022c1a306070f64a66fd114310ade88aa" default))
 '(org-agenda-files '("~/org/test.org"))
 '(package-selected-packages
   '(racket-mode nim-mode vterm company-ebdb haskell-mode pdf-tools lua-mode ace-window cmake-project cmake-mode counsel swiper evil-collection ivy company-c-headers solarized-theme magit gruvbox-theme evil))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq package-native-compile t)

(setq evil-disable-insert-state-bindings t)
(setq evil-want-keybinding nil)
(require 'evil)
(evil-mode)

(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_CA")

(toggle-scroll-bar -1)

;;(add-hook 'vterm-mode-hook (lambda () (setq evil-default-state 'emacs)))
(evil-set-initial-state 'vterm-mode 'insert)
(evil-set-initial-state 'dired-mode 'insert)
(evil-set-initial-state 'git-commit-mode 'insert)

(setq-default c-default-style "linux"
	      c-basic-offset 4
	      indent-tabs-mode nil)

(load-theme 'solarized-light t)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c C-'") 'avy-goto-char)
(global-set-key (kbd "C-c q") 'avy-goto-char-2)

(global-set-key "\C-s" 'swiper)

(counsel-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(require 'cmake-mode)
(require 'cmake-project)

(global-set-key (kbd "M-o") 'ace-window)

(global-display-line-numbers-mode)

;;Org mode stuff
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(add-hook 'org-mode-hook '(lambda ()
                          (visual-line-mode)
                          (toggle-word-wrap)
                          (org-indent-mode)
                          (flyspell-mode)))


;(global-set-key (kbd "C-c C-r") 'ivy-resume)
;(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
;(global-set-key (kbd "<f1> l") 'counsel-find-library)
;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;(global-set-key (kbd "C-c j") 'counsel-git-grep)
;(global-set-key (kbd "C-c k") 'counsel-ag)
;(global-set-key (kbd "C-x l") 'counsel-locate)
;(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))



;;(windmove-default-keybindings)
(put 'set-goal-column 'disabled nil)

;;Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
(setq inhibit-startup-screen t)
