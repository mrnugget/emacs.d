;; Initial bootstrapping for `use-package`

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(unless package--initialized
  (package-initialize t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; General setting

;; no toolbar
(tool-bar-mode -1)
;; show parens
(setq show-paren-delay 0)
(show-paren-mode t)
;; no scroll bar
(scroll-bar-mode -1)
;; show linenumbers
(global-linum-mode)
;; disable bell
(setq ring-bell-function 'ignore)
;; mode line colors - make the active window stand out more
(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "black")
;; display the time in mode-line
(setq display-time-format "-- %H:%M --")
(display-time-mode 1)
;; Do not use the macOS native fullscreen (with animations and everything)
;; when using M-x toggle-frame-fullscreen
(setq ns-use-native-fullscreen nil)
;; Always select the opened help window so it's easily closable with "q"
(setq help-window-select t)

(setq x-select-enable-clipboard t)

;; default font: hack
(set-face-attribute 'default nil :family "Hack")
;; utf-8 & emojis
(set-language-environment "UTF-8")
(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
(prefer-coding-system 'utf-8)
;; mac keybindings
(setq mac-option-modifier nil) ;; nil because I want alt-u behaviour to enter umlauts
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
;; all autosave and backup files in one directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; This allows us to use `switch-to-buffer-other-window` and have it
;; split vertically
(setq split-width-threshold nil)
(setq split-height-threshold nil)
;; whitespace
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-global-modes '(not org-mode))
(global-whitespace-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((t (:foreground "black" :background "lightyellow")))))

;; use spaces instead of tabs

(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; the place for custom emacs lisp code
(add-to-list 'load-path "~/.emacs.d/lisp")

;; winner mode -- C-c left/right
(winner-mode t)
(global-set-key (kbd "C-c C-u") 'winner-undo)
(global-set-key (kbd "C-c C-r") 'winner-redo)

;; comint
;; do not echo commands
(defun my-comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)


;; Packages
;; (setq my-packages '(evil
;;                      evil-collection
;;                      evil-paredit
;;                      evil-visualstar
;;                      evil-surround
;;                      evil-commentary
;;                      evil-org
;;                      evil-matchit
;;                      evil-search-highlight-persist
;;                      evil-magit
;;                      evil-leader
;;                      magit
;;                      ivy
;;                      projectile
;;                      counsel-projectile
;;                      projectile-ripgrep
;;                      exec-path-from-shell
;;                      org
;;                      ox-gfm
;;                      erlang
;;                      htmlize
;;                      alchemist
;;                      edit-indirect
;;                      go-autocomplete
;;                      wgrep
;;                      yaml-mode
;;                      slim-mode
;;                      rjsx-mode
;;                      rspec-mode
;;                      go-mode
;;                      rust-mode
;;                      cargo
;;                      geiser
;;                      markdown-mode
;;                      flycheck
;;                      flycheck-rust))

;; (require 'package)
;; (setq package-archives
;;       '(("melpa" . "https://melpa.org/packages/")
;;         ("gnu" . "https://elpa.gnu.org/packages/")
;;         ("org" . "http://orgmode.org/elpa/")))

                                        ; fetch the list of packages available
;; (package-refresh-contents)

                                        ; install the missing packages
;; (dolist (package my-packages)
;;   (unless (package-installed-p package)
;;     (package-install package)))

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))


;; org-mode
(use-package org
  :ensure t
  :config
  (setq org-startup-indented t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ruby . t)
     (emacs-lisp . t)
     (scheme . t)))
  (setq org-capture-templates
        '(("in" "Mesosphere Note" entry (file+headline "~/Dropbox/org/mesosphere.org" "Mesosphere")
           "* %?")
          )))

(use-package ox-gfm
  :ensure t
  :after org)

(use-package ox-jira
  :ensure t
  :after org)

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;; ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-switch-buffer-faces-alist
        '((emacs-lisp-mode . swiper-match-face-1)
          (dired-mode . ivy-subdir)
          (org-mode . org-level-4)
          (ruby-mode .org-level-2)))
  ;; Set this to a blank string instead of "^"
  (setq ivy-initial-inputs-alist '((counsel-M-x . "")))
  ;; disabled for now, since with this the candidate selection
  ;; doesn't work in switch-buffer minibuffer anymore.
  ;; see: https://github.com/abo-abo/swiper/issues/1159
  ;; (setq ivy-re-builders-alist
  ;;       '((t . ivy--regex-ignore-order)))
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))


;; projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode 1))

(use-package counsel-projectile
  :ensure t
  :after projectile ivy)

(use-package projectile-ripgrep
  :ensure t)

;; switch-window
(use-package switch-window
  :ensure t)

;; highlight
(use-package highlight
  :ensure t)

;; evil-mode
(use-package evil
  :ensure t
  :after ivy
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-?") 'help-command)
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  ;; whitespace settings
  (setq-default evil-shift-width 2)
  ;; (require 'evil-search-highlight-persist)
                                        ; (global-evil-search-highlight-persist t)
  (setq evil-ex-search-persistent-highlight t)
  ;; this should make ivy-occur buffers editable again (https://github.com/syl20bnr/spacemacs/issues/10290)
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)

  ;; allows us to "paste over" things while visually selected
  (fset 'evil-visual-update-x-selection 'ignore)
  ;; comint is the "repl like" mode used by inf-ruby, geiser, etc.
  (dolist (mode '(normal insert))
    (evil-define-key mode comint-mode-map
      (kbd "C-p") 'comint-previous-input
      (kbd "C-n") 'comint-next-input
      (kbd "C-r") 'comint-history-isearch-backward-regexp))
  ;; also use ctrl-n/p for ivy
  (evil-define-key nil ivy-minibuffer-map
    (kbd "C-p") #'ivy-previous-line
    (kbd "C-n") #'ivy-next-line)

  (dolist (map '(help-mode-map compilation-mode-map))
    (evil-define-key nil map
      (kbd "C-h") 'evil-window-left
      (kbd "C-j") 'evil-window-down
      (kbd "C-k") 'evil-window-up
      (kbd "C-l") 'evil-window-right)))

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (global-evil-leader-mode 1)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "e" 'find-file
    "b" 'switch-to-buffer
    "k" 'kill-buffer
    "x" 'counsel-M-x ;; counsel-M-x comes from ivy
    "s" 'swiper ;; swiper is included in ivy
    "g" 'counsel-rg
    "h" 'evil-ex-nohighlight
    "tw" 'delete-trailing-whitespace
    "cp" 'counsel-projectile-switch-project
    "fi" 'counsel-projectile-find-file
    "fd" 'counsel-projectile-find-dir
    "fb" 'counsel-projectile-switch-to-buffer
    "fg" 'counsel-projectile-rg
    "fs" 'projectile-run-shell
    "fr" 'ivy-resume
    "ms" 'magit-status
    "wo" 'switch-window))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode 1))
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))
(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (global-evil-visualstar-mode))

(use-package evil-matchit
  :ensure t)

(use-package evil-search-highlight-persist
  :ensure t)

;; evil-org
(use-package evil-org
  :ensure t
  :after evil org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'org-mode-hook (lambda () (global-visual-line-mode 1)))
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme))))

;; geiser
(use-package geiser
  :ensure t
  :after evil
  :config
  (setq geiser-debug-jump-to-debug-p nil))

;; Ruby & Rspec
;; Using binding.pry in rspec: https://emacs.stackexchange.com/questions/3537/how-do-you-run-pry-from-emacs
(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

(use-package go-mode
  :ensure t
  :after evil projectile evil-leader
  :config
  ;; go -- taken from here: https://johnsogg.github.io/emacs-golang
  ;; Define function to call when go-mode loads
  (require 'go-guru)
  ;;(require 'go-autocomplete)

  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
    (setq gofmt-command "goimports")                ; gofmt uses invokes goimports

    (if (not (string-match "go" compile-command))   ; set compile command default
        (set (make-local-variable 'compile-command)
             "go test -v && go vet"))
    (local-set-key (kbd "M-p") 'projectile-compile-project) ; Invoke compiler
    (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
    (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
    (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
    (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

    (evil-local-set-key 'normal (kbd "C-]") 'godef-jump)

    ;; do not highlight "tabs" in go mode
    (set (make-local-variable 'whitespace-style) '(face empty lines-tail trailing))

    ;; guru settings
    (go-guru-hl-identifier-mode)
    ;; tab width of 4 spaces
    (setq tab-width 4))
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  (evil-leader/set-key-for-mode 'go-mode "god" 'godef-jump)
  (evil-leader/set-key-for-mode 'go-mode "goc" 'projectile-compile-project)
  (evil-leader/set-key-for-mode 'go-mode "got" 'projectile-test-project))

;; Ruby
(use-package ruby-mode
  :ensure t
  :after evil-leader
  :config
  (evil-leader/set-key-for-mode 'ruby-mode "rt" 'rspec-verify-single)
  (evil-leader/set-key-for-mode 'ruby-mode "rf" 'rspec-verify))


;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :after flycheck rust-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; wgrep
(use-package wgrep
  :ensure t)

;; erlang
(use-package erlang
  :ensure t)

;; htmlize
(use-package htmlize
  :ensure t)

;; alchemist
(use-package alchemist
  :ensure t)

;; edit-indirect
(use-package edit-indirect
  :ensure t)

;; go-autocomplete
(use-package go-autocomplete
  :ensure t)

;; smooth-scrolling
(use-package smooth-scrolling
  :ensure t)

;; wgrep
(use-package wgrep
  :ensure t)

;; yaml-mode
(use-package yaml-mode
  :ensure t)

;; slim-mode
(use-package slim-mode
  :ensure t)

;; rjsx-mode
(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq js-indent-level 2))

;; rspec-mode
(use-package rspec-mode
  :ensure t)

;; markdown-mode
(use-package markdown-mode
  :ensure t)

;; Magit
(use-package magit
  :ensure t
  :after ivy
  :config
  (setq git-commit-summary-max-length 50)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
  :ensure t
  :after magit evil)

;; Tramp
(setq tramp-default-method "ssh")

;; smooth-scrolling
(use-package smooth-scrolling
  :ensure t
  :config
  (setq scroll-margin 5)
  (setq scroll-conservatively 9999)
  (setq scroll-step 1))

;; allow piping to jq

(defun pipe-to-jq()
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end)
   "cat | jq ." nil t))

(evil-leader/set-key "jq" 'pipe-to-jq)

(use-package espresso-theme
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes '(leuven))
 '(evil-search-module 'evil-search)
 '(ibuffer-saved-filter-groups nil)
 '(org-agenda-files '("~/Dropbox/org/ioki-journal.org"))
 '(package-selected-packages
   '(ace ace-window ox-jira espresso-theme espresso srcery-theme inf-ruby rjsx-mode yaml-mode wgrep use-package switch-window smooth-scrolling slim-mode rspec-mode projectile-ripgrep ox-gfm htmlize go-mode go-autocomplete geiser flycheck-rust exec-path-from-shell evil-visualstar evil-surround evil-search-highlight-persist evil-org evil-matchit evil-magit evil-leader evil-commentary erlang edit-indirect counsel-projectile cargo alchemist)))
                                        ;  '(package-selected-packages
                                        ;    '(ox-jira twilight-bright-theme cargo rust-mode evil-collection evil-paredit ox-gfm evil-org evil-matchit color-theme-sanityinc-tomorrow evil-search-highlight-persist org erlang htmlize alchemist edit-indirect projectile-rails go-autocomplete switch-window smooth-scrolling rjsx-mode lenlen-theme projectile-ripgrep evil-visualstar evil-surround evil-commentary wgrep yaml-mode slim-mode evil-magit magit rspec-mode go-mode geiser exec-path-from-shell evil-leader markdown-mode ivy counsel-projectile projectile evil)))

(put 'narrow-to-region 'disabled nil)
