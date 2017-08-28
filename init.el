;; no toolbar
(tool-bar-mode -1)
;; show parens
(show-paren-mode t)
(setq show-paren-delay 0)
;; no scroll bar
(scroll-bar-mode -1)
;; show linenumbers
(global-linum-mode)
;; disable bell
(setq ring-bell-function 'ignore)
;; display the time in mode-line
(setq display-time-format "-- %H:%M --")
(display-time-mode 1)
;; Do not use the macOS native fullscreen (with animations and everything)
;; when using M-x toggle-frame-fullscreen
(setq ns-use-native-fullscreen nil)

;; winner mode -- C-c left/right
(winner-mode t)
;; utf-8
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
(global-whitespace-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((t (:foreground "black" :background "red")))))

;; the place for custom emacs lisp code
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-blank-before-new-entry
      '((heading . nil)
        (plain-list-item . nil)))
(setq org-src-fontify-natively t)
(org-indent-mode)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (ruby . t)
   (emacs-lisp . t)
   ))

;; projectile
(require 'projectile)
(projectile-mode 1)
(setq projectile-switch-project-action #'counsel-projectile-find-file)
(setq projectile-completion-system 'ivy)

;; ivy
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order)))
(global-set-key (kbd "M-x") 'counsel-M-x)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; counsel-projectil
;;(require 'counsel-projectile)
;;(counsel-projectile-on)

;; evil-mode
;; (eval-after-load "evil"
;;  '(progn
;;     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
;;     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
;;     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
;;     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))
(require 'evil)
(require 'evil-leader)
(require 'evil-commentary)
(require 'evil-surround)
(require 'evil-visualstar)

(global-evil-leader-mode 1)
(global-evil-surround-mode 1)
(global-evil-visualstar-mode)
(evil-commentary-mode 1)
(evil-mode 1)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "x" 'counsel-M-x ;; counsel-M-x comes from ivy
  "s" 'swiper ;; swiper is included in ivy
  "g" 'counsel-rg
  "cp" 'counsel-projectile-switch-project
  "fi" 'counsel-projectile-find-file
  "fd" 'counsel-projectile-find-dir
  "fb" 'counsel-projectile-switch-to-buffer
  "fg" 'counsel-projectile-rg
  "fr" 'ivy-resume
  "ms" 'magit-status)

;; use spaces instead of tabs
(setq-default evil-shift-width 2)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; go -- taken from here: https://johnsogg.github.io/emacs-golang
;; Define function to call when go-mode loads
(require 'go-guru)
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)
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
  (setq tab-width 4)
  ;; (auto-complete-mode 1)
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)

(defun my-switch-project-hook ()
  (go-set-project))
(add-hook 'projectile-after-switch-project-hook #'my-switch-project-hook)

(evil-leader/set-key-for-mode 'go-mode "god" 'godef-jump)
(evil-leader/set-key-for-mode 'go-mode "got" 'projectile-compile-project)

;; Ruby
(evil-leader/set-key-for-mode 'ruby-mode "rt" 'rspec-verify-single)
(evil-leader/set-key-for-mode 'ruby-mode "rf" 'rspec-verify)

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js-indent-level 2)

;; Magit
(require 'evil-magit)
(setq git-commit-summary-max-length 50)
(global-set-key (kbd "C-x g") 'magit-status)
;; Use ivy for magit completion
(setq magit-completing-read-function 'ivy-completing-read)

;; smooth-scrolling
(require 'smooth-scrolling)
(setq scroll-margin 5)
(setq scroll-conservatively 9999)
(setq scroll-step 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (go-autocomplete switch-window smooth-scrolling rjsx-mode lenlen-theme projectile-ripgrep evil-visualstar evil-surround evil-commentary wgrep yaml-mode column-marker slim-mode evil-magit magit rspec-mode go-mode geiser exec-path-from-shell evil-leader markdown-mode ivy counsel-projectile projectile evil))))

