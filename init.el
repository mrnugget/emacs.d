;; no toolbar
(tool-bar-mode -1)
;; show parens
(show-paren-mode t)
;; show linenumbers
(global-linum-mode)
;; disable bell
(setq ring-bell-function 'ignore)

;; utf-8
(prefer-coding-system 'utf-8)

;; mac keybindings
(setq mac-option-modifier nil) ;; nil because I want alt-u behaviour to enter umaluts
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

;; all autosave and backup files in one directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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
;; Start projectile in dired mode when switching
(setq projectile-switch-project-action #'projectile-dired)

;; ivy
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; counsel-projectil
(require 'counsel-projectile)
(counsel-projectile-on)

;; evil-mode
(setq evil-want-C-u-scroll t)
(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))
(require 'evil)
(require 'evil-leader)
(evil-mode 1)
(global-evil-leader-mode 1)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "x" 'counsel-M-x ;; counsel-M-x comes from ivy
  "s" 'swiper ;; swiper is included in ivy
  "fi" 'counsel-projectile-find-file
  "fb" 'counsel-projectile-switch-to-buffer)


;; geiser 
;; (eval-after-load "geiser-impl"
;;   '(add-to-list 'geiser-implementations-alist
;;                 '((dir "/Users/mrnugget/code/guile") guile))

;; use spaces instead of tabs
(setq-default evil-shift-width 2)
(setq-default indent-tabs-mode nil)

;; go -- taken from here: https://johnsogg.github.io/emacs-golang
;; Define function to call when go-mode loads
(require 'go-guru)
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  ;; guru settings
  (go-guru-hl-identifier-mode))

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

(defun my-switch-project-hook ()
  (go-set-project))
(add-hook 'projectile-after-switch-project-hook #'my-switch-project-hook)

(evil-leader/set-key-for-mode 'go-mode "god" 'godef-jump)
(evil-leader/set-key-for-mode 'go-mode "got" 'projectile-compile-project)

;; Ruby
(evil-leader/set-key-for-mode 'ruby-mode "rt" 'rspec-verify-single)
(evil-leader/set-key-for-mode 'ruby-mode "rf" 'rspec-verify-matching)

;; Magit
(require 'evil-magit)
(global-set-key (kbd "C-x g") 'magit-status)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-magit magit rspec-mode go-mode geiser exec-path-from-shell evil-leader markdown-mode ivy counsel-projectile projectile evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
