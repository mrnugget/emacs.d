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

;; the place for custom emacs lisp code
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Packages

;; winner mode -- C-c left/right
(winner-mode t)
(global-set-key (kbd "C-c C-u") 'winner-undo)
(global-set-key (kbd "C-c C-r") 'winner-redo)

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; shells
;; do not echo commands
(defun my-comint-init ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'my-comint-init)

;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-adapt-indentation nil)
(setq org-log-done t)
(setq org-blank-before-new-entry
      '((heading . nil)
        (plain-list-item . nil)))
(setq org-src-fontify-natively t)
(org-indent-mode)
; (org-babel-do-load-languages
;  'org-babel-load-languages
;  '((sh . t)
;    (ruby . t)
;    (emacs-lisp . t)
;    (scheme . t)
;    ))

;; ivy
(require 'ivy)
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
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; projectile
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-completion-system 'ivy)
(require 'projectile)
(projectile-mode 1)
;; projectile-rails
(projectile-rails-global-mode 1)

;; counsel-projectil
;;(require 'counsel-projectile)
;;(counsel-projectile-on)

(require 'switch-window)
;; see 'switch-window binding further down

;; evil-mode
(eval-after-load "evil"
 '(progn
    (define-key evil-normal-state-map (kbd "C-?") 'help-command)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))
(require 'evil)
(require 'evil-leader)
(require 'evil-commentary)
(require 'evil-surround)
(require 'evil-visualstar)
(require 'highlight)
(require 'evil-search-highlight-persist)

(global-evil-leader-mode 1)
(global-evil-surround-mode 1)
(global-evil-visualstar-mode)
(global-evil-search-highlight-persist t)
(evil-commentary-mode 1)
(evil-mode 1)

;; this should make ivy-occur buffers editable again (https://github.com/syl20bnr/spacemacs/issues/10290)
(evil-set-initial-state 'ivy-occur-grep-mode 'normal)

;; allows us to "paste over" things while visually selected
(fset 'evil-visual-update-x-selection 'ignore)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "x" 'counsel-M-x ;; counsel-M-x comes from ivy
  "s" 'swiper ;; swiper is included in ivy
  "g" 'counsel-rg
  "tw" 'delete-trailing-whitespace
  "cp" 'counsel-projectile-switch-project
  "fi" 'counsel-projectile-find-file
  "fd" 'counsel-projectile-find-dir
  "fb" 'counsel-projectile-switch-to-buffer
  "fg" 'counsel-projectile-rg
  "fr" 'ivy-resume
  "ms" 'magit-status
  "wo" 'switch-window)

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

;; use spaces instead of tabs
(setq-default evil-shift-width 2)
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

;; Ruby & Rspec
;; Using binding.pry in rspec: https://emacs.stackexchange.com/questions/3537/how-do-you-run-pry-from-emacs
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
;; treat _ as part of the word in ruby
;; (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; go -- taken from here: https://johnsogg.github.io/emacs-golang
;; Define function to call when go-mode loads
(require 'go-guru)
(require 'go-autocomplete)

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
  ;; enable auto-complete-mode
  (auto-complete-mode 1))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;;(defun my-switch-project-hook ()
;;  (go-set-project))
;;(add-hook 'projectile-after-switch-project-hook #'my-switch-project-hook)

(evil-leader/set-key-for-mode 'go-mode "god" 'godef-jump)
(evil-leader/set-key-for-mode 'go-mode "goc" 'projectile-compile-project)
(evil-leader/set-key-for-mode 'go-mode "got" 'projectile-test-project)

;; Ruby
(evil-leader/set-key-for-mode 'ruby-mode "rt" 'rspec-verify-single)
(evil-leader/set-key-for-mode 'ruby-mode "rf" 'rspec-verify)

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js-indent-level 2)

;; Magit
(require 'evil-magit)
(setq git-commit-summary-max-length 50)
(setq magit-completing-read-function 'ivy-completing-read)

;; Tramp
(setq tramp-default-method "ssh")

;; smooth-scrolling
(require 'smooth-scrolling)
(setq scroll-margin 5)
(setq scroll-conservatively 9999)
(setq scroll-step 1)

;; allow piping to jq

(defun pipe-to-jq()
  (interactive)
  (message region-beginning)
  (shell-command-on-region
   (region-beginning) (region-end)
   "cat | jq ." nil t))

(evil-leader/set-key "jq" 'pipe-to-jq)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes nil)
 '(evil-search-module (quote evil-search))
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   (quote
    (("hel"
      (projectile-files . "/Users/mrnugget/work/triebwerk/"))
     ("programming"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . compilation-mode)))
     ("text document"
      (and
       (derived-mode . text-mode)
       (not
        (starred-name))))
     ("TeX"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("web"
      (or
       (derived-mode . sgml-mode)
       (derived-mode . css-mode)
       (mode . javascript-mode)
       (mode . js2-mode)
       (mode . scss-mode)
       (derived-mode . haml-mode)
       (mode . sass-mode)))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode))))))
 '(org-agenda-files (quote ("~/org/road_to_emacs.org" "~/org/life.org")))
 '(package-selected-packages
   (quote
    (evil-matchit color-theme-sanityinc-tomorrow evil-search-highlight-persist org erlang htmlize alchemist edit-indirect projectile-rails go-autocomplete switch-window smooth-scrolling rjsx-mode lenlen-theme projectile-ripgrep evil-visualstar evil-surround evil-commentary wgrep yaml-mode slim-mode evil-magit magit rspec-mode go-mode geiser exec-path-from-shell evil-leader markdown-mode ivy counsel-projectile projectile evil))))

