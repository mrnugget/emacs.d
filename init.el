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

;; winner mode -- C-c left/right
(winner-mode t)
(global-set-key (kbd "C-c C-u") 'winner-undo)
(global-set-key (kbd "C-c C-r") 'winner-redo)

;; Packages

(setq my-packages '(evil
                     evil-collection
                     evil-paredit
                     evil-visualstar
                     evil-surround
                     evil-commentary
                     evil-org
                     evil-matchit
                     evil-search-highlight-persist
                     evil-magit
                     evil-leader
                     magit
                     ivy
                     projectile
                     counsel-projectile
                     projectile-rails
                     projectile-ripgrep
                     exec-path-from-shell
                     org
                     ox-gfm
                     erlang
                     htmlize
                     alchemist
                     edit-indirect
                     go-autocomplete
                     switch-window
                     smooth-scrolling
                     wgrep
                     yaml-mode
                     slim-mode
                     rjsx-mode
                     rspec-mode
                     go-mode
                     geiser
                     markdown-mode))

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

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
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-adapt-indentation t)
;; (setq org-log-done t)
;; (setq org-blank-before-new-entry
;;       '((heading . nil)
;;         (plain-list-item . nil)))
;; (setq org-src-fontify-natively t)
(setq org-startup-indented t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (emacs-lisp . t)
   (scheme . t)))
(setq org-capture-templates
 '(("in" "ioki Note" entry (file+headline "~/Dropbox/org/ioki.org" "ioki")
        "* %?")
   ("ij" "ioki Journal entry" entry (file+olp+datetree "~/Dropbox/org/ioki-journal.org" "ioki Journal")
        "* %?\nEntered on %U")))

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (global-visual-line-mode 1)))
(add-hook 'evil-org-mode-hook
          (lambda ()
            (evil-org-set-key-theme)))

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

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
;; (require 'evil-search-highlight-persist)
; (global-evil-search-highlight-persist t)
(setq evil-ex-search-persistent-highlight t)

(global-evil-leader-mode 1)
(global-evil-surround-mode 1)
(global-evil-visualstar-mode)
(evil-commentary-mode 1)
(evil-mode 1)

;; this should make ivy-occur buffers editable again (https://github.com/syl20bnr/spacemacs/issues/10290)
(evil-set-initial-state 'ivy-occur-grep-mode 'normal)
(evil-set-initial-state 'projectile-rails-server 'normal)

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

;; geiser
(setq geiser-debug-jump-to-debug-p nil)

;; Lots of things taken from here https://github.com/emacs-evil/evil-collection/blob/dfa412db04b3714a14a1879679daddefb873b89b/evil-collection-geiser.el
;; With modifications to keybindings
(defun evil-collection-geiser-last-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))))

(advice-add 'geiser-eval-last-sexp :around 'evil-collection-geiser-last-sexp)
(advice-add 'geiser-eval-last-sexp-and-print :around 'evil-collection-geiser-last-sexp)

(evil-set-initial-state 'geiser-debug-mode 'normal)
(evil-set-initial-state 'geiser-doc-mode 'normal)

(evil-define-key 'normal 'geiser-debug-mode-map
  "q" 'quit-window)

(evil-define-key 'normal 'geiser-doc-mode-map
  "gd" 'geiser-edit-symbol-at-point
  (kbd "C-t") 'geiser-pop-symbol-stack
  "gr" 'geiser-doc-refresh
  "q" 'View-quit
  "gz" 'geiser-doc-switch-to-repl
  "gj" 'forward-button
  "gk" 'backward-button
  "]" 'geiser-doc-next-section
  "[" 'geiser-doc-previous-section)

(evil-define-key 'insert 'geiser-repl-mode-map
  (kbd "S-<return>") 'geiser-repl--newline-and-indent)

(evil-define-key 'normal 'geiser-repl-mode-map
  "gd" 'geiser-edit-symbol-at-point
  (kbd "C-t") 'geiser-pop-symbol-stack
  "gj" 'geiser-repl-next-prompt
  "gk" 'geiser-repl-previous-prompt
  "]" 'geiser-repl-next-prompt
  "[" 'geiser-repl-previous-prompt
  "K" 'geiser-doc-symbol-at-point)

(evil-define-key 'normal 'geiser-mode-map
  "gd" 'geiser-edit-symbol-at-point
  (kbd "C-t") 'geiser-pop-symbol-stack
  "gZ" 'geiser-mode-switch-to-repl-and-enter
  "gz" 'geiser-mode-switch-to-repl
  "K" 'geiser-doc-symbol-at-point)

;; Ruby & Rspec
;; Using binding.pry in rspec: https://emacs.stackexchange.com/questions/3537/how-do-you-run-pry-from-emacs
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
;; treat _ as part of the word in ruby
;; (add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

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
  (setq tab-width 4)
  )
  ;; enable auto-complete-mode
  ;;(auto-complete-mode 1))

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
  (shell-command-on-region
   (region-beginning) (region-end)
   "cat | jq ." nil t))

(evil-leader/set-key "jq" 'pipe-to-jq)

;; small compilation window

(defun my-compile ()
  "Run compile and resize the compile window"
  (interactive)
  (progn
    (call-interactively 'compile)
    (setq cur (selected-window))
    (setq w (get-buffer-window "*compilation*"))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (- h 10))
    (select-window cur)))

(defun my-compilation-hook ()
  "Make sure that the compile window is splitting vertically"
  (progn
    (if (not (get-buffer-window "*compilation*"))
        (progn (split-window-vertically)))))

(add-hook 'compilation-mode-hook 'my-compilation-hook)
(global-set-key [f9] 'my-compile)

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
   '(evil-collection evil-paredit ox-gfm evil-org evil-matchit color-theme-sanityinc-tomorrow evil-search-highlight-persist org erlang htmlize alchemist edit-indirect projectile-rails go-autocomplete switch-window smooth-scrolling rjsx-mode lenlen-theme projectile-ripgrep evil-visualstar evil-surround evil-commentary wgrep yaml-mode slim-mode evil-magit magit rspec-mode go-mode geiser exec-path-from-shell evil-leader markdown-mode ivy counsel-projectile projectile evil)))

(put 'narrow-to-region 'disabled nil)
