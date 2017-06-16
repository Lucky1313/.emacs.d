;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Manager Setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Autoinstall use-package
(when (not (package-installed-p 'use-package)) (package-refresh-contents) (package-install 'use-package))
(require 'use-package)

;; Theme
(use-package zenburn-theme
	:ensure t
  :config
  (load-theme 'zenburn t))

;; Add mode settings
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :interpreter "markdown")
(use-package cuda-mode
  :ensure t
  :mode "\\.cu\\'"
  :interpreter "cuda")
(use-package julia-mode
  :ensure t
  :mode "\\.jl\\'"
  :interpreter "julia")
(use-package systemd
  :ensure t
  :mode ("\\.service\\'" . systemd-mode)
  :interpreter ("systemd" . systemd-mode))
(use-package syslog-mode
  :ensure t
  :mode "/var/log.*\\'"
  :interpreter "syslog")
(use-package arduino-mode
  :ensure t
  :mode "\\.ino\\'"
  :interpreter "arduino")
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :interpreter "yaml")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL EMACS SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change file backups location
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Save history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))


;; Editor settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; Global line numbers
(global-linum-mode 1)

;; Auto update files if changed outside emacs
(global-auto-revert-mode t)

;; Disable narrow to region
(put 'narrow-to-region 'disabled nil)

;; Change yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Cycle spacing, switch between 1 space, no spaces or original
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; Indent new lines properly
(global-set-key (kbd "RET") 'newline-and-indent)

;; Switch between header and source file
(global-set-key (kbd "C-x C-h") 'ff-find-other-file)

;; Set tab width
(setq-default tab-width 2
			  indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

;; Delete highlighted text when typing
(delete-selection-mode t)
(transient-mark-mode t)

(setq cc-other-file-alist
      '(("\\.c"   (".h"))
       ("\\.cpp"   (".h"))
       ("\\.h"   (".c"".cpp"))
       ("\\.hpp" (".cpp"))))

(setq ff-search-directories
      '("." "../src" "../include"))

(global-set-key (kbd "M-t") 'ff-find-other-file)

;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;

;; Flyspell for spell checking
(use-package flyspell
  :ensure t
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (setq flyspell-issue-message-flag nil))

;; Jump to next misspelled word and pop-up correction
(use-package flyspell-popup
  :ensure t
  :init
  (define-key flyspell-mode-map (kbd "C-,") 'custom/flyspell-next-word))

(defun custom/flyspell-next-word()
  (interactive)
  (flyspell-goto-next-error)
  (flyspell-popup-correct))


;; Helm: Better file lookup
(use-package helm
  :ensure t
  :init
  (helm-mode 1)
  (setq helm-split-window-in-side-p t)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

;; ;; Projectile: Project manager
;; (use-package projectile
;;   :ensure t
;;   :init
;;   (projectile-global-mode 1)
;;   :bind ("<f5>" . projectile-compile-project))

;; (use-package helm-projectile
;;   :ensure t
;;   :bind ("C-c h" . helm-projectile))

;; Tramp: Remote client connection
(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "ssh"))

;; Multi-term: Terminal
(use-package multi-term
  :ensure t
  :bind ("C-c t" . multi-term-dedicated-open))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

;; Org mode: Organization
(use-package org
  :ensure t
  :init
  (add-hook 'org-mode-hook 'org-indent-mode)
  :config
  (setq org-log-done t
        org-directory "~/org"
        org-support-shift-select t
        org-startup-truncated t
        org-startup-folded "showeverything"
        org-modules
        '(org-babel
          org-bibtex
          org-habit
          org-checklist
          org-depend
          org-notify)
        org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(p)" "|" "DONE(d!)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("NEXT" :foreground "blue" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)))
  ;; Ditaa Drawing in Org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)))

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "ditaa")))
  
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate
        org-ditaa-jar-path "/usr/bin/ditaa"))




;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPLATE PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Skeletor for project templates
(use-package skeletor
  :ensure t
  :config
  (add-to-list 'skeletor-global-substitutions
               '("__USER-NAME__" . "David Hedin"))
  (skeletor-define-template "basic-cpp"
    :title "Basic C++ Project"
    :no-license? t)
  (skeletor-define-template "full-cpp"
    :title "Enhanced C++ Project"
    :default-license "mit"))

;; Yasnippet: Snippet expander
(use-package yasnippet
  :ensure t
  :bind ("C-c s" . yas-insert-snippet)
  :functions custom/skel
  :init
  (add-hook 'term-mode-hook '(lambda () (yas-minor-mode -1)))
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-prompt-functions 'custom/helm-prompt))

;; Use helm to display snippets for yasnippet
(defun custom/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
  (interactive)
  (setq display-fn (or display-fn 'identity))
  (if (require 'helm-config)
      (let (tmpsource cands result rmap)
        (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
        (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
        (setq tmpsource
              (list
               (cons 'name prompt)
               (cons 'candidates cands)
               '(action . (("Expand" . (lambda (selection) selection))))
               ))
        (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
        (if (null result)
            (signal 'quit "user quit!")
          (cdr (assoc result rmap))))
    nil))


;; Auto insert: File skeletons
(auto-insert-mode)
;(setq auto-insert-query nil)

;; Custom function for expanding yasnippets for auto-insert skeletons
(defun custom/skel()
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;; File extension associations
(custom-set-variables
 '(auto-insert (quote other))
 '(auto-insert-alist
   (quote
    ((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
      .
      ["skel.h" custom/skel])
     (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source")
      .
      ["skel.cpp" custom/skel])
     (("\\.\\([c]\\)\\'" . "C source")
      .
      ["skel.c" custom/skel])
     (("\\.sh\\'" . "Shell script")
      .
      ["skel.sh" custom/skel])
     (("\\.py\\'" . "Python script")
      .
      ["skel.py" custom/skel])
     (("[mM]akefile\\'" . "Makefile")
      .
      ["skel.make" custom/skel])
     (("README.md" . "Readme")
      .
      ["skel.readme" custom/skel])
     (("\\.tex\\'" . "TeX/LaTeX")
      .
      ["skel.tex" custom/skel]))))
 '(auto-insert-directory "~/.emacs.d/snippets/skeletons/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROGRAMMING PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ansi-color: Colorize compilation buffer
(use-package ansi-color
  :ensure t
  :init
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(defun colorize-compilation-buffer ()
  (read-only-mode -1)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode 1))

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Auto-highlight-symbol: Highlight current item
(use-package auto-highlight-symbol
  :ensure t
  :config
  (global-auto-highlight-symbol-mode 1))

;; Autopair: Auto complete delimiters
(use-package autopair
  :ensure t
  :config
  (autopair-global-mode 1))

;; Fic-mode: Highlights FIXMEs and TODOs
(use-package fic-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'fic-mode))

;; Flycheck for real time semantic checking
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1))

;; Company: Autocompletion
(use-package company
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-idle-delay 0))

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; Autocomplete, used for backend only
(use-package auto-complete
  :ensure t)

(use-package auto-complete-clang
  :ensure t)

;; Rtags
(use-package rtags
  :ensure t
  :bind (:map c-mode-base-map
              ("M-." . rtags-find-symbol-at-point)
              ("M-," . rtags-find-references-at-point))
  :init
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)
  (rtags-enable-standard-keybindings))

;; Rtags helm integration
(use-package helm-rtags
  :ensure t
  :init
  (setq rtags-use-helm t))

(use-package cmake-ide
  :ensure t
  :bind ("<f8>" . cmake-ide-compile)
  :init
  (cmake-ide-setup))

;; PEP8 reading of python files
(use-package py-autopep8
  :ensure t
  :init
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

;; LaTeX processing
(use-package tex-mik
  :ensure auctex
  :init
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))

