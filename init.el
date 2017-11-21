;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE MANAGER SETUP ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; Autoinstall use-package
(when (not (package-installed-p 'use-package)) (package-refresh-contents) (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;
;; THEME SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;

;; Theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL EMACS SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change file backups location
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq version-control t)
(setq vc-make-backup-files t)
(setq kept-new-versions 10)
(setq delete-old-versions t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Editor settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; Display time in emacs
(display-time-mode 1)

;; Show column number in mode line
(column-number-mode 1)

;; Global line numbers
(global-linum-mode 1)

;; Auto update files if changed outside emacs
(global-auto-revert-mode t)

;; Change yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable local elisp eval
(setq enable-local-eval 't)

;; Indent new lines properly
(add-hook 'prog-mode-hook
          (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; Keybinding to open init.el
(global-set-key (kbd "<f9>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;; Set tab width
(setq-default tab-width 2
			  indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

;; Delete highlighted text when typing
(delete-selection-mode t)
(transient-mark-mode t)

;; Visual line mode for text mode
(add-hook 'text-mode-hook 'visual-line-mode)

;; Kill emacs server if client frame is last one
(defun custom/last-frame-kill(frame)
  (if (= (length (frame-list)) 2) ; One for current frame and one for server
      (progn
        (save-some-buffers)
        (kill-emacs))))
(add-to-list 'delete-frame-functions 'custom/last-frame-kill)

;;;;;;;;;;;;;;;;;;;
;; MODE SETTINGS ;;
;;;;;;;;;;;;;;;;;;;

;; Smart mode line: Makes mode line cleaner
(use-package smart-mode-line
  :config
  (sml/setup)
  ; Convert file paths of ~/Projects/abc/ to :PROJ:ABC:
  (add-to-list 'sml/replacer-regexp-list
             '("^~/Projects/\\(\\w+\\)/"
               (lambda (s) (concat ":PROJ:" (upcase (match-string 1 s)) ":")))
             t)
  
  (add-to-list 'sml/replacer-regexp-list '("^~/Projects/" ":Proj:") t)
  (add-to-list 'sml/replacer-regexp-list '("src/" "SRC:") t)
  ; Only show flycheck and flyspell minor modes on the mode line
  ;(setq rm-whitelist (mapconcat 'identity '("Fly" "FlyC") "\\\|"))
  )
; Delight to remove items from mode line?

;; Print full list of minor modes
;(message rm--help-echo)

;; Add mode settings
(use-package markdown-mode
  :mode "\\.md\\'"
  :interpreter "markdown")
(use-package cuda-mode
  :mode "\\.cu\\'"
  :interpreter "cuda")
(use-package julia-mode
  :mode "\\.jl\\'"
  :interpreter "julia")
(use-package systemd
  :mode ("\\.service\\'" . systemd-mode)
  :interpreter ("systemd" . systemd-mode))
(use-package syslog-mode
  :mode "/var/log.*\\'"
  :interpreter "syslog")
(use-package arduino-mode
  :mode "\\.ino\\'"
  :interpreter "arduino")
(use-package yaml-mode
  :mode "\\.yml\\'"
  :interpreter "yaml")
(use-package glsl-mode
  :mode ("\\.vert\\'" "\\.frag\\'" "\\.glsl\\'" "\\.geom\\'")
  :interpreter "glsl")
(use-package plantuml-mode
  :interpreter "plantuml")

(setq cc-other-file-alist
      '(("\\.c"   (".h"))
       ("\\.cpp"   (".h"))
       ("\\.h"   (".c"".cpp"))
       ("\\.hpp" (".cpp"))))

(setq ff-search-directories
      '("." "../src" "../include"))

(global-set-key (kbd "M-t") 'ff-find-other-file)

;; Open .h files as C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Toggle between C and C++ mode-line
(defun cpp-mode-toggle ()
  "toggles between c-mode and c++-mode"
  (interactive)
  (cond ((string= major-mode "c-mode")
         (c++-mode))
        ((string= major-mode "c++-mode")
         (c-mode))))

;;;;;;;;;;;;;;;;;;;;;
;; GLOBAL PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;

;; Undo tree: Visualize undos
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;; Auto-highlight-symbol: Highlight current item
(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode 1)
  (ahs-set-idle-interval 0.1))

;; Should use electric mode
;; Autopair: Auto complete delimiters
(use-package autopair
  :config
  (autopair-global-mode 1))

;; Multiple Cursors: Multi line editing
(use-package multiple-cursors
  :ensure t
  :bind (("C-'" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this-symbol)
         ("C-<" . mc/unmark-next-like-this)
         ("C-;" . mc/mark-all-like-this)))

;; Flyspell: Spell checking
(use-package flyspell
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(defun custom/flyspell-next-word()
  (interactive)
  (flyspell-goto-next-error)
  (flyspell-popup-correct))

;; Jump to next misspelled word and pop-up correction
(use-package flyspell-popup
  :bind (:map flyspell-mode-map
              ("C-," . custom/flyspell-next-word)))

;; Flycheck: Semantic checking
(use-package flycheck
  :bind (:map flycheck-mode-map
              ("C-." . flycheck-next-error))
  :config
  (global-flycheck-mode 1))

;; Helm: Minibuffer completion
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :init
  (helm-mode 1)
  :config
  (setq helm-split-window-in-side-p t))

;; Helm-Swoop: Fast find within file
(use-package helm-swoop
  :bind (("M-s" . helm-swoop-without-pre-input)
         :map isearch-mode-map
         ("M-s" . helm-swoop-from-isearch)
         :map helm-swoop-map
         ("M-s" . helm-multi-swoop-current-mode-from-helm-swoop))
  :config
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-use-fuzzy-match t))

;; Projectile: Project manager
(use-package projectile
  :bind ("<f5>" . projectile-compile-project)
  :config
  (projectile-mode 1))

(use-package helm-projectile
  :bind ("C-c h" . helm-projectile))

;; Tramp: Remote client connection
(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

;; Multi-term: Terminal
(use-package multi-term
  :bind ("C-c t" . multi-term-dedicated-open)
  :init
  (add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000))))

;; Magit: Git control
(use-package magit
  :bind ("C-c g" . magit-status))

;; Treemacs: File explorer
;; (use-package treemacs)

;; ;; Treemacs-Projectile: Projectile mode for Treemacs
;; (use-package treemacs-projectile)

;; Org mode: Organization
(use-package org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (setq org-log-done t
        org-directory "~/org"
        org-support-shift-select t
        ;org-startup-truncated t
        org-startup-folded "showeverything"
        org-modules
        '(org-babel
          org-bibtex
          org-checklist)
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
   '((ditaa . t)
     (python . t)
     (sh . t)
     (plantuml . t)
     (latex . t)))
  ; Disable confirm execute (Could be dangerous)
  (setq org-confirm-babel-evaluate nil)
  ; Add jar paths
  (setq org-ditaa-jar-path
        (expand-file-name "/usr/bin/ditaa"))
  (setq org-plantuml-jar-path
        (expand-file-name "/usr/share/plantuml/plantuml.jar"))
  ; Display images inline after execution
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; PROGRAMMING PACKAGES ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ansi-color: Colorize compilation buffer
(use-package ansi-color
  :init
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(defun colorize-compilation-buffer ()
  (read-only-mode -1)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode 1))

;; Rainbow-delimiters: Rainbow colors for braces/parentheses
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Fic-mode: Highlights FIXMEs and TODOs
(use-package fic-mode
  :init
  (add-hook 'prog-mode-hook 'fic-mode))

;; Company: Autocompletion
(use-package company
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0))

;; Company-Statistics: Suggest most used completions first
(use-package company-statistics
  :init
  (add-hook 'company-mode-hook 'company-statistics-mode))

;; Company-Quickhelp: Add information about completions
(use-package company-quickhelp
  :init
  (add-hook 'company-mode-hook 'company-quickhelp-mode))

;; Company-C-Headers: Add c headers for autocompletion
(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; Add cmake autocompletion
(use-package company-cmake
  :init
  (add-to-list 'company-backends 'company-cmake))

;; Irony: Autocomplete engine
(use-package irony
  :init
  (add-hook 'c-mode-common-hook 'irony-mode)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; Company-Irony: Use Irony as backend for company
(use-package company-irony
  :init
  (add-to-list 'company-backends 'company-irony)
  :config
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

;; Add c headers to irony
(use-package company-irony-c-headers
  :init
  (add-to-list 'company-backends 'company-irony-c-headers))

;; Add Irony as backend for flymake
(use-package flycheck-irony
  :init
  (add-hook 'flychek-mode-hook #'flycheck-irony-setup))

;; Rtags: C/C++ Indexing
(use-package rtags
  :bind (:map c-mode-base-map
              ("M-." . rtags-find-symbol-at-point)
              ("M-," . rtags-find-references-at-point)
              ("M-/" . rtags-find-symbol))
  :config
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (rtags-enable-standard-keybindings))

;; Company backend from rtags
(use-package company-rtags
 :init
 (setq rtags-completions-enabled t)
 (add-to-list 'company-backends 'company-rtags))

;; Flycheck backend from rtags
(use-package flycheck-rtags)

;; Use helm for rtags
(use-package helm-rtags
  :init
  (setq rtags-use-helm t))

;; (defun custom/cmake-ide-run()
;;   (interactive)
;;   (shell-command (concat cmake-ide-build-dir "/" custom/cmake-ide-run-command)))

;; ;; Cmake-IDE: Adds in RTags support for CMake projects
;; (use-package cmake-ide
;;   :ensure t
;;   :bind (("<f7>" . cmake-ide-compile)
;;          ("<f8>" . custom/cmake-ide-run))
;;   :init
;;   (setq max-mini-window-height 1)
;;   (setq custom/cmake-ide-run-command "main")
;;   (cmake-ide-setup))

;; Load clang format file into string to be put
;; in the -style={} argument
;; Cannot handle all options (Multipart options)
(defun custom/load-format-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (setq custom/clang-style
          (replace-regexp-in-string "^---,[ ]*" "{"
           (replace-regexp-in-string ",[ ]*\.\.\.[ ,]*$" "}"
            (replace-regexp-in-string "[ ]*\n[ ]*" ", " (buffer-string)))))))

;; Use default style for formatting
(defun custom/clang-format-default ()
  (interactive)
  (clang-format-buffer custom/clang-style))

;; Clang format: Autoformat C/C++ with clang-format
(use-package clang-format
  :bind (:map c-mode-base-map
              ("M-f" . custom/clang-format-default))
  :init
  (defvar custom/clang-style "file")
  (custom/load-format-file "~/.emacs.d/clang-format")
  )

;; Py-Autopep8: Auto pep8 format python
(use-package py-autopep8
  :init
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

;; Python autocomplete
;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (setq jedi:environment-virtualenv (list (expand-file-name "~/.emacs.d/.python-environments/")))
;;   (add-to-list 'company-backends 'company-jedi)
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:use-shortcuts t)
;;   (defun custom/enable-company-jedi ()
;;     (add-to-list 'company-backends 'company-jedi))
;;   (add-hook 'python-mode-hook 'custom/enable-company-jedi))

;; LaTeX processing
(use-package tex-mik
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))

;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPLATE PACKAGES ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Yasnippet: Snippet expander
(use-package yasnippet
  :bind ("C-c s" . yas-insert-snippet)
  :init
  (add-hook 'term-mode-hook '(lambda () (yas-minor-mode -1)))
  :config
  (yas-global-mode 1)
  (setq yas-expand-only-for-last-commands '(self-insert-command))
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

;; Skeletor: Create project templates
(use-package skeletor
  :config
  (add-to-list 'skeletor-global-substitutions
               '("__USER-NAME__" . "David Hedin"))
  (skeletor-define-template "basic-cpp"
    :title "Basic C++ Project"
    :no-license? t)
  (skeletor-define-template "full-cpp"
    :title "Enhanced C++ Project"
    :default-license "mit"))

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

