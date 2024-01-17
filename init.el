(require 'org)
(org-babel-tangle-file
 (expand-file-name "init.org"
                   user-emacs-directory))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))  (require 'org)
  (org-babel-tangle-file
   (expand-file-name "init.org"
		     user-emacs-directory))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq custom-file
      (expand-file-name "custom.el"
                        user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(org-babel-load-file
 (expand-file-name "editor.org"
                   user-emacs-directory))
(org-babel-load-file
 (expand-file-name "programming.org"
                   user-emacs-directory))
