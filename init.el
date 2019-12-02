(package-initialize)
(require 'org)
(org-babel-tangle-file
 (expand-file-name "init.org"
                   user-emacs-directory))
(load-file
 (expand-file-name "package.el"
                   user-emacs-directory))

(setq custom-file
      (expand-file-name "custom.el"
                        user-emacs-directory))
(load custom-file :noerror)

(org-babel-load-file
 (expand-file-name "editor.org"
                   user-emacs-directory))

(org-babel-load-file
 (expand-file-name "org.org"
                   user-emacs-directory))

(org-babel-load-file
 (expand-file-name "programming.org"
                   user-emacs-directory))

(org-babel-load-file
 (expand-file-name "templates.org"
                   user-emacs-directory))

(defun custom/last-frame-kill(frame)
  (if (= (length (frame-list)) 2) ; One for current frame and one for server
      (progn
        (save-some-buffers)
        (kill-emacs))))
(add-to-list 'delete-frame-functions 'custom/last-frame-kill)
