;;; init.el --- nibon7's configuration entry point -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq user-init-file
      (or load-file-name (buffer-file-name)))

(setq user-emacs-directory
      (file-name-directory user-init-file))

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

;; load path
(add-to-list 'load-path
	     (expand-file-name "init" user-emacs-directory))

(require 'init-builtin)
(require 'init-package)
(require 'init-common)
(require 'init-prog)

(when (not (file-exists-p custom-file))
  (make-empty-file custom-file))

(load-file custom-file)

;;; init.el ends here
