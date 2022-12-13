;;; init-prog.el --- programming related settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (when (executable-find "rg")
    (setq grep-program "rg"))
  (when (executable-find "fd")
    (setq find-program "fd"))
  ;; show line numbers
  (global-display-line-numbers-mode)
  :hook
  ;; show pretty symbols
  (prog-mode . global-prettify-symbols-mode)
  ;; hide/show code and comment blocks
  (prog-mode . hs-minor-mode))

;; tree-sitter
(if (< emacs-major-version 29)
    (use-package rust-mode)
  (use-package emacs
    :ensure nil
    :config
    (require 'treesit)
    (defun nibon7/generate-treesit-language-source (lang)
      "Generate tree-sitter language source for specified language LANG."
      (let ((repo (format "tree-sitter-%s" lang))
	    (org "tree-sitter")
	    url)
	(cond ((eq lang 'cmake) (setq org "uyha"))
	      ((eq lang 'yaml) (setq org "ikatyang")))
	(setq url (format "https://github.com/%s/%s" org repo))
	(add-to-list 'treesit-language-source-alist `(,lang ,url) t)))
    (defun nibon7/build-and-install-treesit-grammars ()
      "Build and install all tree-sitter language grammars."
      (interactive)
      (dolist (lang '(bash c cmake cpp javascript json python rust toml yaml))
	(nibon7/generate-treesit-language-source lang)
	(treesit-install-language-grammar lang)
	(message "Language grammar for `%s' installed." lang)
	(sit-for 0.5)))
    (unless (file-exists-p
	     (expand-file-name "tree-sitter" user-emacs-directory))
      (nibon7/build-and-install-treesit-grammars))
    (when (treesit-ready-p 'c t)
      (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
      (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
      (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))
    (when (treesit-ready-p 'rust t)
      (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))
    (when (treesit-ready-p 'toml t)
      (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode)))
    (when (treesit-ready-p 'cmake t)
      (add-to-list 'auto-mode-alist
                   '("\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'" . cmake-ts-mode)))))

;; cargo
(use-package cargo)

;; markdown
(use-package markdown-mode)

;; orgmode
(use-package org)

;; yaml
(use-package yaml-mode)

;; auctex
(use-package tex
  :ensure auctex)

;; smartparens
(use-package smartparens
  :hook
  (prog-mode TeX-mode))

;; highlights delimiters according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode TeX-mode))

;; auto format
(use-package format-all
  :hook
  ((emacs-lisp-mode rust-ts-mode TeX-mode)
   (format-all-mode . format-all-ensure-formatter))
  :custom
  (format-all-mode-lighter " AF"))

;; magit
(use-package magit)

;; flyspell
(use-package flyspell
  :when (executable-find "aspell")
  :ensure nil
  :hook
  (prog-mode . flyspell-prog-mode)
  (git-commit-mode markdown-mode org-mode TeX-mode))

;; flymake
(use-package flymake
  :ensure nil
  :bind
  (:map flymake-mode-map
	("C-c n" . flymake-goto-next-error)
	("C-c p" . flymake-goto-prev-error))
  :hook prog-mode)

;; yasnippet
(use-package yasnippet
  :hook
  (after-init . yas-global-mode)
  :config
  (nibon7/delight 'yas-minor-mode " YAS"))

;; yasnippet snippets
(use-package yasnippet-snippets
  :after snippet)

;; auto completion
(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  (company-show-quick-access t)
  (company-lighter-base "AC"))

;; eglot
(use-package eglot
  :hook
  (c-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (TeX-mode . eglot-ensure)
  :bind
  (:map eglot-mode-map
	("C-c r" . eglot-rename)
	("C-c a" . eglot-code-actions)
	("C-c h" . eldoc))
  :init
  (setq read-process-output-max
	(* 1024 1024))
  :custom
  (eglot-report-progress t))

(use-package xcscope
  :when (executable-find "cscope")
  :bind
  ("C-c s f" . cscope-find-this-file)
  ("C-c s s" . cscope-find-this-symbol)
  ("C-c s t" . cscope-find-this-text-string)
  ("C-c s =" . cscope-find-assignments-to-this-symbol)
  ("C-c s d" . cscope-find-global-definiton)
  ("C-c s G" . cscope-find-global-definiton-no-prompting)
  ("C-c s c" . cscope-find-functions-calling-this-function)
  ("C-c s C" . cscope-find-called-functions)
  ("C-c s e" . cscope-find-egrep-pattern)
  ("C-c s i" . cscope-find-files-including-file)
  ("C-c s n" . cscope-history-forward-line)
  ("C-c s p" . cscope-history-backward-line)
  ("C-c s N" . cscope-history-foward-file)
  ("C-c s P" . cscope-history-backward-file)
  ("C-c s I" . cscope-index-files)
  ("C-c s u" . cscope-pop-mark)
  ("C-c s b" . cscope-display-buffer-toggle))

(provide 'init-prog)

;;; init-prog.el ends here
