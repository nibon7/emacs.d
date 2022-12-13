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
  :custom
  ;; c-ts-mode
  (c-ts-mode-indent-style 'linux)
  (c-ts-mode-indent-offset 8)
  :hook
  ;; show pretty symbols
  (prog-mode . global-prettify-symbols-mode)
  ;; hide/show code and comment blocks
  (prog-mode . hs-minor-mode))

;; tree-sitter
(use-package treesit-auto
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  :hook
  (prog-mode . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(bash blueprint c cmake cpp javascript json nu python rust toml yaml)))

;; projectile
(use-package projectile
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :hook
  (prog-mode . projectile-mode))

;; blamer
(use-package blamer
  :hook
  (prog-mode . blamer-mode))

;; cargo
(use-package cargo)

;; meson
(use-package meson-mode)

;; markdown
(use-package markdown-mode)

;; orgmode
(use-package org)

;; blueprint
(use-package blueprint-ts-mode)

;; nushell
(use-package nushell-ts-mode)

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
  (emacs-lisp-mode rust-mode rust-ts-mode TeX-mode)
  (format-all-mode . format-all-ensure-formatter)
  ((rust-mode rust-ts-mode) . (lambda ()
				(setq-local format-all-formatters
					    '(("Rust" (rustfmt "--edition" "2021"))))))
  :custom
  (format-all-mode-lighter " AF"))

;; magit
(use-package magit)

;; flyspell
(use-package flyspell
  :when (or (executable-find "hunspell")
	    (executable-find "aspell"))
  :ensure nil
  :hook
  (prog-mode . flyspell-prog-mode)
  (git-commit-mode markdown-mode org-mode TeX-mode))

;; flymake
(use-package flymake
  :ensure nil
  :bind
  (:map flymake-mode-map
	("C-c f n" . flymake-goto-next-error)
	("C-c f p" . flymake-goto-prev-error))
  :hook prog-mode)

;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode " YAS"
  :hook
  (after-init . yas-global-mode))

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
  (company-minimum-prefix-length 1)
  (company-lighter-base "AC"))

;; eglot
(use-package eglot
  :hook
  (c-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (TeX-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (nushell-ts-mode . eglot-ensure)
  :bind
  (:map eglot-mode-map
	("C-c r" . eglot-rename)
	("C-c a" . eglot-code-actions)
	("C-c s" . xref-find-apropos))
  :init
  (setq read-process-output-max
	(* 1024 1024))
  :config
  (add-to-list 'eglot-server-programs '((blueprint-mode blueprint-ts-mode) . ("blueprint-compiler" "lsp")))
  (add-to-list 'eglot-server-programs '((tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab")))
  :custom
  (eglot-report-progress t))

(provide 'init-prog)

;;; init-prog.el ends here
