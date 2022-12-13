;;; init-common.el --- common settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; delight builtin modes
(dolist (feature-alist '((eldoc . eldoc-mode)
			 (hideshow . hs-minor-mode)))
  (with-eval-after-load (car feature-alist)
    (diminish (cdr feature-alist))))

;; garbage collector magic hack
(use-package gcmh
  :diminish gcmh-mode
  :hook after-init)

;; vim-like key bindings
(use-package evil
  :hook after-init
  :config
  (evil-define-key 'normal 'evil-normal-state-map (kbd "gr")
    'xref-find-references)
  (evil-define-key 'normal 'evil-normal-state-map (kbd "C-g")
    (evil-define-command evil-show-current-file-name ()
      (message (buffer-file-name (current-buffer)))))
  :custom
  (evil-want-C-i-jump nil)
  (evil-undo-system 'undo-redo))

;; highlight the cursor whenever the window scrolls
(use-package beacon
  :hook after-init)

;; vertical interactive completion
(use-package vertico
  :hook after-init
  :custom
  (vertico-cycle t))

;; orderless completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; consult
(use-package consult
  :bind
  ([remap apropos] . consult-apropos)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap goto-line] . consult-goto-line)
  ([remap imenu] . consult-imenu)
  ([remap locate] . consult-locate)
  ([remap load-theme] . consult-theme)
  ([remap man] . consult-man)
  ([remap recentf-open-files] . consult-recent-file)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap yank-pop] . consult-yank-pop)
  ("C-c f" . consult-flymake)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<"
	consult-line-numbers-widen t
	consult-async-min-input 2
	consult-async-refresh-delay 0.15
	consult-async-input-throttle 0.2
	consult-async-input-debounce 0.1))

;; marginalia in the minibuffer
(use-package marginalia
  :hook after-init)

;; suggest next keys
(use-package which-key
  :diminish which-key-mode
  :hook after-init)

;; emoji
(use-package emojify
  :hook
  (after-init . global-emojify-mode))

;; mode line
(use-package doom-modeline
  :hook after-init
  :custom
  (doom-modeline-support-imenu t)
  (doom-modeline-minor-modes t)
  (doom-modeline-hud t))

;; theme
(use-package doom-themes
  :init
  (load-theme 'doom-monokai-classic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(provide 'init-common)

;;; init-common.el ends here
